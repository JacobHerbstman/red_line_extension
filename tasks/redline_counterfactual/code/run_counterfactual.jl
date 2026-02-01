# Red Line Extension counterfactual with exact hat algebra and endogenous wages.

using CSV
using DataFrames
using Statistics
using Printf

#=============================================================================
                            PARAMETERS
=============================================================================#

# Read command line arguments
if length(ARGS) < 2
    error("Usage: julia run_counterfactual.jl <catchment_radius_m> <kappa_reduction_pct>")
end

const CATCHMENT_RADIUS = parse(Int, ARGS[1])  # retained for filename compatibility
const SHOCK_LABEL_PCT = Int(round(parse(Float64, ARGS[2])))
if SHOCK_LABEL_PCT != 100
    error("This specification now applies the full GTFS shock. Pass kappa_reduction_pct = 100.")
end
const KAPPA_REDUCTION = 1.0

println("="^70)
println("RED LINE EXTENSION COUNTERFACTUAL")
println("="^70)
println("\nSpecification:")
println("  Catchment radius: $(CATCHMENT_RADIUS) m")
println("  GTFS shock scaling: $(Int(KAPPA_REDUCTION * 100))% (full shock)")

# Defaults are overridden by model_parameters.csv when available.
ν = 0.039
ε = 6.83
const α_housing = 0.30
GAMMA_LABOR = 0.65  # Labor share of production

const TOL = 1e-6
const MAX_ITER = 500
const DAMP = 0.5
const L_M_FLOOR = 1.0

# RLE Station coordinates (lat, lon)
const RLE_STATIONS = [
    ("103rd Street", 41.7068, -87.6326),
    ("111th Street", 41.6926, -87.6326),
    ("Michigan Avenue", 41.6839, -87.6217),
    ("130th Street", 41.6534, -87.6046)
]

const MIN_TRAVEL_TIME = 5.0       # minutes for within-tract minimum

#=============================================================================
                            HELPER FUNCTIONS
=============================================================================#

function haversine_distance(lat1, lon1, lat2, lon2)
    """Compute distance in meters between two lat/lon points."""
    R = 6371000.0  # Earth radius in meters
    φ1, φ2 = deg2rad(lat1), deg2rad(lat2)
    Δφ = deg2rad(lat2 - lat1)
    Δλ = deg2rad(lon2 - lon1)
    
    a = sin(Δφ/2)^2 + cos(φ1) * cos(φ2) * sin(Δλ/2)^2
    c = 2 * atan(sqrt(a), sqrt(1-a))
    
    return R * c
end

function find_nearest_station(lat, lon, stations)
    """Find the nearest station and return (name, distance_m)."""
    min_dist = Inf
    nearest = ""
    for (name, slat, slon) in stations
        d = haversine_distance(lat, lon, slat, slon)
        if d < min_dist
            min_dist = d
            nearest = name
        end
    end
    return nearest, min_dist
end

function parse_tt(x)
    if x === missing
        return missing
    elseif x isa AbstractString
        s = strip(x)
        if s == "" || uppercase(s) == "NA"
            return missing
        end
        v = tryparse(Float64, s)
        return v === nothing ? missing : v
    else
        return Float64(x)
    end
end


#=============================================================================
                            LOAD DATA
=============================================================================#

println("\nLoading data...")

# Paths
code_dir = @__DIR__
task_dir = dirname(code_dir)
input_dir = joinpath(task_dir, "input")
output_dir = joinpath(task_dir, "output")

# Load model fundamentals
fundamentals = CSV.read(joinpath(input_dir, "model_fundamentals.csv"), DataFrame)
N = nrow(fundamentals)
println("  Loaded $(N) tracts")

# Load tract centroids from floorspace file
floorspace = CSV.read(joinpath(input_dir, "tract_floorspace.csv"), DataFrame)
# Convert tract IDs to strings for consistent matching across files
centroids = Dict(string(row.census_tract_geoid) => (row.centroid_lat, row.centroid_lon)
                 for row in eachrow(floorspace))

# Load parameters
params_df = CSV.read(joinpath(input_dir, "model_parameters.csv"), DataFrame)

# Override parameters from inversion if available
nu_row = params_df[params_df.parameter .== "nu_time_per_min", :value]
eps_row = params_df[params_df.parameter .== "epsilon", :value]
gamma_row = params_df[params_df.parameter .== "gamma_labor", :value]
if length(nu_row) > 0
    ν = nu_row[1]
end
if length(eps_row) > 0
    ε = eps_row[1]
end
if length(gamma_row) > 0
    GAMMA_LABOR = gamma_row[1]
end

Φ_baseline_loaded = params_df[params_df.parameter .== "Phi", :value][1]
println("  Baseline Φ (from inversion): $(Φ_baseline_loaded)")
println("  Using ν = $(round(ν, digits=5)) per minute, ε = $(round(ε, digits=4)), γ = $(round(GAMMA_LABOR, digits=3))")

# Load travel time change matrix (baseline vs extension)
tt_change = CSV.read(joinpath(input_dir, "travel_time_change.csv"), DataFrame)

# Create tract index mapping (convert to strings for consistent matching)
tracts = string.(fundamentals.tract)
tract_to_idx = Dict(t => i for (i, t) in enumerate(tracts))

# Load baseline conditional commuting shares (exact hat algebra)
lambda_df = CSV.read(joinpath(input_dir, "commuting_shares_model.csv"), DataFrame)
lambda_cond = zeros(N, N)
for row in eachrow(lambda_df)
    origin_str = string(row.origin_tract)
    dest_str = string(row.dest_tract)
    if haskey(tract_to_idx, origin_str) && haskey(tract_to_idx, dest_str)
        n = tract_to_idx[origin_str]
        i = tract_to_idx[dest_str]
        lambda_cond[n, i] = row.lambda_cond
    end
end

# Extract baseline values
w = fundamentals.wage
Q = fundamentals.floor_price
B = fundamentals.amenity
L_R = fundamentals.residents_observed
L_M = fundamentals.workers_observed
H_R = fundamentals.floor_space_residential
W_access = fundamentals.commuting_access

# Get tract centroids from the centroids dict we loaded
tract_lats = zeros(N)
tract_lons = zeros(N)
missing_centroid_tracts = String[]
for (idx, t) in enumerate(tracts)
    if haskey(centroids, t)
        tract_lats[idx], tract_lons[idx] = centroids[t]
    else
        # Fallback for missing tracts - use location far from RLE
        tract_lats[idx] = 41.85
        tract_lons[idx] = -87.65
        push!(missing_centroid_tracts, t)
    end
end
n_missing_centroids = length(missing_centroid_tracts)
if n_missing_centroids > 0
    println("  Warning: $(n_missing_centroids) tracts missing centroids:")
    println("  GEOIDs of missing tracts:")
    for t in missing_centroid_tracts
        println("    - $t")
    end
    println("  WARNING: These tracts won't be identified as treated!")
end
println("  Loaded centroids for $(N - n_missing_centroids) of $N tracts")

#=============================================================================
                    BUILD TRAVEL TIME MATRICES
=============================================================================#

println("\nBuilding travel time matrices...")

T_baseline = fill(Inf, N, N)
T_extension = fill(Inf, N, N)

for row in eachrow(tt_change)
    origin_str = string(row.origin_tract)
    dest_str = string(row.dest_tract)
    if haskey(tract_to_idx, origin_str) && haskey(tract_to_idx, dest_str)
        n = tract_to_idx[origin_str]
        i = tract_to_idx[dest_str]

        base = parse_tt(row.travel_time_baseline)
        ext = parse_tt(row.travel_time_extension)

        if ismissing(base) && !ismissing(ext)
            base = ext
        end
        if ismissing(ext) && !ismissing(base)
            ext = base
        end

        if !ismissing(base) && !ismissing(ext)
            T_baseline[n, i] = max(base, MIN_TRAVEL_TIME)
            T_extension[n, i] = max(ext, MIN_TRAVEL_TIME)
        end
    end
end

# Ensure within-tract times are at least MIN_TRAVEL_TIME
for n in 1:N
    T_baseline[n, n] = max(T_baseline[n, n], MIN_TRAVEL_TIME)
    T_extension[n, n] = max(T_extension[n, n], MIN_TRAVEL_TIME)
end

# Impute missing travel times using a high percentile of observed baseline times
finite_base = T_baseline[isfinite.(T_baseline)]
impute_tt = quantile(finite_base, 0.99)
T_baseline[.!isfinite.(T_baseline)] .= impute_tt
T_extension[.!isfinite.(T_extension)] .= impute_tt

ΔT = T_extension .- T_baseline
ΔT_effective = ΔT .* KAPPA_REDUCTION

finite_base = T_baseline[isfinite.(T_baseline)]
finite_ext = T_extension[isfinite.(T_extension)]
println("  Mean baseline travel time (finite): $(mean(finite_base)) minutes")
println("  Mean extension travel time (finite): $(mean(finite_ext)) minutes")
println("  Mean change: $(mean(ΔT_effective)) minutes")

#=============================================================================
                    COMPUTE TREATMENT INTENSITY (FROM GTFS TIME CHANGES)
=============================================================================#

println("\nComputing treatment intensity from GTFS travel-time changes...")

# Distance to nearest new station (for reporting/maps only)
tract_station_dist = zeros(N)
for n in 1:N
    lat, lon = tract_lats[n], tract_lons[n]
    _, dist = find_nearest_station(lat, lon, RLE_STATIONS)
    tract_station_dist[n] = dist
end

# Intensity based on mean improvement across destinations (scaled ΔT)
improve = max.(0.0, -ΔT_effective)
mean_improve = [mean(improve[n, :]) for n in 1:N]
max_improve = maximum(mean_improve)
intensity = max_improve > 0 ? mean_improve ./ max_improve : zeros(N)

treated_tracts = findall(intensity .> 0.1)
println("  Tracts with intensity > 10%: $(length(treated_tracts))")
println("  Mean intensity: $(round(mean(intensity), digits=4))")
println("  Max intensity: $(round(maximum(intensity), digits=4))")

# Summary stats for treated pairs
n_treated_pairs = sum(ΔT_effective .< 0)
println("  Improved OD pairs (ΔT < 0): $(n_treated_pairs)")

# κ_hat^{-ε} directly from ΔT (minutes)
κ_hat_neg_ε = exp.(-ν .* ΔT_effective)

#=============================================================================
                    SOLVE COUNTERFACTUAL EQUILIBRIUM
=============================================================================#

println("\nSolving counterfactual equilibrium...")

# Initialize
w_hat = ones(N)
Q_hat = ones(N)
L_R_hat = ones(N)
L_M_hat = ones(N)

v_bar_baseline = lambda_cond * w

println("  Iterating...")

θ = α_housing * ε
s_R = L_R ./ sum(L_R)

for iter in 1:MAX_ITER
    global w_hat, Q_hat, L_R_hat, L_M_hat
    w_hat_prev = copy(w_hat)
    L_R_hat_prev = copy(L_R_hat)
    Q_hat_prev = copy(Q_hat)

    adj = κ_hat_neg_ε .* (w_hat .^ ε)'
    lambda_weighted = lambda_cond .* adj
    W_hat = vec(sum(lambda_weighted, dims = 2))
    W_hat = max.(W_hat, 1e-12)
    lambda_cond_new = lambda_weighted ./ W_hat

    L_M_new = lambda_cond_new' * (L_R .* L_R_hat)
    L_M_hat_new = L_M_new ./ max.(L_M, L_M_FLOOR)
    L_M_hat_new = max.(L_M_hat_new, 1e-8)
    w_hat_new = L_M_hat_new .^ (GAMMA_LABOR - 1.0)
    w_hat = (1.0 - DAMP) .* w_hat .+ DAMP .* w_hat_new

    adj = κ_hat_neg_ε .* (w_hat .^ ε)'
    lambda_weighted = lambda_cond .* adj
    W_hat = vec(sum(lambda_weighted, dims = 2))
    W_hat = max.(W_hat, 1e-12)
    lambda_cond_new = lambda_weighted ./ W_hat

    v_bar_new = lambda_cond_new * (w .* w_hat)
    v_bar_hat = v_bar_new ./ v_bar_baseline

    x_lr = (v_bar_hat .^ (-θ)) .* W_hat
    x_lr = max.(x_lr, 1e-12)
    L_R_hat_new = x_lr .^ (1.0 / (1.0 + θ))
    scale_lr = 1.0 / sum(s_R .* L_R_hat_new)
    L_R_hat_new .= L_R_hat_new .* scale_lr
    L_R_hat = (1.0 - DAMP) .* L_R_hat .+ DAMP .* L_R_hat_new
    L_R_hat .= L_R_hat .* (1.0 / sum(s_R .* L_R_hat))

    Q_hat = v_bar_hat .* L_R_hat
    Q_hat = max.(Q_hat, 1e-8)

    L_M_new = lambda_cond_new' * (L_R .* L_R_hat)
    L_M_hat = L_M_new ./ max.(L_M, L_M_FLOOR)
    L_M_hat = max.(L_M_hat, 1e-8)

    diff_Q = maximum(abs.(Q_hat .- Q_hat_prev))
    diff_L = maximum(abs.(L_R_hat .- L_R_hat_prev))
    diff_w = maximum(abs.(w_hat .- w_hat_prev))

    if iter % 100 == 0 || iter == 1
        @printf("  Iter %4d: ΔQ=%.2e, ΔL=%.2e, Δw=%.2e\n", iter, diff_Q, diff_L, diff_w)
    end

    if max(diff_Q, diff_L, diff_w) < TOL
        @printf("  Converged after %d iterations\n", iter)
        break
    end

    if iter == MAX_ITER
        println("  WARNING: Did not converge after $(MAX_ITER) iterations")
    end
end

#=============================================================================
                    COMPUTE WELFARE CHANGE
=============================================================================#

println("\nComputing welfare change...")

# Compute baseline Φ for a consistency check against inversion output.
Q_term_baseline = Q .^ (-α_housing * ε)
Φ_check = sum(B .* Q_term_baseline .* W_access)

# Verify consistency with inversion
Φ_diff_pct = abs(Φ_check - Φ_baseline_loaded) / Φ_baseline_loaded * 100
println("  Φ computed here: $(Φ_check)")
println("  Φ from inversion: $(Φ_baseline_loaded)")
println("  Difference: $(round(Φ_diff_pct, digits=4))%")
if Φ_diff_pct > 1.0
    println("  WARNING: Large discrepancy in Φ - check that input data matches inversion!")
end

# Compute Φ_hat using exact hat algebra
s_R = L_R ./ sum(L_R)
row_term = s_R .* (Q_hat .^ (-α_housing * ε))
col_term = w_hat .^ ε
Φ_hat = sum(lambda_cond .* κ_hat_neg_ε .* row_term .* (col_term'))
Φ_new = Φ_check * Φ_hat
welfare_change_pct = (Φ_hat^(1/ε) - 1) * 100

println("  Φ_hat: $(Φ_hat)")
println("  Welfare change: $(round(welfare_change_pct, digits=4))%")

#=============================================================================
                    SUMMARIZE RESULTS
=============================================================================#

println("\n" * "="^70)
println("RESULTS SUMMARY")
println("="^70)

# Changes for treated tracts (intensity > 10%)
treated_mask = intensity .> 0.1

println("\nTreated tracts (intensity > 10%, n = $(sum(treated_mask))):")
@printf("  Mean Q_hat: %.4f (%.2f%% change)\n", mean(Q_hat[treated_mask]), (mean(Q_hat[treated_mask])-1)*100)
@printf("  Mean L_R_hat: %.4f (%.2f%% change)\n", mean(L_R_hat[treated_mask]), (mean(L_R_hat[treated_mask])-1)*100)
@printf("  Mean w_hat: %.4f (%.2f%% change)\n", mean(w_hat[treated_mask]), (mean(w_hat[treated_mask])-1)*100)

println("\nAll tracts (n = $N):")
@printf("  Mean Q_hat: %.4f (%.2f%% change)\n", mean(Q_hat), (mean(Q_hat)-1)*100)
@printf("  Mean L_R_hat: %.4f (%.2f%% change)\n", mean(L_R_hat), (mean(L_R_hat)-1)*100)
@printf("  Mean w_hat: %.4f\n", mean(w_hat))
@printf("  Total welfare change: %.4f%%\n", welfare_change_pct)

#=============================================================================
                    SAVE RESULTS
=============================================================================#

println("\nSaving results...")

# Tract-level results
results_df = DataFrame(
    tract = tracts,
    intensity = intensity,
    wage_baseline = w,
    w_hat = w_hat,
    wage_new = w .* w_hat,
    w_pct_change = (w_hat .- 1) .* 100,
    floor_price_baseline = Q,
    amenity = B,
    residents_baseline = L_R,
    workers_baseline = L_M,
    floor_space = H_R,
    Q_hat = Q_hat,
    L_R_hat = L_R_hat,
    L_M_hat = L_M_hat,
    Q_new = Q .* Q_hat,
    L_R_new = L_R .* L_R_hat,
    L_M_new = L_M .* L_M_hat,
    Q_pct_change = (Q_hat .- 1) .* 100,
    L_R_pct_change = (L_R_hat .- 1) .* 100,
    treated = treated_mask,
    dist_to_rle_station = tract_station_dist
)

output_file = joinpath(output_dir, "counterfactual_r$(CATCHMENT_RADIUS)_k$(SHOCK_LABEL_PCT).csv")
CSV.write(output_file, results_df)
println("  Saved: $(output_file)")

# Welfare summary
welfare_df = DataFrame(
    catchment_radius_m = CATCHMENT_RADIUS,
    kappa_reduction_pct = SHOCK_LABEL_PCT,
    n_treated_tracts = length(treated_tracts),
    n_treated_pairs = n_treated_pairs,
    Phi_baseline = Φ_check,
    Phi_new = Φ_new,
    Phi_hat = Φ_hat,
    welfare_change_pct = welfare_change_pct,
    mean_Q_hat_treated = mean(Q_hat[treated_mask]),
    mean_L_R_hat_treated = mean(L_R_hat[treated_mask]),
    mean_w_hat_treated = mean(w_hat[treated_mask]),
    mean_Q_hat_all = mean(Q_hat),
    mean_L_R_hat_all = mean(L_R_hat),
    mean_w_hat_all = mean(w_hat)
)

welfare_file = joinpath(output_dir, "welfare_r$(CATCHMENT_RADIUS)_k$(SHOCK_LABEL_PCT).csv")
CSV.write(welfare_file, welfare_df)
println("  Saved: $(welfare_file)")

println("\nDone!")
