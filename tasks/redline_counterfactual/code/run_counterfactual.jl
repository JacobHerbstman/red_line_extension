#=
Red Line Extension Counterfactual
==================================
Solves for the new spatial equilibrium after the RLE reduces commuting costs
for tracts near new stations.

Usage: julia run_counterfactual.jl <catchment_radius_m> <kappa_reduction_pct>
Example: julia run_counterfactual.jl 800 20

Inputs:
- model_fundamentals.csv (from inversion)
- model_parameters.csv (from inversion)
- commuting_matrix.csv (distances)
- chicago_tracts.gpkg (for tract centroids)

Outputs:
- counterfactual_r{radius}_k{reduction}.csv (tract-level changes)
- welfare_r{radius}_k{reduction}.csv (aggregate welfare)
=#

using CSV
using DataFrames
using Statistics
using LinearAlgebra
using Printf

#=============================================================================
                            PARAMETERS
=============================================================================#

# Read command line arguments
if length(ARGS) < 2
    error("Usage: julia run_counterfactual.jl <catchment_radius_m> <kappa_reduction_pct>")
end

const CATCHMENT_RADIUS = parse(Int, ARGS[1])  # meters
const KAPPA_REDUCTION = parse(Float64, ARGS[2]) / 100  # convert to decimal

println("="^70)
println("RED LINE EXTENSION COUNTERFACTUAL")
println("="^70)
println("\nSpecification:")
println("  Catchment radius: $(CATCHMENT_RADIUS) m")
println("  κ reduction: $(Int(KAPPA_REDUCTION * 100))%")

# Model parameters - MUST MATCH invert_model.jl!
# From PPML travel-time gravity: ν_time = 0.039/min → ν_dist = 0.078/km
const ν = 0.078
const ε = 6.83
const κ_param = ν / ε  # ≈ 0.0114
const α_housing = 0.30  # Housing expenditure share (1-α) in model notation
const GAMMA_LABOR = 0.65  # Labor share of production (for wage adjustment)

# Convergence
const TOL = 1e-6  # Relaxed for faster convergence
const MAX_ITER = 500

# RLE Station coordinates (lat, lon)
const RLE_STATIONS = [
    ("103rd Street", 41.7068, -87.6326),
    ("111th Street", 41.6926, -87.6326),
    ("Michigan Avenue", 41.6839, -87.6217),
    ("130th Street", 41.6534, -87.6046)
]

# Existing Red Line stations - these are the destinations that become more accessible
# Key downtown stations
const DOWNTOWN_STATIONS = [
    ("Roosevelt", 41.8674, -87.6266),
    ("Harrison", 41.8740, -87.6277),
    ("Jackson", 41.8781, -87.6277),
    ("Monroe", 41.8808, -87.6277),
    ("Lake", 41.8857, -87.6277),
    ("Grand", 41.8916, -87.6281),
    ("Chicago", 41.8967, -87.6285)
]

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

function is_downtown_tract(lat, lon)
    """Check if tract centroid is near downtown (within 3km of Loop)."""
    loop_lat, loop_lon = 41.8781, -87.6298  # Jackson/State
    d = haversine_distance(lat, lon, loop_lat, loop_lon)
    return d < 3000  # 3km
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
Φ_baseline_loaded = params_df[params_df.parameter .== "Phi", :value][1]
println("  Baseline Φ (from inversion): $(Φ_baseline_loaded)")

# Load commuting matrix
commuting = CSV.read(joinpath(input_dir, "commuting_matrix.csv"), DataFrame)

# Create tract index mapping (convert to strings for consistent matching)
tracts = string.(fundamentals.tract)
tract_to_idx = Dict(t => i for (i, t) in enumerate(tracts))

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
                    BUILD DISTANCE MATRIX
=============================================================================#

println("\nBuilding distance matrix...")

# Initialize distance matrix
D = fill(50.0, N, N)  # Default to 50km for missing pairs

# Fill from commuting data (convert tract IDs to strings for matching)
for row in eachrow(commuting)
    origin_str = string(row.origin_tract)
    dest_str = string(row.dest_tract)
    if haskey(tract_to_idx, origin_str) && haskey(tract_to_idx, dest_str)
        n = tract_to_idx[origin_str]
        i = tract_to_idx[dest_str]
        D[n, i] = max(row.distance_km, 0.5)  # Minimum 0.5km
    end
end

# Set diagonal (within-tract) to small value
for n in 1:N
    D[n, n] = 0.5
end

println("  Mean distance: $(mean(D)) km")

#=============================================================================
                    COMPUTE TREATMENT INTENSITY (Continuous Decay)
=============================================================================#

println("\nComputing treatment intensity with spatial decay...")

# For each tract, compute a continuous intensity based on distance to nearest RLE station
# Using Gaussian decay: intensity = exp(-(distance^2) / (2 * σ^2))
# where σ = CATCHMENT_RADIUS defines the spatial reach

tract_station_dist = zeros(N)
intensity = zeros(N)

for n in 1:N
    lat, lon = tract_lats[n], tract_lons[n]
    _, dist = find_nearest_station(lat, lon, RLE_STATIONS)
    tract_station_dist[n] = dist
    # Gaussian decay with catchment radius as σ
    intensity[n] = exp(-(dist^2) / (2 * CATCHMENT_RADIUS^2))
end

# For reporting, count "treated" as tracts with > 10% intensity
treated_tracts = findall(intensity .> 0.1)
println("  Tracts with intensity > 10%: $(length(treated_tracts))")
println("  Mean intensity: $(round(mean(intensity), digits=4))")
println("  Max intensity: $(round(maximum(intensity), digits=4))")

# Error check - make sure we have treatment effect
if maximum(intensity) < 0.01
    println("  ERROR: No tracts have meaningful treatment intensity!")
    println("  Missing centroid tracts:")
    for t in missing_centroid_tracts
        println("    - $t")
    end
    error("No meaningful treatment intensity. Check tract centroids match RLE station locations.")
end

#=============================================================================
                    IDENTIFY DESTINATION TRACTS
=============================================================================#

# Destinations that benefit = tracts near Red Line (especially downtown)
println("\nIdentifying destination tracts (downtown/Red Line accessible)...")

downtown_tracts = Int[]
for n in 1:N
    lat, lon = tract_lats[n], tract_lons[n]
    if is_downtown_tract(lat, lon)
        push!(downtown_tracts, n)
    end
end

println("  Downtown tracts: $(length(downtown_tracts))")

#=============================================================================
                    COMPUTE κ CHANGE MATRIX
=============================================================================#

println("\nComputing commuting cost changes...")

# κ_hat[n,i] = ratio of new to old iceberg cost
# Using continuous intensity for smooth spatial spillovers:
# κ_hat[n,i] = exp(-κ_param * D[n,i] * KAPPA_REDUCTION * intensity[n])
# This ensures:
#   - κ_hat <= 1 always (cost reduction, not increase)
#   - Smooth spatial decay of treatment effect
#   - Larger reductions for closer tracts and longer commutes

κ_hat = ones(N, N)

for n in 1:N
    if intensity[n] > 1e-6  # Only compute for tracts with meaningful intensity
        for i in downtown_tracts
            # Apply weighted reduction based on tract's intensity
            κ_hat[n, i] = exp(-κ_param * D[n, i] * KAPPA_REDUCTION * intensity[n])
        end
    end
end

# Summary stats
n_affected_tracts = sum(intensity .> 1e-6)
n_treated_pairs = n_affected_tracts * length(downtown_tracts)
affected_kappa_hats = [κ_hat[n, i] for n in 1:N for i in downtown_tracts if intensity[n] > 1e-6]
println("  Tracts with non-zero intensity: $(n_affected_tracts)")
println("  Affected (origin, destination) pairs: $(n_treated_pairs)")
if length(affected_kappa_hats) > 0
    println("  κ_hat for affected pairs: mean=$(round(mean(affected_kappa_hats), digits=4)), min=$(round(minimum(affected_kappa_hats), digits=4)), max=$(round(maximum(affected_kappa_hats), digits=4))")
end

#=============================================================================
                    COMPUTE BASELINE κ^(-ε) MATRIX
=============================================================================#

# Baseline commuting costs
κ_baseline = exp.(κ_param .* D)
κ_neg_ε_baseline = κ_baseline .^ (-ε)

#=============================================================================
                    SOLVE COUNTERFACTUAL EQUILIBRIUM
=============================================================================#

println("\nSolving counterfactual equilibrium...")

# We use the exact hat algebra approach
# 
# The key equations in changes (hat = new/old):
#
# 1. Commuting market access:
#    W_n' = Σ_i κ_ni^(-ε) * w_i^ε * κ_hat_ni^(-ε) * w_hat_i^ε
#
# 2. Labor market clearing:
#    L_i^M * L_hat_i^M = Σ_n λ_ni|n * L_n^R * λ_hat_ni|n * L_hat_n^R
#
# 3. Land market clearing:
#    Q_hat_n = v_bar_hat_n * L_hat_n^R
#
# 4. Population:
#    Σ_n L_n^R * L_hat_n^R = L_bar (unchanged)
#
# Now solving with ENDOGENOUS wages for full general equilibrium.

# Initialize
w_hat = ones(N)  # Now endogenous - will be updated in iteration
Q_hat = ones(N)
L_R_hat = ones(N)
L_M_hat = ones(N)

# Precompute baseline objects
w_ε = w .^ ε
W_baseline = κ_neg_ε_baseline * w_ε

# Baseline conditional commuting probabilities λ_ni|n
λ_cond = zeros(N, N)
for n in 1:N
    for i in 1:N
        λ_cond[n, i] = κ_neg_ε_baseline[n, i] * w_ε[i] / W_baseline[n]
    end
end

# Baseline expected income
v_bar_baseline = zeros(N)
for n in 1:N
    v_bar_baseline[n] = sum(λ_cond[n, i] * w[i] for i in 1:N)
end

# Iterate to find new equilibrium
println("  Iterating...")

for iter in 1:MAX_ITER
    # Step 1: Compute new commuting market access W_hat
    # W_n' = Σ_i κ_ni^(-ε) * κ_hat_ni^(-ε) * w_i^ε * w_hat_i^ε
    W_new = zeros(N)
    for n in 1:N
        for i in 1:N
            W_new[n] += κ_neg_ε_baseline[n, i] * (κ_hat[n, i]^(-ε)) * w_ε[i] * (w_hat[i]^ε)
        end
    end
    W_hat = W_new ./ W_baseline
    
    # Step 2: Compute new conditional commuting probabilities
    λ_cond_new = zeros(N, N)
    for n in 1:N
        for i in 1:N
            λ_cond_new[n, i] = κ_neg_ε_baseline[n, i] * (κ_hat[n, i]^(-ε)) * w_ε[i] * (w_hat[i]^ε) / W_new[n]
        end
    end
    
    # Step 3: Compute new expected income
    v_bar_new = zeros(N)
    for n in 1:N
        v_bar_new[n] = sum(λ_cond_new[n, i] * w[i] * w_hat[i] for i in 1:N)
    end
    v_bar_hat = v_bar_new ./ v_bar_baseline
    
    # Step 4: Residential choice - compute new residential probabilities
    # λ_n^R ∝ B_n * Q_n^(-(1-α)ε) * W_n where (1-α) = α_housing = housing share
    # λ_n^R_hat ∝ Q_hat_n^(-(1-α)ε) * W_hat_n (B unchanged)

    λ_R_hat_unnorm = (Q_hat .^ (-α_housing*ε)) .* W_hat
    λ_R_hat_sum = sum((L_R ./ sum(L_R)) .* λ_R_hat_unnorm)
    
    # New residential population (preserving total)
    L_R_hat_new = λ_R_hat_unnorm ./ λ_R_hat_sum
    
    # Step 5: Land market clearing - update prices
    # Q_hat = v_bar_hat * L_R_hat (since H_R is fixed)
    Q_hat_new = v_bar_hat .* L_R_hat_new
    
    # Step 6: Labor market clearing - compute new workplace employment
    L_M_new = zeros(N)
    for i in 1:N
        for n in 1:N
            L_M_new[i] += λ_cond_new[n, i] * L_R[n] * L_R_hat_new[n]
        end
    end
    L_M_hat_new = L_M_new ./ max.(L_M, 1.0)

    # Step 7: WAGE ADJUSTMENT via labor demand curve
    # Using diminishing returns to labor: w = A * L^(γ-1) where γ = GAMMA_LABOR
    # So w_hat = L_hat^(γ-1)
    # This is simpler and converges faster than the full GE approach
    w_hat_new = L_M_hat_new .^ (GAMMA_LABOR - 1.0)
    w_hat_new = w_hat_new ./ mean(w_hat_new)  # Normalize mean wage = 1

    # Check convergence (now including wages)
    diff_Q = maximum(abs.(Q_hat_new .- Q_hat))
    diff_L = maximum(abs.(L_R_hat_new .- L_R_hat))
    diff_w = maximum(abs.(w_hat_new .- w_hat))

    if iter % 100 == 0 || iter == 1
        @printf("  Iter %4d: ΔQ=%.2e, ΔL=%.2e, Δw=%.2e\n", iter, diff_Q, diff_L, diff_w)
    end

    # Update with damping for stability
    global Q_hat = 0.5 * Q_hat + 0.5 * Q_hat_new
    global L_R_hat = 0.5 * L_R_hat + 0.5 * L_R_hat_new
    global L_M_hat = L_M_hat_new
    global w_hat = 0.5 * w_hat + 0.5 * w_hat_new

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

# Welfare: U_bar ∝ Φ^(1/ε)
# Φ = Σ_n Σ_i B_n * κ_ni^(-ε) * Q_n^(-(1-α)ε) * w_i^ε
#
# Φ'/Φ = Σ_n Σ_i [share_ni] * κ_hat_ni^(-ε) * Q_hat_n^(-(1-α)ε) * w_hat_i^ε

# Compute baseline Φ (for verification against loaded value)
# Note: (1-α) = α_housing = housing expenditure share
Q_term_baseline = Q .^ (-α_housing*ε)
Φ_check = sum(B[n] * κ_neg_ε_baseline[n,i] * Q_term_baseline[n] * w_ε[i] for n in 1:N, i in 1:N)

# Verify consistency with inversion
Φ_diff_pct = abs(Φ_check - Φ_baseline_loaded) / Φ_baseline_loaded * 100
println("  Φ computed here: $(Φ_check)")
println("  Φ from inversion: $(Φ_baseline_loaded)")
println("  Difference: $(round(Φ_diff_pct, digits=4))%")
if Φ_diff_pct > 1.0
    println("  WARNING: Large discrepancy in Φ - check that input data matches inversion!")
end

# Compute Φ_new at counterfactual equilibrium
Φ_new = sum(B[n] * κ_neg_ε_baseline[n,i] * (κ_hat[n,i]^(-ε)) *
            (Q[n] * Q_hat[n])^(-α_housing*ε) * (w[i] * w_hat[i])^ε
            for n in 1:N, i in 1:N)

Φ_hat = Φ_new / Φ_check
welfare_change_pct = (Φ_hat^(1/ε) - 1) * 100

println("  Φ_new: $(Φ_new)")
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
@printf("  Mean w_hat: %.4f (should be ~1)\n", mean(w_hat))
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

output_file = joinpath(output_dir, "counterfactual_r$(CATCHMENT_RADIUS)_k$(Int(KAPPA_REDUCTION*100)).csv")
CSV.write(output_file, results_df)
println("  Saved: $(output_file)")

# Welfare summary
welfare_df = DataFrame(
    catchment_radius_m = CATCHMENT_RADIUS,
    kappa_reduction_pct = KAPPA_REDUCTION * 100,
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

welfare_file = joinpath(output_dir, "welfare_r$(CATCHMENT_RADIUS)_k$(Int(KAPPA_REDUCTION*100)).csv")
CSV.write(welfare_file, welfare_df)
println("  Saved: $(welfare_file)")

println("\nDone!")
