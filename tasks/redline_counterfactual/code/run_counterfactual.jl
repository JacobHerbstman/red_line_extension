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

# Model parameters
const ν = 0.11
const ε = 6.83
const κ_param = ν / ε
const α_housing = 0.30  # Housing expenditure share (1-α) in model notation

# Convergence
const TOL = 1e-8
const MAX_ITER = 1000

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
    for t in missing_centroid_tracts[1:min(5, n_missing_centroids)]
        println("    - $t")
    end
    if n_missing_centroids > 5
        println("    ... and $(n_missing_centroids - 5) more")
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
                    IDENTIFY TREATED TRACTS
=============================================================================#

println("\nIdentifying treated tracts...")

# For each tract, find distance to nearest RLE station
treated_tracts = Int[]
tract_station_dist = zeros(N)

for n in 1:N
    # Get tract centroid
    # We need to get this from the data - for now use a lookup
    lat, lon = tract_lats[n], tract_lons[n]
    
    # Find nearest RLE station
    nearest, dist = find_nearest_station(lat, lon, RLE_STATIONS)
    tract_station_dist[n] = dist
    
    if dist <= CATCHMENT_RADIUS
        push!(treated_tracts, n)
    end
end

println("  Tracts within $(CATCHMENT_RADIUS)m of RLE station: $(length(treated_tracts))")

if length(treated_tracts) == 0
    println("  WARNING: No treated tracts found! Check coordinates.")
    # Print some diagnostics
    println("  Sample tract coordinates:")
    for n in 1:min(5, N)
        println("    Tract $(tracts[n]): ($(tract_lats[n]), $(tract_lons[n]))")
    end
    println("  RLE stations:")
    for (name, lat, lon) in RLE_STATIONS
        println("    $name: ($lat, $lon)")
    end
end

#=============================================================================
                    IDENTIFY DESTINATION TRACTS
=============================================================================#

# Destinations that benefit = tracts near Red Line (especially downtown)
# For simplicity, we'll apply the reduction to commutes to downtown tracts

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

# κ_hat[n,i] = κ'[n,i] / κ[n,i]
# = 1 for non-treated pairs
# = (1 - KAPPA_REDUCTION) for treated origin n to downtown destination i

κ_hat = ones(N, N)

# Count treated pairs
n_treated_pairs = length(treated_tracts) * length(downtown_tracts)
for n in treated_tracts
    for i in downtown_tracts
        κ_hat[n, i] = 1.0 - KAPPA_REDUCTION
    end
end

println("  Treated (origin, destination) pairs: $(n_treated_pairs)")
println("  κ_hat for treated pairs: $(1.0 - KAPPA_REDUCTION)")

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
# For this first pass, we'll hold wages fixed (w_hat = 1) and solve for
# residential reallocation and price changes.

# Initialize
w_hat = ones(N)  # Hold wages fixed
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
    
    # Check convergence
    diff_Q = maximum(abs.(Q_hat_new .- Q_hat))
    diff_L = maximum(abs.(L_R_hat_new .- L_R_hat))
    
    if iter % 100 == 0 || iter == 1
        @printf("  Iter %4d: max ΔQ_hat = %.2e, max ΔL_R_hat = %.2e\n", iter, diff_Q, diff_L)
    end
    
    # Update (use global to modify outer scope variables)
    global Q_hat = 0.5 * Q_hat + 0.5 * Q_hat_new  # Damping for stability
    global L_R_hat = 0.5 * L_R_hat + 0.5 * L_R_hat_new
    global L_M_hat = L_M_hat_new
    
    if max(diff_Q, diff_L) < TOL
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

# Changes for treated tracts
treated_mask = [n in treated_tracts for n in 1:N]

println("\nTreated tracts (n = $(sum(treated_mask))):")
@printf("  Mean Q_hat: %.4f (%.2f%% change)\n", mean(Q_hat[treated_mask]), (mean(Q_hat[treated_mask])-1)*100)
@printf("  Mean L_R_hat: %.4f (%.2f%% change)\n", mean(L_R_hat[treated_mask]), (mean(L_R_hat[treated_mask])-1)*100)

println("\nAll tracts (n = $N):")
@printf("  Mean Q_hat: %.4f (%.2f%% change)\n", mean(Q_hat), (mean(Q_hat)-1)*100)
@printf("  Mean L_R_hat: %.4f (%.2f%% change)\n", mean(L_R_hat), (mean(L_R_hat)-1)*100)
@printf("  Total welfare change: %.4f%%\n", welfare_change_pct)

#=============================================================================
                    SAVE RESULTS
=============================================================================#

println("\nSaving results...")

# Tract-level results
results_df = DataFrame(
    tract = tracts,
    wage_baseline = w,
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
    mean_Q_hat_all = mean(Q_hat),
    mean_L_R_hat_all = mean(L_R_hat)
)

welfare_file = joinpath(output_dir, "welfare_r$(CATCHMENT_RADIUS)_k$(Int(KAPPA_REDUCTION*100)).csv")
CSV.write(welfare_file, welfare_df)
println("  Saved: $(welfare_file)")

println("\nDone!")
