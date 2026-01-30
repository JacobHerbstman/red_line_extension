#=
Model Inversion for Chicago Spatial Equilibrium
================================================
Recovers fundamentals (wages, floor prices, amenities) from observed data.

Model: Simplified Monte et al. (2018)
- Workers choose residence n and workplace i
- Utility: V_{ni} = (b_{ni} * w_i) / (κ_{ni} * Q_n^{1-α})
- Fréchet preference shocks with shape ε

Inputs:
- Commuting flows λ_{ni}
- Residents by tract L_n^R
- Workers by tract L_i^M  
- Distances d_{ni}
- Residential floor space H_n^R

Parameters:
- ν = ε*κ (from gravity): 0.11
- ε (Fréchet shape): 6.83
- (1-α) (housing share): 0.30

Outputs:
- Wages w_i
- Floor prices Q_n
- Amenities B_n
=#

using CSV
using DataFrames
using Statistics
using LinearAlgebra
using Printf

#=============================================================================
                            PARAMETERS
=============================================================================#

# From gravity estimation (PPML two-way FE)
const ν = 0.11  # Semi-elasticity of commuting w.r.t. distance

# From Ahlfeldt et al. (2015)
const ε = 6.83  # Fréchet shape parameter

# Implied commuting cost parameter
const κ = ν / ε  # ≈ 0.016

# Housing expenditure share (from literature)
const α_housing = 0.30  # Housing share = (1-α) in model notation where α is goods share

# Convergence parameters
const TOL = 1e-8
const MAX_ITER = 1000

#=============================================================================
                            DATA LOADING
=============================================================================#

function load_data(input_dir::String)
    println("Loading data...")
    
    # Load commuting matrix (OD flows + distances)
    commuting_file = joinpath(input_dir, "commuting_matrix.csv")
    println("  Loading commuting matrix from: $commuting_file")
    commuting_df = CSV.read(commuting_file, DataFrame)
    
    # Load residential employment (RAC)
    rac_file = joinpath(input_dir, "lodes_rac.csv")
    println("  Loading RAC from: $rac_file")
    rac_df = CSV.read(rac_file, DataFrame)
    
    # Load workplace employment (WAC)
    wac_file = joinpath(input_dir, "lodes_wac.csv")
    println("  Loading WAC from: $wac_file")
    wac_df = CSV.read(wac_file, DataFrame)
    
    # Load floor space
    floor_file = joinpath(input_dir, "tract_floorspace.csv")
    println("  Loading floor space from: $floor_file")
    floor_df = CSV.read(floor_file, DataFrame)
    
    return commuting_df, rac_df, wac_df, floor_df
end

#=============================================================================
                            DATA PROCESSING
=============================================================================#

function process_data(commuting_df, rac_df, wac_df, floor_df)
    println("\nProcessing data...")

    # Minimum residential floor space threshold (sqft)
    # Tracts below this are mostly commercial and create extreme outliers
    MIN_RESIDENTIAL_SQFT = 100_000

    # Get list of Chicago tracts with sufficient residential floor space
    # Exclude tracts with zero/low floor space (airports, pure commercial, etc.)
    # as they break the residential choice model with extreme floor prices
    floor_df_positive = filter(row -> row.total_sqft_residential >= MIN_RESIDENTIAL_SQFT, floor_df)
    excluded_zero = filter(row -> row.total_sqft_residential == 0 || ismissing(row.total_sqft_residential), floor_df)
    excluded_low = filter(row -> row.total_sqft_residential > 0 && row.total_sqft_residential < MIN_RESIDENTIAL_SQFT, floor_df)

    if nrow(excluded_zero) > 0
        println("  Excluding $(nrow(excluded_zero)) tracts with zero residential floor space:")
        for row in eachrow(excluded_zero)
            println("    - $(row.census_tract_geoid)")
        end
    end

    if nrow(excluded_low) > 0
        println("  Excluding $(nrow(excluded_low)) tracts with < $(MIN_RESIDENTIAL_SQFT) sqft residential:")
        for row in eachrow(excluded_low)
            println("    - $(row.census_tract_geoid) ($(Int(round(row.total_sqft_residential))) sqft)")
        end
    end

    chicago_tracts = sort(unique(floor_df_positive.census_tract_geoid))
    N = length(chicago_tracts)
    println("  Number of Chicago tracts (with >= $(MIN_RESIDENTIAL_SQFT) sqft residential): $N")
    
    # Create tract index mapping
    tract_to_idx = Dict(t => i for (i, t) in enumerate(chicago_tracts))
    idx_to_tract = Dict(i => t for (t, i) in tract_to_idx)
    
    # Filter LODES data to Chicago tracts
    rac_chicago = filter(row -> row.h_tract in chicago_tracts, rac_df)
    wac_chicago = filter(row -> row.w_tract in chicago_tracts, wac_df)
    
    # Build L_n^R (residents) vector
    L_R = zeros(N)
    for row in eachrow(rac_chicago)
        idx = tract_to_idx[row.h_tract]
        L_R[idx] = row.C000
    end
    println("  Total residents: $(sum(L_R))")
    println("  Tracts with zero residents: $(sum(L_R .== 0))")
    
    # Build L_i^M (workers) vector
    L_M = zeros(N)
    for row in eachrow(wac_chicago)
        idx = tract_to_idx[row.w_tract]
        L_M[idx] = row.C000
    end
    println("  Total workers: $(sum(L_M))")
    println("  Tracts with zero workers: $(sum(L_M .== 0))")
    
    # Build H_n^R (residential floor space) vector
    H_R = zeros(N)
    for row in eachrow(floor_df)
        if row.census_tract_geoid in chicago_tracts
            idx = tract_to_idx[row.census_tract_geoid]
            H_R[idx] = row.total_sqft_residential
        end
    end
    println("  Total residential floor space: $(sum(H_R) / 1e6) million sqft")
    println("  Tracts with zero floor space: $(sum(H_R .== 0))")
    
    # Build distance matrix and flow matrix
    # Filter to Chicago-only pairs
    commuting_chicago = filter(row -> 
        row.origin_tract in chicago_tracts && row.dest_tract in chicago_tracts,
        commuting_df)
    
    # Initialize matrices
    D = fill(Inf, N, N)  # Distance matrix
    Λ = zeros(N, N)       # Flow matrix
    
    for row in eachrow(commuting_chicago)
        n = tract_to_idx[row.origin_tract]
        i = tract_to_idx[row.dest_tract]
        D[n, i] = row.distance_km
        Λ[n, i] = row.flow
    end
    
    # Handle within-tract distance (set to small positive number)
    for n in 1:N
        if D[n, n] == 0.0 || D[n, n] == Inf
            D[n, n] = 0.5  # 0.5 km for within-tract
        end
    end
    
    # Check for missing distances
    missing_dist = sum(D .== Inf)
    if missing_dist > 0
        println("  WARNING: $missing_dist tract pairs have missing distances")
        # Fill with large distance (won't matter much due to decay)
        D[D .== Inf] .= 50.0
    end
    
    println("  Mean distance: $(mean(D)) km")
    println("  Total observed flows: $(sum(Λ))")
    
    return chicago_tracts, tract_to_idx, idx_to_tract, L_R, L_M, H_R, D, Λ, N
end

#=============================================================================
                        COMMUTING COST MATRIX
=============================================================================#

function compute_commuting_costs(D::Matrix{Float64}, N::Int)
    """
    Compute iceberg commuting cost matrix κ_{ni} = exp(κ * d_{ni})
    and the transformed version κ_{ni}^{-ε} used in the model.
    """
    println("\nComputing commuting costs...")
    
    # κ_{ni} = exp(κ * d_{ni})
    κ_matrix = exp.(κ .* D)
    
    # κ_{ni}^{-ε} (this is what enters the choice probabilities)
    κ_neg_ε = κ_matrix .^ (-ε)
    
    println("  κ (cost per km): $κ")
    println("  ε (Fréchet shape): $ε")
    println("  Mean κ_{ni}: $(mean(κ_matrix))")
    println("  Mean κ_{ni}^{-ε}: $(mean(κ_neg_ε))")
    
    return κ_matrix, κ_neg_ε
end

#=============================================================================
                    STEP 1: RECOVER WAGES
=============================================================================#

function recover_wages(L_R::Vector{Float64}, L_M::Vector{Float64}, 
                       κ_neg_ε::Matrix{Float64}, N::Int)
    """
    Recover wages from commuting market clearing condition:
    
    L_i^M = Σ_n [κ_{ni}^{-ε} * w_i^ε / W_n] * L_n^R
    
    where W_n = Σ_s κ_{ns}^{-ε} * w_s^ε
    
    This is a fixed-point problem. We iterate until convergence.
    """
    println("\nRecovering wages from commuting market clearing...")
    
    # Initialize wages
    w = ones(N)
    
    # Handle tracts with zero workers (set target to small positive)
    L_M_adj = copy(L_M)
    L_M_adj[L_M_adj .== 0] .= 1.0
    
    # Handle tracts with zero residents
    L_R_adj = copy(L_R)
    L_R_adj[L_R_adj .== 0] .= 1.0
    
    for iter in 1:MAX_ITER
        # Compute W_n = Σ_s κ_{ns}^{-ε} * w_s^ε
        w_ε = w .^ ε
        W = κ_neg_ε * w_ε  # N x 1 vector
        
        # Compute predicted employment at each workplace
        # L_i^M_predicted = w_i^ε * Σ_n [κ_{ni}^{-ε} / W_n * L_n^R]
        # Note: κ_neg_ε[n,i] is κ_{ni}^{-ε}, so we need to transpose for the sum over n
        
        L_M_pred = zeros(N)
        for i in 1:N
            for n in 1:N
                L_M_pred[i] += (κ_neg_ε[n, i] / W[n]) * L_R_adj[n]
            end
            L_M_pred[i] *= w_ε[i]
        end
        
        # Update wages
        # w_new = w * (L_M / L_M_pred)^{1/ε}
        w_new = w .* (L_M_adj ./ L_M_pred) .^ (1/ε)
        
        # Normalize (mean wage = 1)
        w_new = w_new ./ mean(w_new)
        
        # Check convergence
        diff = maximum(abs.(w_new .- w))
        
        if iter % 100 == 0 || iter == 1
            @printf("  Iteration %4d: max wage change = %.2e\n", iter, diff)
        end
        
        if diff < TOL
            @printf("  Converged after %d iterations (tol = %.2e)\n", iter, TOL)
            w = w_new
            break
        end
        
        w = w_new
        
        if iter == MAX_ITER
            println("  WARNING: Did not converge after $MAX_ITER iterations")
        end
    end
    
    # Summary statistics
    println("\n  Wage statistics:")
    println("    Mean: $(mean(w))")
    println("    Std:  $(std(w))")
    println("    Min:  $(minimum(w))")
    println("    Max:  $(maximum(w))")
    println("    P10:  $(quantile(w, 0.10))")
    println("    P50:  $(quantile(w, 0.50))")
    println("    P90:  $(quantile(w, 0.90))")
    
    return w
end

#=============================================================================
                    STEP 2: RECOVER FLOOR PRICES
=============================================================================#

function recover_floor_prices(w::Vector{Float64}, L_R::Vector{Float64}, 
                              H_R::Vector{Float64}, κ_neg_ε::Matrix{Float64}, N::Int)
    """
    Recover floor prices from land market clearing:
    
    Q_n = (1-α) * v̄_n * L_n^R / H_n^R
    
    where v̄_n = Σ_i λ_{ni|n} * w_i is expected income of residents at n.
    
    First compute W_n and then expected income.
    """
    println("\nRecovering floor prices from land market clearing...")
    
    # Compute W_n = Σ_s κ_{ns}^{-ε} * w_s^ε
    w_ε = w .^ ε
    W = κ_neg_ε * w_ε
    
    # Compute expected income v̄_n = Σ_i λ_{ni|n} * w_i
    # λ_{ni|n} = κ_{ni}^{-ε} * w_i^ε / W_n
    # So v̄_n = Σ_i [κ_{ni}^{-ε} * w_i^ε / W_n] * w_i
    #        = (1/W_n) * Σ_i κ_{ni}^{-ε} * w_i^{ε+1}
    
    w_ε_plus_1 = w .^ (ε + 1)
    v_bar = (κ_neg_ε * w_ε_plus_1) ./ W
    
    println("  Expected income statistics:")
    println("    Mean: $(mean(v_bar))")
    println("    Std:  $(std(v_bar))")
    println("    Min:  $(minimum(v_bar))")
    println("    Max:  $(maximum(v_bar))")
    
    # Compute floor prices
    # Q_n = (1-α) * v̄_n * L_n^R / H_n^R
    
    # Handle zero floor space (set to small positive to avoid division by zero)
    H_R_adj = copy(H_R)
    H_R_adj[H_R_adj .== 0] .= 1.0
    
    # Handle zero residents
    L_R_adj = copy(L_R)
    L_R_adj[L_R_adj .== 0] .= 1.0
    
    Q = α_housing .* v_bar .* L_R_adj ./ H_R_adj
    
    # Normalize (geometric mean = 1)
    Q = Q ./ exp(mean(log.(Q[Q .> 0])))
    
    println("\n  Floor price statistics:")
    println("    Mean: $(mean(Q))")
    println("    Std:  $(std(Q))")
    println("    Min:  $(minimum(Q))")
    println("    Max:  $(maximum(Q))")
    println("    P10:  $(quantile(Q, 0.10))")
    println("    P50:  $(quantile(Q, 0.50))")
    println("    P90:  $(quantile(Q, 0.90))")
    
    return Q, v_bar, W
end

#=============================================================================
                    STEP 3: RECOVER AMENITIES
=============================================================================#

function recover_amenities(L_R::Vector{Float64}, Q::Vector{Float64}, 
                           W::Vector{Float64}, N::Int)
    """
    Recover amenities from residential choice probabilities:
    
    λ_n^R = B_n * Q_n^{-(1-α)ε} * W_n / Φ
    
    Since L_n^R / L̄ = λ_n^R, we have:
    
    B_n ∝ (L_n^R / L̄) / [Q_n^{-(1-α)ε} * W_n]
    
    Amenities are identified up to scale; we normalize geometric mean = 1.
    """
    println("\nRecovering amenities from residential choice probabilities...")
    
    L_bar = sum(L_R)
    λ_R = L_R ./ L_bar  # Residential shares
    
    # Q_n^{-(1-α)ε}
    Q_term = Q .^ (-α_housing * ε)
    
    # B_n ∝ λ_n^R / (Q_term * W_n)
    # Handle zeros
    λ_R_adj = copy(λ_R)
    λ_R_adj[λ_R_adj .== 0] .= 1e-10
    
    B = λ_R_adj ./ (Q_term .* W)
    
    # Normalize (geometric mean = 1)
    B = B ./ exp(mean(log.(B[B .> 0])))
    
    println("  Amenity statistics:")
    println("    Mean: $(mean(B))")
    println("    Std:  $(std(B))")
    println("    Min:  $(minimum(B))")
    println("    Max:  $(maximum(B))")
    println("    P10:  $(quantile(B, 0.10))")
    println("    P50:  $(quantile(B, 0.50))")
    println("    P90:  $(quantile(B, 0.90))")
    
    return B
end

#=============================================================================
                        COMPUTE WELFARE
=============================================================================#

function compute_welfare(B::Vector{Float64}, Q::Vector{Float64}, 
                        w::Vector{Float64}, κ_neg_ε::Matrix{Float64}, N::Int)
    """
    Compute welfare (expected utility):
    
    Φ = Σ_n Σ_i B_n * κ_{ni}^{-ε} * Q_n^{-(1-α)ε} * w_i^ε
    
    Welfare = γ * Φ^{1/ε}
    
    where γ = Γ((ε-1)/ε) is a constant.
    """
    println("\nComputing welfare...")
    
    Q_term = Q .^ (-α_housing * ε)
    w_ε = w .^ ε
    
    # Φ = Σ_n Σ_i B_n * κ_{ni}^{-ε} * Q_n^{-(1-α)ε} * w_i^ε
    Φ = 0.0
    for n in 1:N
        for i in 1:N
            Φ += B[n] * κ_neg_ε[n, i] * Q_term[n] * w_ε[i]
        end
    end
    
    # Welfare (up to constant γ)
    welfare = Φ^(1/ε)
    
    println("  Φ = $Φ")
    println("  Welfare (Φ^{1/ε}) = $welfare")
    
    return Φ, welfare
end

#=============================================================================
                        VERIFICATION
=============================================================================#

function verify_equilibrium(w::Vector{Float64}, Q::Vector{Float64}, B::Vector{Float64},
                           L_R::Vector{Float64}, L_M::Vector{Float64},
                           κ_neg_ε::Matrix{Float64}, N::Int)
    """
    Verify that the recovered fundamentals rationalize the observed equilibrium.
    """
    println("\n" * "="^60)
    println("VERIFICATION")
    println("="^60)
    
    # Compute model-implied quantities
    w_ε = w .^ ε
    W = κ_neg_ε * w_ε
    Q_term = Q .^ (-α_housing * ε)
    
    # Compute Φ
    Φ = sum(B[n] * κ_neg_ε[n, i] * Q_term[n] * w_ε[i] for n in 1:N, i in 1:N)
    
    # Model-implied residential population
    L_R_model = zeros(N)
    L_bar = sum(L_R)
    for n in 1:N
        λ_n_R = B[n] * Q_term[n] * W[n] / Φ
        L_R_model[n] = λ_n_R * L_bar
    end
    
    # Model-implied workplace employment
    L_M_model = zeros(N)
    for i in 1:N
        for n in 1:N
            λ_ni_given_n = κ_neg_ε[n, i] * w_ε[i] / W[n]
            L_M_model[i] += λ_ni_given_n * L_R[n]
        end
    end
    
    # Check residential fit
    L_R_pos = L_R[L_R .> 0]
    L_R_model_pos = L_R_model[L_R .> 0]
    corr_R = cor(L_R_pos, L_R_model_pos)
    rmse_R = sqrt(mean((L_R .- L_R_model).^2))
    
    println("\nResidential population fit:")
    println("  Correlation: $corr_R")
    println("  RMSE: $rmse_R")
    println("  Total observed: $(sum(L_R))")
    println("  Total model: $(sum(L_R_model))")
    
    # Check employment fit
    L_M_pos = L_M[L_M .> 0]
    L_M_model_pos = L_M_model[L_M .> 0]
    corr_M = cor(L_M_pos, L_M_model_pos)
    rmse_M = sqrt(mean((L_M .- L_M_model).^2))
    
    println("\nWorkplace employment fit:")
    println("  Correlation: $corr_M")
    println("  RMSE: $rmse_M")
    println("  Total observed: $(sum(L_M))")
    println("  Total model: $(sum(L_M_model))")
    
    return L_R_model, L_M_model
end

#=============================================================================
                            MAIN
=============================================================================#

function main()
    println("="^60)
    println("MODEL INVERSION: Chicago Spatial Equilibrium")
    println("="^60)
    println("\nParameters:")
    println("  ν (gravity semi-elasticity): $ν")
    println("  ε (Fréchet shape): $ε")
    println("  κ (commuting cost per km): $κ")
    println("  (1-α) (housing share): $α_housing")
    
    # Paths - relative to code/ directory
    code_dir = @__DIR__
    task_dir = dirname(code_dir)
    input_dir = joinpath(task_dir, "input")
    output_dir = joinpath(task_dir, "output")
    
    println("\nInput directory: $input_dir")
    println("Output directory: $output_dir")
    
    # Load data
    commuting_df, rac_df, wac_df, floor_df = load_data(input_dir)
    
    # Process data
    chicago_tracts, tract_to_idx, idx_to_tract, L_R, L_M, H_R, D, Λ, N = 
        process_data(commuting_df, rac_df, wac_df, floor_df)
    
    # Compute commuting costs
    κ_matrix, κ_neg_ε = compute_commuting_costs(D, N)
    
    # Step 1: Recover wages
    w = recover_wages(L_R, L_M, κ_neg_ε, N)
    
    # Step 2: Recover floor prices
    Q, v_bar, W = recover_floor_prices(w, L_R, H_R, κ_neg_ε, N)
    
    # Step 3: Recover amenities
    B = recover_amenities(L_R, Q, W, N)
    
    # Compute welfare
    Φ, welfare = compute_welfare(B, Q, w, κ_neg_ε, N)
    
    # Verify equilibrium
    L_R_model, L_M_model = verify_equilibrium(w, Q, B, L_R, L_M, κ_neg_ε, N)
    
    # Save results
    println("\n" * "="^60)
    println("SAVING RESULTS")
    println("="^60)
    
    results_df = DataFrame(
        tract = [idx_to_tract[i] for i in 1:N],
        wage = w,
        floor_price = Q,
        amenity = B,
        expected_income = v_bar,
        commuting_access = W,
        residents_observed = L_R,
        workers_observed = L_M,
        residents_model = L_R_model,
        workers_model = L_M_model,
        floor_space_residential = H_R
    )
    
    output_file = joinpath(output_dir, "model_fundamentals.csv")
    CSV.write(output_file, results_df)
    println("  Saved fundamentals to: $output_file")
    
    # Save parameters and welfare
    params_df = DataFrame(
        parameter = ["nu", "epsilon", "kappa", "alpha_housing", "Phi", "welfare", "N_tracts"],
        value = [ν, ε, κ, α_housing, Φ, welfare, N]
    )
    params_file = joinpath(output_dir, "model_parameters.csv")
    CSV.write(params_file, params_df)
    println("  Saved parameters to: $params_file")
    
    println("\nDone!")
    
    return results_df, params_df
end

# Run if called directly
if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
