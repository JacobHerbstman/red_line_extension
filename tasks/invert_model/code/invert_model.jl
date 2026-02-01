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
- Travel times τ_{ni}
- Residential floor space H_n^R

Parameters:
- ν = ε*κ (from gravity): 0.078 (PPML travel-time converted)
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

# From gravity estimation (PPML travel-time, main spec)
# PPML travel-time: ν_time ≈ 0.039 per minute (from estimate_gravity_travel_time)
ν = 0.039  # Semi-elasticity of commuting w.r.t. travel time (per minute)

# From Ahlfeldt et al. (2015)
const ε = 6.83  # Fréchet shape parameter

# Implied commuting cost parameter (per minute in exponent)
κ = ν / ε

# Housing expenditure share (from literature)
const α_housing = 0.30  # Housing share = (1-α) in model notation where α is goods share

# Labor share in production (for endogenous wages)
const GAMMA_LABOR = 0.65

# Convergence parameters
const TOL = 1e-8
const MAX_ITER = 1000

#=============================================================================
                            DATA LOADING
=============================================================================#

function load_data(input_dir::String)
    println("Loading data...")
    
    # Load commuting matrix (OD flows + travel times)
    commuting_file = joinpath(input_dir, "commuting_matrix_travel_time.csv")
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
    println("  Total workers (raw): $(sum(L_M))")
    println("  Tracts with zero workers: $(sum(L_M .== 0))")

    # Scale workplace totals to match total residents
    scale_LM = sum(L_R) / sum(L_M)
    L_M .= L_M .* scale_LM
    println("  Scaled total workers: $(sum(L_M)) (scale factor = $(round(scale_LM, digits=4)))")
    
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
    
    # Verify commuting matrix has travel times
    if !("travel_time_min" in names(commuting_df))
        error("Expected travel_time_min in commuting matrix. Use commuting_matrix_travel_time.csv.")
    end

    # Build travel time matrix and flow matrix
    # Filter to Chicago-only pairs
    commuting_chicago = filter(row -> 
        row.origin_tract in chicago_tracts && row.dest_tract in chicago_tracts,
        commuting_df)
    
    # Initialize matrices
    T = fill(Inf, N, N)  # Travel time matrix (minutes)
    Λ = zeros(N, N)      # Flow matrix
    
    for row in eachrow(commuting_chicago)
        n = tract_to_idx[row.origin_tract]
        i = tract_to_idx[row.dest_tract]
        tt = row.travel_time_min
        if !ismissing(tt) && isfinite(tt)
            T[n, i] = tt
        end
        Λ[n, i] = row.flow
    end
    
    # Handle within-tract travel time (set to small positive number)
    MIN_WITHIN_MINUTES = 5.0
    for n in 1:N
        if T[n, n] == 0.0 || T[n, n] == Inf
            T[n, n] = MIN_WITHIN_MINUTES
        end
    end
    
    # Check for missing travel times
    missing_tt = sum(T .== Inf)
    finite_T = T[isfinite.(T)]
    if missing_tt > 0
        println("  WARNING: $missing_tt tract pairs have missing travel times")
        # Impute missing travel times using a high percentile of observed times
        impute_tt = quantile(finite_T, 0.99)
        T[T .== Inf] .= impute_tt
        println("  Imputed missing travel times with p99 = $(round(impute_tt, digits=2)) minutes")
    end

    finite_T = T[isfinite.(T)]
    println("  Mean travel time (finite): $(mean(finite_T)) minutes")
    println("  Total observed flows: $(sum(Λ))")

    # Observed conditional commuting shares (by origin)
    λ_obs_cond = zeros(N, N)
    Λ_row = vec(sum(Λ, dims = 2))
    for n in 1:N
        if Λ_row[n] > 0
            λ_obs_cond[n, :] = Λ[n, :] ./ Λ_row[n]
        end
    end
    
    return chicago_tracts, tract_to_idx, idx_to_tract, L_R, L_M, H_R, T, Λ, λ_obs_cond, N
end

#=============================================================================
                        COMMUTING COST MATRIX
=============================================================================#

function compute_commuting_costs(T::Matrix{Float64}, N::Int)
    """
    Compute iceberg commuting cost matrix κ_{ni} = exp(κ * τ_{ni})
    and the transformed version κ_{ni}^{-ε} used in the model.
    """
    println("\nComputing commuting costs...")
    
    # κ_{ni}^{-ε} = exp(-ν * τ_{ni}) (direct parameterization)
    κ_neg_ε = exp.(-ν .* T)
    
    # Report implied κ for reference
    κ_matrix = exp.(κ .* T)
    κ_finite = κ_matrix[isfinite.(κ_matrix)]
    κ_neg_finite = κ_neg_ε[isfinite.(κ_neg_ε)]
    println("  κ (cost per minute): $κ")
    println("  ε (Fréchet shape): $ε")
    println("  Mean κ_{ni} (finite): $(mean(κ_finite))")
    println("  Mean κ_{ni}^{-ε} (finite): $(mean(κ_neg_finite))")
    
    return κ_matrix, κ_neg_ε
end

#=============================================================================
                    STEP 1: RECOVER WAGES
=============================================================================#

function recover_wages(L_R::Vector{Float64}, L_M::Vector{Float64},
                       κ_neg_ε::Matrix{Float64}, N::Int; w_init::Union{Nothing, Vector{Float64}} = nothing)
    """
    Recover wages from commuting market clearing condition:
    
    L_i^M = Σ_n [κ_{ni}^{-ε} * w_i^ε / W_n] * L_n^R
    
    where W_n = Σ_s κ_{ns}^{-ε} * w_s^ε
    
    This is a fixed-point problem. We iterate until convergence.
    """
    println("\nRecovering wages from commuting market clearing...")
    
    # Initialize wages
    w = isnothing(w_init) ? ones(N) : copy(w_init)
    
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
                           κ_neg_ε::Matrix{Float64}, λ_obs_cond::Matrix{Float64}, N::Int)
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
    
    # Commuting share fit (conditional on origin)
    λ_model_cond = zeros(N, N)
    for n in 1:N
        for i in 1:N
            λ_model_cond[n, i] = κ_neg_ε[n, i] * w_ε[i] / W[n]
        end
    end
    pos_pairs = λ_obs_cond .> 0
    corr_lambda = cor(vec(λ_model_cond[pos_pairs]), vec(λ_obs_cond[pos_pairs]))
    mae_lambda = mean(abs.(λ_model_cond[pos_pairs] .- λ_obs_cond[pos_pairs]))
    println("\nConditional commuting share fit:")
    println("  Corr (model vs obs, positive flows): $corr_lambda")
    println("  MAE (model vs obs, positive flows): $mae_lambda")
    
    return L_R_model, L_M_model, λ_model_cond, corr_lambda, mae_lambda
end

#=============================================================================
                            MAIN
=============================================================================#

function main()
    println("="^60)
    println("MODEL INVERSION: Chicago Spatial Equilibrium")
    println("="^60)
    # Paths - relative to code/ directory
    code_dir = @__DIR__
    task_dir = dirname(code_dir)
    input_dir = joinpath(task_dir, "input")
    output_dir = joinpath(task_dir, "output")

    # Override ν from gravity estimates if provided
    nu_file = joinpath(input_dir, "nu_time_per_min.csv")
    if isfile(nu_file)
        nu_df = CSV.read(nu_file, DataFrame)
        nu_row = nu_df[nu_df.parameter .== "nu_time_per_min", :value]
        if length(nu_row) > 0
            global ν = nu_row[1]
            global κ = ν / ε
        end
    end

    println("\nParameters:")
    println("  ν (gravity semi-elasticity): $ν")
    println("  ε (Fréchet shape): $ε")
    println("  κ (commuting cost per minute): $κ")
    println("  γ (labor share): $GAMMA_LABOR")
    println("  (1-α) (housing share): $α_housing")
    
    println("\nInput directory: $input_dir")
    println("Output directory: $output_dir")
    
    # Load data
    commuting_df, rac_df, wac_df, floor_df = load_data(input_dir)
    
    # Process data
    chicago_tracts, tract_to_idx, idx_to_tract, L_R, L_M, H_R, T, Λ, λ_obs_cond, N = 
        process_data(commuting_df, rac_df, wac_df, floor_df)
    
    # Compute commuting costs
    κ_matrix, κ_neg_ε = compute_commuting_costs(T, N)
    
    # Step 1: Recover wages (optionally initialize from gravity destination FE)
    w_init = nothing
    dest_fe_file = joinpath(input_dir, "gravity_destination_fe.csv")
    if isfile(dest_fe_file)
        println("\nLoading gravity destination FE for wage initialization...")
        fe_df = CSV.read(dest_fe_file, DataFrame)
        fe_map = Dict(string(row.dest_tract) => row.dest_fe for row in eachrow(fe_df))
        w0_eps = zeros(N)
        for i in 1:N
            tract = idx_to_tract[i]
            if haskey(fe_map, tract)
                w0_eps[i] = exp(fe_map[tract])
            else
                w0_eps[i] = 1.0
            end
        end
        w_init = w0_eps .^ (1 / ε)
        w_init = w_init ./ mean(w_init)
    end
    w = recover_wages(L_R, L_M, κ_neg_ε, N; w_init = w_init)
    
    # Step 2: Recover floor prices
    Q, v_bar, W = recover_floor_prices(w, L_R, H_R, κ_neg_ε, N)
    
    # Step 3: Recover amenities
    B = recover_amenities(L_R, Q, W, N)
    
    # Compute welfare
    Φ, welfare = compute_welfare(B, Q, w, κ_neg_ε, N)
    
    # Verify equilibrium
    L_R_model, L_M_model, λ_model_cond, corr_lambda, mae_lambda = verify_equilibrium(
        w, Q, B, L_R, L_M, κ_neg_ε, λ_obs_cond, N
    )
    
    # Save results
    println("\n" * "="^60)
    println("SAVING RESULTS")
    println("="^60)
    
    # Recover productivity from production function
    L_M_adj = copy(L_M)
    L_M_adj[L_M_adj .== 0] .= 1.0
    A = w ./ (GAMMA_LABOR .* (L_M_adj .^ (GAMMA_LABOR - 1.0)))

    results_df = DataFrame(
        tract = [idx_to_tract[i] for i in 1:N],
        wage = w,
        productivity = A,
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

    # Save baseline conditional commuting shares (for exact hat algebra)
    origin_vec = Vector{String}(undef, N * N)
    dest_vec = Vector{String}(undef, N * N)
    lambda_vec = Vector{Float64}(undef, N * N)
    idx = 1
    for n in 1:N
        for i in 1:N
            origin_vec[idx] = string(idx_to_tract[n])
            dest_vec[idx] = string(idx_to_tract[i])
            lambda_vec[idx] = λ_model_cond[n, i]
            idx += 1
        end
    end
    lambda_df = DataFrame(origin_tract = origin_vec,
                          dest_tract = dest_vec,
                          lambda_cond = lambda_vec)
    lambda_file = joinpath(output_dir, "commuting_shares_model.csv")
    CSV.write(lambda_file, lambda_df)
    println("  Saved commuting shares to: $lambda_file")
    
    # Save parameters and welfare
    params_df = DataFrame(
        parameter = ["nu_time_per_min", "epsilon", "kappa_per_min", "alpha_housing",
                     "gamma_labor", "Phi", "welfare", "N_tracts"],
        value = [ν, ε, κ, α_housing, GAMMA_LABOR, Φ, welfare, N]
    )

    # Save commuting fit diagnostics
    diag_df = DataFrame(
        metric = ["corr_lambda_positive", "mae_lambda_positive"],
        value = [corr_lambda, mae_lambda]
    )
    diag_file = joinpath(output_dir, "diagnostic_commuting_fit.csv")
    CSV.write(diag_file, diag_df)
    println("  Saved commuting fit diagnostics to: $diag_file")
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
