import numpy as np
import pandas as pd
import math
from itertools import combinations
import independence
import utils
from scipy import stats
import streamlit as st
from scipy.stats import chi2, norm
import plotly.graph_objects as go
from plotly.subplots import make_subplots
from itertools import permutations

# Add this utility function near the top of the file, after imports
def safe_report_item(item):
    """
    Ensure that an item is a valid type for report_items (string or DataFrame).
    If it's an integer or other non-string type, convert to string.
    """
    if isinstance(item, (str, pd.DataFrame)):
        return item
    return str(item)

def consecutive_partition(n, p):
    if p == 1:
        return np.ones((n, 1), dtype=int)
    elif p == n:
        return np.arange(1, n+1).reshape(-1, 1)
    else:
        partitions = np.full((n, math.comb(n-1, p-1)), p, dtype=int)
        current_col_idx = 0
        for size_of_last_set in range(n-(p-1), 0, -1):
            partitions_of_first = consecutive_partition(n - size_of_last_set, p - 1)
            partitions[:n - size_of_last_set, current_col_idx:current_col_idx+partitions_of_first.shape[1]] = partitions_of_first
            current_col_idx += partitions_of_first.shape[1]
        return partitions

def is_stochastically_less_than_or_equal(lhs, rhs):
    return all(lhs[:-1] <= rhs[:-1])

def stochastic_ordering(cdf):
    import numpy as np
    
    cdf[:, -1] = 1  # Set the last column of CDF to 1
    current_ordering = [0]  # Start with the first row
    num_cdf = cdf.shape[0]  # Number of rows in CDF
    
    if num_cdf == 1:
        return current_ordering
    
    for i in range(1, num_cdf):  # Start from the second row
        insert_to_end = True
        for j in range(len(current_ordering)):
            if is_stochastically_less_than_or_equal(cdf[current_ordering[j], :], cdf[i, :]):
                continue
            elif is_stochastically_less_than_or_equal(cdf[i, :], cdf[current_ordering[j], :]):
                if j == 0:
                    current_ordering = [i] + current_ordering
                else:
                    current_ordering = current_ordering[:j] + [i] + current_ordering[j:]
                insert_to_end = False
                break
            else:
                return None
        
        if insert_to_end:
            current_ordering.append(i)
    
    return current_ordering

def adjust_mu_or_nu(x, weights=None):
    """
    Adjust mu or nu vectors to have weighted mean of 0 and weighted variance of 1.
    
    Args:
        x: Input vector to adjust
        weights: Optional weights for the adjustment
        
    Returns:
        Adjusted vector with weighted mean 0 and variance 1
    """
    if weights is None:
        weights = np.ones_like(x) / len(x)
    
    # Ensure weights sum to 1
    weights = weights / np.sum(weights)
    
    # Calculate weighted mean
    x_mean = np.sum(x * weights)
    
    # Calculate weighted variance
    x_var = np.sum(weights * ((x - x_mean) ** 2))
    
    # Avoid division by zero
    if x_var < 1e-10:
        x_var = 1.0
    
    # Return standardized values
    return ((x-x_mean)/np.sqrt(x_var))

def exponential_spacing_computation(contingency_table, poly_deg_row=None, poly_deg_col=None, p_value_threshold=0.05, debug=False):
    """
    Compute the exponential spacing model for ranking data.
    Implementation follows the R function spacing.exponential.spacing.computation
    
    Parameters:
    -----------
    contingency_table : numpy.ndarray
        The input contingency table
    poly_deg_row : int, optional
        Polynomial degree for row interpolation (if None, calculated based on data)
    poly_deg_col : int, optional
        Polynomial degree for column interpolation (if None, calculated based on data)
    p_value_threshold : float, optional
        Threshold for statistical significance
    debug : bool, optional
        Whether to print debug information
        
    Returns:
    --------
    dict
        A dictionary containing the results of the computation
    """
    # Convert to numpy array if it's a DataFrame
    if hasattr(contingency_table, 'values'):
        working_table = contingency_table.values.copy()
    else:
        working_table = contingency_table.copy()
    
    # Basic initializations
    observed = working_table
    num_row, num_col = observed.shape
    
    # Determine polynomial degrees based on data dimensions if not provided
    # Following R's convention in spacing.exponential.spacing.computation
    if poly_deg_row is None:
        # Use num_row-1 as in the R code
        poly_deg_row = num_row-1
    
    if poly_deg_col is None:
        # Use num_col-1 as in the R code
        poly_deg_col = num_col-1
    
    observed_sum = np.sum(observed)
    observed_row_sum = np.sum(observed, axis=1)
    observed_col_sum = np.sum(observed, axis=0)
    observed_row_freq = observed_row_sum / observed_sum
    observed_col_freq = observed_col_sum / observed_sum

    total = np.sum(working_table)  # Total count
    row_sums = np.sum(working_table, axis=1)  # Row sums
    col_sums = np.sum(working_table, axis=0)  # Column sums
    
    # For storing debug information
    debug_info = []
    if debug:
        debug_info.append(f"=== EXPONENTIAL SPACING ALGORITHM DEBUGGING ===")
        debug_info.append(f"Contingency table shape: {working_table.shape}")
        debug_info.append(f"Total sum: {total}")
        debug_info.append(f"Row sums: {row_sums}")
        debug_info.append(f"Column sums: {col_sums}")
        debug_info.append(f"Polynomial degree row: {poly_deg_row}")
        debug_info.append(f"Polynomial degree col: {poly_deg_col}")
    
    # Define helper functions matching R implementation
    def compute_expected(alpha, beta, mu, nu, phi, observed_sum):
        """Compute expected values based on current parameters"""
        # In R: outer(alpha, beta) * exp(phi * outer(mu, nu)) * observed_sum
        return np.outer(alpha, beta) * np.exp(phi * np.outer(mu, nu)) * observed_sum
    
    def update_alpha_beta_expected(alpha, beta, mu, nu, phi, observed_sum, observed_row_sum, observed_col_sum):
        """Update alpha and beta parameters through iteration (IPF)"""
        expected = compute_expected(alpha, beta, mu, nu, phi, observed_sum)
        num_iter = 25

        for i in range(num_iter):
            # Update alpha
            alpha = alpha * observed_row_sum / np.maximum(np.sum(expected, axis=1), 1e-10)
            expected = compute_expected(alpha, beta, mu, nu, phi, observed_sum)
            
            # Update beta
            beta = beta * observed_col_sum / np.maximum(np.sum(expected, axis=0), 1e-10)
            expected = compute_expected(alpha, beta, mu, nu, phi, observed_sum)
            
            if debug and (i == 0 or i == num_iter-1):
                row_errors = np.abs(observed_row_sum - np.sum(expected, axis=1))
                col_errors = np.abs(observed_col_sum - np.sum(expected, axis=0))
                debug_info.append(f"Iteration {i+1}: max row error={np.max(row_errors):.6f}, max col error={np.max(col_errors):.6f}")
        
        if debug:
            debug_info.append(f"Final alpha: {alpha.round(4)}")
            debug_info.append(f"Final beta: {beta.round(4)}")
        
        return {"alpha": alpha, "beta": beta, "expected": expected}
    
    def lagrange_interpolation(x, fx, x_new):
        """Lagrange polynomial interpolation"""
        result = np.zeros(len(x_new))
        for i, xi in enumerate(x_new):
            # Basic Lagrange interpolation formula
            result[i] = 0
            for j, xj in enumerate(x):
                # Calculate the Lagrange basis polynomial
                L = 1
                for k, xk in enumerate(x):
                    if k != j:
                        L *= (xi - xk) / (xj - xk)
                result[i] += fx[j] * L
        return result
    
    # Pseudo-inverse function for computing standard errors
    def pseudo_inv(x):
        """
        Calculate the Moore-Penrose pseudo-inverse.
        
        For a matrix A, the pseudoinverse A+ satisfies:
        - If A is m×n, then A+ is n×m
        - A*A+*A = A and A+*A*A+ = A+
        
        Args:
            x (ndarray): Input matrix
            
        Returns:
            ndarray: Pseudo-inverse of x
        """
        try:
            # Save original shape information
            orig_shape = x.shape
            
            # Calculate SVD
            u, s, vh = np.linalg.svd(x, full_matrices=False)
            
            # Set a threshold for singular values
            singular_value_threshold = max(x.shape) * np.max(s) * np.finfo(float).eps
            
            # Find positive singular values
            is_positive = s > singular_value_threshold
            
            if not np.any(is_positive):
                # Return zero matrix with proper shape (n,m) for a (m,n) input matrix
                result = np.zeros((x.shape[1], x.shape[0]))
                return result
            else:
                # Create diagonal matrix of reciprocals of singular values
                s_inv = np.zeros_like(s)
                s_inv[is_positive] = 1.0 / s[is_positive]
                
                # Compute pseudo-inverse
                result = np.dot(vh.T[:, is_positive], np.dot(np.diag(s_inv[is_positive]), u.T[is_positive, :]))
                
                # Verify dimensions for consistency in broadcasting
                expected_shape = (orig_shape[1], orig_shape[0])  # Transpose of original shape
                if result.shape != expected_shape:
                    if result.shape == (expected_shape[1], expected_shape[0]):
                        # If we got the transpose of expected shape, transpose it
                        result = result.T
                
                return result
        except Exception as e:
            # Fall back to numpy's built-in pinv as a last resort
            result = np.linalg.pinv(x)
            return result
    
    # Initialize model parameters
    alpha = np.ones(num_row) / num_row
    beta = np.ones(num_col) / num_col
    mu = adjust_mu_or_nu(np.arange(num_row), observed_row_freq)
    nu = adjust_mu_or_nu(np.arange(num_col), observed_col_freq)
    phi = 0.1

    # Log initial parameters if debugging
    if debug:
        debug_info.append("\n=== INITIAL PARAMETERS ===")
        debug_info.append(f"Alpha: {alpha.round(4)}")
        debug_info.append(f"Beta: {beta.round(4)}")
        debug_info.append(f"Mu: {mu.round(4)}")
        debug_info.append(f"Nu: {nu.round(4)}")
        debug_info.append(f"Phi: {phi:.4f}")
        debug_info.append("\n=== ITERATION PROCESS STARTS ===")
    
    # Main iteration loop
    total_num_iter = 50
    total_num_mu_nu_iter = 36
    
    for t in range(total_num_iter):
        # Store old values to check differences
        old_mu = mu.copy()
        old_nu = nu.copy()
        old_phi = phi
        
        # Update alpha, beta, and expected
        updated = update_alpha_beta_expected(alpha, beta, mu, nu, phi, total, row_sums, col_sums)
        alpha = updated["alpha"]
        beta = updated["beta"]
        expected = updated["expected"]
        
        # Calculate current errors
        row_errors = np.abs(row_sums - np.sum(expected, axis=1))
        col_errors = np.abs(col_sums - np.sum(expected, axis=0))
        
        # Update mu and nu if in the first total_num_mu_nu_iter iterations
        if t < total_num_mu_nu_iter:
            if debug:
                debug_info.append(f"\n--- Iteration {t+1} ---")
                debug_info.append(f"Before update - Mu: min={np.min(mu):.4f}, max={np.max(mu):.4f}, mean={np.mean(mu):.4f}")
                debug_info.append(f"Before update - Nu: min={np.min(nu):.4f}, max={np.max(nu):.4f}, mean={np.mean(nu):.4f}")
                debug_info.append(f"Before update - Phi: {phi:.6f}")
            
            # Update mu
            temp_o = working_table[:poly_deg_row+1, :]
            temp_e = expected[:poly_deg_row+1, :]
            fx = np.dot(temp_e - temp_o, nu)
            dfdx = np.dot(temp_e, nu**2) * phi
            
            # Newton-Raphson update for mu
            mu[:poly_deg_row+1] = mu[:poly_deg_row+1] - fx / np.maximum(dfdx, 1e-10)
            
            # Interpolate remaining values if needed
            if poly_deg_row + 1 < num_row:
                mu[poly_deg_row+1:] = lagrange_interpolation(
                    np.arange(poly_deg_row+1), 
                    mu[:poly_deg_row+1], 
                    np.arange(poly_deg_row+1, num_row)
                )
            
            # Re-standardize mu
            mu = adjust_mu_or_nu(mu, observed_row_freq)
            
            # Update nu
            temp_o = working_table[:, :poly_deg_col+1]
            temp_e = expected[:, :poly_deg_col+1]
            fx = np.dot(mu, temp_e - temp_o)
            dfdx = np.dot(mu**2, temp_e) * phi
            
            # Newton-Raphson update for nu
            nu[:poly_deg_col+1] = nu[:poly_deg_col+1] - fx / np.maximum(dfdx, 1e-10)
            
            # Interpolate remaining values if needed
            if poly_deg_col + 1 < num_col:
                nu[poly_deg_col+1:] = lagrange_interpolation(
                    np.arange(poly_deg_col+1), 
                    nu[:poly_deg_col+1], 
                    np.arange(poly_deg_col+1, num_col)
                )
            
            # Re-standardize nu
            nu = adjust_mu_or_nu(nu, observed_col_freq)
            
            # Update phi
            fx = np.sum((expected - working_table) * np.outer(mu, nu))
            dfx = np.sum(expected * np.outer(mu, nu)**2)
            
            # Newton-Raphson update for phi
            phi = phi - fx / np.maximum(dfx, 1e-10)
            
            # Ensuring phi doesn't go out of bounds
            phi = max(min(phi, 10.0), -10.0)
            
            if debug:
                debug_info.append(f"After update - Mu: min={np.min(mu):.4f}, max={np.max(mu):.4f}, mean={np.mean(mu):.4f}")
                debug_info.append(f"After update - Nu: min={np.min(nu):.4f}, max={np.max(nu):.4f}, mean={np.mean(nu):.4f}")
                debug_info.append(f"After update - Phi: {phi:.6f}")
        
        # Check for convergence (simplified)
        if t > 0:
            mu_change = np.max(np.abs(mu - old_mu))
            nu_change = np.max(np.abs(nu - old_nu))
            phi_change = np.abs(phi - old_phi)
            
            if debug:
                debug_info.append(f"Changes - Mu: {mu_change:.6f}, Nu: {nu_change:.6f}, Phi: {phi_change:.6f}")
            
            # Exit if changes are very small
            if max(mu_change, nu_change, phi_change) < 1e-6:
                if debug:
                    debug_info.append(f"Converged after {t+1} iterations.")
                break
    
    # Recompute expected values with final parameters
    expected = compute_expected(alpha, beta, mu, nu, phi, total)

    # Compute G-squared (likelihood ratio) statistic
    # G^2 = 2 * sum(O_ij * log(O_ij / E_ij))
    # For cells with O_ij = 0, the contribution is 0
    nonzero_mask = working_table > 0
    g_squared = 2 * np.sum(working_table[nonzero_mask] * np.log(working_table[nonzero_mask] / expected[nonzero_mask]))
    
    # Compute degrees of freedom
    df = (num_row - 1) * (num_col - 1) - poly_deg_row - poly_deg_col - 1
    
    # Compute p-value for the model fit
    p_value = 1 - chi2.cdf(g_squared, df)
    
    # Compute additional statistics
    # Pearson's chi-squared
    pearson_chisq = np.sum((working_table - expected)**2 / expected)
    
    # Dissimilarity coefficient
    dissimilarity = np.sum(np.abs(working_table - expected)) / (2 * total)
    
    # Compute Goodman-Kruskal tau measures of association
    # TAU(Y|X) - predictability of columns given rows
    # TAU(X|Y) - predictability of rows given columns
    
    # For TAU(Y|X)
    col_probabilities = col_sums / total
    row_conditional_col_probabilities = expected / np.sum(expected, axis=1, keepdims=True)
    
    # Weighted average of conditional entropy
    conditional_entropy_yx = 0
    for i in range(num_row):
        conditional_entropy_yx += -np.sum(row_conditional_col_probabilities[i] * np.log(row_conditional_col_probabilities[i])) * (row_sums[i] / total)
    
    # Entropy of marginal distribution
    marginal_entropy_y = -np.sum(col_probabilities * np.log(col_probabilities))
    
    # Calculate TAU(Y|X)
    tau_given_row = 1 - conditional_entropy_yx / marginal_entropy_y if marginal_entropy_y > 0 else 0
    
 
    row_probabilities = row_sums / total
    

    # st.write(expected.T / np.sum(expected, axis=0, keepdims=True).sum())
    # Correct calculation for column conditional row probabilities
    # We want P(row|column) which means dividing each column by its sum
    col_conditional_row_probabilities = expected.T / np.sum(expected.T, axis=0, keepdims=True)

    
    # Weighted average of conditional entropy
    conditional_entropy_xy = 0
    for j in range(num_col):
        conditional_entropy_xy += -np.sum(col_conditional_row_probabilities[j] * np.log(col_conditional_row_probabilities[j])) * (col_sums[j] / total)
    
    # Entropy of marginal distribution
    marginal_entropy_x = -np.sum(row_probabilities * np.log(row_probabilities))
    
    # Calculate TAU(X|Y)
    tau_given_col = 1 - conditional_entropy_xy / marginal_entropy_x if marginal_entropy_x > 0 else 0
    
    # Calculate standardized residuals
    std_residuals = (working_table - expected) / np.sqrt(expected)

    # Compute standard errors for parameters using Fisher Information Matrix
    # Fisher information matrix has blocks for alpha, beta, mu, nu, phi, and Lagrange multipliers
    
    # Pre-allocate matrices for the blocks of the Fisher information matrix
    # For Fisher information, we compute second derivatives of log-likelihood with respect to each parameter pair
    # For mu vs mu 
    fisher_info_mu_mu = np.zeros((num_row, num_row))
    for i in range(num_row):
        for j in range(num_row):
            if i == j:
                fisher_info_mu_mu[i, j] = phi**2 * np.sum(expected[i, :] * nu**2)
            else:
                fisher_info_mu_mu[i, j] = 0  # Independence of mu_i and mu_j
    
    # For nu vs nu
    fisher_info_nu_nu = np.zeros((num_col, num_col))
    for i in range(num_col):
        for j in range(num_col):
            if i == j:
                fisher_info_nu_nu[i, j] = phi**2 * np.sum(expected[:, i] * mu**2)
            else:
                fisher_info_nu_nu[i, j] = 0  # Independence of nu_i and nu_j
    
    # For mu vs nu
    fisher_info_mu_nu = np.zeros((num_row, num_col))
    for i in range(num_row):
        for j in range(num_col):
            fisher_info_mu_nu[i, j] = phi * expected[i, j] * mu[i] * nu[j]
    
    # For mu vs phi and nu vs phi
    fisher_info_mu_phi = np.zeros(num_row)
    fisher_info_nu_phi = np.zeros(num_col)
    
    for i in range(num_row):
        fisher_info_mu_phi[i] = np.sum(expected[i, :] * nu)
    
    for j in range(num_col):
        fisher_info_nu_phi[j] = np.sum(expected[:, j] * mu)
    
    # For alpha vs alpha (diagonal matrix)
    fisher_info_alpha_alpha = np.zeros((num_row, num_row))
    np.fill_diagonal(fisher_info_alpha_alpha, np.sum(expected, axis=1) / (alpha**2))
    
    # For beta vs beta
    fisher_info_beta_beta = np.zeros((num_col, num_col))
    np.fill_diagonal(fisher_info_beta_beta, np.sum(expected, axis=0) / (beta**2))
    
    # For alpha vs beta
    fisher_info_alpha_beta = expected / np.outer(alpha, beta)
    
    # For alpha vs phi
    fisher_info_alpha_phi = np.dot(expected, nu) * mu / alpha
    
    # For beta vs phi
    fisher_info_beta_phi = np.dot(mu, expected) * nu / beta
    
    # For phi vs phi
    fisher_info_phi_phi = np.sum(expected * np.outer(mu**2, nu**2))
    
    # For alpha, beta, mu, nu, phi vs eta (Lagrange multipliers)
    # These are needed for the full Fisher information matrix but won't affect 
    # the covariance matrix of mu, nu, phi, so we'll compute them as placeholders
    eta_dim = 5  # Number of Lagrange multipliers
    
    # Build the complete Fisher information matrix
    total_dim = 2*(num_row + num_col) + 1 + eta_dim
    fisher_info = np.zeros((total_dim, total_dim))
    
    # Fill in all the blocks
    # Alpha block
    fisher_info[:num_row, :num_row] = fisher_info_alpha_alpha
    fisher_info[:num_row, num_row:num_row+num_col] = fisher_info_alpha_beta
    
    # For alpha vs mu
    fisher_info_alpha_mu = np.zeros((num_row, num_row))
    for i in range(num_row):
        for j in range(num_row):
            if i == j:
                fisher_info_alpha_mu[i, j] = phi * np.sum(expected[i, :] * nu)
            else:
                fisher_info_alpha_mu[i, j] = 0
    
    # For alpha vs nu
    fisher_info_alpha_nu = np.zeros((num_row, num_col))
    for i in range(num_row):
        for j in range(num_col):
            fisher_info_alpha_nu[i, j] = phi * expected[i, j] * nu[j]
    
    # For beta vs mu
    fisher_info_beta_mu = np.zeros((num_col, num_row))
    for i in range(num_col):
        for j in range(num_row):
            fisher_info_beta_mu[i, j] = phi * expected[j, i] * mu[j]
    
    # For beta vs nu
    fisher_info_beta_nu = np.zeros((num_col, num_col))
    for i in range(num_col):
        for j in range(num_col):
            if i == j:
                fisher_info_beta_nu[i, j] = phi * np.sum(expected[:, i] * mu)
            else:
                fisher_info_beta_nu[i, j] = 0
    
    fisher_info[:num_row, num_row+num_col:num_row+num_col+num_row] = fisher_info_alpha_mu
    fisher_info[:num_row, num_row+num_col+num_row:num_row+num_col+num_row+num_col] = fisher_info_alpha_nu
    fisher_info[:num_row, -eta_dim-1] = fisher_info_alpha_phi
    
    # Beta block
    fisher_info[num_row:num_row+num_col, :num_row] = fisher_info_alpha_beta.T
    fisher_info[num_row:num_row+num_col, num_row:num_row+num_col] = fisher_info_beta_beta
    fisher_info[num_row:num_row+num_col, num_row+num_col:num_row+num_col+num_row] = fisher_info_beta_mu
    fisher_info[num_row:num_row+num_col, num_row+num_col+num_row:num_row+num_col+num_row+num_col] = fisher_info_beta_nu
    fisher_info[num_row:num_row+num_col, -eta_dim-1] = fisher_info_beta_phi
    
    # Mu block
    fisher_info[num_row+num_col:num_row+num_col+num_row, :num_row] = fisher_info_alpha_mu.T
    fisher_info[num_row+num_col:num_row+num_col+num_row, num_row:num_row+num_col] = fisher_info_beta_mu.T
    fisher_info[num_row+num_col:num_row+num_col+num_row, num_row+num_col:num_row+num_col+num_row] = fisher_info_mu_mu
    fisher_info[num_row+num_col:num_row+num_col+num_row, num_row+num_col+num_row:num_row+num_col+num_row+num_col] = fisher_info_mu_nu
    fisher_info[num_row+num_col:num_row+num_col+num_row, -eta_dim-1] = fisher_info_mu_phi
    
    # Nu block
    fisher_info[num_row+num_col+num_row:num_row+num_col+num_row+num_col, :num_row] = fisher_info_alpha_nu.T
    fisher_info[num_row+num_col+num_row:num_row+num_col+num_row+num_col, num_row:num_row+num_col] = fisher_info_beta_nu.T
    fisher_info[num_row+num_col+num_row:num_row+num_col+num_row+num_col, num_row+num_col:num_row+num_col+num_row] = fisher_info_mu_nu.T
    fisher_info[num_row+num_col+num_row:num_row+num_col+num_row+num_col, num_row+num_col+num_row:num_row+num_col+num_row+num_col] = fisher_info_nu_nu
    fisher_info[num_row+num_col+num_row:num_row+num_col+num_row+num_col, -eta_dim-1] = fisher_info_nu_phi
    
    # Phi block
    fisher_info[-eta_dim-1, :num_row] = fisher_info_alpha_phi
    fisher_info[-eta_dim-1, num_row:num_row+num_col] = fisher_info_beta_phi
    fisher_info[-eta_dim-1, num_row+num_col:num_row+num_col+num_row] = fisher_info_mu_phi
    fisher_info[-eta_dim-1, num_row+num_col+num_row:num_row+num_col+num_row+num_col] = fisher_info_nu_phi
    fisher_info[-eta_dim-1, -eta_dim-1] = fisher_info_phi_phi
    
    # Skip filling Eta blocks as they won't affect our parameter covariance matrix
    
    # Extract submatrix corresponding to mu, nu, phi (parameters we're interested in)
    start_idx = num_row + num_col
    end_idx = start_idx + num_row + num_col + 1
    param_idx = list(range(start_idx, end_idx))
    
    # Extract the block of Fisher information matrix for mu, nu, phi
    fisher_submatrix = fisher_info[start_idx:end_idx, start_idx:end_idx]
    
    # Get covariance matrix as inverse of Fisher information
    try:
        # First try using our pseudo-inverse function
        cov_matrix = pseudo_inv(fisher_submatrix)
    except Exception as e:
        # If that fails, try numpy's pinv
        cov_matrix = np.linalg.pinv(fisher_submatrix)
    
    # Extract standard errors
    mu_se = np.sqrt(np.diag(cov_matrix)[:num_row])
    nu_se = np.sqrt(np.diag(cov_matrix)[num_row:num_row+num_col])
    phi_se = np.sqrt(np.diag(cov_matrix)[-1])
    
    # Calculate odds ratios
    max_odd_ratio = -np.inf
    min_odd_ratio = np.inf
    row_of_max_or = [0, 0]
    col_of_max_or = [0, 0]
    row_of_min_or = [0, 0]
    col_of_min_or = [0, 0]
    
    for i1 in range(num_row-1):
        for i2 in range(i1+1, num_row):
            for j1 in range(num_col-1):
                for j2 in range(j1+1, num_col):
                    # Calculate odds ratio
                    odd_ratio = expected[i1,j1] * expected[i2,j2] / (expected[i2,j1] * expected[i1,j2])
                    
                    if odd_ratio > max_odd_ratio:
                        max_odd_ratio = odd_ratio
                        row_of_max_or = [i1, i2]
                        col_of_max_or = [j1, j2]
                    
                    if odd_ratio < min_odd_ratio:
                        min_odd_ratio = odd_ratio
                        row_of_min_or = [i1, i2]
                        col_of_min_or = [j1, j2]
    
    # Handle special cases for polynomial degrees
    if poly_deg_row == 1:
        row_of_min_or = "Any"
    if poly_deg_col == 1:
        col_of_min_or = "Any"
    

    
    # Create and return results dictionary
    result = {
        'observed': working_table,
        'expected': expected,
        'alpha': alpha,
        'beta': beta,
        'mu': mu,
        'nu': nu,
        'phi': phi,
        'mu_se': mu_se,
        'nu_se': nu_se,
        'phi_se': phi_se,
        'cov_matrix': cov_matrix,
        'g_squared': g_squared,
        'd_o_f': df,
        'p_value': p_value,
        'dissimilarity': dissimilarity,
        'tau_given_row': tau_given_row,
        'tau_given_col': tau_given_col,
        'std_residuals': std_residuals,
        'max_odd_ratio': max_odd_ratio,
        'min_odd_ratio': min_odd_ratio,
        'row_of_max_or': row_of_max_or,
        'col_of_max_or': col_of_max_or,
        'row_of_min_or': row_of_min_or,
        'col_of_min_or': col_of_min_or,
        'model_is_fit': p_value > p_value_threshold,
        'poly_deg_row': poly_deg_row,
        'poly_deg_col': poly_deg_col,
        'debug_info': debug_info if debug else None
    }
    
    return result

def exploratory_computation(contingency_table, upper_polarity_idx=None, satisfaction_constraint=None, debug=False):
    """
    Perform exploratory analysis on ranking data.
    
    Args:
        contingency_table (np.ndarray): The contingency table of rankings
        upper_polarity_idx (int or list, optional): Index or list of indices for upper polarity
        satisfaction_constraint (str or int, optional): "First", "Last", "None", or integer index of satisfaction level to omit
        debug (bool, optional): Whether to print debug information
        
    Returns:
        dict: Results of exploratory analysis
    """
    try:
        # Ensure that we're working with numpy arrays, not pandas DataFrames
        if hasattr(contingency_table, 'values'):
            original_contingency_table = contingency_table.values.copy()
        else:
            original_contingency_table = contingency_table.copy()
        
        # Store the original table before any modifications
        working_table = original_contingency_table.copy()
        
        # Define the constrained satisfaction level index
        constraint_index = None
        satisfaction_level_removed = False
        
        # Convert satisfaction constraint from text to index
        if isinstance(satisfaction_constraint, str):
            if satisfaction_constraint.lower() == "none":
                satisfaction_constraint = None
                constraint_index = None
                satisfaction_level_removed = False
            elif satisfaction_constraint.lower() == "first":
                constraint_index = 0
                satisfaction_level_removed = True
            elif satisfaction_constraint.lower() == "last":
                constraint_index = working_table.shape[1] - 1
                satisfaction_level_removed = True
            else:
                # Invalid constraint - default to None
                satisfaction_constraint = None
                constraint_index = None
                satisfaction_level_removed = False
        elif isinstance(satisfaction_constraint, (int, np.integer)):
            # Direct index provided
            if 0 <= satisfaction_constraint < working_table.shape[1]:
                constraint_index = satisfaction_constraint
                satisfaction_level_removed = True
            else:
                # Out of bounds - default to None
                constraint_index = None
                satisfaction_level_removed = False
        else:
            # Not a string or integer, default to None
            satisfaction_constraint = None
            constraint_index = None
            satisfaction_level_removed = False
        
        # Create the omitted contingency table (without the constrained level) if needed
        omitted_column = None
        if satisfaction_level_removed and constraint_index is not None:
            # Extract the omitted column for later use
            omitted_column = working_table[:, constraint_index].copy()
            # Create a contingency table without the constrained level
            working_table = np.delete(working_table, constraint_index, axis=1)
            
        # Replace zeros with 0.5 (for computational stability)
        working_table[working_table == 0] = 0.5
        
        contingency_table = working_table  # Update the working table
    
        def distance_between_two_brands(first_level, second_level):
            observed = np.column_stack((first_level, second_level))
            expected = independence.expected_count(observed)  # Placeholder for R function
            return independence.l_sq_chi_sq(observed, expected)  # Placeholder for R function
    
        # Clustering the brands
        num_brand = contingency_table.shape[0]
        num_level = contingency_table.shape[1]
        predefined_dist_threshold = [2.71, 4.60 ,6.25 ,7.78 ,9.24,
                                     10.64, 12.02 ,13.36 ,14.68, 15.99, 17.27 ,18.55, 19.81, 21.06 ,22.31,
                                     23.54, 24.77, 25.99, 27.20, 28.41, 29.61]
        dist_threshold = predefined_dist_threshold[num_level-2]
        brand_dist = np.zeros((num_brand, num_brand))

        for j in range(num_brand-1):
            for i in range(j+1, num_brand):
                brand_dist[i, j] = distance_between_two_brands(contingency_table[i, :], contingency_table[j, :])
                brand_dist[j, i] = brand_dist[i, j]
        
        np.fill_diagonal(brand_dist, dist_threshold + 1)
        # Iteratively cluster the brands
        cluster_label = np.arange(1, num_brand+1)
        for _ in range(num_brand):
            idx = np.unravel_index(np.argmin(brand_dist, axis=None), brand_dist.shape)
            b1, b2 = idx
            if brand_dist[b1, b2] > dist_threshold:
                break
            cluster_label[cluster_label == cluster_label[b2]] = cluster_label[b1]
            merged_cluster_idx = np.where(cluster_label == cluster_label[b1])[0]
            
            for b in range(num_brand):
                dist_from_b = np.max(brand_dist[b, merged_cluster_idx])
                brand_dist[b, merged_cluster_idx] = dist_from_b
                brand_dist[merged_cluster_idx, b] = dist_from_b

        # Assign temporary labels to clusters
        temp_label = np.zeros(num_brand)
        cluster_order = cluster_label[np.unique(cluster_label, return_index=True)[1]]
        for i, order in enumerate(cluster_order):
            temp_label[cluster_label == order] = i + 1
        cluster_label = temp_label.astype(int)
        num_cluster = len(np.unique(cluster_label))

        # Collapse the contingency table by clusters of brands
        if num_cluster == num_brand:
            collapsed_table = contingency_table
        else:
            collapsed_table = np.zeros((num_cluster, num_level))
            for i in range(num_cluster):
                collapsed_table[i, :] = np.sum(contingency_table[cluster_label == i+1, :], axis=0)

        collapsed_contingency_table_rows = collapsed_table.copy()
        collapsed_table = collapsed_table / np.sum(collapsed_table, axis=1, keepdims=True)

        # Finding best stochastic ordering and partition
        found_stochastic_ordering = False
        highest_l_sq_stat = -np.inf
        best_partition = None

        for num_col in range(num_level, 0, -1):
            partitions = consecutive_partition(num_level, num_col)  # Placeholder for R function
            for p in range(partitions.shape[1]):
                partition = partitions[:, p]
                collapsed_c_table = np.zeros((num_cluster, num_col))
                for i in range(num_col):
                    collapsed_c_table[:, i] = np.sum(collapsed_table[:, partition == i+1], axis=1)

                cdf = np.apply_along_axis(np.cumsum, 1, collapsed_c_table)
                sto_ordering = stochastic_ordering(cdf)  # Placeholder for R function
                if sto_ordering is not None:
                    found_stochastic_ordering = True
                    l_sq_stat = independence.l_sq_chi_sq(collapsed_c_table, independence.expected_count(collapsed_c_table))  # Placeholder
                    if l_sq_stat > highest_l_sq_stat:
                        highest_l_sq_stat = l_sq_stat
                        best_partition = partition
        
            if found_stochastic_ordering:
                break
        
        num_collapsed_level = len(np.unique(best_partition))
        collapsed_level_table = np.zeros((num_cluster, num_collapsed_level))
        for i in range(num_collapsed_level):
            collapsed_level_table[:, i] = np.sum(collapsed_table[:, best_partition == i+1], axis=1)
        
        cdf = np.apply_along_axis(np.cumsum, 1, collapsed_level_table)
        cdf[:, -1] = 1
        stochastic_ordering_result = stochastic_ordering(cdf)  # Placeholder for R function

        # Perform ANOAS analysis on collapsed table
        brand_groups = [np.where(cluster_label == i)[0] for i in range(1, num_cluster+1)]
        anoas_result = independence.anoas_computation2(contingency_table, contingency_table, brand_groups)  # Placeholder

        # Compute polarity index
        if upper_polarity_idx is None:
            polarity_index = collapsed_level_table[:, -1] / collapsed_level_table[:, 0]
        else:
            first_upper_idx = min(upper_polarity_idx)
            partition_i = best_partition[first_upper_idx - 1]
            if partition_i == 1:
                partition_i = 2
            polarity_index = np.sum(collapsed_level_table[:, partition_i-1:], axis=1) / np.sum(collapsed_level_table[:, :partition_i-1], axis=1)

        # Compute basic rankings
        full_pmf = contingency_table / np.sum(contingency_table, axis=1, keepdims=True)
        avg_satisfaction = np.dot(full_pmf, np.arange(1, num_level+1))
        rank_by_avg = np.argsort(avg_satisfaction)[::-1]
        rank_by_best = np.argsort(full_pmf[:, -1])[::-1]
        rank_by_best_two = np.argsort(np.sum(full_pmf[:, [-2, -1]], axis=1))[::-1]
        rank_by_worst = np.argsort(full_pmf[:, 0])[::-1]
        rank_by_worst_two = np.argsort(np.sum(full_pmf[:, [0, 1]], axis=1))[::-1]

        # Handle reintegration of the omitted satisfaction level if needed
        pmf_with_constraint = None
        if satisfaction_level_removed and constraint_index is not None:
            # Create collapsed version of the omitted column based on brand clusters
            omitted_column_collapsed = np.zeros(num_cluster)
            for i in range(1, num_cluster + 1):
                indices = np.where(cluster_label == i)[0]
                omitted_column_collapsed[i-1] = np.sum(omitted_column[indices])
            
            # Create matrix m for collapsed table without omitted category
            m = np.zeros((num_cluster, num_collapsed_level))
            for i in range(num_collapsed_level):
                mask = best_partition == i + 1
                m[:, i] = np.sum(collapsed_contingency_table_rows[:, mask], axis=1)
            
            # Create contingency table with the omitted column reintegrated
            if constraint_index == 0:  # First column
                # Add as first column
                m_with_constraint = np.column_stack((
                    omitted_column_collapsed, m
                ))
            else:  # Last column or other
                # Add as last column
                m_with_constraint = np.column_stack((
                    m, omitted_column_collapsed
                ))
            
            # Calculate PMF with the constraint included
            pmf_with_constraint = m_with_constraint / np.sum(m_with_constraint, axis=1, keepdims=True)
        
        # Calculate Z-matrix for stochastic ordering
        # Get dimensions and data
        mid_category = np.ceil(contingency_table.shape[1] / 2).astype(int)
        mid_category_partition = best_partition[mid_category - 1]  # -1 for 0-based indexing
        
        # Calculate bottom and top halves
        bottom_mask = np.array([p <= mid_category_partition for p in best_partition])
        top_mask = np.array([p > mid_category_partition for p in best_partition])
        
        # Handle both matrix and vector cases
        if len(collapsed_contingency_table_rows.shape) > 1:
            bottom_half = np.sum(collapsed_contingency_table_rows[:, bottom_mask], axis=1)
            top_half = np.sum(collapsed_contingency_table_rows[:, top_mask], axis=1)
        else:
            bottom_half = collapsed_contingency_table_rows[bottom_mask]
            top_half = collapsed_contingency_table_rows[top_mask]
        
        # Calculate Z-statistics
        z_matrix = np.zeros((num_cluster, num_cluster))
        for i in range(num_cluster):
            for j in range(i + 1, num_cluster):
                bottom_i = bottom_half[i]
                top_i = top_half[i]
                bottom_j = bottom_half[j]
                top_j = top_half[j]
                
                # Calculate z-statistic
                z_ij = (np.log(top_i/bottom_i) - np.log(top_j/bottom_j)) / np.sqrt(1/top_i + 1/bottom_i + 1/top_j + 1/bottom_j)
                z_matrix[i, j] = z_ij
        
        # Create the results dictionary
        results = {
            'observed': anoas_result['full_result']['observed'],  # Placeholder for anoas result structure
            'collapsed_contingency_table_rows': collapsed_contingency_table_rows,
            'full_l_sq_stat': anoas_result['full_result']['l_sq_stat'],  # Placeholder
            'full_d_o_f': anoas_result['full_result']['d_of'],  # Placeholder
            'full_p_value': anoas_result['full_result']['p_value'],  # Placeholder
            'critical_value': dist_threshold,
            'brand_cluster': cluster_label,
            'satisfaction_partition': best_partition,
            'collapsed_pmf': collapsed_level_table,
            'stochastic_ordering': stochastic_ordering_result,
            'polarity_index': polarity_index,
            'upper_polarity_idx': upper_polarity_idx,  # Add the upper polarity index to the results
            'satisfaction_constraint': constraint_index,  # Add the satisfaction constraint to the results
            'change_in_l_sq': anoas_result['change_in_l_sq'],  # Placeholder
            'change_in_d_o_f': anoas_result['change_in_d_of'],  # Placeholder
            'information_p_value': anoas_result['information_p_value'],  # Placeholder
            'information_is_lost': anoas_result['information_is_lost'],  # Placeholder
            'avg_satisfaction': avg_satisfaction,
            'rank_by_avg': rank_by_avg,
            'rank_by_best': rank_by_best,
            'rank_by_best_two': rank_by_best_two,
            'rank_by_worst': rank_by_worst,
            'rank_by_worst_two': rank_by_worst_two,
            'z_matrix': z_matrix,
            'bottom_half': bottom_half,
            'top_half': top_half,
            'mid_category_partition': mid_category_partition
        }
        
        # Add satisfaction constraint information if applicable
        if satisfaction_level_removed:
            results['original_contingency_table'] = original_contingency_table
            results['omitted_column'] = omitted_column
            results['omitted_column_collapsed'] = omitted_column_collapsed
            results['pmf_with_constraint'] = pmf_with_constraint
        
        return results
        
    except Exception as e:
        # Create a minimal result dict with necessary keys
        import traceback
        traceback.print_exc()
        basic_result = {
            'brand_cluster': np.ones(contingency_table.shape[0], dtype=int),
            'satisfaction_partition': np.arange(1, contingency_table.shape[1] + 1),
            'polarity_index': np.ones(contingency_table.shape[0]),
            'avg_satisfaction': np.ones(contingency_table.shape[0])
        }
        return basic_result

def confirmatory_computation(contingency_table, upper_polarity_idx=None, satisfaction_constraint=None, debug=False, mu_diff_threshold=1.96):
    """
    Perform confirmatory analysis on ranking data.
    
    Args:
        contingency_table (np.ndarray): The contingency table of rankings
        upper_polarity_idx (int, optional): Index for upper polarity
        satisfaction_constraint (str or int, optional): "First", "Last", "None", or integer index of satisfaction level to omit
        debug (bool, optional): Whether to print debug information
        mu_diff_threshold (float, optional): Threshold for brand clustering, default is 1.96 (same as R code)
        
    Returns:
        dict: Results of confirmatory analysis
    """
    # Comment out the problematic st.write calls
    # st.write('start confirmatory computation')
    # st.write(contingency_table)
    
    try:
        # Ensure that we're working with numpy arrays, not pandas DataFrames
        if hasattr(contingency_table, 'values'):
            # Convert pandas DataFrame to numpy array
            original_contingency_table = contingency_table.values.copy()
        else:
            # Already a numpy array
            original_contingency_table = contingency_table.copy()
        
        # Store the original table before any modifications
        working_table = original_contingency_table.copy()
        
        # Define the constrained satisfaction level index
        constraint_index = None
        satisfaction_level_removed = False
        
        # Convert satisfaction constraint from text to index
        if isinstance(satisfaction_constraint, str):
            if satisfaction_constraint.lower() == "none":
                satisfaction_constraint = None
                constraint_index = None
                satisfaction_level_removed = False
            elif satisfaction_constraint.lower() == "first":
                constraint_index = 0
                satisfaction_level_removed = True
            elif satisfaction_constraint.lower() == "last":
                constraint_index = working_table.shape[1] - 1
                satisfaction_level_removed = True
            else:
                # Invalid constraint - default to None
                satisfaction_constraint = None
                constraint_index = None
                satisfaction_level_removed = False
        else:
            # If not a string, default to None
            satisfaction_constraint = None
            constraint_index = None
            satisfaction_level_removed = False
        
        # Create the omitted contingency table (without the constrained level) if needed
        if satisfaction_level_removed and constraint_index is not None:
            # Extract the omitted column for later use
            omitted_column = working_table[:, constraint_index].copy()
            # Create a contingency table without the constrained level
            working_table = np.delete(working_table, constraint_index, axis=1)
        
        # Replace zeros with 0.5 (as in R code)
        working_table[working_table == 0] = 0.5
        
        # Compute exponential spacing model
        n_rows, n_cols = working_table.shape
        exp_spacing_result = exponential_spacing_computation(
            working_table, 
            poly_deg_row=n_rows-1, 
            poly_deg_col=n_cols-1
        )
        

        # Iteratively cluster the brands as in the R code
        cluster_label = np.zeros(n_rows, dtype=int)
        
        for t in range(1, n_rows + 1):
            # Find unclustered indices
            unclustered_idx = np.where(cluster_label == 0)[0]
            if len(unclustered_idx) == 0:
                break
                
            # Find the brand with max mu value among unclustered brands
            mu_values = exp_spacing_result['mu'][unclustered_idx]
            max_mu_idx = unclustered_idx[np.argmax(mu_values)]
            
            # Get mu and variance for this brand
            b_mu = exp_spacing_result['mu'][max_mu_idx]
            b_var = exp_spacing_result['cov_matrix'][max_mu_idx, max_mu_idx]
            
            # Check which other brands should be in this cluster
            in_next_cluster = np.zeros(len(unclustered_idx), dtype=bool)
            
            for i, idx in enumerate(unclustered_idx):
                if idx == max_mu_idx:
                    in_next_cluster[i] = True
                    continue
                    
                i_mu = exp_spacing_result['mu'][idx]
                i_var = exp_spacing_result['cov_matrix'][idx, idx]
                b_i_cov = exp_spacing_result['cov_matrix'][max_mu_idx, idx]
                
                # Calculate mu difference statistic
                mu_diff = (b_mu - i_mu) / np.sqrt(b_var + i_var - 2 * b_i_cov)
                
                if mu_diff <= mu_diff_threshold:
                    in_next_cluster[i] = True
            
            # Assign cluster labels
            cluster_label[unclustered_idx[in_next_cluster]] = t
        
        # Reindex cluster labels to be consecutive 1, 2, 3, ...
        temp_label = np.zeros(n_rows, dtype=int)
        unique_clusters = np.unique(cluster_label)
        unique_clusters = unique_clusters[unique_clusters != 0]  # Remove 0 if present
        
        for i, cluster in enumerate(unique_clusters):
            temp_label[cluster_label == cluster] = i + 1
            
        cluster_label = temp_label
        
        # Get number of clusters
        num_cluster = len(np.unique(cluster_label))
        
        # Create collapsed table
        if num_cluster == n_rows:
            collapsed_table = working_table.copy()
        else:
            collapsed_table = np.zeros((num_cluster, n_cols))
            for i in range(1, num_cluster + 1):
                collapsed_table[i-1] = np.sum(working_table[cluster_label == i], axis=0)
        
        # Define function to merge opposite trend
        def merge_opposite_trend(x, increasing=True):
            if len(x) == 1:
                return np.array([1])
                
            if increasing:
                if x[1] <= x[0]:
                    possible_j = np.where(x > x[0])[0]
                    if len(possible_j) == 0:
                        return np.ones(len(x), dtype=int)
                    j = possible_j[0]
                else:
                    j = 1
            else:
                if x[1] > x[0]:
                    possible_j = np.where(x <= x[0])[0]
                    if len(possible_j) <= 1:
                        return np.ones(len(x), dtype=int)
                    j = possible_j[1]
                else:
                    j = 1
                    
            result = np.concatenate([
                np.ones(j, dtype=int),
                merge_opposite_trend(x[j:], increasing) + 1
            ])
            return result
        
        # Implement the collapsing levels logic
        current_partition = np.arange(1, n_cols + 1)
        no_more_collapsing = False
        
        
        while current_partition[-1] > 2 and not no_more_collapsing:
            num_col = current_partition[-1]
            collapsed_c_table = np.zeros((num_cluster, num_col))
            
            for i in range(1, num_col + 1):
                mask = current_partition == i
                collapsed_c_table[:, i-1] = np.sum(collapsed_table[:, mask], axis=1)
            
            # Compute another exponential spacing model on the collapsed table
            collapsed_spacing_result = exponential_spacing_computation(
                collapsed_c_table,
                poly_deg_row=num_cluster-1,
                poly_deg_col=num_col-1
            )
            
            # Get nu partition
            nu = collapsed_spacing_result['nu']
            phi = collapsed_spacing_result['phi']
            nu_partition = merge_opposite_trend(nu, increasing=(phi > 0))
            
            if nu_partition[-1] == num_col:
                no_more_collapsing = True
            else:
                # Update current partition
                new_partition = np.zeros_like(current_partition)
                for i in range(len(nu_partition)):
                    new_partition[current_partition == i+1] = nu_partition[i]
                current_partition = new_partition
        
        # Get the number of collapsed levels
        num_collapsed_level = current_partition[-1]
        
        # Create the collapsed level table
        collapsed_level_table = np.zeros((num_cluster, num_collapsed_level))
        for i in range(1, num_collapsed_level + 1):
            mask = current_partition == i
            collapsed_level_table[:, i-1] = np.sum(collapsed_table[:, mask], axis=1)
        
        # Fit an exponential spacing model to the collapsed level table
        collapsed_exp_spacing_result = exponential_spacing_computation(
            collapsed_level_table,
            poly_deg_row=num_cluster-1,
            poly_deg_col=num_collapsed_level-1
        )
        # Calculate PMF
        expected_collapsed_pmf = collapsed_exp_spacing_result['expected'] / np.sum(collapsed_exp_spacing_result['expected'], axis=1, keepdims=True)
        
        # Compute CDF for stochastic ordering
        cdf = np.apply_along_axis(np.cumsum, 1, expected_collapsed_pmf)
        cdf[:, -1] = 1  # Ensure last column is exactly 1 to avoid numerical issues
        
        # Get stochastic ordering
        stochastic_order = stochastic_ordering(cdf)
        has_stochastic_ordering = stochastic_order is not None
        
        # Calculate average satisfaction (weighted by position)
        weights = np.arange(1, n_cols + 1)
        
        # Ensure correct matrix orientation and broadcasting
        try:
            # Reshape weights for broadcasting with columns
            weights_reshaped = weights.reshape(1, n_cols)
            
            # Try broadcasting using numpy's broadcasting rules
            weighted_table = working_table * weights_reshaped
            
            # Calculate the average satisfaction
            avg_satisfaction = np.sum(weighted_table, axis=1) / np.sum(working_table, axis=1)
        except Exception as e:
            # Try using numpy's einsum for explicit broadcasting control
            try:
                # Use einsum for explicit dimension control
                weighted_sum = np.einsum('ij,j->i', working_table, weights)
                row_sums = np.sum(working_table, axis=1)
                avg_satisfaction = weighted_sum / row_sums
            except Exception as e2:
                # Last resort - try transposing
                working_table_t = working_table.T
                weights_t = np.arange(1, working_table_t.shape[1] + 1)
                
                try:
                    weighted_sum_t = np.sum(working_table_t * weights_t.reshape(1, -1), axis=1)
                    row_sums_t = np.sum(working_table_t, axis=1)
                    avg_satisfaction = weighted_sum_t / row_sums_t
                    
                    # Need to transpose everything else accordingly
                    working_table = working_table_t
                    n_rows, n_cols = n_cols, n_rows  # Swap dimensions
                except Exception as e3:
                    raise e  # Re-raise original error
        
        # Compute polarity index
        if upper_polarity_idx is None:
            # Default: last level divided by first level
            polarity_index = expected_collapsed_pmf[:, -1] / expected_collapsed_pmf[:, 0]
        else:
            # Custom upper tail: combined probability of upper tail divided by (1 - upper tail)
            try:
                # Parse the upper polarity input (e.g., "4-7")
                if isinstance(upper_polarity_idx, str) and "-" in upper_polarity_idx:
                    parts = upper_polarity_idx.split("-")
                    start_idx = int(parts[0])
                    end_idx = int(parts[1])
                    upper_polarity_idx = list(range(start_idx, end_idx + 1))
                
                # For list input, find the minimum index (starting point of upper tail)
                if isinstance(upper_polarity_idx, list) and len(upper_polarity_idx) > 0:
                    first_upper_idx = min(upper_polarity_idx)
                    # Convert to 0-based indexing
                    first_upper_idx = first_upper_idx - 1  # Convert to 0-based index
                    
                    # Ensure index doesn't exceed number of columns
                    if first_upper_idx >= n_cols:
                        first_upper_idx = n_cols - 1
                    
                    # Get the partition index if using collapsed levels
                    if len(current_partition) > first_upper_idx:
                        partition_i = current_partition[first_upper_idx]
                    else:
                        partition_i = current_partition[-1]  # Use last partition
                    
                    # Calculate upper tail probability (sum of probabilities in the upper tail)
                    upper_tail = np.sum(expected_collapsed_pmf[:, partition_i-1:], axis=1)
                    
                    # Calculate polarity as upper_tail / (1 - upper_tail)
                    polarity_index = upper_tail / (1 - upper_tail)
                else:
                    # Fallback to default if the input is not properly formatted
                    polarity_index = expected_collapsed_pmf[:, -1] / expected_collapsed_pmf[:, 0]
            except Exception as e:
                if debug:
                    print(f"Error calculating polarity index with upper_polarity_idx={upper_polarity_idx}: {str(e)}")
                # Fall back to default calculation
                polarity_index = expected_collapsed_pmf[:, -1] / expected_collapsed_pmf[:, 0]
        
        # Calculate PMF for each brand
        pmf = working_table / np.sum(working_table, axis=1, keepdims=True)
        
        # Calculate basic rankings
        full_pmf = working_table / np.sum(working_table, axis=1, keepdims=True)
        rank_by_avg = np.argsort(-avg_satisfaction)  # negative for descending order
        rank_by_best = np.argsort(-full_pmf[:, -1])
        rank_by_best_two = np.argsort(-np.sum(full_pmf[:, [-2, -1]], axis=1))
        rank_by_worst = np.argsort(-full_pmf[:, 0])
        rank_by_worst_two = np.argsort(-np.sum(full_pmf[:, :2], axis=1))
        
        # Compute Z-test for stochastic ordering (matching R code logic)
        mid_category = np.ceil(n_cols / 2).astype(int)  # Use ceiling as in R code
        mid_category_partition = current_partition[mid_category - 1] if mid_category <= len(current_partition) else current_partition[-1]
        
        # Split columns by satisfaction partition value
        bottom_half_cols = np.where(current_partition <= mid_category_partition)[0]
        top_half_cols = np.where(current_partition > mid_category_partition)[0]
        
        # Calculate sums for each half
        if len(bottom_half_cols) == 0:
            # Handle edge case where partitioning puts everything in top half
            bottom_half = np.ones(collapsed_table.shape[0])
        elif len(bottom_half_cols) == 1:
            # Single column case
            bottom_half = collapsed_table[:, bottom_half_cols[0]]
        else:
            bottom_half = np.sum(collapsed_table[:, bottom_half_cols], axis=1)
            
        if len(top_half_cols) == 0:
            # Handle edge case where partitioning puts everything in bottom half
            top_half = np.ones(collapsed_table.shape[0])
        elif len(top_half_cols) == 1:
            # Single column case
            top_half = collapsed_table[:, top_half_cols[0]]
        else:
            top_half = np.sum(collapsed_table[:, top_half_cols], axis=1)
        
        # Initialize z_matrix with string values to match R output format
        z_matrix_str = np.full((num_cluster, num_cluster), "", dtype=object)
        # Fill diagonal with "\"
        np.fill_diagonal(z_matrix_str, "\\")
        
        # Calculate z-values only for upper triangle (i < j)
        z_matrix = np.zeros((num_cluster, num_cluster))
        critical_value = stats.norm.ppf(0.95)
        
        for i in range(num_cluster):
            for j in range(i+1, num_cluster):
                bottom_i, top_i = bottom_half[i], top_half[i]
                bottom_j, top_j = bottom_half[j], top_half[j]
                
                # Calculate z-statistic without sqrt in denominator (matching R code)
                z_ij = (np.log(top_i/bottom_i) - np.log(top_j/bottom_j)) / \
                       (1/top_i + 1/bottom_i + 1/top_j + 1/bottom_j)
                z_matrix[i, j] = z_ij
                
                # Mark with "X" if significant
                if z_ij > critical_value:
                    z_matrix_str[i, j] = "X"
        
        # Create brand labels based on clusters
        brand_clusters = {}
        for i, cluster_id in enumerate(cluster_label):
            if cluster_id not in brand_clusters:
                brand_clusters[cluster_id] = []
            brand_clusters[cluster_id].append(i + 1)  # Convert to 1-based indexing
        
        # Create brand labels for the DataFrame
        brand_labels = []
        for cluster_id in sorted(brand_clusters.keys()):
            brands = brand_clusters[cluster_id]
            if len(brands) == 1:
                brand_label = f"Brand {brands[0]}"
            else:
                brand_label = f"Brands {', '.join(map(str, brands))}"
            
            brand_labels.append(brand_label)
        
        # Create DataFrame for z-matrix
        z_df = pd.DataFrame("", index=brand_labels, columns=brand_labels)
        np.fill_diagonal(z_df.values, "\\")
        
        # Fill in the X's for significant values
        for i in range(num_cluster):
            for j in range(i+1, num_cluster):
                if z_matrix[i, j] > critical_value:
                    z_df.iloc[i, j] = "X"
        
        # Handle reintegration of the omitted satisfaction level if needed
        pmf_with_constraint = None
        if satisfaction_level_removed and constraint_index is not None:
            # Create collapsed version of the omitted column based on brand clusters
            omitted_column_collapsed = np.zeros(num_cluster)
            for i in range(1, num_cluster + 1):
                indices = np.where(cluster_label == i)[0]
                omitted_column_collapsed[i-1] = np.sum(omitted_column[indices])
            
            # Create contingency table with the omitted column reintegrated
            if constraint_index == 0:  # First column
                # Add as first column
                omitted_table_collapsed_with_constraint = np.column_stack((
                    omitted_column_collapsed, collapsed_level_table
                ))
            else:  # Last column or other
                # Add as last column
                omitted_table_collapsed_with_constraint = np.column_stack((
                    collapsed_level_table, omitted_column_collapsed
                ))
            
            # Calculate PMF with the constraint included
            pmf_with_constraint = omitted_table_collapsed_with_constraint / np.sum(omitted_table_collapsed_with_constraint, axis=1, keepdims=True)
        
        # Create and return results dictionary
        chi_sq_stat = independence.pearson_chi_squared(exp_spacing_result['observed'], exp_spacing_result['expected'])
        results = {
            'observed': exp_spacing_result['observed'],
            'expected': exp_spacing_result['expected'],
            'org_exp_spacing_result': exp_spacing_result,
            'brand_cluster': cluster_label,
            'satisfaction_partition': current_partition,
            'collapsed_exp_spacing_result': collapsed_exp_spacing_result,
            'expected_collapsed_pmf': expected_collapsed_pmf,
            'stochastic_ordering': stochastic_order,
            'has_stochastic_ordering': has_stochastic_ordering,
            'polarity_index': polarity_index,
            'upper_polarity_idx': upper_polarity_idx,  # Add the upper polarity index to the results
            'avg_satisfaction': avg_satisfaction,
            'collapsed_pmf': expected_collapsed_pmf,
            'collapsed_contingency_table_rows': collapsed_table,
            'z_matrix': z_matrix,
            'z_matrix_str': z_matrix_str,
            'z_df': z_df,
            'critical_value': critical_value,
            'rank_by_avg': rank_by_avg,
            'rank_by_best': rank_by_best,
            'rank_by_best_two': rank_by_best_two,
            'rank_by_worst': rank_by_worst,
            'rank_by_worst_two': rank_by_worst_two,
            'tau_given_row': exp_spacing_result['tau_given_row'],
            'tau_given_col': exp_spacing_result['tau_given_col'],
            'p_value': exp_spacing_result['p_value'],
            'polynomial_degree_row': exp_spacing_result['poly_deg_row'],
            'polynomial_degree_col': exp_spacing_result['poly_deg_col'],
            'l_sq_stat': exp_spacing_result['g_squared'],
            'model_is_fit': exp_spacing_result['model_is_fit'],
            'chi_sq_stat': chi_sq_stat,
            'g_squared': exp_spacing_result['g_squared'],
            'd_o_f': exp_spacing_result['d_o_f'],
            'mu': exp_spacing_result['mu'],
            'nu': exp_spacing_result['nu'],
            'phi': exp_spacing_result['phi'],
            'mu_se': exp_spacing_result['mu_se'],
            'nu_se': exp_spacing_result['nu_se'],
            'phi_se': exp_spacing_result['phi_se'],
        }
        
        # Add satisfaction constraint information
        if satisfaction_level_removed:
            results['satisfaction_constraint'] = constraint_index
            results['original_contingency_table'] = original_contingency_table
            results['omitted_column'] = omitted_column
            results['omitted_column_collapsed'] = omitted_column_collapsed
            results['pmf_with_constraint'] = pmf_with_constraint
        
        return results
    except Exception as e:
        # Create a minimal result dict with necessary keys
        import traceback
        traceback.print_exc()
        basic_result = {
            'brand_cluster': np.ones(contingency_table.shape[0], dtype=int),
            'satisfaction_partition': np.arange(1, contingency_table.shape[1] + 1),
            'polarity_index': np.ones(contingency_table.shape[0]),
            'avg_satisfaction': np.ones(contingency_table.shape[0])
        }
        return basic_result




# ------------------------------
# Confirmatory Report Function
# ------------------------------
def confirmatory_report(result):
    """
    Generate a report for the confirmatory analysis.
    
    Args:
        result (dict): The output of ranking.confirmatory_computation.
        
    Returns:
        list: A list of report items (strings and DataFrames).
    """
    try:
        report_items = []
        
        # Check if a satisfaction constraint was applied
        has_constraint = 'satisfaction_constraint' in result and result['satisfaction_constraint'] is not None
        
        # Add a note about satisfaction constraint if applicable
        if has_constraint:
            constraint_index = result['satisfaction_constraint']
            if constraint_index == 0:
                constraint_desc = "first"
            else:  # Must be the last column
                constraint_desc = "last"
            
            report_items.append(f"### Analysis with Satisfaction Constraint")
            report_items.append(f"The {constraint_desc} satisfaction level was omitted during analysis and reintegrated afterward.")
        
        # 1. Observed contingency table
        report_items.append("### Observed Data Matrix")
        observed_df = pd.DataFrame(result['observed'])
        report_items.append(observed_df)
        
        # 2. Expected data matrix
        report_items.append("### Expected Data Matrix")
        expected_df = pd.DataFrame(result['expected'])
        report_items.append(expected_df)
        
        # 3. Model statistics (tau, chi-squared, l-squared, d_o_f, p_value)
        report_items.append("### Model Statistics")
        stats_text = f"TAU(Y|X) = {result.get('tau_given_row', 'N/A'):.4f}, TAU(X|Y) = {result.get('tau_given_col', 'N/A'):.4f}\n"
        stats_text += f"Chi-square = {result.get('chi_sq_stat', 'N/A'):.2f}, L-square = {result.get('g_squared', 'N/A'):.2f}\n"
        stats_text += f"D.O.F. = {result.get('d_o_f', 'N/A')}, p-value = {result.get('p_value', 'N/A'):.4f}"
        report_items.append(stats_text)
        
        # 4. Polynomial degrees
        report_items.append("### Polynomial Degrees")
        poly_text = f"Row polynomial degree: {result.get('polynomial_degree_row', 'N/A')}\n"
        poly_text += f"Column polynomial degree: {result.get('polynomial_degree_col', 'N/A')}"
        report_items.append(poly_text)
        
        # 5. Parameter estimates (mu, nu, phi)
        report_items.append("### Parameter Estimates")
        param_data = {
            "Parameter": [],
            "Estimate": [],
            "Std. Error": []
        }
        
        # Add mu parameters
        if 'mu' in result and 'mu_se' in result:
            for i, (mu, se) in enumerate(zip(result['mu'], result['mu_se'])):
                param_data['Parameter'].append(f"mu_{i+1}")
                param_data['Estimate'].append(round(mu, 4))
                param_data['Std. Error'].append(round(se, 4))
        
        # Add nu parameters
        if 'nu' in result and 'nu_se' in result:
            for i, (nu, se) in enumerate(zip(result['nu'], result['nu_se'])):
                param_data['Parameter'].append(f"nu_{i+1}")
                param_data['Estimate'].append(round(nu, 4))
                param_data['Std. Error'].append(round(se, 4))
        
        # Add phi parameter
        if 'phi' in result and 'phi_se' in result:
            param_data['Parameter'].append("phi")
            param_data['Estimate'].append(round(result['phi'], 4))
            param_data['Std. Error'].append(round(result['phi_se'], 4))
        
        param_df = pd.DataFrame(param_data)
        report_items.append(param_df)
        
        # 6. Model diagnostics
        report_items.append("### Model Diagnostics")
        model_fit = result.get('model_is_fit', False)
        diag_text = "Model fits the observed data." if model_fit else "Model does not fit the observed data."
        report_items.append(diag_text)
        
        # 7. Standardized residuals
        if 'std_residuals' in result:
            report_items.append("### Standardized Residuals")
            std_res_df = pd.DataFrame(result['std_residuals'])
            report_items.append(std_res_df)
        
        # 8. Final Ranking with Stochastic Ordering
        report_items.append("### Final Ranking with Stochastic Ordering")
        
        # Add note about polarity index calculation if an upper tail was specified
        if 'upper_polarity_idx' in result and result['upper_polarity_idx'] is not None:
            if isinstance(result['upper_polarity_idx'], list) and len(result['upper_polarity_idx']) > 0:
                levels_str = ", ".join(map(str, result['upper_polarity_idx']))
                report_items.append(f"Note: Polarity Index calculated as: P(Levels {levels_str}) / (1 - P(Levels {levels_str}))")
            elif isinstance(result['upper_polarity_idx'], str) and "-" in result['upper_polarity_idx']:
                report_items.append(f"Note: Polarity Index calculated as: P(Levels {result['upper_polarity_idx']}) / (1 - P(Levels {result['upper_polarity_idx']}))")
        
        # Create brand clusters mapping
        brand_clusters = {}
        for i, cluster_id in enumerate(result['brand_cluster']):
            if cluster_id not in brand_clusters:
                brand_clusters[cluster_id] = []
            brand_clusters[cluster_id].append(i + 1)  # Convert to 1-based indexing
        
        # Create ranking DataFrame
        ranking_dict = {"Brands": []}
        
        # Get dimensions
        num_cluster = result['collapsed_pmf'].shape[0]
        num_collapsed_level = result['collapsed_pmf'].shape[1]
        
        # Add brand labels based on clusters and stochastic ordering
        for i in range(num_cluster):
            # Get brand index based on stochastic ordering if available
            brand_idx = result['stochastic_ordering'][i] if result.get('stochastic_ordering') is not None else i
            
            # Get cluster ID from the sorted list of cluster IDs
            cluster_ids = sorted(brand_clusters.keys())
            if brand_idx < len(cluster_ids):
                cluster_id = cluster_ids[brand_idx]
                brands_in_cluster = brand_clusters[cluster_id]
                
                if len(brands_in_cluster) == 1:
                    brand_label = f"Brand {brands_in_cluster[0]}"
                else:
                    brand_label = f"Brands {', '.join(map(str, brands_in_cluster))}"
                
                ranking_dict["Brands"].append(brand_label)
            else:
                # Fallback if index out of range
                ranking_dict["Brands"].append(f"Brand Cluster {i+1}")
        
        # Create satisfaction level partitions info
        satisfaction_partitions = {}
        for i, partition_id in enumerate(result['satisfaction_partition']):
            if partition_id not in satisfaction_partitions:
                satisfaction_partitions[partition_id] = []
            satisfaction_partitions[partition_id].append(i + 1)  # Level IDs are 1-based
        
        # Add PMF columns with proper labels
        for j in range(num_collapsed_level):
            # Get partition ID for this column
            partition_id = j + 1
            
            # Get levels associated with this partition
            levels_in_partition = satisfaction_partitions.get(partition_id, [])
            
            # Create level label
            if has_constraint and constraint_index == 0:
                # When first level is omitted, add +1 to all level numbers
                if not levels_in_partition:
                    level_key = f"Level {j+2}"  # +2 because index starts at 0 and we skip level 1
                elif len(levels_in_partition) == 1:
                    level_key = f"Level {levels_in_partition[0]+1}"  # +1 to account for omitted level
                else:
                    level_key = f"Levels {min(levels_in_partition)+1}-{max(levels_in_partition)+1}"  # +1 to account for omitted level
            else:
                # Normal labeling without shifting
                if not levels_in_partition:
                    level_key = f"Level {j+1}"
                elif len(levels_in_partition) == 1:
                    level_key = f"Level {levels_in_partition[0]}"
                else:
                    level_key = f"Levels {min(levels_in_partition)}-{max(levels_in_partition)}"
            
            # Initialize column in dict
            ranking_dict[level_key] = []
            
            # Add PMF values for each brand
            for i in range(num_cluster):
                # Get brand index based on stochastic ordering if available
                brand_idx = result['stochastic_ordering'][i] if result.get('stochastic_ordering') is not None else i
                
                # Add the PMF value
                try:
                    pmf_value = result['collapsed_pmf'][brand_idx, j]
                    ranking_dict[level_key].append(round(pmf_value, 2))
                except IndexError:
                    ranking_dict[level_key].append("N/A")
        
        # Add polarity index
        ranking_dict["Polarity Index"] = []
        for i in range(num_cluster):
            # Get brand index based on stochastic ordering if available
            brand_idx = result['stochastic_ordering'][i] if result.get('stochastic_ordering') is not None else i
            
            # Add the polarity index value
            try:
                polarity_value = result['polarity_index'][brand_idx]
                ranking_dict["Polarity Index"].append(round(polarity_value, 2))
            except IndexError:
                ranking_dict["Polarity Index"].append("N/A")
        
        # Create DataFrame for display
        ranking_df = pd.DataFrame(ranking_dict)
        report_items.append(ranking_df)
        
        # Distributions with Omitted Category Reintegrated (if applicable)
        if has_constraint and 'pmf_with_constraint' in result and result['pmf_with_constraint'] is not None:
            report_items.append("\n### Distributions with Omitted Category Reintegrated")
            report_items.append("Distribution after the stochastic ordering was obtained, with the omitted category added back.")
            
            # Create a display table for the pmf with constraint
            pmf_dict = {"Brands": ranking_dict["Brands"].copy()}
            
            # Get number of clusters and columns in the PMF with constraint
            num_cluster = result['pmf_with_constraint'].shape[0]
            num_levels = result['pmf_with_constraint'].shape[1]
            
            # Create satisfaction level labels based on constraint
            if constraint_index == 0:  # First column was omitted
                # When first level is omitted, it's added back as the first column in pmf_with_constraint
                # So the first column is "Level 1" and the rest need to match their original level numbers
                
                # First level is the omitted constraint
                level_labels = ["Level 1"]
                
                # For remaining levels (which are the same as in the collapsed_pmf), add level number +1
                for j in range(num_collapsed_level):
                    partition_id = j + 1
                    levels_in_partition = satisfaction_partitions.get(partition_id, [])
                    
                    if not levels_in_partition:
                        level_key = f"Level {j+2}"  # +2 because j starts at 0 and we skip level 1
                    elif len(levels_in_partition) == 1:
                        level_key = f"Level {levels_in_partition[0]+1}"  # +1 for omitted level
                    else:
                        level_key = f"Levels {min(levels_in_partition)+1}-{max(levels_in_partition)+1}"  # +1 for omitted level
                    
                    level_labels.append(level_key)
                
            else:  # Last column was omitted
                # When last level is omitted, it's added back as the last column in pmf_with_constraint
                # So we need the regular labels plus the omitted level at the end
                
                # Create labels for all existing columns from collapsed_pmf (unmodified)
                level_labels = []
                
                for j in range(num_collapsed_level):
                    partition_id = j + 1
                    levels_in_partition = satisfaction_partitions.get(partition_id, [])
                    
                    if not levels_in_partition:
                        level_key = f"Level {j+1}"
                    elif len(levels_in_partition) == 1:
                        level_key = f"Level {levels_in_partition[0]}"
                    else:
                        level_key = f"Levels {min(levels_in_partition)}-{max(levels_in_partition)}"
                    
                    level_labels.append(level_key)
                
                # Add the omitted level as the last column
                level_labels.append(f"Level {constraint_index+1}")
            
            # Add PMF columns with proper labels
            for j in range(num_levels):
                level_key = level_labels[j]
                
                # Initialize column in dict
                pmf_dict[level_key] = []
                
                # Add PMF values for each brand
                for i in range(num_cluster):
                    # Get brand index based on stochastic ordering if available
                    brand_idx = result['stochastic_ordering'][i] if result.get('stochastic_ordering') is not None else i
                    
                    # Add the PMF value
                    pmf_value = result['pmf_with_constraint'][brand_idx, j]
                    pmf_dict[level_key].append(round(pmf_value, 2))
                
            # Create DataFrame for display
            pmf_df = pd.DataFrame(pmf_dict)
            report_items.append(pmf_df)
            
            # Add note about which level was omitted
            if constraint_index == 0:
                report_items.append("Note: The first satisfaction level (Level 1) was omitted during the analysis.")
            else:
                report_items.append(f"Note: The last satisfaction level (Level {constraint_index+1}) was omitted during the analysis.")
        
        # Add Z-test for stochastic ordering
        report_items.append("### Z-test for Stochastic Ordering")
        report_items.append("--------------------------------------")
        
        # Get dimensions and data
        contingency_table = result['observed']
        satisfaction_partition = result['satisfaction_partition']
        mid_category = np.ceil(contingency_table.shape[1] / 2).astype(int)
        mid_category_partition = satisfaction_partition[mid_category - 1]  # -1 for 0-based indexing
        
        # Get collapsed contingency table rows
        contingency_table_collapsed_rows = result['collapsed_contingency_table_rows']
        
        # Calculate bottom and top halves
        bottom_mask = np.array([p <= mid_category_partition for p in satisfaction_partition])
        top_mask = np.array([p > mid_category_partition for p in satisfaction_partition])
        
        # Handle both matrix and vector cases
        if len(contingency_table_collapsed_rows.shape) > 1:
            bottom_half = np.sum(contingency_table_collapsed_rows[:, bottom_mask], axis=1)
            top_half = np.sum(contingency_table_collapsed_rows[:, top_mask], axis=1)
        else:
            bottom_half = contingency_table_collapsed_rows[bottom_mask]
            top_half = contingency_table_collapsed_rows[top_mask]
        
        # Create the matrix for display
        num_cluster = len(np.unique(result['brand_cluster']))
        temp = np.full((num_cluster + 1, num_cluster + 1), "", dtype=object)
        
        # Fill headers
        temp[0, 0] = "Brands"

        
        # Fill brand labels
        for i in range(num_cluster):
            brand_idx = result['stochastic_ordering'][i] if result.get('stochastic_ordering') is not None else i
            cluster_ids = sorted(brand_clusters.keys())
            if brand_idx < len(cluster_ids):
                cluster_id = cluster_ids[brand_idx]
                brands = brand_clusters[cluster_id]
                brand_label = f"Brand {brands[0]}" if len(brands) == 1 else f"Brands {', '.join(map(str, brands))}"
            else:
                brand_label = f"Brand Cluster {i+1}"
            
            temp[i + 1, 0] = brand_label
            temp[0, i + 1] = brand_label
            temp[i + 1, i + 1] = "\\"
        
        # Fill Z-test results
        for i in range(num_cluster):
            for j in range(i + 1, num_cluster):
                bottom_i = bottom_half[i]
                top_i = top_half[i]
                bottom_j = bottom_half[j]
                top_j = top_half[j]
                
                # Calculate z-statistic
                z_ij = (np.log(top_i/bottom_i) - np.log(top_j/bottom_j)) / np.sqrt(1/top_i + 1/bottom_i + 1/top_j + 1/bottom_j)
                
                if z_ij > 1.645:  # qnorm(0.95)
                    temp[i + 1, j + 1] = "X"
        
        # Convert to DataFrame for better display
        z_test_df = pd.DataFrame(temp)
        report_items.append(z_test_df)
        
        return report_items
        
    except Exception as e:
        import traceback
        traceback.print_exc()
        return [f"Error generating report: {str(e)}"]

# ------------------------------
# Plotting Functions for Confirmatory Analysis
# ------------------------------
def plot_confirmatory_brand_clusters(result):
    """
    Plot the average satisfaction per brand.
    Here we assume result['avg_satisfaction'] exists.
    """
    if 'avg_satisfaction' not in result:
        raise ValueError("Result does not contain 'avg_satisfaction'.")
    avg = result['avg_satisfaction']
    
    # Create hover text
    hover_text = [
        f"<b>Brand {i}</b><br>Avg Satisfaction: {val:.4f}"
        for i, val in enumerate(avg, 1)
    ]
    
    fig = go.Figure()
    fig.add_trace(go.Bar(
        x=list(range(1, len(avg)+1)),
        y=avg,
        marker=dict(color='skyblue', line=dict(color='black', width=1)),
        hovertext=hover_text,
        hoverinfo='text',
        name='Average Satisfaction'
    ))
    
    fig.update_layout(
        title=dict(text="Average Satisfaction per Brand", font=dict(size=16)),
        xaxis=dict(
            title="Brand (by cluster ordering)",
            tickmode='linear',
            tick0=1,
            dtick=1
        ),
        yaxis=dict(title="Average Satisfaction"),
        hovermode='closest',
        showlegend=False
    )
    
    return fig

def plot_confirmatory_brand_distribution(result):
    """
    Plot the distribution of satisfaction per brand group.
    We assume result contains a collapsed PMF (e.g. 'collapsed_pmf') or similar.
    Creates a grid of bar plots, one for each brand cluster.
    
    If a satisfaction constraint was used, the function will show the PMF with
    the constrained level reintegrated.
    """
    if 'collapsed_pmf' not in result:
        raise ValueError("Result does not contain 'collapsed_pmf'.")
    
    # Determine which PMF to use
    if 'pmf_with_constraint' in result and result['pmf_with_constraint'] is not None:
        # Use the PMF that includes the constrained level
        collapsed = result['pmf_with_constraint']
        has_constraint = True
        constraint_index = result.get('satisfaction_constraint', None)
    else:
        # Use the regular collapsed PMF
        collapsed = result['collapsed_pmf']
        has_constraint = False
        constraint_index = None
    
    # For each brand cluster, compute the satisfaction levels.
    num_clusters, num_levels = collapsed.shape
    
    # Calculate grid dimensions
    n_cols = min(3, num_clusters)  # Maximum 3 columns
    n_rows = (num_clusters + n_cols - 1) // n_cols  # Ceiling division
    
    # Group brands by cluster
    brand_clusters = {}
    for i, cluster_id in enumerate(result['brand_cluster']):
        if cluster_id not in brand_clusters:
            brand_clusters[cluster_id] = []
        brand_clusters[cluster_id].append(i + 1)  # Convert to 1-based indexing
    
    # Get sorted cluster IDs
    sorted_cluster_ids = sorted(brand_clusters.keys())
    
    # Create satisfaction level labels
    satisfaction_labels = []
    if has_constraint:
        # Create labels for when a constraint was applied
        if constraint_index == 0:
            # First level was omitted - start with "Level 1"
            satisfaction_labels = ["Level 1"]
            
            # Add labels for remaining levels - these are shifted by 1
            partition_max = max(result['satisfaction_partition'])
            
            for j in range(1, num_levels):
                if j < num_levels - 1:
                    # Look for a partition in the satisfaction partition
                    current_indices = np.where(result['satisfaction_partition'] == j)[0]
                    
                    if len(current_indices) == 0:
                        # No partition - use simple label
                        satisfaction_labels.append(f"Level {j+1}")
                    elif len(current_indices) == 1:
                        # Single level
                        satisfaction_labels.append(f"Level {current_indices[0]+2}")  # +2: +1 for 1-indexing, +1 for skipped level
                    else:
                        # Multiple levels in partition
                        min_idx = min(current_indices) + 2  # +2 as above
                        max_idx = max(current_indices) + 2
                        satisfaction_labels.append(f"Levels {min_idx}-{max_idx}")
                else:
                    # Last level may not have a partition
                    satisfaction_labels.append(f"Level {j+1}")
        else:
            # Last or other level was omitted
            partition_max = max(result['satisfaction_partition'])
            
            # Create labels for each level before the constraint
            for j in range(num_levels - 1):
                # Get indices for this partition
                current_indices = np.where(result['satisfaction_partition'] == j+1)[0]
                
                if len(current_indices) == 0:
                    # No partition - use simple label
                    satisfaction_labels.append(f"Level {j+1}")
                elif len(current_indices) == 1:
                    # Single level
                    satisfaction_labels.append(f"Level {current_indices[0]+1}")  # +1 for 1-indexing
                else:
                    # Multiple levels in partition
                    min_idx = min(current_indices) + 1  # +1 for 1-indexing
                    max_idx = max(current_indices) + 1
                    satisfaction_labels.append(f"Levels {min_idx}-{max_idx}")
            
            # Add the constraint level
            satisfaction_labels.append(f"Level {constraint_index+1}")
    else:
        # No constraint was applied - use partition information directly
        for j in range(num_levels):
            # Get indices for this partition
            current_indices = np.where(result['satisfaction_partition'] == j+1)[0]
            
            if len(current_indices) == 0:
                # No partition - use simple label
                satisfaction_labels.append(f"Level {j+1}")
            elif len(current_indices) == 1:
                # Single level
                satisfaction_labels.append(f"Level {current_indices[0]+1}")  # +1 for 1-indexing
            else:
                # Multiple levels in partition
                min_idx = min(current_indices) + 1  # +1 for 1-indexing
                max_idx = max(current_indices) + 1
                satisfaction_labels.append(f"Levels {min_idx}-{max_idx}")
    
    # Ensure we have enough labels
    while len(satisfaction_labels) < num_levels:
        satisfaction_labels.append(f"Level {len(satisfaction_labels)+1}")
    
    # Trim excess labels
    satisfaction_labels = satisfaction_labels[:num_levels]
    
    # Get stochastic ordering if available
    ordering = result.get('stochastic_ordering', None)
    
    # Create brand cluster info with ordering position for sorting
    cluster_info = []
    for cluster_idx, cluster_id in enumerate(sorted_cluster_ids):
        brands = brand_clusters[cluster_id]
        
        if len(brands) == 1:
            brand_label = f"Brand {brands[0]}"
        else:
            brand_label = f"Brands {', '.join(map(str, brands))}"
        
        # Get ordering position if stochastic ordering is available
        if ordering is not None:
            order_position = next((i+1 for i, idx in enumerate(ordering) 
                                 if idx < len(sorted_cluster_ids) and sorted_cluster_ids[idx] == cluster_id), 
                                None)
            if order_position is not None:
                brand_label = f"{order_position}. {brand_label}"
        else:
            order_position = cluster_idx + 1
        
        # Get the data index for this cluster
        if ordering is not None:
            data_idx = next((idx for idx in ordering if idx < len(sorted_cluster_ids) and sorted_cluster_ids[idx] == cluster_id), cluster_idx)
        else:
            data_idx = cluster_idx
        
        cluster_info.append({
            'order_position': order_position if order_position is not None else cluster_idx + 1,
            'cluster_id': cluster_id,
            'brand_label': brand_label,
            'data_idx': data_idx
        })
    
    # Sort clusters by their ordering position so plots appear in rank order
    cluster_info.sort(key=lambda x: x['order_position'])
    
    # Create Plotly subplots
    subplot_titles = [info['brand_label'] for info in cluster_info]
    fig = make_subplots(rows=n_rows, cols=n_cols, subplot_titles=subplot_titles)
    
    # Determine highlight index if constraint was applied
    highlight_idx = None
    if has_constraint:
        if constraint_index == 0:
            highlight_idx = 0  # First bar
        else:
            highlight_idx = num_levels - 1  # Last bar
    
    # Create a bar plot for each cluster in ranking order
    y_max = max(collapsed.max() * 1.1, 0.1)
    
    for i, info in enumerate(cluster_info):
        row = i // n_cols + 1
        col = i % n_cols + 1
        
        # Get data for this cluster
        data = collapsed[info['data_idx'], :]
        
        # Create bar colors (highlight constrained level if applicable)
        colors = ['skyblue'] * num_levels
        if highlight_idx is not None:
            colors[highlight_idx] = 'lightcoral'
        
        # Create hover text
        hover_text = [
            f"<b>{info['brand_label']}</b><br>{satisfaction_labels[j]}<br>Probability: {data[j]:.4f}"
            for j in range(num_levels)
        ]
        
        fig.add_trace(
            go.Bar(
                x=satisfaction_labels,
                y=data,
                marker=dict(color=colors, line=dict(color='black', width=1)),
                hovertext=hover_text,
                hoverinfo='text',
                showlegend=False
            ),
            row=row, col=col
        )
        
        # Update axes for this subplot
        fig.update_xaxes(title_text="Satisfaction Level", row=row, col=col)
        fig.update_yaxes(title_text="Probability", range=[0, y_max], row=row, col=col)
    
    # Update layout
    height = n_rows * 300
    annotation_text = ""
    if has_constraint:
        if constraint_index == 0:
            constraint_desc = "first"
        else:
            constraint_desc = "last"
        annotation_text = f"Note: The {constraint_desc} satisfaction level (highlighted in coral) was omitted during analysis and reintegrated afterward."
    
    fig.update_layout(
        height=height,
        showlegend=False,
        hovermode='closest'
    )
    
    # Add annotation if needed
    if annotation_text:
        fig.add_annotation(
            text=annotation_text,
            xref="paper", yref="paper",
            x=0.5, y=-0.05,
            showarrow=False,
            font=dict(size=10, style="italic"),
            xanchor='center'
        )
    
    return fig

def plot_exploratory_brand_distribution(result, contingency_table):
    """
    Plot the distribution of satisfaction per brand group.
    Creates a grid of bar plots, one for each brand cluster, ordered by ranking.
    
    Args:
        result: Dictionary from exploratory_computation
        contingency_table: The original contingency table
    """
    if 'collapsed_pmf' not in result:
        raise ValueError("Result does not contain 'collapsed_pmf'.")
    
    collapsed = result['collapsed_pmf']
    if isinstance(collapsed, pd.DataFrame):
        collapsed = np.array(collapsed)
    
    # For each brand cluster, compute the satisfaction levels.
    num_clusters, num_levels = collapsed.shape
    
    # Calculate grid dimensions
    n_cols = min(3, num_clusters)  # Maximum 3 columns
    n_rows = (num_clusters + n_cols - 1) // n_cols  # Ceiling division
    
    # Group brands by cluster
    brand_clusters = {}
    for i, cluster_id in enumerate(result['brand_cluster']):
        if cluster_id not in brand_clusters:
            brand_clusters[cluster_id] = []
        brand_clusters[cluster_id].append(i + 1)  # Convert to 1-based indexing
    
    # Get sorted cluster IDs
    sorted_cluster_ids = sorted(brand_clusters.keys())
    
    # Create satisfaction level labels based on partition
    satisfaction_labels = []
    for j in range(num_levels):
        # Get indices for this partition
        current_indices = np.where(result['satisfaction_partition'] == j+1)[0]
        
        if len(current_indices) == 0:
            # No partition - use simple label
            satisfaction_labels.append(f"Level {j+1}")
        elif len(current_indices) == 1:
            # Single level
            satisfaction_labels.append(f"Level {current_indices[0]+1}")  # +1 for 1-indexing
        else:
            # Multiple levels in partition
            min_idx = min(current_indices) + 1  # +1 for 1-indexing
            max_idx = max(current_indices) + 1
            satisfaction_labels.append(f"Levels {min_idx}-{max_idx}")
    
    # Ensure we have enough labels
    while len(satisfaction_labels) < num_levels:
        satisfaction_labels.append(f"Level {len(satisfaction_labels)+1}")
    
    # Trim excess labels
    satisfaction_labels = satisfaction_labels[:num_levels]
    
    # Get stochastic ordering if available
    ordering = result.get('stochastic_ordering', None)
    
    # Create brand cluster info with ordering position for sorting
    cluster_info = []
    for cluster_idx, cluster_id in enumerate(sorted_cluster_ids):
        brands = brand_clusters[cluster_id]
        
        if len(brands) == 1:
            brand_label = f"Brand {brands[0]}"
        else:
            brand_label = f"Brands {', '.join(map(str, brands))}"
        
        # Get ordering position if stochastic ordering is available
        if ordering is not None:
            order_position = next((i+1 for i, idx in enumerate(ordering) 
                                 if idx < len(sorted_cluster_ids) and sorted_cluster_ids[idx] == cluster_id), 
                                None)
            if order_position is not None:
                brand_label = f"{order_position}. {brand_label}"
        else:
            order_position = cluster_idx + 1
        
        # Get the data index for this cluster
        if ordering is not None:
            data_idx = next((idx for idx in ordering if idx < len(sorted_cluster_ids) and sorted_cluster_ids[idx] == cluster_id), cluster_idx)
        else:
            data_idx = cluster_idx
        
        cluster_info.append({
            'order_position': order_position if order_position is not None else cluster_idx + 1,
            'cluster_id': cluster_id,
            'brand_label': brand_label,
            'data_idx': data_idx
        })
    
    # Sort clusters by their ordering position so plots appear in rank order
    cluster_info.sort(key=lambda x: x['order_position'])
    
    # Create Plotly subplots
    subplot_titles = [info['brand_label'] for info in cluster_info]
    fig = make_subplots(rows=n_rows, cols=n_cols, subplot_titles=subplot_titles)
    
    # Create a bar plot for each cluster in ranking order
    y_max = max(collapsed.max() * 1.1, 0.1)
    
    for i, info in enumerate(cluster_info):
        row = i // n_cols + 1
        col = i % n_cols + 1
        
        # Get data for this cluster
        data = collapsed[info['data_idx'], :]
        
        # Create hover text
        hover_text = [
            f"<b>{info['brand_label']}</b><br>{satisfaction_labels[j]}<br>Probability: {data[j]:.4f}"
            for j in range(num_levels)
        ]
        
        fig.add_trace(
            go.Bar(
                x=satisfaction_labels,
                y=data,
                marker=dict(color='skyblue', line=dict(color='black', width=1)),
                hovertext=hover_text,
                hoverinfo='text',
                showlegend=False
            ),
            row=row, col=col
        )
        
        # Update axes for this subplot
        fig.update_xaxes(title_text="Satisfaction Level", row=row, col=col)
        fig.update_yaxes(title_text="Probability", range=[0, y_max], row=row, col=col)
    
    # Update layout
    height = n_rows * 300
    fig.update_layout(
        height=height,
        showlegend=False,
        hovermode='closest'
    )
    
    return fig

def plot_exploratory_brand_clusters(result):
    """
    Generate a faceted bar plot showing the clustering of brands based on satisfaction profiles.
    
    Args:
        result: Dictionary from exploratory_computation containing the collapsed PMF matrix
    """
    # Get the collapsed PMF matrix from the result
    if 'collapsed_pmf' not in result:
        raise ValueError("Result dictionary does not contain collapsed PMF matrix")
    
    pmf = result['collapsed_pmf']
    if isinstance(pmf, pd.DataFrame):
        pmf = np.array(pmf)
    
    # Get brand groups from result
    brand_groups = result.get('brand_groups', None)
    if brand_groups is None:
        group_labels = [f"Group {i+1}" for i in range(len(pmf))]
    else:
        group_labels = [f"Brand Group = Brand {','.join(map(str, group))}" for group in brand_groups]
    
    # Create Plotly subplots
    n_groups = len(pmf)
    fig = make_subplots(rows=1, cols=n_groups, subplot_titles=group_labels)
    
    # Plot each group's distribution
    for i, (group_pmf, group_label) in enumerate(zip(pmf, group_labels)):
        x_labels = [f'Level {j+1}' for j in range(len(group_pmf))]
        
        # Create hover text
        hover_text = [
            f"<b>{group_label}</b><br>Level {j+1}<br>Probability: {prob:.4f}"
            for j, prob in enumerate(group_pmf)
        ]
        
        fig.add_trace(
            go.Bar(
                x=x_labels,
                y=group_pmf,
                marker=dict(color='steelblue', line=dict(color='black', width=1)),
                hovertext=hover_text,
                hoverinfo='text',
                showlegend=False
            ),
            row=1, col=i+1
        )
        
        # Update axes
        fig.update_xaxes(title_text="Combined Satisfaction Levels", row=1, col=i+1)
        fig.update_yaxes(title_text="Probability", range=[0, 0.3], row=1, col=i+1)
    
    # Update layout
    fig.update_layout(
        title=dict(text="Ranking of Brand Groups", font=dict(size=16), x=0.5, xanchor='center'),
        height=400,
        showlegend=False,
        hovermode='closest'
    )
    
    return fig


def exploratory_report(result):
    """
    Generate a report for the exploratory analysis.
    
    Args:
        result (dict): The output of ranking.exploratory_computation.
        
    Returns:
        list: A list of report items (strings and DataFrames).
    """
    try:
        report_items = []
        
        # Check if a satisfaction constraint was applied (if available in result)
        has_constraint = 'satisfaction_constraint' in result and result['satisfaction_constraint'] is not None
        
        # Add a note about satisfaction constraint if applicable
        if has_constraint:
            constraint_index = result['satisfaction_constraint']
            if constraint_index == 0:
                constraint_desc = "first"
            else:  # Must be the last column
                constraint_desc = "last"
            
            report_items.append(f"### Analysis with Satisfaction Constraint")
            report_items.append(f"The {constraint_desc} satisfaction level was omitted during analysis and reintegrated afterward.")
        
        # 1. Observed contingency table
        report_items.append("### Observed Data Matrix")
        observed_df = pd.DataFrame(result['observed'])
        report_items.append(observed_df)
        
        # 2. ANOAS Statistics (instead of model statistics)
        # stats_text = f"Full L-Squared = {result.get('full_l_sq_stat', 'N/A'):.2f}\n"
        # stats_text += f"Full D.O.F. = {result.get('full_d_o_f', 'N/A')}\n"
        # stats_text += f"Full p-value = {result.get('full_p_value', 'N/A'):.4f}\n"
        # stats_text += f"Change in L-Squared = {result.get('change_in_l_sq', 'N/A'):.2f}\n"
        # stats_text += f"Change in D.O.F. = {result.get('change_in_d_o_f', 'N/A')}\n"
        # stats_text += f"Information p-value = {result.get('information_p_value', 'N/A'):.4f}\n"
        # stats_text += f"Information is lost = {result.get('information_is_lost', 'N/A')}"
        report_items.append("### Model Statistics")
        stats_text = f"L-square = {result.get('full_l_sq_stat', 'N/A'):.2f}, D.O.F. = {result.get('full_d_o_f', 'N/A')}, p-value = {utils.signif(result.get('full_p_value', 'N/A'), 4)}"
        report_items.append(stats_text)
        
        # 3. Brand Clustering Information
        # report_items.append("### Brand Clustering")
        
        # Group brands by cluster
        brand_clusters = {}
        for i, cluster_id in enumerate(result['brand_cluster']):
            if cluster_id not in brand_clusters:
                brand_clusters[cluster_id] = []
            brand_clusters[cluster_id].append(i + 1)  # Convert to 1-based indexing
        
        # # Create a table of brand clusters
        # cluster_data = {"Cluster": [], "Brands": []}
        # for cluster_id in sorted(brand_clusters.keys()):
        #     brands = brand_clusters[cluster_id]
        #     cluster_data["Cluster"].append(f"Cluster {cluster_id}")
            
        #     if len(brands) == 1:
        #         brand_str = f"Brand {brands[0]}"
        #     else:
        #         brand_str = f"Brands {', '.join(map(str, brands))}"
            
        #     cluster_data["Brands"].append(brand_str)
        
        # cluster_df = pd.DataFrame(cluster_data)
        # report_items.append(cluster_df)
        
        # 4. Final Ranking with Stochastic Ordering
        report_items.append("### Final Ranking with Stochastic Ordering")
        report_items.append(f"Distance threshold used: {result.get('critical_value', 'N/A'):.4f}")

        # Add note about polarity index calculation if an upper tail was specified
        if 'upper_polarity_idx' in result and result['upper_polarity_idx'] is not None:
            if isinstance(result['upper_polarity_idx'], list) and len(result['upper_polarity_idx']) > 0:
                levels_str = ", ".join(map(str, result['upper_polarity_idx']))
                report_items.append(f"Note: Polarity Index calculated as: P(Levels {levels_str}) / (1 - P(Levels {levels_str}))")
            elif isinstance(result['upper_polarity_idx'], str) and "-" in result['upper_polarity_idx']:
                report_items.append(f"Note: Polarity Index calculated as: P(Levels {result['upper_polarity_idx']}) / (1 - P(Levels {result['upper_polarity_idx']}))")
        
        # Create ranking DataFrame
        ranking_dict = {"Brands": []}
        
        # Get dimensions
        num_cluster = result['collapsed_pmf'].shape[0]
        num_collapsed_level = result['collapsed_pmf'].shape[1]
        
        # Add brand labels based on clusters and stochastic ordering
        for i in range(num_cluster):
            # Get brand index based on stochastic ordering if available
            brand_idx = result['stochastic_ordering'][i] if result.get('stochastic_ordering') is not None else i
            
            # Get cluster ID from the sorted list of cluster IDs
            cluster_ids = sorted(brand_clusters.keys())
            if brand_idx < len(cluster_ids):
                cluster_id = cluster_ids[brand_idx]
                brands_in_cluster = brand_clusters[cluster_id]
                
                if len(brands_in_cluster) == 1:
                    brand_label = f"Brand {brands_in_cluster[0]}"
                else:
                    brand_label = f"Brands {', '.join(map(str, brands_in_cluster))}"
                
                ranking_dict["Brands"].append(brand_label)
            else:
                # Fallback if index out of range
                ranking_dict["Brands"].append(f"Brand Cluster {i+1}")
        
        # Create satisfaction level partitions info
        satisfaction_partitions = {}
        for i, partition_id in enumerate(result['satisfaction_partition']):
            if partition_id not in satisfaction_partitions:
                satisfaction_partitions[partition_id] = []
            satisfaction_partitions[partition_id].append(i + 1)  # Level IDs are 1-based
        
        # Add PMF columns with proper labels
        for j in range(num_collapsed_level):
            # Get partition ID for this column
            partition_id = j + 1
            
            # Get levels associated with this partition
            levels_in_partition = satisfaction_partitions.get(partition_id, [])
            
            # Create level label
            if has_constraint and result.get('satisfaction_constraint') == 0:
                # When first level is omitted, add +1 to all level numbers
                if not levels_in_partition:
                    level_key = f"Level {j+2}"  # +2 because index starts at 0 and we skip level 1
                elif len(levels_in_partition) == 1:
                    level_key = f"Level {levels_in_partition[0]+1}"  # +1 to account for omitted level
                else:
                    level_key = f"Levels {min(levels_in_partition)+1}-{max(levels_in_partition)+1}"  # +1 to account for omitted level
            else:
                # Normal labeling without shifting
                if not levels_in_partition:
                    level_key = f"Level {j+1}"
                elif len(levels_in_partition) == 1:
                    level_key = f"Level {levels_in_partition[0]}"
                else:
                    level_key = f"Levels {min(levels_in_partition)}-{max(levels_in_partition)}"
            
            # Initialize column in dict
            ranking_dict[level_key] = []
            
            # Add PMF values for each brand
            for i in range(num_cluster):
                # Get brand index based on stochastic ordering if available
                brand_idx = result['stochastic_ordering'][i] if result.get('stochastic_ordering') is not None else i
                
                # Add the PMF value
                try:
                    pmf_value = result['collapsed_pmf'][brand_idx, j]
                    ranking_dict[level_key].append(round(pmf_value, 2))
                except IndexError:
                    ranking_dict[level_key].append("N/A")
        
        # Add polarity index
        ranking_dict["Polarity Index"] = []
        for i in range(num_cluster):
            # Get brand index based on stochastic ordering if available
            brand_idx = result['stochastic_ordering'][i] if result.get('stochastic_ordering') is not None else i
            
            # Add the polarity index value
            try:
                polarity_value = result['polarity_index'][brand_idx]
                ranking_dict["Polarity Index"].append(round(polarity_value, 2))
            except IndexError:
                ranking_dict["Polarity Index"].append("N/A")
        
        # Create DataFrame for display
        ranking_df = pd.DataFrame(ranking_dict)
        report_items.append(ranking_df)
        
        # Distributions with Omitted Category Reintegrated (if applicable)
        if has_constraint and 'pmf_with_constraint' in result and result['pmf_with_constraint'] is not None:
            report_items.append("\n### Distributions with Omitted Category Reintegrated")
            report_items.append("Distribution after the stochastic ordering was obtained, with the omitted category added back.")
            
            # Create a display table for the pmf with constraint
            pmf_dict = {"Brands": ranking_dict["Brands"].copy()}
            
            # Get number of clusters and columns in the PMF with constraint
            num_cluster = result['pmf_with_constraint'].shape[0]
            num_levels = result['pmf_with_constraint'].shape[1]
            
            # Create satisfaction level labels based on constraint
            constraint_index = result['satisfaction_constraint']
            if constraint_index == 0:  # First column was omitted
                # When first level is omitted, it's added back as the first column in pmf_with_constraint
                # So the first column is "Level 1" and the rest need to match their original level numbers
                
                # First level is the omitted constraint
                level_labels = ["Level 1"]
                
                # For remaining levels (which are the same as in the collapsed_pmf), add level number +1
                for j in range(num_collapsed_level):
                    partition_id = j + 1
                    levels_in_partition = satisfaction_partitions.get(partition_id, [])
                    
                    if not levels_in_partition:
                        level_key = f"Level {j+2}"  # +2 because j starts at 0 and we skip level 1
                    elif len(levels_in_partition) == 1:
                        level_key = f"Level {levels_in_partition[0]+1}"  # +1 for omitted level
                    else:
                        level_key = f"Levels {min(levels_in_partition)+1}-{max(levels_in_partition)+1}"  # +1 for omitted level
                    
                    level_labels.append(level_key)
                
            else:  # Last column was omitted
                # When last level is omitted, it's added back as the last column in pmf_with_constraint
                # So we need the regular labels plus the omitted level at the end
                
                # Create labels for all existing columns from collapsed_pmf (unmodified)
                level_labels = []
                
                for j in range(num_collapsed_level):
                    partition_id = j + 1
                    levels_in_partition = satisfaction_partitions.get(partition_id, [])
                    
                    if not levels_in_partition:
                        level_key = f"Level {j+1}"
                    elif len(levels_in_partition) == 1:
                        level_key = f"Level {levels_in_partition[0]}"
                    else:
                        level_key = f"Levels {min(levels_in_partition)}-{max(levels_in_partition)}"
                    
                    level_labels.append(level_key)
                
                # Add the omitted level as the last column
                level_labels.append(f"Level {constraint_index+1}")
            
            # Add PMF columns with proper labels
            for j in range(num_levels):
                level_key = level_labels[j]
                
                # Initialize column in dict
                pmf_dict[level_key] = []
                
                # Add PMF values for each brand
                for i in range(num_cluster):
                    # Get brand index based on stochastic ordering if available
                    brand_idx = result['stochastic_ordering'][i] if result.get('stochastic_ordering') is not None else i
                    
                    # Get cluster ID from the sorted list of cluster IDs
                    cluster_ids = sorted(brand_clusters.keys())
                    if brand_idx < len(cluster_ids):
                        cluster_id = cluster_ids[brand_idx]
                        # Add the PMF value using the correct brand index
                        pmf_value = result['pmf_with_constraint'][brand_idx, j]
                        pmf_dict[level_key].append(round(pmf_value, 2))
                    else:
                        pmf_dict[level_key].append("N/A")
                
            # Create DataFrame for display
            pmf_df = pd.DataFrame(pmf_dict)
            report_items.append(pmf_df)
            
            # Add note about which level was omitted
            if constraint_index == 0:
                report_items.append("Note: The first satisfaction level (Level 1) was omitted during the analysis.")
            else:
                report_items.append(f"Note: The last satisfaction level (Level {constraint_index+1}) was omitted during the analysis.")
        
        z_matrix = result['z_matrix']

        
        # Add Z-test for stochastic ordering
        report_items.append("### Z-test for Stochastic Ordering")
        report_items.append("--------------------------------------")
        
        # Get dimensions and data
        contingency_table = result['observed']
        satisfaction_partition = result['satisfaction_partition']
        mid_category = np.ceil(contingency_table.shape[1] / 2).astype(int)
        mid_category_partition = satisfaction_partition[mid_category - 1]  # -1 for 0-based indexing
        
        # Get collapsed contingency table rows
        contingency_table_collapsed_rows = result['collapsed_contingency_table_rows']
        
        # Calculate bottom and top halves
        bottom_mask = np.array([p <= mid_category_partition for p in satisfaction_partition])
        top_mask = np.array([p > mid_category_partition for p in satisfaction_partition])
        
        # Handle both matrix and vector cases
        if len(contingency_table_collapsed_rows.shape) > 1:
            bottom_half = np.sum(contingency_table_collapsed_rows[:, bottom_mask], axis=1)
            top_half = np.sum(contingency_table_collapsed_rows[:, top_mask], axis=1)
        else:
            bottom_half = contingency_table_collapsed_rows[bottom_mask]
            top_half = contingency_table_collapsed_rows[top_mask]
        
        # Create the matrix for display
        num_cluster = len(np.unique(result['brand_cluster']))
        temp = np.full((num_cluster + 1, num_cluster + 1), "", dtype=object)
        
        # Fill headers
        temp[0, 0] = "Brands"
        
        # Fill brand labels
        for i in range(num_cluster):
            brand_idx = result['stochastic_ordering'][i] if result.get('stochastic_ordering') is not None else i
            cluster_ids = sorted(brand_clusters.keys())
            if brand_idx < len(cluster_ids):
                cluster_id = cluster_ids[brand_idx]
                brands = brand_clusters[cluster_id]
                brand_label = f"Brand {brands[0]}" if len(brands) == 1 else f"Brands {', '.join(map(str, brands))}"
            else:
                brand_label = f"Brand Cluster {i+1}"
            
            temp[i + 1, 0] = brand_label
            temp[0, i + 1] = brand_label
            temp[i + 1, i + 1] = "\\"
        
        # Fill Z-test results
        for i in range(num_cluster):
            for j in range(i + 1, num_cluster):
                bottom_i = bottom_half[i]
                top_i = top_half[i]
                bottom_j = bottom_half[j]
                top_j = top_half[j]
                
                # Calculate z-statistic
                z_ij = (np.log(top_i/bottom_i) - np.log(top_j/bottom_j)) / np.sqrt(1/top_i + 1/bottom_i + 1/top_j + 1/bottom_j)
                
                if z_ij > 1.645:  # qnorm(0.95)
                    temp[i + 1, j + 1] = "X"
        
        # Convert to DataFrame for better display
        z_test_df = pd.DataFrame(temp)
        report_items.append(z_test_df)
        
        return report_items
        
    except Exception as e:
        import traceback
        traceback.print_exc()
        return [f"Error generating report: {str(e)}"]

def add_target_column_ranking(data, brands, ranking_range, brand_col, ranking_col):
    """
    Create a target column for explanatory variable analysis in ranking.
    
    Args:
        data (pd.DataFrame): The raw data with rankings
        brands (list): List of brand indices (1-based) to consider
        ranking_range (list): List of ranking values to consider [min_rank, max_rank] (1-based)
        brand_col (int): Column index for brands (0-based)
        ranking_col (int): Column index for rankings (0-based)
    
    Returns:
        pd.DataFrame: Data with target column added (1 if specified brands have rankings in range, 2 otherwise)
    """
    try:
        if brand_col is None or ranking_col is None:
            raise ValueError("brand_col and ranking_col must be provided")
            
        # Create a copy of the data
        new_data = data.copy()
        
        # Create a mask for rows where brand is in brands list and ranking is in range
        mask = (data.iloc[:, brand_col].isin(brands)) & \
               (data.iloc[:, ranking_col] >= ranking_range[0]) & \
               (data.iloc[:, ranking_col] <= ranking_range[1])
        
        # Create target column (default to 2)
        new_data['target'] = 2
        
        # Set target=1 for rows matching our condition
        new_data.loc[mask, 'target'] = 1
        
        return new_data
    except Exception as e:
        print(f"Error in add_target_column_ranking: {str(e)}")
        return None