"""
Spacing Models module for INSIGHT.
Implements Exponential Spacing and Canonical Correlation models for contingency table analysis.
"""

import numpy as np
import pandas as pd
from scipy.stats import chi2
import independence


def adjust_mu_or_nu(x, prob):
    """
    Adjust mu or nu to have weighted mean 0 and weighted variance 1.
    
    Args:
        x: Array of values to adjust
        prob: Probability weights
        
    Returns:
        Adjusted values with weighted mean 0 and variance 1
    """
    x = np.array(x, dtype=float)
    prob = np.array(prob, dtype=float)
    x_mean = np.sum(x * prob)
    x_var = np.sum((x - x_mean) ** 2 * prob)
    if x_var <= 0:
        return x - x_mean
    return (x - x_mean) / np.sqrt(x_var)


def lagrange_interpolation(x, fx, x_new):
    """
    Perform Lagrange polynomial interpolation.
    
    Args:
        x: Known x values
        fx: Function values at known x points
        x_new: New x values to interpolate
        
    Returns:
        Interpolated function values at x_new
    """
    x = np.array(x, dtype=float)
    fx = np.array(fx, dtype=float)
    x_new = np.array(x_new, dtype=float)
    
    num_x_new = len(x_new)
    num_x = len(x)
    
    result = np.zeros(num_x_new)
    for i in range(num_x_new):
        for j in range(num_x):
            # Compute Lagrange basis polynomial
            basis = 1.0
            for k in range(num_x):
                if k != j:
                    basis *= (x_new[i] - x[k]) / (x[j] - x[k])
            result[i] += fx[j] * basis
    
    return result


def pseudo_inv(x):
    """
    Compute the pseudo-inverse of a matrix using SVD.
    
    Args:
        x: Input matrix
        
    Returns:
        Pseudo-inverse of x
    """
    x = np.array(x, dtype=float)
    
    try:
        u, s, vh = np.linalg.svd(x, full_matrices=False)
        
        # Compute threshold for singular values
        singular_value_threshold = max(x.shape) * max(s) * np.finfo(float).eps
        
        # Identify significant singular values
        is_positive = s > singular_value_threshold
        
        if not np.any(is_positive):
            return np.zeros((x.shape[1], x.shape[0]))
        
        # Compute pseudo-inverse
        s_inv = np.zeros_like(s)
        s_inv[is_positive] = 1.0 / s[is_positive]
        
        return vh.T @ np.diag(s_inv) @ u.T
    except:
        # Fallback to numpy's pinv
        return np.linalg.pinv(x)


def exponential_spacing_computation(contingency_table, poly_deg_row, poly_deg_col, p_value_threshold=0.05):
    """
    Compute the Exponential Spacing model for a contingency table.
    
    The model assumes: E[i,j] = alpha[i] * beta[j] * exp(phi * mu[i] * nu[j]) * N
    
    Args:
        contingency_table: Input contingency table (DataFrame or array)
        poly_deg_row: Polynomial degree for rows
        poly_deg_col: Polynomial degree for columns
        p_value_threshold: Threshold for model fit (default 0.05)
        
    Returns:
        Dictionary containing model results
    """
    # Convert to numpy array
    if isinstance(contingency_table, pd.DataFrame):
        observed = contingency_table.values.astype(float)
    else:
        observed = np.array(contingency_table, dtype=float)
    
    observed_sum = np.sum(observed)
    observed_row_sum = np.sum(observed, axis=1)
    observed_col_sum = np.sum(observed, axis=0)
    observed_row_freq = observed_row_sum / observed_sum
    observed_col_freq = observed_col_sum / observed_sum
    num_row = observed.shape[0]
    num_col = observed.shape[1]
    
    def compute_expected(alpha, beta, mu, nu, phi, n):
        """Compute expected values given parameters."""
        return np.outer(alpha, beta) * np.exp(phi * np.outer(mu, nu)) * n
    
    def update_alpha_beta_expected(alpha, beta, mu, nu, phi, n, row_sum, col_sum):
        """Update alpha and beta using IPF algorithm."""
        expected = compute_expected(alpha, beta, mu, nu, phi, n)
        num_iter = 25
        for _ in range(num_iter):
            alpha = alpha * row_sum / np.sum(expected, axis=1)
            expected = compute_expected(alpha, beta, mu, nu, phi, n)
            beta = beta * col_sum / np.sum(expected, axis=0)
            expected = compute_expected(alpha, beta, mu, nu, phi, n)
        return {'alpha': alpha, 'beta': beta, 'expected': expected}
    
    # Initialize model parameters
    alpha = np.ones(num_row) / num_row
    beta = np.ones(num_col) / num_col
    mu = adjust_mu_or_nu(np.arange(1, num_row + 1), observed_row_freq)
    nu = adjust_mu_or_nu(np.arange(1, num_col + 1), observed_col_freq)
    phi = 0.1
    
    total_num_iter = 50
    total_num_mu_nu_iter = 36
    
    for t in range(1, total_num_iter + 1):
        # Update alpha and beta
        updated = update_alpha_beta_expected(alpha, beta, mu, nu, phi, observed_sum,
                                              observed_row_sum, observed_col_sum)
        alpha = updated['alpha']
        beta = updated['beta']
        expected = updated['expected']
        
        if t <= total_num_mu_nu_iter:
            # Update mu
            temp_o = observed[:poly_deg_row + 1, :]
            temp_e = expected[:poly_deg_row + 1, :]
            fx = (temp_e - temp_o) @ nu
            dfdx = (temp_e @ (nu ** 2)) * phi
            # Avoid division by zero
            dfdx = np.where(np.abs(dfdx) < 1e-10, 1e-10, dfdx)
            mu[:poly_deg_row + 1] = mu[:poly_deg_row + 1] - fx / dfdx
            
            if poly_deg_row + 1 < num_row:
                mu[poly_deg_row + 1:] = lagrange_interpolation(
                    np.arange(1, poly_deg_row + 2),
                    mu[:poly_deg_row + 1],
                    np.arange(poly_deg_row + 2, num_row + 1)
                )
            mu = adjust_mu_or_nu(mu, observed_row_freq)
            
            # Update nu
            temp_o = observed[:, :poly_deg_col + 1]
            temp_e = expected[:, :poly_deg_col + 1]
            fx = mu @ (temp_e - temp_o)
            dfdx = (mu ** 2) @ temp_e * phi
            dfdx = np.where(np.abs(dfdx) < 1e-10, 1e-10, dfdx)
            nu[:poly_deg_col + 1] = nu[:poly_deg_col + 1] - fx / dfdx
            
            if poly_deg_col + 1 < num_col:
                nu[poly_deg_col + 1:] = lagrange_interpolation(
                    np.arange(1, poly_deg_col + 2),
                    nu[:poly_deg_col + 1],
                    np.arange(poly_deg_col + 2, num_col + 1)
                )
            nu = adjust_mu_or_nu(nu, observed_col_freq)
        
        # Update phi
        fx = np.sum((expected - observed) * np.outer(mu, nu))
        dfdx = np.sum(expected * np.outer(mu ** 2, nu ** 2))
        if np.abs(dfdx) > 1e-10:
            phi = phi - fx / dfdx
    
    # Final update
    updated = update_alpha_beta_expected(alpha, beta, mu, nu, phi, observed_sum,
                                          observed_row_sum, observed_col_sum)
    alpha = updated['alpha']
    beta = updated['beta']
    expected = updated['expected']
    
    # Compute Fisher information matrix
    fisher_info = _compute_fisher_info_exponential(
        num_row, num_col, expected, mu, nu, phi, alpha, beta, observed_sum
    )
    
    # Compute EASD (Estimated Asymptotic Standard Deviation)
    cov_matrix = pseudo_inv(fisher_info)[num_row + num_col:2 * (num_row + num_col) + 1,
                                          num_row + num_col:2 * (num_row + num_col) + 1] / observed_sum
    
    # For mu - use .copy() to avoid read-only array issue
    mu_easd = np.diag(cov_matrix[:num_row, :num_row]).copy()
    mu_easd[mu_easd < 0] = np.nan
    mu_easd = np.sqrt(mu_easd)
    
    # For nu - use .copy() to avoid read-only array issue
    nu_easd = np.diag(cov_matrix[num_row:num_row + num_col, num_row:num_row + num_col]).copy()
    nu_easd[nu_easd < 0] = np.nan
    nu_easd = np.sqrt(nu_easd)
    
    # For phi
    phi_easd = cov_matrix[num_row + num_col, num_row + num_col]
    if phi_easd < 0:
        phi_easd = np.nan
    else:
        phi_easd = np.sqrt(phi_easd)
    
    # Compute statistics
    tau_given_row = independence.goodman_kruskal_tau(pd.DataFrame(observed), by_col_given_row=True)
    tau_given_col = independence.goodman_kruskal_tau(pd.DataFrame(observed), by_col_given_row=False)
    chi_sq_stat = independence.pearson_chi_squared(observed, expected)
    l_sq_stat = independence.l_sq_chi_sq(observed, expected)
    d_o_f = (num_row - 1) * (num_col - 1) - poly_deg_row - poly_deg_col + 1
    p_value = chi2.sf(l_sq_stat, df=d_o_f) if d_o_f > 0 else 1.0
    model_is_fit = p_value > p_value_threshold
    
    # Compute standardized residuals
    std_residuals = (observed - expected) / np.sqrt(expected)
    num_sig_residuals = np.sum(np.abs(std_residuals) > 1.96)
    
    # Compute max and min odd ratios
    max_odd_ratio = -np.inf
    min_odd_ratio = np.inf
    row_of_max_or = [0, 0]
    col_of_max_or = [0, 0]
    row_of_min_or = [0, 0]
    col_of_min_or = [0, 0]
    
    for i1 in range(num_row - 1):
        for i2 in range(i1 + 1, num_row):
            for j1 in range(num_col - 1):
                for j2 in range(j1 + 1, num_col):
                    denom = expected[i2, j1] * expected[i1, j2]
                    if denom > 0:
                        odd_ratio = expected[i1, j1] * expected[i2, j2] / denom
                        if odd_ratio > max_odd_ratio:
                            max_odd_ratio = odd_ratio
                            row_of_max_or = [i1 + 1, i2 + 1]
                            col_of_max_or = [j1 + 1, j2 + 1]
                        if odd_ratio < min_odd_ratio:
                            min_odd_ratio = odd_ratio
                            row_of_min_or = [i1 + 1, i2 + 1]
                            col_of_min_or = [j1 + 1, j2 + 1]
    
    # Special case for U model
    if poly_deg_row == 1:
        row_of_min_or = "Any"
    if poly_deg_col == 1:
        col_of_min_or = "Any"
    
    return {
        'observed': observed,
        'expected': expected,
        'tau_given_row': tau_given_row,
        'tau_given_col': tau_given_col,
        'chi_sq_stat': chi_sq_stat,
        'l_sq_stat': l_sq_stat,
        'd_o_f': d_o_f,
        'p_value': p_value,
        'poly_deg_row': poly_deg_row,
        'poly_deg_col': poly_deg_col,
        'mu': mu,
        'nu': nu,
        'phi': phi,
        'mu_easd': mu_easd,
        'nu_easd': nu_easd,
        'phi_easd': phi_easd,
        'cov_matrix': cov_matrix,
        'model_is_fit': model_is_fit,
        'std_residuals': std_residuals,
        'num_sig_residuals': num_sig_residuals,
        'row_of_max_or': row_of_max_or,
        'col_of_max_or': col_of_max_or,
        'max_odd_ratio': max_odd_ratio,
        'row_of_min_or': row_of_min_or,
        'col_of_min_or': col_of_min_or,
        'min_odd_ratio': min_odd_ratio
    }


def _compute_fisher_info_exponential(num_row, num_col, expected, mu, nu, phi, alpha, beta, n):
    """Compute Fisher information matrix for exponential spacing model."""
    size = 2 * (num_row + num_col) + 6
    fisher_info = np.zeros((size, size))
    
    # For mu vs mu
    fisher_info_mu_mu = np.diag((expected @ (nu ** 2)) * phi ** 2)
    
    # For nu vs nu
    fisher_info_nu_nu = np.diag(((mu ** 2) @ expected) * phi ** 2)
    
    # For mu vs nu
    fisher_info_mu_nu = expected * ((phi ** 2) * np.outer(mu, nu) - phi)
    
    # For mu vs phi
    fisher_info_mu_phi = phi * mu * (expected @ (nu ** 2))
    
    # For nu vs phi
    fisher_info_nu_phi = phi * nu * ((mu ** 2) @ expected)
    
    # For alpha vs mu
    fisher_info_alpha_mu = np.diag(phi / alpha * (expected @ nu))
    
    # For beta vs nu
    fisher_info_beta_nu = np.diag(phi / beta * (mu @ expected))
    
    # For beta vs mu
    fisher_info_beta_mu = phi * expected.T * (nu / beta)[:, np.newaxis]
    
    # For alpha vs nu
    fisher_info_alpha_nu = phi * expected * (mu / alpha)[:, np.newaxis]
    
    # For alpha vs alpha
    fisher_info_alpha_alpha = np.diag(np.sum(expected, axis=1) / (alpha ** 2))
    
    # For beta vs beta
    fisher_info_beta_beta = np.diag(np.sum(expected, axis=0) / (beta ** 2))
    
    # For alpha vs beta
    fisher_info_alpha_beta = expected / np.outer(alpha, beta)
    
    # For alpha vs phi
    fisher_info_alpha_phi = (expected @ nu) * mu / alpha
    
    # For beta vs phi
    fisher_info_beta_phi = (mu @ expected) * nu / beta
    
    # For phi vs phi
    fisher_info_phi_phi = np.sum(expected * np.outer(mu ** 2, nu ** 2))
    
    # Assemble Fisher information matrix
    # Alpha block
    fisher_info[:num_row, :num_row] = fisher_info_alpha_alpha
    fisher_info[:num_row, num_row:num_row + num_col] = fisher_info_alpha_beta
    fisher_info[:num_row, num_row + num_col:2 * num_row + num_col] = fisher_info_alpha_mu
    fisher_info[:num_row, 2 * num_row + num_col:2 * (num_row + num_col)] = fisher_info_alpha_nu
    fisher_info[:num_row, 2 * (num_row + num_col)] = fisher_info_alpha_phi
    
    # Beta block
    fisher_info[num_row:num_row + num_col, num_row:num_row + num_col] = fisher_info_beta_beta
    fisher_info[num_row:num_row + num_col, num_row + num_col:2 * num_row + num_col] = fisher_info_beta_mu
    fisher_info[num_row:num_row + num_col, 2 * num_row + num_col:2 * (num_row + num_col)] = fisher_info_beta_nu
    fisher_info[num_row:num_row + num_col, 2 * (num_row + num_col)] = fisher_info_beta_phi
    
    # Mu block
    fisher_info[num_row + num_col:2 * num_row + num_col, num_row + num_col:2 * num_row + num_col] = fisher_info_mu_mu
    fisher_info[num_row + num_col:2 * num_row + num_col, 2 * num_row + num_col:2 * (num_row + num_col)] = fisher_info_mu_nu
    fisher_info[num_row + num_col:2 * num_row + num_col, 2 * (num_row + num_col)] = fisher_info_mu_phi
    
    # Nu block
    fisher_info[2 * num_row + num_col:2 * (num_row + num_col), 2 * num_row + num_col:2 * (num_row + num_col)] = fisher_info_nu_nu
    fisher_info[2 * num_row + num_col:2 * (num_row + num_col), 2 * (num_row + num_col)] = fisher_info_nu_phi
    
    # Phi
    fisher_info[2 * (num_row + num_col), 2 * (num_row + num_col)] = fisher_info_phi_phi
    
    # Make symmetric
    fisher_info = fisher_info + fisher_info.T - np.diag(np.diag(fisher_info))
    fisher_info = fisher_info / n
    
    return fisher_info


def canonical_correlation_computation(contingency_table, poly_deg_row, poly_deg_col, p_value_threshold=0.05):
    """
    Compute the Canonical Correlation model for a contingency table.
    
    The model assumes: E[i,j] = alpha[i] * beta[j] * (1 + phi * mu[i] * nu[j]) * N
    
    Args:
        contingency_table: Input contingency table (DataFrame or array)
        poly_deg_row: Polynomial degree for rows
        poly_deg_col: Polynomial degree for columns
        p_value_threshold: Threshold for model fit (default 0.05)
        
    Returns:
        Dictionary containing model results
    """
    # Convert to numpy array
    if isinstance(contingency_table, pd.DataFrame):
        observed = contingency_table.values.astype(float)
    else:
        observed = np.array(contingency_table, dtype=float)
    
    observed_sum = np.sum(observed)
    observed_row_sum = np.sum(observed, axis=1)
    observed_col_sum = np.sum(observed, axis=0)
    observed_row_freq = observed_row_sum / observed_sum
    observed_col_freq = observed_col_sum / observed_sum
    num_row = observed.shape[0]
    num_col = observed.shape[1]
    
    # Initialize model parameters
    alpha = observed_row_freq.copy()
    beta = observed_col_freq.copy()
    mu = adjust_mu_or_nu(np.arange(1, num_row + 1), observed_row_freq)
    nu = adjust_mu_or_nu(np.arange(1, num_col + 1), observed_col_freq)
    phi = abs(np.sum(observed * np.outer(mu, nu)) / observed_sum)
    
    # Iterative algorithm
    total_num_iter = 50
    
    for t in range(total_num_iter):
        # Update mu
        temp_one = observed[:poly_deg_row + 1, :] / (1 + phi * np.outer(mu[:poly_deg_row + 1], nu))
        temp_two = np.outer(observed_row_sum[:poly_deg_row + 1], observed_col_sum)
        fx = (temp_one - temp_two) @ nu
        temp = observed[:poly_deg_row + 1, :] / (1 + phi * np.outer(mu[:poly_deg_row + 1], nu)) ** 2
        dfdx = -phi * (temp @ (nu ** 2))
        dfdx = np.where(np.abs(dfdx) < 1e-10, -1e-10, dfdx)
        mu[:poly_deg_row + 1] = mu[:poly_deg_row + 1] - fx / dfdx
        
        if poly_deg_row + 1 < num_row:
            mu[poly_deg_row + 1:] = lagrange_interpolation(
                np.arange(1, poly_deg_row + 2),
                mu[:poly_deg_row + 1],
                np.arange(poly_deg_row + 2, num_row + 1)
            )
        mu = adjust_mu_or_nu(mu, observed_row_freq)
        
        # Update nu
        temp_one = observed[:, :poly_deg_col + 1] / (1 + phi * np.outer(mu, nu[:poly_deg_col + 1]))
        temp_two = np.outer(observed_row_sum, observed_col_sum[:poly_deg_col + 1])
        fx = mu @ (temp_one - temp_two)
        temp = observed[:, :poly_deg_col + 1] / (1 + phi * np.outer(mu, nu[:poly_deg_col + 1])) ** 2
        dfdx = -phi * ((mu ** 2) @ temp)
        dfdx = np.where(np.abs(dfdx) < 1e-10, -1e-10, dfdx)
        nu[:poly_deg_col + 1] = nu[:poly_deg_col + 1] - fx / dfdx
        
        if poly_deg_col + 1 < num_col:
            nu[poly_deg_col + 1:] = lagrange_interpolation(
                np.arange(1, poly_deg_col + 2),
                nu[:poly_deg_col + 1],
                np.arange(poly_deg_col + 2, num_col + 1)
            )
        nu = adjust_mu_or_nu(nu, observed_col_freq)
        
        # Update phi
        temp_omn = np.outer(mu, nu)
        denom = 1 + phi * temp_omn
        fx = np.sum((observed / denom - np.outer(observed_row_sum, observed_col_sum)) * temp_omn)
        dfdx = -np.sum(observed * np.outer(mu ** 2, nu ** 2) / denom ** 2)
        if np.abs(dfdx) > 1e-10:
            phi = phi - fx / dfdx
    
    # Compute expected data
    expected = np.outer(observed_row_freq, observed_col_freq) * (1 + phi * np.outer(mu, nu)) * observed_sum
    
    # Compute Fisher information matrix
    fisher_info = _compute_fisher_info_canonical(
        num_row, num_col, expected, mu, nu, phi, alpha, beta, observed_sum, observed_row_freq, observed_col_freq
    )
    
    # Compute EASD
    cov_matrix = pseudo_inv(fisher_info)[num_row + num_col:2 * (num_row + num_col) + 1,
                                          num_row + num_col:2 * (num_row + num_col) + 1] / observed_sum
    
    # For mu - use .copy() to avoid read-only array issue
    mu_easd = np.diag(cov_matrix[:num_row, :num_row]).copy()
    mu_easd[mu_easd < 0] = np.nan
    mu_easd = np.sqrt(mu_easd)
    
    # For nu - use .copy() to avoid read-only array issue
    nu_easd = np.diag(cov_matrix[num_row:num_row + num_col, num_row:num_row + num_col]).copy()
    nu_easd[nu_easd < 0] = np.nan
    nu_easd = np.sqrt(nu_easd)
    
    # For phi
    phi_easd = cov_matrix[num_row + num_col, num_row + num_col]
    if phi_easd < 0:
        phi_easd = np.nan
    else:
        phi_easd = np.sqrt(phi_easd)
    
    # Compute statistics
    tau_given_row = independence.goodman_kruskal_tau(pd.DataFrame(observed), by_col_given_row=True)
    tau_given_col = independence.goodman_kruskal_tau(pd.DataFrame(observed), by_col_given_row=False)
    chi_sq_stat = independence.pearson_chi_squared(observed, expected)
    l_sq_stat = independence.l_sq_chi_sq(observed, expected)
    d_o_f = (num_row - 1) * (num_col - 1) - poly_deg_row - poly_deg_col + 1
    p_value = chi2.sf(l_sq_stat, df=d_o_f) if d_o_f > 0 else 1.0
    model_is_fit = p_value > p_value_threshold
    
    # Compute standardized residuals
    std_residuals = (observed - expected) / np.sqrt(np.maximum(expected, 1e-10))
    num_sig_residuals = np.sum(np.abs(std_residuals) > 1.96)
    
    return {
        'observed': observed,
        'expected': expected,
        'tau_given_row': tau_given_row,
        'tau_given_col': tau_given_col,
        'chi_sq_stat': chi_sq_stat,
        'l_sq_stat': l_sq_stat,
        'd_o_f': d_o_f,
        'p_value': p_value,
        'poly_deg_row': poly_deg_row,
        'poly_deg_col': poly_deg_col,
        'mu': mu,
        'nu': nu,
        'phi': phi,
        'mu_easd': mu_easd,
        'nu_easd': nu_easd,
        'phi_easd': phi_easd,
        'cov_matrix': cov_matrix,
        'model_is_fit': model_is_fit,
        'std_residuals': std_residuals,
        'num_sig_residuals': num_sig_residuals
    }


def _compute_fisher_info_canonical(num_row, num_col, expected, mu, nu, phi, alpha, beta, n, row_freq, col_freq):
    """Compute Fisher information matrix for canonical correlation model."""
    size = 2 * (num_row + num_col) + 7
    fisher_info = np.zeros((size, size))
    
    zeta = 1 + phi * np.outer(mu, nu)
    
    # For mu vs mu
    fisher_info_mu_mu = np.diag(phi ** 2 * ((expected / zeta ** 2) @ (nu ** 2)) / n)
    
    # For nu vs nu
    fisher_info_nu_nu = np.diag(phi ** 2 * ((mu ** 2) @ (expected / zeta ** 2)) / n)
    
    # For mu vs nu
    fisher_info_mu_nu = phi ** 2 * (expected * np.outer(mu, nu) / zeta ** 2 / n) + phi * np.outer(alpha, beta)
    
    # For mu vs phi
    fisher_info_mu_phi = phi * ((expected / zeta ** 2) @ (nu ** 2)) * mu / n + np.sum(beta * nu) * alpha
    
    # For nu vs phi
    fisher_info_nu_phi = phi * ((mu ** 2) @ (expected / zeta ** 2)) * nu / n + np.sum(alpha * mu) * beta
    
    # For alpha vs mu
    fisher_info_alpha_mu = np.diag(np.full(num_row, phi * np.sum(beta * nu)))
    
    # For beta vs nu
    fisher_info_beta_nu = np.diag(np.full(num_col, phi * np.sum(alpha * mu)))
    
    # For beta vs mu
    fisher_info_beta_mu = phi * np.outer(nu, alpha)
    
    # For alpha vs nu
    fisher_info_alpha_nu = phi * np.outer(mu, beta)
    
    # For alpha vs alpha
    fisher_info_alpha_alpha = np.diag(row_freq / (alpha ** 2))
    
    # For beta vs beta
    fisher_info_beta_beta = np.diag(col_freq / (beta ** 2))
    
    # For alpha vs beta
    fisher_info_alpha_beta = zeta
    
    # For alpha vs phi
    fisher_info_alpha_phi = np.sum(beta * nu) * mu
    
    # For beta vs phi
    fisher_info_beta_phi = np.sum(alpha * mu) * nu
    
    # For phi vs phi
    fisher_info_phi_phi = np.sum(np.outer(mu ** 2, nu ** 2) * expected / zeta ** 2) / n
    
    # Assemble Fisher information matrix
    fisher_info[:num_row, :num_row] = fisher_info_alpha_alpha
    fisher_info[:num_row, num_row:num_row + num_col] = fisher_info_alpha_beta
    fisher_info[:num_row, num_row + num_col:2 * num_row + num_col] = fisher_info_alpha_mu
    fisher_info[:num_row, 2 * num_row + num_col:2 * (num_row + num_col)] = fisher_info_alpha_nu
    fisher_info[:num_row, 2 * (num_row + num_col)] = fisher_info_alpha_phi
    
    fisher_info[num_row:num_row + num_col, num_row:num_row + num_col] = fisher_info_beta_beta
    fisher_info[num_row:num_row + num_col, num_row + num_col:2 * num_row + num_col] = fisher_info_beta_mu
    fisher_info[num_row:num_row + num_col, 2 * num_row + num_col:2 * (num_row + num_col)] = fisher_info_beta_nu
    fisher_info[num_row:num_row + num_col, 2 * (num_row + num_col)] = fisher_info_beta_phi
    
    fisher_info[num_row + num_col:2 * num_row + num_col, num_row + num_col:2 * num_row + num_col] = fisher_info_mu_mu
    fisher_info[num_row + num_col:2 * num_row + num_col, 2 * num_row + num_col:2 * (num_row + num_col)] = fisher_info_mu_nu
    fisher_info[num_row + num_col:2 * num_row + num_col, 2 * (num_row + num_col)] = fisher_info_mu_phi
    
    fisher_info[2 * num_row + num_col:2 * (num_row + num_col), 2 * num_row + num_col:2 * (num_row + num_col)] = fisher_info_nu_nu
    fisher_info[2 * num_row + num_col:2 * (num_row + num_col), 2 * (num_row + num_col)] = fisher_info_nu_phi
    
    fisher_info[2 * (num_row + num_col), 2 * (num_row + num_col)] = fisher_info_phi_phi
    
    # Make symmetric
    fisher_info = fisher_info + fisher_info.T - np.diag(np.diag(fisher_info))
    
    return fisher_info


def model_selection(contingency_table, model_name="Exponential Spacing", p_value_threshold=0.05):
    """
    Perform model selection to find the most parsimonious model that fits the data.
    
    Args:
        contingency_table: Input contingency table
        model_name: Either "Exponential Spacing" or "Canonical Correlation"
        p_value_threshold: Threshold for model fit
        
    Returns:
        Dictionary containing model selection results
    """
    # Convert to numpy array
    if isinstance(contingency_table, pd.DataFrame):
        observed = contingency_table.values.astype(float)
    else:
        observed = np.array(contingency_table, dtype=float)
    
    num_row = observed.shape[0]
    num_col = observed.shape[1]
    
    # Select computation function
    if model_name == "Exponential Spacing":
        computation = exponential_spacing_computation
    else:
        computation = canonical_correlation_computation
    
    # Compute all four models
    rc_model = computation(contingency_table, num_row - 1, num_col - 1, p_value_threshold)
    r_model = computation(contingency_table, num_row - 1, 1, p_value_threshold)
    c_model = computation(contingency_table, 1, num_col - 1, p_value_threshold)
    u_model = computation(contingency_table, 1, 1, p_value_threshold)
    
    models = [
        {'name': 'RC Model', 'result': rc_model, 'tier': 3, 'poly_row': num_row - 1, 'poly_col': num_col - 1},
        {'name': 'R Model', 'result': r_model, 'tier': 2, 'poly_row': num_row - 1, 'poly_col': 1},
        {'name': 'C Model', 'result': c_model, 'tier': 2, 'poly_row': 1, 'poly_col': num_col - 1},
        {'name': 'U Model', 'result': u_model, 'tier': 1, 'poly_row': 1, 'poly_col': 1}
    ]
    
    # Find the best model using hierarchical selection
    # Start with U model and check if it fits, if not move to higher tier
    best_model = None
    
    for tier in [1, 2, 3]:
        tier_models = [m for m in models if m['tier'] == tier]
        fitting_models = [m for m in tier_models if m['result']['model_is_fit']]
        
        if fitting_models:
            # Among fitting models in this tier, pick the one with smallest L-squared
            best_in_tier = min(fitting_models, key=lambda x: x['result']['l_sq_stat'])
            best_model = best_in_tier
            break
    
    return {
        'models': models,
        'best_model': best_model,
        'model_name': model_name,
        'observed': observed
    }


def model_selection_report(selection_result):
    """
    Generate a report for model selection.
    
    Args:
        selection_result: Result from model_selection()
        
    Returns:
        List of report items (strings and DataFrames)
    """
    result = []
    model_name = selection_result['model_name']
    
    result.append(f"# {model_name} Model Selection\n")
    result.append("## Observed Data Matrix")
    result.append(pd.DataFrame(selection_result['observed']))
    result.append("")
    result.append("## Models")
    
    for model in selection_result['models']:
        r = model['result']
        fit_status = "Model fits the observed data." if r['model_is_fit'] else "Model does not fit the observed data."
        
        result.append(f"### {model['name']}")
        result.append(f"- Polynomial Degree for Rows: {model['poly_row']}")
        result.append(f"- Polynomial Degree for Columns: {model['poly_col']}")
        result.append(f"- X-Square: {r['chi_sq_stat']:.2f}, L-Square: {r['l_sq_stat']:.2f}, D.O.F.: {r['d_o_f']}, p-value: {r['p_value']:.4g}")
        result.append(f"- {fit_status}")
        result.append("")
    
    result.append("## Optimal Model")
    if selection_result['best_model'] is None:
        result.append("No model fits the observed data.")
    else:
        result.append(f"{selection_result['best_model']['name']} is the most parsimonious model that fits the observed data.")
    
    return result


def spacing_model_report(result, model_type, model_name="Exponential Spacing"):
    """
    Generate a report for a specific spacing model.
    
    Args:
        result: Computation result from exponential_spacing_computation or canonical_correlation_computation
        model_type: One of "RC Model", "R Model", "C Model", "U Model"
        model_name: Either "Exponential Spacing" or "Canonical Correlation"
        
    Returns:
        List of report items (strings and DataFrames)
    """
    report = []
    
    report.append(f"# {model_name} Model Result")
    
    report.append("## Observed Data Matrix")
    report.append(pd.DataFrame(result['observed']))
    
    report.append("## Expected Data Matrix")
    report.append(pd.DataFrame(np.round(result['expected'], 2)))
    
    report.append("## Association Measures")
    report.append(f"TAU(Y|X) = {result['tau_given_row']:.4g}  |  TAU(X|Y) = {result['tau_given_col']:.4g}")
    
    report.append("## Goodness of Fit")
    report.append(f"X-Square: {result['chi_sq_stat']:.2f}  |  L-Square: {result['l_sq_stat']:.2f}  |  D.O.F.: {result['d_o_f']}  |  p-value: {result['p_value']:.4g}")
    
    report.append("## Model Specification")
    report.append(f"Model Type: {model_type}")
    report.append(f"Polynomial Degree for Rows: {result['poly_deg_row']}")
    report.append(f"Polynomial Degree for Columns: {result['poly_deg_col']}")
    
    report.append("## Parameter Estimates")
    
    # Create parameter table
    num_row = len(result['mu'])
    num_col = len(result['nu'])
    
    params_data = []
    for i in range(num_row):
        params_data.append({
            'Parameter': f'Mu[{i+1}]',
            'Estimate': f"{result['mu'][i]:.4f}",
            'E.A.S.D.': f"{result['mu_easd'][i]:.4f}" if not np.isnan(result['mu_easd'][i]) else "NA"
        })
    
    for i in range(num_col):
        params_data.append({
            'Parameter': f'Nu[{i+1}]',
            'Estimate': f"{result['nu'][i]:.4f}",
            'E.A.S.D.': f"{result['nu_easd'][i]:.4f}" if not np.isnan(result['nu_easd'][i]) else "NA"
        })
    
    params_data.append({
        'Parameter': 'Phi',
        'Estimate': f"{result['phi']:.4f}",
        'E.A.S.D.': f"{result['phi_easd']:.4f}" if not np.isnan(result['phi_easd']) else "NA"
    })
    
    report.append(pd.DataFrame(params_data))
    
    # For Exponential Spacing, add max/min odd ratios
    if model_name == "Exponential Spacing" and 'max_odd_ratio' in result:
        report.append("## Maximum and Minimum Odd Ratios")
        
        if result['max_odd_ratio'] != -np.inf:
            report.append(f"**Max OR:** {result['max_odd_ratio']:.4f}")
            if isinstance(result['row_of_max_or'], list):
                report.append(f"  - Rows: {result['row_of_max_or']}")
            if isinstance(result['col_of_max_or'], list):
                report.append(f"  - Columns: {result['col_of_max_or']}")
        
        if result['min_odd_ratio'] != np.inf:
            report.append(f"**Min OR (Any two adjusted rows/columns):** {result['min_odd_ratio']:.4f}")
            row_str = str(result['row_of_min_or']) if isinstance(result['row_of_min_or'], str) else str(result['row_of_min_or'])
            col_str = str(result['col_of_min_or']) if isinstance(result['col_of_min_or'], str) else str(result['col_of_min_or'])
            report.append(f"  - Rows: {row_str}")
            report.append(f"  - Columns: {col_str}")
    
    report.append("## Model Diagnostics")
    if result['model_is_fit']:
        report.append("**Model fits the observed data.**")
    else:
        report.append("**Model does not fit the observed data.**")
    
    report.append("## Standardized Residuals")
    report.append(pd.DataFrame(np.round(result['std_residuals'], 2)))
    report.append(f"Number of significant residuals = {result['num_sig_residuals']}")
    
    return report

