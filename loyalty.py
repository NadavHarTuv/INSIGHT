import pandas as pd
import utils
import numpy as np
from matplotlib import pyplot as plt
from scipy.linalg import solve
import independence
import streamlit as st
from scipy.stats import chi2




def adjust_phi(phi):
    num_brand = len(phi)
    temp_a = num_brand / (num_brand - 1)
    temp_b = 2 * np.sum(phi) / (num_brand - 1)
    temp_c = np.sum(phi)**2 / (num_brand - 1) - np.sum(phi**2)
    temp_d = temp_b**2 - 4 * temp_a * temp_c

    if temp_d < 0:
        temp_d = 0  # Ensure non-negative value for square root
    
    adjusted_phi_1 = phi + (-temp_b - np.sqrt(temp_d)) / (2 * temp_a)
    sum_adjusted_phi_1 = np.sum(adjusted_phi_1)
    if sum_adjusted_phi_1 == 0:
        sum_adjusted_phi_1 = 1e-10  # Avoid division by zero
    
    adjusted_phi_1 = adjusted_phi_1 / sum_adjusted_phi_1

    adjusted_phi_2 = phi + (-temp_b + np.sqrt(temp_d)) / (2 * temp_a)
    sum_adjusted_phi_2 = np.sum(adjusted_phi_2)
    if sum_adjusted_phi_2 == 0:
        sum_adjusted_phi_2 = 1e-10  # Avoid division by zero
    
    adjusted_phi_2 = adjusted_phi_2 / sum_adjusted_phi_2

    if np.sum((phi - adjusted_phi_1)**2) < np.sum((phi - adjusted_phi_2)**2):
        return adjusted_phi_1
    else:
        return adjusted_phi_2
    
    
def pseudo_inv(x):
    x_svd = np.linalg.svd(x, full_matrices=False)
    singular_value_threshold = max(x.shape) * np.max(x_svd[1]) * np.finfo(float).eps
    is_positive = x_svd[1] > singular_value_threshold
    
    if not np.any(is_positive):
        return np.zeros((x.shape[1], x.shape[0]))
    else:
        return np.dot(x_svd[2].T[:, is_positive], 
                    np.dot(np.diag(1 / x_svd[1][is_positive]), 
                            x_svd[0].T[:, is_positive]))

def update_alpha_beta_m(beta_old, delta, phi, observed_row_freq, observed_col_freq):
    temp_m = np.outer(1 - phi, phi)
    np.fill_diagonal(temp_m, delta * phi)
    temp_m = np.exp(temp_m)
    
    num_iter = 30
    for _ in range(num_iter):
        alpha = observed_row_freq / np.dot(temp_m, beta_old)
        beta = observed_col_freq / np.dot(alpha, temp_m)
        scaling = np.mean(np.sqrt(beta / alpha))
        alpha = scaling * alpha
        beta = beta / scaling
        
        if np.sqrt(np.sum((beta - beta_old) ** 2)) < 0.00001:
            break
        beta_old = beta
    return alpha, beta

import numpy as np

def update_alpha_beta_q(beta_old, phi, observed_row_freq, observed_col_freq, diag_expected_probs):
    # Create the matrix temp_m as outer(1 - phi, phi) and exponentiate it
    temp_m = np.exp(np.outer(1 - phi, phi))
    
    # Set the diagonal of temp_m to 0
    np.fill_diagonal(temp_m, 0)
    
    num_iter = 30
    for _ in range(num_iter):
        # Update alpha and beta with the given formula
        alpha = (observed_row_freq - diag_expected_probs) / np.dot(temp_m, beta_old)
        beta = (observed_col_freq - diag_expected_probs) / np.dot(alpha, temp_m)
        
        # Check convergence
        if np.sqrt(np.sum((beta - beta_old) ** 2)) < 0.00001:
            break
        
        # Update beta_old for the next iteration
        beta_old = beta
    
    return alpha, beta


def update_expected_probs_m(alpha, beta, delta, phi):
    temp_m = np.outer(1 - phi, phi)
    np.fill_diagonal(temp_m, delta * phi)
    return np.outer(alpha, beta) * np.exp(temp_m)

import numpy as np

def update_expected_probs_q(alpha, beta, phi, diag_observed_freq):
    # Create the matrix temp_m as the outer product of alpha and beta, multiplied by exp(outer(1 - phi, phi))
    temp_m = np.outer(alpha, beta) * np.exp(np.outer(1 - phi, phi))
    
    # Set the diagonal of temp_m to diag_observed_freq
    np.fill_diagonal(temp_m, diag_observed_freq)
    
    return temp_m


def calculate_proj(phi):
    temp_m = np.c_[np.ones_like(phi), 2 * phi]
    # Use high precision matrix inversion
    try:
        temp_inv = np.linalg.inv(temp_m.T @ temp_m)
    except np.linalg.LinAlgError:
        temp_inv = np.linalg.pinv(temp_m.T @ temp_m)
    
    proj = np.eye(len(phi)) - temp_m @ temp_inv @ temp_m.T
    return proj

def m_model_computation(contingency_table, significance_threshold=1.64, p_value_threshold=0.05, p_value_only=False):
    num_brand = contingency_table.shape[0]
    contingency_table = np.array(contingency_table)
    num_people = contingency_table.sum()
    observed_freq = contingency_table / num_people
    observed_row_freq = np.sum(contingency_table, axis=1) / num_people
    observed_col_freq = np.sum(contingency_table, axis=0) / num_people
    
    # Initialize alpha, beta, and delta
    alpha = observed_row_freq
    beta = observed_col_freq
    observed_diag = np.diag(contingency_table)
    temp_mlog = np.log(np.outer(observed_diag, observed_diag) / contingency_table / contingency_table.T)
    delta = (np.sum(temp_mlog) / 2 - (num_brand - 2) / (num_brand - 1)) / (num_brand * (num_brand - 1))
    
    # For phi
    temp_r = np.sum(temp_mlog, axis=0)
    temp_disc = 1 - 4 * (temp_r / 2 + (num_brand - 1) * delta)
    if (temp_disc < 0).any():
        phi = adjust_phi((observed_row_freq + observed_col_freq) / 2)
    else:
        phi = (-1 - np.sqrt(temp_disc)) / 2
    
    # Adjust alpha and beta using the update functions
    alpha, beta = update_alpha_beta_m(beta, delta, phi, observed_row_freq, observed_col_freq)
    expected_probs = update_expected_probs_m(alpha, beta, delta, phi)
    # st.write(f'alpha: {alpha}, beta: {beta}, expected_probs: {expected_probs}')
    num_iter = 5000
    for _ in range(num_iter):
        proj = calculate_proj(phi)
        temp_d = observed_freq - expected_probs
        d_diag = np.diag(temp_d.copy())
        np.fill_diagonal(temp_d, 0)
        grad_phi = (1 - phi) @ temp_d - temp_d @ phi + d_diag * delta
        phi_step = proj @ grad_phi
        
        delta_step = np.sum(d_diag * phi)
        phi = adjust_phi(phi + phi_step)
        delta += delta_step
        alpha, beta = update_alpha_beta_m(beta, delta, phi, observed_row_freq, observed_col_freq)
        expected_probs = update_expected_probs_m(alpha, beta, delta, phi)

        
        if np.sqrt(np.sum(phi_step ** 2) + delta_step ** 2) < 1e-7:
            st.write(f'last iteration is {_}')
            break
    
    # st.write(f'phi: {phi}, delta: {delta}, alpha: {alpha}, beta: {beta}, expected_probs: {expected_probs}')
    # Compute predator and prey tables
    expected_row_probs = np.sum(expected_probs, axis=1)
    expected_col_probs = np.sum(expected_probs, axis=0)
    predator_table = expected_probs / (expected_row_probs[:, np.newaxis] - np.diag(expected_probs))
    np.fill_diagonal(predator_table, np.nan)
    prey_table = expected_probs.T / (expected_col_probs[:, np.newaxis] - np.diag(expected_probs))
    np.fill_diagonal(prey_table, np.nan)
    
    # Compute TRL and TAL
    tal = np.sum(np.diag(observed_freq))
    trl = tal / np.sum(np.minimum(observed_row_freq, observed_col_freq))
    
    # Compute BRL and BRA
    bra = phi
    brl = np.diag(expected_probs) / np.minimum(expected_row_probs, expected_col_probs)
    
    # Compute Fisher information matrix
    expected = expected_probs * num_people
    
    if p_value_only:
        l_sq_stat = np.sum((observed_freq - expected) ** 2 / expected) * num_people
        dof = (num_brand - 1) * (num_brand - 2)
        p_value = 1 - chi2.cdf(l_sq_stat, dof)
        model_is_fit = p_value > p_value_threshold
        return {"p_value": p_value, "model_is_fit": model_is_fit}
    
    expected_offdiag = np.copy(expected)
    np.fill_diagonal(expected_offdiag, 0)
    expected_diag = np.diag(expected)
    
    fisher_info_phi_phi = expected * (1 - np.outer(1 - phi, phi)) + expected.T * (1 - np.outer(phi, 1 - phi))
    np.fill_diagonal(fisher_info_phi_phi, 2 * num_people + np.dot(expected_offdiag, phi**2) + np.dot((1 - phi)**2, expected_offdiag) + delta**2 * expected_diag)
    
    fisher_info_phi_alpha = expected.T * np.outer(np.ones(num_brand), (1 - phi) / alpha)
    np.fill_diagonal(fisher_info_phi_alpha, (delta * expected_diag - np.dot(expected_offdiag, phi)) / alpha)
    
    fisher_info_phi_beta = -expected * np.outer(np.ones(num_brand), phi / beta)
    np.fill_diagonal(fisher_info_phi_beta, (delta * expected_diag + np.dot(1 - phi, expected_offdiag)) / beta)
    
    fisher_info_phi_delta = (delta * phi - 1) * expected_diag
    
    fisher_info_alpha_alpha = np.zeros((num_brand, num_brand))
    np.fill_diagonal(fisher_info_alpha_alpha, np.sum(expected, axis=1) / alpha**2)
    
    fisher_info_alpha_beta = expected / np.outer(alpha, beta)
    
    fisher_info_delta_alpha = expected_diag * phi / alpha
    
    fisher_info_beta_beta = np.zeros((num_brand, num_brand))
    np.fill_diagonal(fisher_info_beta_beta, np.sum(expected, axis=0) / beta**2)
    
    fisher_info_delta_beta = expected_diag * phi / beta
    
    fisher_info_delta_delta = np.sum(expected_diag * phi**2)
    
    fisher_info_phi_eta1 = np.dot((1 - phi), expected_offdiag) - np.dot(expected_offdiag, phi) + delta * expected_diag
    fisher_info_alpha_eta1 = np.sum(expected, axis=1) / alpha
    fisher_info_beta_eta1 = np.sum(expected, axis=0) / beta
    fisher_info_delta_eta1 = delta * np.sum(expected_diag)
    
    fisher_info_phi_eta2 = np.full(num_brand, num_people)
    fisher_info_phi_eta3 = 2 * phi * num_people
    
    fisher_info = np.zeros((3*num_brand+4, 3*num_brand+4))
    
   # Fisher information matrix population
    fisher_info[:num_brand, :num_brand] = fisher_info_phi_phi
    fisher_info[:num_brand, num_brand] = fisher_info_phi_delta
    fisher_info[:num_brand, num_brand+1:2*num_brand+1] = fisher_info_phi_alpha
    fisher_info[:num_brand, 2*num_brand+1:3*num_brand+1] = fisher_info_phi_beta
    fisher_info[:num_brand, 3*num_brand+1] = fisher_info_phi_eta1  # R index 3*num.brand+2 -> Python 3*num_brand+1 (0-indexed)
    fisher_info[:num_brand, 3*num_brand+2] = fisher_info_phi_eta2  # R index 3*num.brand+3 -> Python 3*num_brand+2 (0-indexed)
    fisher_info[:num_brand, 3*num_brand+3] = fisher_info_phi_eta3  # R index 3*num.brand+4 -> Python 3*num_brand+3 (0-indexed)

    fisher_info[num_brand, num_brand] = fisher_info_delta_delta
    fisher_info[num_brand, num_brand+1:2*num_brand+1] = fisher_info_delta_alpha
    fisher_info[num_brand, 2*num_brand+1:3*num_brand+1] = fisher_info_delta_beta
    fisher_info[num_brand, 3*num_brand+1] = fisher_info_delta_eta1  # R index 3*num.brand+2 -> Python 3*num_brand+1 (0-indexed)

    fisher_info[num_brand+1:num_brand+1+num_brand, num_brand+1:num_brand+1+num_brand] = fisher_info_alpha_alpha
    fisher_info[num_brand+1:num_brand+1+num_brand, 2*num_brand+1:3*num_brand+1] = fisher_info_alpha_beta
    fisher_info[num_brand+1:num_brand+1+num_brand, 3*num_brand+1] = fisher_info_alpha_eta1  # R index 3*num.brand+2 -> Python 3*num_brand+1 (0-indexed)

    fisher_info[2*num_brand+1:3*num_brand+1, 2*num_brand+1:3*num_brand+1] = fisher_info_beta_beta
    fisher_info[2*num_brand+1:3*num_brand+1, 3*num_brand+1] = fisher_info_beta_eta1  # R index 3*num.brand+2 -> Python 3*num_brand+1 (0-indexed)
    
    # Make symmetric: copy upper triangle to lower triangle (like R)
    i_lower = np.tril_indices(fisher_info.shape[0], -1)
    fisher_info[i_lower] = fisher_info.T[i_lower]
    fisher_info = fisher_info / num_people
    
    # Compute significance matrix
    # Extract phi-phi block using simple slicing (like R: fisher.info[phi.idx, phi.idx])
    cov_matrix = pseudo_inv(fisher_info[:num_brand, :num_brand])
    diag_cov_matrix = np.diag(cov_matrix)
    
    # Match R: abs(outer(phi, phi, "-")) / sqrt(outer(diag.cov, diag.cov, "+") - 2*cov)
    significance_stat = np.abs(np.subtract.outer(phi, phi)) / np.sqrt(np.add.outer(diag_cov_matrix, diag_cov_matrix) - 2 * cov_matrix) * np.sqrt(num_people / 2)
    significance_table = significance_stat > significance_threshold
    
    l_sq_stat = np.sum((observed_freq - expected_probs) ** 2 / expected_probs) * num_people
    chi_square_stat = independence.pearson_chi_squared(contingency_table, expected)
    dof = (num_brand - 1) * (num_brand - 2)
    p_value = 1 - chi2.cdf(l_sq_stat, dof)
    model_is_fit = p_value > p_value_threshold
    
    tau_given_row = independence.goodman_kruskal_tau(contingency_table, by_col_given_row=True)
    tau_given_col = independence.goodman_kruskal_tau(contingency_table, by_col_given_row=False)
    
    std_residuals, num_sig_residuals = independence.standardized_residuals(pd.DataFrame(contingency_table), pd.DataFrame(expected))
    
    
    
    # Final results dictionary
    return {
        "observed": contingency_table,
        "expected": expected,
        "tau_given_row": tau_given_row,
        "tau_given_col": tau_given_col,
        "l_sq_stat": l_sq_stat,
        "chi_sq_stat": chi_square_stat,
        "d_o_f": dof,
        "predator_table": predator_table,
        "prey_table": prey_table,
        "trl": trl,
        "tal": tal,
        "brl": brl,
        "bra": bra,
        "delta": delta,
        "phi": phi,
        "num_people": num_people,
        "fisher_info": fisher_info,
        "cov_matrix": cov_matrix,
        "significance_stat": significance_stat,
        "significance_table": significance_table,
        "p_value": p_value,
        "model_is_fit": model_is_fit,
        'std_residuals': std_residuals,
        'num_sig_residuals': num_sig_residuals
    }
    
    
def m_model_report(result):
    report = []
    original_contingency_table = result['observed']
    # Add a title and section headings as strings
    report.append("# M Model Result\n")
    report.append("## Observed Data Matrix")
    
    # Add observed data matrix as a dataframe
    report.append(pd.DataFrame(original_contingency_table))

    # Add expected data matrix section
    report.append("## Expected Data Matrix")
    report.append(pd.DataFrame(result['expected']).round(2))

    # Add TAU and model statistics as strings
    report.append(f"TAU(Y|X)= {result['tau_given_row']:.4f}  TAU(X|Y)= {result['tau_given_col']:.4f}")
    report.append(f"X-Square= {result['chi_sq_stat']:.2f}  L-Square= {result['l_sq_stat']:.2f}  D.O.F.= {result['d_o_f']}  p-value= {result['p_value']:.4f}")

    # Parameter estimates as strings
    report.append("## Parameter Estimates")
    report.append(f"Delta: {result['delta']:.4f}")
    report.append("Phi:")
    for i, phi_value in enumerate(result['phi']):
        report.append(f"Phi[{i+1}] = {phi_value:.4f}")

    # Add predator and prey tables as dataframes
    report.append("## Predator Table (1 is very dangerous, 0 is not)")
    report.append(pd.DataFrame(result['predator_table']).round(2))

    report.append("## Prey Table (1 is very weak, 0 is not)")
    report.append(pd.DataFrame(result['prey_table']).round(2))

    # Add measures like TRL, TAL, BRL, and BRA as strings
    report.append("## Measures")
    report.append(f"TRL = {result['trl']:.4f}   TAL = {result['tal']:.4f}")
    report.append(f"BRL: {' '.join([str(round(val, 2)) for val in result['brl']])}")
    report.append(f"BRA: {' '.join([str(round(val, 2)) for val in result['bra']])}")

    report.append("## Significance Matrix")
    significance_matrix = build_significance_display(result['phi'], result['significance_table'])
    report.append(significance_matrix)
    # Model diagnostics as strings
    report.append("## Model Diagnostics")
    if result['model_is_fit']:
        report.append("Model fits the observed data.")
    else:
        report.append("Model does not fit the observed data.")

    # Standardized residuals and significant residuals as dataframes
    report.append("## Standardized Residuals")
    report.append(pd.DataFrame(result['std_residuals']))

    report.append(f"Number of significant residuals = {result['num_sig_residuals']}")

    return report


def q_model_computation(contingency_table, significance_threshold=1.64, p_value_threshold=0.05, p_value_only=False):
    num_brand = contingency_table.shape[0]
    contingency_table = np.array(contingency_table)
    num_people = contingency_table.sum()
    observed_freq = contingency_table / num_people
    observed_row_freq = np.sum(contingency_table, axis=1) / num_people
    observed_col_freq = np.sum(contingency_table, axis=0) / num_people
    
    # Initialize model parameters
    observed_diag = np.diag(contingency_table)
    temp_mlog = np.log(np.outer(observed_diag, observed_diag) / contingency_table / contingency_table.T)
    temp_r = np.sum(temp_mlog, axis=0)
    temp_dl = temp_r - np.diag(temp_mlog)
    temp_sumdl = np.sum(temp_dl)
    temp_disc = (num_brand - 2)**2 - 4 * (temp_r - temp_sumdl + (num_brand - 2) * temp_dl)
    
    if (temp_disc < 0).any():
        phi = adjust_phi((observed_row_freq + observed_col_freq) / 2)
    else:
        phi = (-1 - np.sqrt(temp_disc)) / 2
    
    
    alpha, beta = update_alpha_beta_q(observed_col_freq, phi, observed_row_freq, observed_col_freq, np.zeros(num_brand))
    expected_probs = update_expected_probs_q(alpha, beta, phi, np.diag(observed_freq))

    num_iter = 5000
    for _ in range(num_iter):
        proj = calculate_proj(phi)
        temp_d = observed_freq - expected_probs
        np.fill_diagonal(temp_d, 0)
        grad_phi = (1 - phi) @ temp_d - temp_d @ phi
        phi_step = proj @ grad_phi
        
        phi = adjust_phi(phi + phi_step)
        alpha, beta = update_alpha_beta_q(beta, phi, observed_row_freq, observed_col_freq, np.diag(expected_probs))
        expected_probs = update_expected_probs_q(alpha, beta, phi, np.diag(observed_freq))
        
        if np.sqrt(np.sum(phi_step ** 2)) < 1e-7:
            break

    # Compute predator and prey tables
    expected_row_probs = np.sum(expected_probs, axis=1)
    expected_col_probs = np.sum(expected_probs, axis=0)
    predator_table = expected_probs / (expected_row_probs[:, np.newaxis] - np.diag(expected_probs))
    np.fill_diagonal(predator_table, np.nan)
    prey_table = expected_probs.T / (expected_col_probs[:, np.newaxis] - np.diag(expected_probs))
    np.fill_diagonal(prey_table, np.nan)
    # Compute TRL and TAL
    tal = np.sum(np.diag(observed_freq))
    trl = tal / np.sum(np.minimum(observed_row_freq, observed_col_freq))
    
    # Compute BRL and BRA
    bra = phi
    brl = np.diag(observed_freq) / np.minimum(expected_row_probs, expected_col_probs)
    # Compute Fisher information matrix
    expected = expected_probs * num_people
    
    if p_value_only:
        l_sq_stat = np.sum((observed_freq - expected) ** 2 / expected) * num_people
        dof = (num_brand - 1) * (num_brand - 3)
        p_value = 1 - chi2.cdf(l_sq_stat, dof)
        model_is_fit = p_value > p_value_threshold
        return {"p_value": p_value, "model_is_fit": model_is_fit}
    
    expected_offdiag = np.copy(expected)
    np.fill_diagonal(expected_offdiag, 0)
    expected_diag = np.diag(expected)

    fisher_info_phi_phi = expected * (1 - np.outer(1 - phi, phi)) + expected.T * (1 - np.outer(phi, 1 - phi))
    np.fill_diagonal(fisher_info_phi_phi, 2 * num_people + np.dot(expected_offdiag, phi**2) + np.dot((1 - phi)**2, expected_offdiag))
    
    fisher_info_phi_alpha = expected.T * np.outer(np.ones(num_brand), (1 - phi) / alpha)
    np.fill_diagonal(fisher_info_phi_alpha, np.dot(expected_offdiag, phi) / alpha)
    
    fisher_info_phi_beta = -expected * np.outer(np.ones(num_brand), phi / beta)
    np.fill_diagonal(fisher_info_phi_beta, np.dot(1 - phi, expected_offdiag) / beta)
    
    fisher_info_alpha_alpha = np.zeros((num_brand, num_brand))
    np.fill_diagonal(fisher_info_alpha_alpha, np.sum(expected_offdiag, axis=1) / alpha**2)
    
    fisher_info_beta_beta = np.zeros((num_brand, num_brand))
    np.fill_diagonal(fisher_info_beta_beta, np.sum(expected_offdiag, axis=0) / beta**2)
    
    fisher_info_phi_eta1 = np.dot((1 - phi), expected_offdiag) - np.dot(expected_offdiag, phi)
    fisher_info_alpha_eta1 = np.sum(expected_offdiag, axis=1) / alpha
    fisher_info_beta_eta1 = np.sum(expected_offdiag, axis=0) / beta

    fisher_info_phi_eta2 = np.full(num_brand, num_people)
    fisher_info_phi_eta3 = 2 * phi * num_people
  
    fisher_info = np.zeros((3 * num_brand + 3, 3 * num_brand + 3))
  
    
    fisher_info[:num_brand, :num_brand] = fisher_info_phi_phi

    fisher_info[:num_brand, np.arange(num_brand)+num_brand] = fisher_info_phi_alpha
    
    fisher_info[:num_brand, 2*num_brand+np.arange(num_brand)] = fisher_info_phi_beta
    
    fisher_info[:num_brand, 3*num_brand] = fisher_info_phi_eta1
    
    fisher_info[:num_brand, 3*num_brand+1] = fisher_info_phi_eta2
    
    fisher_info[:num_brand, 3*num_brand+2] = fisher_info_phi_eta3
    
    
    

    fisher_info[num_brand:num_brand + num_brand, num_brand:num_brand + num_brand] = fisher_info_alpha_alpha
    
    fisher_info[num_brand:num_brand+5, 3*num_brand] = fisher_info_alpha_eta1
    
    
    fisher_info[2*num_brand:3*num_brand, 2*num_brand:3*num_brand] = fisher_info_beta_beta
    
    fisher_info[2*num_brand:3*num_brand, 3*num_brand] = fisher_info_beta_eta1
    
    i_lower = np.tril_indices(fisher_info.shape[0], -1)
    fisher_info[i_lower] = fisher_info.T[i_lower]
    fisher_info = fisher_info  / num_people


    # Compute significance matrix
    # R: non.a.b.idx <- c(1:num.brand, 3*num.brand+1:3)
    # In 0-indexed Python: [0:num_brand, 3*num_brand:3*num_brand+2]
    non_a_b_idx = np.concatenate([np.arange(num_brand), np.arange(3*num_brand, 3*num_brand+3)])
    # Use simple slicing like R: fisher.info[non.a.b.idx, non.a.b.idx]
    # Extract rows and columns using the indices
    fisher_subset = fisher_info[np.ix_(non_a_b_idx, non_a_b_idx)]
    cov_matrix = pseudo_inv(fisher_subset)
    cov_matrix = cov_matrix[:num_brand, :num_brand]
    diag_cov_matrix = np.diag(cov_matrix)
    significance_stat = np.abs(np.subtract.outer(phi, phi)) / np.sqrt(np.add.outer(diag_cov_matrix, diag_cov_matrix) - 2 * cov_matrix) * np.sqrt(num_people / 2)
    significance_table = significance_stat > significance_threshold
    
    
    l_sq_stat = np.sum((observed_freq - expected_probs) ** 2 / expected_probs) * num_people
    dof = (num_brand - 1) * (num_brand - 3)
    p_value = 1 - chi2.cdf(l_sq_stat, dof)
    model_is_fit = p_value > p_value_threshold
    
    tau_given_row = independence.goodman_kruskal_tau(contingency_table, by_col_given_row=False)
    tau_given_col = independence.goodman_kruskal_tau(contingency_table, by_col_given_row=True)
    chi_sq_stat = independence.pearson_chi_squared(contingency_table, expected)
    l_sq_stat = independence.l_sq_chi_sq(contingency_table, expected)
    
    std_residuals, num_sig_residuals = independence.standardized_residuals(pd.DataFrame(contingency_table), pd.DataFrame(expected))
    
    return {
        "observed": contingency_table,
        "expected": expected,
        "predator_table": predator_table,
        "prey_table": prey_table,
        "tau_given_row": tau_given_row,
        "tau_given_col": tau_given_col,
        "chi_sq_stat": chi_sq_stat,
        "l_sq_stat": l_sq_stat,
        "d_o_f": dof,
        "phi": phi,
        "trl": trl,
        "tal": tal,
        "brl": brl,
        "bra": bra,
        "num_people": num_people,
        "fisher_info": fisher_info,
        "cov_matrix": cov_matrix,
        "significance_stat": significance_stat,
        "significance_table": significance_table,
        "p_value": p_value,
        "model_is_fit": model_is_fit,
        'std_residuals': std_residuals,
        'num_sig_residuals': num_sig_residuals
    }
    
def q_model_report(result):
    report = []
    original_contingency_table = result['observed']

    # Add a title and section headings as strings
    report.append("# Q Model Result\n")
    report.append("## Observed Data Matrix")
    
    # Add observed data matrix as a dataframe
    report.append(pd.DataFrame(original_contingency_table))

    # Add expected data matrix section
    report.append("## Expected Data Matrix")
    report.append(pd.DataFrame(result['expected']).round(2))

    # Add TAU and model statistics as strings
    report.append(f"TAU(Y|X)= {result['tau_given_row']:.4f}  TAU(X|Y)= {result['tau_given_col']:.4f}")
    report.append(f"X-Square= {result['chi_sq_stat']:.2f}  L-Square= {result['l_sq_stat']:.2f}  D.O.F.= {result['d_o_f']}  p-value= {result['p_value']:.4f}")

    # Parameter estimates as strings
    report.append("## Parameter Estimates")
    report.append("Phi:")
    for i, phi_value in enumerate(result['phi']):
        report.append(f"Phi[{i+1}] = {phi_value:.4f}")

    # Add predator and prey tables as dataframes
    report.append("## Predator Table (1 is very dangerous, 0 is not)")
    report.append(pd.DataFrame(result['predator_table']).round(2))

    report.append("## Prey Table (1 is very weak, 0 is not)")
    report.append(pd.DataFrame(result['prey_table']).round(2))

    # Add measures like TRL, TAL, BRL, and BRA as strings
    report.append("## Measures")
    report.append(f"TRL = {result['trl']:.4f}   TAL = {result['tal']:.4f}")
    report.append(f"BRL: {' '.join([str(round(val, 2)) for val in result['brl']])}")
    report.append(f"BRA: {' '.join([str(round(val, 2)) for val in result['bra']])}")

    report.append("## Significance Matrix")
    significance_matrix = build_significance_display(result['phi'], result['significance_table'])
    report.append(significance_matrix)

    # Model diagnostics as strings
    report.append("## Model Diagnostics")
    if result['model_is_fit']:
        report.append("Model fits the observed data.")
    else:
        report.append("Model does not fit the observed data.")

    # Standardized residuals and significant residuals as dataframes
    report.append("## Standardized Residuals")
    report.append(pd.DataFrame(result['std_residuals']))

    report.append(f"Number of significant residuals = {result['num_sig_residuals']}")

    return report

def bra_brl_plot(bra, brl):
    # Convert BRA and BRL into a DataFrame for plotting
    df = pd.DataFrame({'BRA': bra, 'BRL': brl})

    # Create the plot
    fig, ax = plt.subplots()

    # Plot the points with a specific style
    ax.scatter(df['BRA'], df['BRL'], color="#0072B2", s=200, edgecolor='black', facecolor='lightblue')

    # Label points with their indices with slight offsets
    for i, (x, y) in enumerate(zip(df['BRA'], df['BRL']), start=1):
        ax.text(x + 0.01, y, str(i), fontsize=12, ha='center', va='center', color="darkblue", weight='bold')

    # Add labels and title
    ax.set_xlabel("BRA", fontsize=14)
    ax.set_ylabel("BRL", fontsize=14)
    ax.set_title("Brand Loyalty vs Appeal", fontsize=18, pad=20)

    # Center the title
    ax.title.set_position([0.5, 1.05])

    # Improve layout and style
    ax.grid(True, linestyle='--', alpha=0.6)
    ax.set_facecolor('#f0f0f0')

    # Return the figure object
    return fig


def build_significance_display(phi_values, significance_table):
    """
    Replicates the R reporting logic:
    - sort brands by descending phi
    - construct the same temp matrix with header/separator rows
    - place 'X' using brand indices (1-indexed in R)
    """
    num_brand = len(phi_values)
    phi_ordering = sorted(range(num_brand), key=lambda k: phi_values[k], reverse=True)
    phi_ordering_1 = [idx + 1 for idx in phi_ordering]

    rows = [["" for _ in range(num_brand + 1)] for _ in range(num_brand + 2)]

    for i, brand_idx in enumerate(phi_ordering, start=1):
        rows[1 + i][0] = f"Phi[{brand_idx + 1}]"

    for j, brand_idx in enumerate(phi_ordering, start=1):
        header = f"[{brand_idx + 1}]{phi_values[brand_idx]:.2f}"
        rows[0][j] = header
        rows[1][j] = "=" * (len(header) + 1)

    for j_r in range(1, num_brand + 1):
        for i_r in range(1, num_brand + 1):
            if significance_table[i_r - 1, j_r - 1] and phi_ordering_1[i_r - 1] < phi_ordering_1[j_r - 1]:
                row_idx = 1 + phi_ordering_1[i_r - 1]
                col_idx = phi_ordering_1[j_r - 1]
                rows[row_idx][col_idx] = "X"

    data = [row[1:] for row in rows[2:]]
    index = [row[0] for row in rows[2:]]
    columns = rows[0][1:]

    return pd.DataFrame(data, index=index, columns=columns)


def explanatory_input():
    col1, col2 = st.sidebar.columns(2)
    first_purchase_brands = None
    second_purchase_brands = None
    with col1:
        first_purchase_with = st.selectbox(
            label="1st purchase is", 
            options=['with', 'without'], 
            key='loyalty_1st_purchase_with'
        )
    with col2:
        first_purchase_brand_text = st.text_input("brand", key='brand1')
        if first_purchase_brand_text:
            try:
                first_purchase_brands = [brand+1 for brand in utils.parse_text_groups(first_purchase_brand_text)[0]]
            except:
                st.warning("Brand must be an integer")
            if first_purchase_brands is not None:
                col_idx = int(st.session_state.get('loyalty_col_1', 1)) - 1
                for brand in first_purchase_brands:
                    if brand not in st.session_state['raw_data'].iloc[:, col_idx].unique():
                        first_purchase_brands = None
                        st.warning(f'Brand {brand} is not in the data')
    col3, col4 = st.sidebar.columns(2)
    with col3:
        second_purchase_with = st.selectbox(
            label="2nd purchase is", 
            options=['with', 'without'], 
            key='loyalty_2nd_purchase_with'
        )
    with col4:
        second_purchase_brand_text = st.text_input("brand", key='brand2')
        if second_purchase_brand_text:
            try:
                second_purchase_brands = [brand+1 for brand in utils.parse_text_groups(second_purchase_brand_text)[0]]
            except:
                st.warning("Brand must be an integer")
            if second_purchase_brands is not None:
                col_idx = int(st.session_state.get('loyalty_col_2', 1)) - 1
                for brand in second_purchase_brands:
                    if brand not in st.session_state['raw_data'].iloc[:, col_idx].unique():
                        second_purchase_brands = None
                        st.warning(f'Brand {brand} is not in the data')
                        
    return first_purchase_with, first_purchase_brands, second_purchase_with, second_purchase_brands

def compute_explanatory_df():
    # Retrieve current values from st.session_state
    first_purchase_with = st.session_state.get("loyalty_1st_purchase_with")
    first_purchase_brand_text = st.session_state.get("brand1")
    second_purchase_with = st.session_state.get("loyalty_2nd_purchase_with")
    second_purchase_brand_text = st.session_state.get("brand2")
    
    # If no input is given yet, return None.
    if not (first_purchase_brand_text and second_purchase_brand_text):
        return None

    # Parse the text input values
    try:
        first_purchase_brands = [brand + 1 for brand in utils.parse_text_groups(first_purchase_brand_text)[0]]
    except Exception as e:
        st.warning("First purchase brand: Brand must be an integer")
        first_purchase_brands = None

    try:
        second_purchase_brands = [brand + 1 for brand in utils.parse_text_groups(second_purchase_brand_text)[0]]
    except Exception as e:
        st.warning("Second purchase brand: Brand must be an integer")
        second_purchase_brands = None

    # Validate the brand values against the raw data.
    if first_purchase_brands is not None:
        col_idx = int(st.session_state.get('loyalty_col_1', 1)) - 1
        unique_vals = st.session_state['raw_data'].iloc[:, col_idx].unique()
        for brand in first_purchase_brands:
            if brand not in unique_vals:
                st.warning(f'First purchase: Brand {brand} is not in the data')
                first_purchase_brands = None
                break

    if second_purchase_brands is not None:
        col_idx = int(st.session_state.get('loyalty_col_2', 1)) - 1
        unique_vals = st.session_state['raw_data'].iloc[:, col_idx].unique()
        for brand in second_purchase_brands:
            if brand not in unique_vals:
                st.warning(f'Second purchase: Brand {brand} is not in the data')
                second_purchase_brands = None
                break

    if first_purchase_brands is not None and second_purchase_brands is not None:
        df = st.session_state['raw_data'].copy()
        brand1 = df.iloc[:, int(st.session_state['loyalty_col_1']) - 1]
        brand2 = df.iloc[:, int(st.session_state['loyalty_col_2']) - 1]
        # Create a new column: if brand1 and brand2 match the conditions, set to 1; else 2.
        new_column = np.where((brand1.isin(first_purchase_brands)) & (brand2.isin(second_purchase_brands)), 1, 2)
        # Name the new column as n_cols+1 (next column number)
        new_col_name = len(df.columns) + 1
        df[new_col_name] = new_column
        return df
    return None

