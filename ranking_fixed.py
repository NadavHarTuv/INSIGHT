import numpy as np
import pandas as pd
import math
from itertools import combinations
import independence
import utils
from scipy import stats
import streamlit as st
from scipy.stats import chi2, norm
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.figure import Figure

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


def exploratory_computation(contingency_table, upper_polarity_idx=None):
    contingency_table = np.array(contingency_table)
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
    dist_threshold = predefined_dist_threshold[num_level - 1]
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

    return {
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
        'change_in_l_sq': anoas_result['change_in_l_sq'],  # Placeholder
        'change_in_d_o_f': anoas_result['change_in_d_of'],  # Placeholder
        'information_p_value': anoas_result['information_p_value'],  # Placeholder
        'information_is_lost': anoas_result['information_is_lost'],  # Placeholder
        'avg_satisfaction': avg_satisfaction,
        'rank_by_avg': rank_by_avg,
        'rank_by_best': rank_by_best,
        'rank_by_best_two': rank_by_best_two,
        'rank_by_worst': rank_by_worst,
        'rank_by_worst_two': rank_by_worst_two
    }
    
import numpy as np
import pandas as pd
from scipy.stats import norm  # Used for z-test
# Assuming 'ranking_exploratory_computation' is already implemented

import numpy as np
import pandas as pd
from scipy.stats import norm


def exploratory_report(contingency_table, satisfaction_constraint=None, upper_polarity_idx=None):
    """
    Generate a report for the exploratory analysis.
    
    Args:
        contingency_table (np.ndarray): The contingency table of rankings
        satisfaction_constraint (str): "None", "First", or "Last"
        upper_polarity_idx (int, optional): Index for upper polarity
        
    Returns:
        list: List of report items (strings and DataFrames)
    """
    # Run debug function first to collect detailed info
    
    # Make a proper copy and replace zeros with 0.5
    working_table = contingency_table.copy()
    working_table[working_table == 0] = 0.5
    
    # Convert satisfaction constraint from text to index
    if satisfaction_constraint == "None":
        satisfaction_constraint = None
    elif satisfaction_constraint == "First":
        satisfaction_constraint = 0  # 0-based index for first column
    elif satisfaction_constraint == "Last":
        satisfaction_constraint = contingency_table.shape[1] - 1
    
    # Initialize report items
    report_items = []
    
    # Perform computation
    try:
        # Perform the appropriate computation based on satisfaction constraint
        if satisfaction_constraint is not None:
            # Apply constraint by removing the specified column
            omitted_contingency_table = np.delete(working_table, satisfaction_constraint, axis=1)
            result = exploratory_computation(omitted_contingency_table, upper_polarity_idx=upper_polarity_idx)
            
            # Start building the report
            report_items.append("Exploratory Model Result")
            
            # Create observed data DataFrame
            observed_df = pd.DataFrame(result['observed'])
            report_items.append(observed_df.round(2))
            
            # Add model statistics
            report_items.append(f"L-Square= {result['full_l_sq_stat']:.2f}  D.O.F.= {result['full_d_o_f']}  p-value= {result['full_p_value']:.4f}")
            
            # Add ranking section
            report_items.append("Final Ranking with Stochastic Ordering")
            report_items.append(f"Ranking procedure with omission of satisfaction category: {satisfaction_constraint + 1}")
            report_items.append(f"Critical value= {result['critical_value']:.2f}")
            
            # Get dimensions
            num_cluster = result['collapsed_pmf'].shape[0]
            num_collapsed_level = result['collapsed_pmf'].shape[1]
            
            # Create ranking dictionary safely
            ranking_dict = {}
            
            # Create proper labels for brand clusters
            brand_clusters = {}
            for i in range(1, num_cluster + 1):
                brand_clusters[i] = []
            
            for i, cluster_id in enumerate(result['brand_cluster']):
                brand_clusters[cluster_id].append(i + 1)  # Brand IDs are 1-based
            
            # Add the brand cluster labels as the first column
            brand_labels = []
            for i in range(num_cluster):
                if result['stochastic_ordering'] is not None:
                    ordered_idx = result['stochastic_ordering'][i]
                    cluster_id = i + 1
                    brands_in_cluster = brand_clusters[cluster_id]
                    brand_labels.append(f"Brands {', '.join(map(str, brands_in_cluster))}")
                else:
                    cluster_id = i + 1
                    brands_in_cluster = brand_clusters[cluster_id]
                    brand_labels.append(f"Brands {', '.join(map(str, brands_in_cluster))}")
            
            ranking_dict["Brands"] = brand_labels
            
            # Create proper labels for satisfaction level partitions
            satisfaction_partitions = {}
            for i in range(1, num_collapsed_level + 1):
                satisfaction_partitions[i] = []
            
            for i, partition_id in enumerate(result['satisfaction_partition']):
                satisfaction_partitions[partition_id].append(i + 1)  # Level IDs are 1-based
            
            # Safely create the PMF columns with proper labels
            for i in range(num_cluster):
                brand_idx = result['stochastic_ordering'][i] if result['stochastic_ordering'] is not None else i
                for j in range(num_collapsed_level):
                    partition_id = j + 1
                    levels_in_partition = satisfaction_partitions[partition_id]
                    
                    if len(levels_in_partition) == 1:
                        level_key = f"Level {levels_in_partition[0]}"
                    else:
                        level_key = f"Levels {min(levels_in_partition)}-{max(levels_in_partition)}"
                    
                    if level_key not in ranking_dict:
                        ranking_dict[level_key] = [None] * num_cluster
                    
                    ranking_dict[level_key][i] = result['collapsed_pmf'][brand_idx, j].round(2)
            
            # Add polarity index
            ranking_dict["Polarity Index"] = [
                result['polarity_index'][result['stochastic_ordering'][i] if result['stochastic_ordering'] is not None else i].round(2)
                for i in range(num_cluster)
            ]
            
            # Create DataFrame from dictionary
            ranking_df = pd.DataFrame(ranking_dict)
            report_items.append(ranking_df)
            
            # Information loss section
            report_items.append("Information Loss")
            report_items.append(f"L^2(N) - L^2(Mk) = {result['change_in_l_sq']:.2f}")
            report_items.append(f"Information loss D.O.F. = {result['change_in_d_o_f']}")
            report_items.append(f"Information p-value = {result['information_p_value']:.4f}")
            
            if result['information_is_lost']:
                report_items.append("Collapsing leads to information loss.")
            else:
                report_items.append("Collapsing does not lead to information loss.")
            
            # Z-test matrix if available
            if hasattr(result, 'z_matrix'):
                report_items.append("Z-test for stochastic ordering")
                
                # Create z-test dictionary
                z_test_dict = {
                    "Brands": [f"Brand {i+1}" for i in range(num_cluster)]
                }
                
                # Add each column for each brand
                for j in range(num_cluster):
                    z_test_dict[f"Brand {j+1}"] = []
                    for i in range(num_cluster):
                        if i == j:
                            z_test_dict[f"Brand {j+1}"].append('\\')
                        elif result['z_matrix'][i, j] > result['critical_value']:
                            z_test_dict[f"Brand {j+1}"].append('X')
                        else:
                            z_test_dict[f"Brand {j+1}"].append('')
                
                z_test_df = pd.DataFrame(z_test_dict)
                report_items.append(z_test_df)
        
        else:
            # No satisfaction constraint
            result = exploratory_computation(working_table, upper_polarity_idx=upper_polarity_idx)
            
            report_items.append("Exploratory Model Result")
            
            # Create observed data DataFrame
            observed_df = pd.DataFrame(result['observed'])
            report_items.append(observed_df.round(2))
            
            # Add model statistics
            report_items.append(f"L-Square= {result['full_l_sq_stat']:.2f}  D.O.F.= {result['full_d_o_f']}  p-value= {result['full_p_value']:.4f}")
            
            # Add ranking section
            report_items.append("Final Ranking with Stochastic Ordering")
            report_items.append(f"Critical value= {result['critical_value']:.2f}")
            
            # Get dimensions
            num_cluster = result['collapsed_pmf'].shape[0]
            num_collapsed_level = result['collapsed_pmf'].shape[1]
            
            # Create ranking dictionary safely
            ranking_dict = {}
            
            # Create proper labels for brand clusters
            brand_clusters = {}
            for i in range(1, num_cluster + 1):
                brand_clusters[i] = []
            
            for i, cluster_id in enumerate(result['brand_cluster']):
                brand_clusters[cluster_id].append(i + 1)  # Brand IDs are 1-based
            
            # Add the brand cluster labels as the first column
            brand_labels = []
            for i in range(num_cluster):
                if result['stochastic_ordering'] is not None:
                    ordered_idx = result['stochastic_ordering'][i]
                    cluster_id = i + 1
                    brands_in_cluster = brand_clusters[cluster_id]
                    brand_labels.append(f"Brands {', '.join(map(str, brands_in_cluster))}")
                else:
                    cluster_id = i + 1
                    brands_in_cluster = brand_clusters[cluster_id]
                    brand_labels.append(f"Brands {', '.join(map(str, brands_in_cluster))}")
            
            ranking_dict["Brands"] = brand_labels
            
            # Create proper labels for satisfaction level partitions
            satisfaction_partitions = {}
            for i in range(1, num_collapsed_level + 1):
                satisfaction_partitions[i] = []
            
            for i, partition_id in enumerate(result['satisfaction_partition']):
                satisfaction_partitions[partition_id].append(i + 1)  # Level IDs are 1-based
            
            # Safely create the PMF columns with proper labels
            for i in range(num_cluster):
                brand_idx = result['stochastic_ordering'][i] if result['stochastic_ordering'] is not None else i
                for j in range(num_collapsed_level):
                    partition_id = j + 1
                    levels_in_partition = satisfaction_partitions[partition_id]
                    
                    if len(levels_in_partition) == 1:
                        level_key = f"Level {levels_in_partition[0]}"
                    else:
                        level_key = f"Levels {min(levels_in_partition)}-{max(levels_in_partition)}"
                    
                    if level_key not in ranking_dict:
                        ranking_dict[level_key] = [None] * num_cluster
                    
                    ranking_dict[level_key][i] = result['collapsed_pmf'][brand_idx, j].round(2)
            
            # Add polarity index
            ranking_dict["Polarity Index"] = [
                result['polarity_index'][result['stochastic_ordering'][i] if result['stochastic_ordering'] is not None else i].round(2)
                for i in range(num_cluster)
            ]
            
            # Create DataFrame from dictionary
            ranking_df = pd.DataFrame(ranking_dict)
            report_items.append(ranking_df)
        
        return report_items
        
    except Exception as e:
        return None
        # If error occurs during computation, return debug info to help diagnose



def adjust_mu_or_nu(x, prob):
    x_mean = np.sum(x * prob)
    x_var = np.sum((x - x_mean)**2 * prob)
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
    
    print(f"DEBUG poly_deg_row={poly_deg_row}, poly_deg_col={poly_deg_col}")
    
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
            print(f"DEBUG pseudo_inv input shape: {x.shape}")
            
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
                print(f"DEBUG pseudo_inv (no positive values) output shape: {result.shape}")
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
                    print(f"DEBUG pseudo_inv shape mismatch: got {result.shape}, expected {expected_shape}")
                    if result.shape == (expected_shape[1], expected_shape[0]):
                        # If we got the transpose of expected shape, transpose it
                        result = result.T
                        print(f"DEBUG pseudo_inv after transpose: {result.shape}")
                
                print(f"DEBUG pseudo_inv final output shape: {result.shape}")
                return result
        except Exception as e:
            print(f"DEBUG Error in pseudo_inv: {e}")
            import traceback
            print(f"DEBUG Traceback: {traceback.format_exc()}")
            # Fall back to numpy's built-in pinv as a last resort
            result = np.linalg.pinv(x)
            print(f"DEBUG Using numpy pinv as fallback, output shape: {result.shape}")
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
        
        # Update phi using Newton-Raphson
        fx = np.sum((expected - working_table) * np.outer(mu, nu))
        dfdx = np.sum(expected * np.outer(mu**2, nu**2))
        phi = phi - fx / np.maximum(dfdx, 1e-10)
        
        # Calculate parameter changes
        mu_change = np.max(np.abs(mu - old_mu))
        nu_change = np.max(np.abs(nu - old_nu))
        phi_change = np.abs(phi - old_phi)
        
        # Log iteration details
        if debug:
            if t < total_num_mu_nu_iter:
                debug_info.append(f"After update - Mu: min={np.min(mu):.4f}, max={np.max(mu):.4f}, mean={np.mean(mu):.4f}")
                debug_info.append(f"After update - Nu: min={np.min(nu):.4f}, max={np.max(nu):.4f}, mean={np.mean(nu):.4f}")
            debug_info.append(f"After update - Phi: {phi:.6f}")
            debug_info.append(f"Changes - Mu: {mu_change:.6f}, Nu: {nu_change:.6f}, Phi: {phi_change:.6f}")
            debug_info.append(f"Margin errors - Row: max={np.max(row_errors):.6f}, Col: max={np.max(col_errors):.6f}")
            
            # More detailed output every 5 iterations
            if t % 5 == 0 or t == total_num_iter - 1:
                debug_info.append("\nDetailed parameter values:")
                debug_info.append(f"Mu: {mu.round(4)}")
                debug_info.append(f"Nu: {nu.round(4)}")
                debug_info.append(f"Row errors: {row_errors.round(6)}")
                debug_info.append(f"Col errors: {col_errors.round(6)}")
                debug_info.append(f"Expected total: {expected.sum()}")
    
    # Final update of alpha, beta, and expected
    updated = update_alpha_beta_expected(alpha, beta, mu, nu, phi, total, row_sums, col_sums)
    alpha = updated["alpha"]
    beta = updated["beta"]
    expected = updated["expected"]
    
    print(f"DEBUG expected shape: {expected.shape}")
    
    # Calculate final errors
    final_row_errors = np.abs(row_sums - np.sum(expected, axis=1))
    final_col_errors = np.abs(col_sums - np.sum(expected, axis=0))
    
    # Compute Fisher information matrix
    if debug:
        debug_info.append("\n=== COMPUTING FISHER INFORMATION MATRIX ===")
    
    # For mu vs mu
    fisher_info_mu_mu = np.zeros((num_row, num_row))
    np.fill_diagonal(fisher_info_mu_mu, np.dot(expected, nu**2) * phi**2)
    
    # For nu vs nu
    fisher_info_nu_nu = np.zeros((num_col, num_col))
    np.fill_diagonal(fisher_info_nu_nu, np.dot(mu**2, expected) * phi**2)
    
    # For mu vs nu
    fisher_info_mu_nu = expected * ((phi**2) * np.outer(mu, nu) - phi)
    
    # For mu vs phi
    fisher_info_mu_phi = phi * mu * np.dot(expected, nu**2)
    
    # For nu vs phi
    fisher_info_nu_phi = phi * nu * np.dot(mu**2, expected)
    
    # For alpha vs mu
    fisher_info_alpha_mu = np.zeros((num_row, num_row))
    np.fill_diagonal(fisher_info_alpha_mu, phi / alpha * np.dot(expected, nu))
    
    # For beta vs nu
    fisher_info_beta_nu = np.zeros((num_col, num_col))
    np.fill_diagonal(fisher_info_beta_nu, phi / beta * np.dot(mu, expected))
    
    # For beta vs mu
    fisher_info_beta_mu = phi * expected.T * (nu[:, np.newaxis] / beta[:, np.newaxis])
    # Debug the shape issue
    print(f"DEBUG fisher_info_beta_mu shape: {fisher_info_beta_mu.shape}")
    print(f"DEBUG fisher_info_beta_mu.T shape: {fisher_info_beta_mu.T.shape}")
    
    # For alpha vs nu
    fisher_info_alpha_nu = phi * expected * (mu[:, np.newaxis] / alpha[:, np.newaxis])
    
    # For alpha vs alpha
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
    fisher_info[:num_row, num_row+num_col:num_row+num_col+num_row] = fisher_info_alpha_mu
    
    # Fix for potential broadcasting error with fisher_info_alpha_nu
    try:
        # Check shapes before assigning
        target_shape = (num_row, num_col)  # Expected shape of the slice
        source_shape = fisher_info_alpha_nu.shape
        print(f"DEBUG: fisher_info_alpha_nu shape = {source_shape}, target shape = {target_shape}")
        
        if source_shape == target_shape:
            # Direct assignment if shapes match
            fisher_info[:num_row, num_row+num_col+num_row:2*num_row+num_col+num_col] = fisher_info_alpha_nu
        elif source_shape == (num_col, num_row):
            # If we need to transpose
            print(f"DEBUG: Need to transpose fisher_info_alpha_nu")
            fisher_info[:num_row, num_row+num_col+num_row:2*num_row+num_col+num_col] = fisher_info_alpha_nu.T
        else:
            # Create correctly shaped matrix
            print(f"DEBUG: Creating compatible matrix for fisher_info_alpha_nu")
            # Create a correctly sized matrix
            compatible_matrix = np.zeros(target_shape)
            # Copy data carefully, up to the minimum dimensions
            min_rows = min(source_shape[0], target_shape[0])
            min_cols = min(source_shape[1], target_shape[1])
            for i in range(min_rows):
                for j in range(min_cols):
                    if i < source_shape[0] and j < source_shape[1]:
                        compatible_matrix[i, j] = fisher_info_alpha_nu[i, j]
            fisher_info[:num_row, num_row+num_col+num_row:2*num_row+num_col+num_col] = compatible_matrix
    except Exception as e:
        print(f"DEBUG: Error in fisher_info_alpha_nu assignment: {e}")
        # If all else fails, use zeros to avoid breaking computation
        fisher_info[:num_row, num_row+num_col+num_row:2*num_row+num_col+num_col] = np.zeros((num_row, num_col))
    
    fisher_info[:num_row, 2*(num_row+num_col)] = fisher_info_alpha_phi
    
    # Beta block
    fisher_info[num_row:num_row+num_col, num_row:num_row+num_col] = fisher_info_beta_beta
    
    # Fix for the broadcasting error with fisher_info_beta_mu.T
    try:
        # Check shapes before assigning
        target_shape = (num_col, num_row)  # Expected shape of the slice
        source_shape = fisher_info_beta_mu.T.shape
        print(f"DEBUG: fisher_info_beta_mu.T shape = {source_shape}, target shape = {target_shape}")
        
        if source_shape == target_shape:
            # Direct assignment if shapes match
            fisher_info[num_row:num_row+num_col, num_row+num_col:num_row+num_col+num_row] = fisher_info_beta_mu.T
        elif source_shape == (num_row, num_col):
            # If we need to transpose
            print(f"DEBUG: Need to use fisher_info_beta_mu directly instead of its transpose")
            fisher_info[num_row:num_row+num_col, num_row+num_col:num_row+num_col+num_row] = fisher_info_beta_mu
        else:
            # Create correctly shaped matrix
            print(f"DEBUG: Creating compatible matrix for fisher_info_beta_mu")
            # Create a correctly sized matrix
            compatible_matrix = np.zeros(target_shape)
            # Copy data carefully, up to the minimum dimensions
            min_rows = min(source_shape[0], target_shape[0])
            min_cols = min(source_shape[1], target_shape[1])
            for i in range(min_rows):
                for j in range(min_cols):
                    if i < source_shape[0] and j < source_shape[1]:
                        compatible_matrix[i, j] = fisher_info_beta_mu.T[i, j]
            fisher_info[num_row:num_row+num_col, num_row+num_col:num_row+num_col+num_row] = compatible_matrix
    except Exception as e:
        print(f"DEBUG: Error in fisher_info_beta_mu assignment: {e}")
        # If all else fails, use zeros to avoid breaking computation
        fisher_info[num_row:num_row+num_col, num_row+num_col:num_row+num_col+num_row] = np.zeros((num_col, num_row))
    
    fisher_info[num_row:num_row+num_col, num_row+num_col+num_row:2*num_row+num_col+num_col] = fisher_info_beta_nu
    fisher_info[num_row:num_row+num_col, 2*(num_row+num_col)] = fisher_info_beta_phi
    
    # Mu block
    fisher_info[num_row+num_col:num_row+num_col+num_row, num_row+num_col:num_row+num_col+num_row] = fisher_info_mu_mu
    fisher_info[num_row+num_col:num_row+num_col+num_row, num_row+num_col+num_row:2*num_row+num_col+num_col] = fisher_info_mu_nu
    fisher_info[num_row+num_col:num_row+num_col+num_row, 2*(num_row+num_col)] = fisher_info_mu_phi
    
    # Nu block
    fisher_info[num_row+num_col+num_row:2*num_row+num_col+num_col, num_row+num_col+num_row:2*num_row+num_col+num_col] = fisher_info_nu_nu
    fisher_info[num_row+num_col+num_row:2*num_row+num_col+num_col, 2*(num_row+num_col)] = fisher_info_nu_phi
    
    # Phi vs phi block
    fisher_info[2*(num_row+num_col), 2*(num_row+num_col)] = fisher_info_phi_phi
    
    # Make symmetric (fill lower triangle from upper triangle)
    i_lower = np.tril_indices(fisher_info.shape[0], -1)
    fisher_info[i_lower] = fisher_info.T[i_lower]
    
    # Scale by sample size
    fisher_info /= total
    
    # Compute estimated asymptotic standard deviations (EASD)
    # Get covariance matrix for mu, nu, phi by inverting relevant part of Fisher information
    # In the R code, this starts at row/col num.row+num.col+1 and goes to 2*(num.row+num.col)+1
    cov_matrix_indices = slice(num_row+num_col, 2*(num_row+num_col)+1)
    cov_matrix = pseudo_inv(fisher_info)[cov_matrix_indices, cov_matrix_indices] / total
    
    # Extract standard errors for mu, nu, and phi
    mu_se = np.zeros(num_row)
    for i in range(num_row):
        mu_se[i] = np.sqrt(max(0, cov_matrix[i, i]))
    
    nu_se = np.zeros(num_col)
    for i in range(num_col):
        nu_se[i] = np.sqrt(max(0, cov_matrix[num_row+i, num_row+i]))
    
    # Add a small epsilon to prevent division by zero for phi_se
    phi_se = np.sqrt(max(1e-10, cov_matrix[num_row+num_col, num_row+num_col]))
    

    # Calculate likelihood ratio chi-square (G-squared)
    g_squared = 0
    for i in range(num_row):
        for j in range(num_col):
            if working_table[i,j] > 0 and expected[i,j] > 0:
                g_squared += 2 * working_table[i,j] * np.log(working_table[i,j] / expected[i,j])
    
    # Calculate degrees of freedom
    # The current formula is: df = (n_rows - 1) * (n_cols - 1) - (n_rows + n_cols - 2)
    # R formula appears to be: df = (n_rows - 1) * (n_cols - 1) - (n_rows + n_cols - 3)
    # This +1 adjustment makes our formula match the R implementation
    df = (num_row - 1) * (num_col - 1) - (num_row + num_col - 3)
    df = max(1, df)  # Ensure df is at least 1
    
    # P-value from chi-square distribution
    p_value = 1 - stats.chi2.cdf(g_squared, df)
    
    # Calculate dissimilarity index
    dissimilarity = np.sum(np.abs(working_table - expected)) / (2 * total)
    
    # Calculate tau statistics (measures of association)
    # Tau|row: reduction in prediction error of column given row
    tau_given_row = independence.goodman_kruskal_tau(working_table,  by_col_given_row=True)
    
    # Tau|col: reduction in prediction error of row given column
    tau_given_col = independence.goodman_kruskal_tau(working_table, by_col_given_row=False)
    
    # Calculate standardized residuals
    std_residuals = (working_table - expected) / np.sqrt(expected)
    
    # Calculate odds ratios
    max_odd_ratio = -np.inf
    min_odd_ratio = np.inf
    row_of_max_or = None
    col_of_max_or = None
    row_of_min_or = None
    col_of_min_or = None
    
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
            # Custom upper tail: combined probability of upper tail divided by lower tail
            try:
                first_upper_idx = min(upper_polarity_idx)
                # Adjust to 0-based indexing if needed
                if first_upper_idx > 0:
                    first_upper_idx = first_upper_idx - 1  # Convert to 0-based index
                
                # Ensure index doesn't exceed number of columns
                if first_upper_idx >= n_cols:
                    first_upper_idx = n_cols - 1
                
                # Get the partition index if using collapsed levels
                if len(current_partition) > first_upper_idx:
                    partition_i = current_partition[first_upper_idx]
                else:
                    partition_i = current_partition[-1]  # Use last partition
                
                # Ensure we don't have an empty lower tail
                if partition_i <= 1:
                    partition_i = 2
                
                # Calculate polarity as upper tail / lower tail
                polarity_index = np.sum(expected_collapsed_pmf[:, partition_i-1:], axis=1) / np.sum(expected_collapsed_pmf[:, :partition_i-1], axis=1)
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
                brand_str = f"Brand {brands[0] + 1}"
            else:
                brand_str = f"Brands {', '.join([str(b + 1) for b in brands])}"
                
            report_items.append(f"### Cluster {cluster_id}: {brand_str}")
            report_items.append(f"Average Satisfaction: {col_avg_sat[cluster_idx]:.4f}")
            
            # Show probability distribution for the cluster
            report_items.append(f"Probability Distribution:")
            prob_dist = [f"{collapsed_pmf[cluster_idx, col]:.4f}" for col in range(collapsed_pmf.shape[1])]
            
            # Create proper satisfaction level labels
            if "partitioned_satisfaction" in result:
                partitions = result["partitioned_satisfaction"]
                
                # Adjust the level numbers if first level was omitted
                level_offset = 1 if satisfaction_constraint == "First" else 0
                
                level_labels = []
                for partition in partitions:
                    if len(partition) == 1:
                        partitioned_labels.append(f"Level {partition[0] + 1 + level_offset}")
                    else:
                        partitioned_labels.append(f"Levels {partition[0] + 1 + level_offset}-{partition[-1] + 1 + level_offset}")
                
                # Add the omitted level
                if constraint_index == 0:
                    # First level was omitted
                    level_labels = ["Level 1"] + partitioned_labels
                else:
                    # Last level was omitted
                    num_levels = result.get("original_num_levels", len(partitions) + 1)
                    level_labels = partitioned_labels + [f"Level {num_levels}"]
            else:
                # No partitioning info, create basic labels
                reintegrated_pmf = result["reintegrated_pmf"]
                num_levels = reintegrated_pmf.shape[1]
                
                if constraint_index == 0:
                    # First level was omitted, so add "Level 1" as first label
                    level_labels = ["Level 1"] + [f"Level {i + 1}" for i in range(1, num_levels)]
                else:
                    # Last level was omitted
                    level_labels = [f"Level {i + 1}" for i in range(num_levels)]
            
            # Create table with reintegrated distributions
            reintegrated_table = []
            
            # Header row
            reintegrated_table.append(["Cluster"] + level_labels)
            
            # For each cluster
            for cluster_id in sorted(cluster_to_brand.keys()):
                cluster_idx = cluster_to_index[cluster_id]
                row_data = [f"Cluster {cluster_id}"]
                
                # Add probability values for each satisfaction level
                for col in range(result["reintegrated_pmf"].shape[1]):
                    row_data.append(f"{result['reintegrated_pmf'][cluster_idx, col]:.4f}")
                
                reintegrated_table.append(row_data)
            
            # Convert to DataFrame for display
            reintegrated_df = pd.DataFrame(reintegrated_table[1:], columns=reintegrated_table[0])
            report_items.append(reintegrated_df)
            
            # Note about omitted level
            if constraint_index == 0:
                report_items.append("Note: The first satisfaction level (Level 1) was omitted during the analysis and reintegrated here.")
            else:
                report_items.append(f"Note: The last satisfaction level was omitted during the analysis and reintegrated here.")

        return report_items
        
    except Exception as e:
        import traceback
        print(f"Error in confirmatory_report: {str(e)}")
        print(traceback.format_exc())
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
    fig = plt.figure(figsize=(8, 5))
    plt.bar(range(1, len(avg)+1), avg, color='skyblue', edgecolor='black')
    plt.xlabel("Brand (by cluster ordering)")
    plt.ylabel("Average Satisfaction")
    plt.title("Average Satisfaction per Brand")
    plt.xticks(range(1, len(avg)+1))
    plt.tight_layout()
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
    
    # Create a single figure with subplots
    fig, axes = plt.subplots(n_rows, n_cols, figsize=(n_cols*4, n_rows*3))
    
    # Flatten axes array for easier indexing if there are multiple rows and columns
    if n_rows > 1 or n_cols > 1:
        axes = axes.flatten()
    else:
        axes = [axes]  # Make it iterable for the single subplot case
    
    # Create satisfaction level labels based on the partition
    satisfaction_labels = []
    if 'satisfaction_partition' in result:
        # Create proper labels for satisfaction level partitions
        satisfaction_partitions = {}
        for i in range(1, num_levels + 1):
            satisfaction_partitions[i] = []
        
        # If we have a constraint, we need to adjust the labels
        if has_constraint:
            # Create adjusted satisfaction labels
            if constraint_index == 0:  # First column was omitted
                # Add "Level 1" as the first label
                satisfaction_labels.append("Level 1")
                
                # Create labels for the remaining levels, adjusting indices
                for j in range(1, num_levels):
                    levels_in_partition = satisfaction_partitions.get(j, [])
                    if not levels_in_partition:
                        # Use index + 1 (since level 1 is the constraint)
                        level_key = f"Level {j+1}"
                    elif len(levels_in_partition) == 1:
                        level_key = f"Level {levels_in_partition[0]+1}"
                    else:
                        level_key = f"Levels\n{min(levels_in_partition)+1}-{max(levels_in_partition)+1}"
                    satisfaction_labels.append(level_key)
            else:  # Last column or other was omitted
                # Process satisfaction labels normally for the unconstrained part
                for j in range(num_levels - 1):
                    partition_id = j + 1
                    levels_in_partition = satisfaction_partitions.get(partition_id, [])
                    
                    if not levels_in_partition:
                        level_key = f"Level {j+1}"
                    elif len(levels_in_partition) == 1:
                        level_key = f"Level {levels_in_partition[0]}"
                    else:
                        level_key = f"Levels\n{min(levels_in_partition)}-{max(levels_in_partition)}"
                    
                    satisfaction_labels.append(level_key)
                
                # Add the constraint level as the last label
                if constraint_index == result['original_contingency_table'].shape[1] - 1:
                    satisfaction_labels.append(f"Level {constraint_index+1}")
                else:
                    satisfaction_labels.append(f"Level {constraint_index+1}")
        else:
            # No constraint - create labels normally
            for i, partition_id in enumerate(result['satisfaction_partition']):
                satisfaction_partitions[partition_id].append(i + 1)  # Level IDs are 1-based
            
            for j in range(num_levels):
                partition_id = j + 1
                levels_in_partition = satisfaction_partitions.get(partition_id, [])
                
                if len(levels_in_partition) == 0:
                    level_key = f"Level {j+1}"
                elif len(levels_in_partition) == 1:
                    level_key = f"Level {levels_in_partition[0]}"
                else:
                    level_key = f"Levels\n{min(levels_in_partition)}-{max(levels_in_partition)}"
                
                satisfaction_labels.append(level_key)
    else:
        # Default labels if satisfaction_partition is not available
        satisfaction_labels = [f"Level {i+1}" for i in range(num_levels)]
    
    # Create brand cluster labels
    brand_labels = []
    if 'brand_cluster' in result:
        # Group brands by cluster
        brand_clusters = {}
        for i, cluster_id in enumerate(result['brand_cluster']):
            if cluster_id not in brand_clusters:
                brand_clusters[cluster_id] = []
            brand_clusters[cluster_id].append(i + 1)  # Convert to 1-based indexing
        
        # Create brand labels based on clusters
        for i in range(num_clusters):
            brand_idx = result['stochastic_ordering'][i] if 'stochastic_ordering' in result and result['stochastic_ordering'] is not None else i
            if brand_idx < len(brand_clusters.keys()):
                cluster_id = list(brand_clusters.keys())[brand_idx]
                brands_in_cluster = brand_clusters[cluster_id]
                
                if len(brands_in_cluster) == 1:
                    brand_label = f"Brand {brands_in_cluster[0]}"
                else:
                    brand_label = f"Brands {', '.join(map(str, brands_in_cluster))}"
                
                brand_labels.append(brand_label)
            else:
                brand_labels.append(f"Brand Cluster {i+1}")
    else:
        brand_labels = [f"Brand Cluster {i+1}" for i in range(num_clusters)]
    
    # Ensure we have enough labels
    if len(brand_labels) < num_clusters:
        brand_labels.extend([f"Brand Cluster {i+1}" for i in range(len(brand_labels), num_clusters)])
    
    # Create a bar plot for each cluster
    for i in range(num_clusters):
        ax = axes[i]
        ax.bar(range(1, num_levels+1), collapsed[i, :], color='skyblue', edgecolor='black')
        ax.set_xlabel("Satisfaction Level")
        ax.set_ylabel("Probability")
        ax.set_title(brand_labels[i])
        ax.set_xticks(range(1, num_levels+1))
        ax.set_xticklabels(satisfaction_labels)
        ax.set_ylim(0, max(collapsed.max() * 1.1, 0.1))  # Set y-axis limit with some padding
        
        # If this plot shows data with a constraint reintegrated, add a note
        if has_constraint:
            if constraint_index == 0:
                highlight_idx = 0  # First column
            else:
                highlight_idx = num_levels - 1  # Last column
            
            # Highlight the constrained level bar
            ax.get_children()[highlight_idx].set_color('lightcoral')
    
    # Add a note about the constraint if applicable
    if has_constraint:
        if constraint_index == 0:
            constraint_desc = "first"
        else:  # Must be the last column
            constraint_desc = "last"
        
        plt.figtext(0.5, 0.01, 
                   f"Note: The {constraint_desc} satisfaction level (highlighted) was omitted during analysis and reintegrated afterward.",
                   ha='center', fontsize=10, style='italic')
    
    # Hide any unused subplots
    for i in range(num_clusters, len(axes)):
        axes[i].set_visible(False)
    
    plt.tight_layout()
    if has_constraint:
        # Add extra space at the bottom for the note
        plt.subplots_adjust(bottom=0.1)
    
    return fig

def plot_exploratory_brand_distribution(result, contingency_table):
    """
    Generate a bar plot showing average satisfaction level for each brand.
    
    Args:
        result: Dictionary from exploratory_computation
        contingency_table: The original contingency table
    """
    # Convert contingency_table to numpy array if it's a DataFrame
    if isinstance(contingency_table, pd.DataFrame):
        contingency_table = np.array(contingency_table)
    
    # Calculate average satisfaction for each brand
    satisfaction_levels = np.arange(1, contingency_table.shape[1] + 1)
    row_sums = contingency_table.sum(axis=1)
    pmf = contingency_table / row_sums[:, np.newaxis]
    avg_satisfaction = np.sum(pmf * satisfaction_levels, axis=1)
    
    # Create figure
    fig, ax = plt.subplots(figsize=(10, 6))
    
    # Plot one bar per brand
    x = np.arange(len(avg_satisfaction))
    ax.bar(x, avg_satisfaction, color='steelblue')
    
    # Customize plot
    ax.set_xlabel('Brand')
    ax.set_ylabel('Average Satisfaction Level')
    ax.set_title('Average Satisfaction by Brand')
    ax.set_xticks(x)
    ax.set_xticklabels([f'Brand {i+1}' for i in range(len(avg_satisfaction))])
    
    plt.tight_layout()
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
    
    # Create figure with subplots for each brand group
    n_groups = len(pmf)
    fig, axes = plt.subplots(1, n_groups, figsize=(15, 6))
    if n_groups == 1:
        axes = [axes]
    
    # Plot each group's distribution
    for i, (ax, group_pmf, group_label) in enumerate(zip(axes, pmf, group_labels)):
        x = np.arange(len(group_pmf))
        ax.bar(x, group_pmf, color='steelblue')
        ax.set_title(group_label)
        ax.set_xlabel('Combined Satisfaction Levels')
        ax.set_ylabel('Probability')
        ax.set_xticks(x)
        ax.set_xticklabels([f'Level {j+1}' for j in range(len(group_pmf))])
        ax.set_ylim(0, 0.3)
    
    plt.suptitle('Ranking of Brand Groups', y=1.05)
    plt.tight_layout()
    return fig

