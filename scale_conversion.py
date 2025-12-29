"""
Scale Conversion module for INSIGHT.
Converts between two different scales/distributions using optimal joint probability computation.
"""

import numpy as np
import pandas as pd


def scale_conversion_computation(row_prob, col_prob):
    """
    Compute the optimal joint probability matrix from row and column marginal probabilities.
    
    This algorithm finds the joint probability distribution that maximizes the correlation
    between the two scales by greedily assigning probability mass.
    
    Args:
        row_prob: Array of row marginal probabilities (must sum to 1)
        col_prob: Array of column marginal probabilities (must sum to 1)
        
    Returns:
        Joint probability matrix of shape (num_row, num_col)
    """
    # Make copies to avoid modifying originals
    row_prob = row_prob.copy()
    col_prob = col_prob.copy()
    
    num_row = len(row_prob)
    num_col = len(col_prob)
    joint_prob = np.zeros((num_row, num_col))
    
    i = 0
    j = 0
    
    while i < num_row and j < num_col:
        joint_prob[i, j] = min(row_prob[i], col_prob[j])
        
        if row_prob[i] > col_prob[j]:
            row_prob[i] = row_prob[i] - joint_prob[i, j]
            j += 1
        else:
            col_prob[j] = col_prob[j] - joint_prob[i, j]
            i += 1
    
    return joint_prob


def scale_conversion_report(row_data, col_data, row_labels=None, col_labels=None):
    """
    Generate a complete scale conversion report.
    
    Args:
        row_data: Array of row values (counts/frequencies)
        col_data: Array of column values (counts/frequencies)
        row_labels: Optional labels for row categories
        col_labels: Optional labels for column categories
        
    Returns:
        List of report items (strings and DataFrames)
    """
    report = []
    
    # Create default labels if not provided
    if row_labels is None:
        row_labels = [f"Row {i+1}" for i in range(len(row_data))]
    else:
        row_labels = [str(x) for x in row_labels]
    
    if col_labels is None:
        col_labels = [f"Col {j+1}" for j in range(len(col_data))]
    else:
        col_labels = [str(x) for x in col_labels]
    
    # Compute marginal probabilities
    row_prob = row_data / np.sum(row_data)
    col_prob = col_data / np.sum(col_data)
    
    # Compute joint probability distribution
    joint_prob = scale_conversion_computation(row_prob, col_prob)
    
    # Compute row-wise and column-wise proportions
    row_sums = joint_prob.sum(axis=1, keepdims=True)
    row_wise_prop = np.divide(joint_prob, row_sums, where=row_sums != 0)
    row_wise_prop[row_sums.flatten() == 0, :] = np.nan
    
    col_sums = joint_prob.sum(axis=0, keepdims=True)
    col_wise_prop = np.divide(joint_prob, col_sums, where=col_sums != 0)
    col_wise_prop[:, col_sums.flatten() == 0] = np.nan
    
    # Build report
    report.append("# Scale Conversion Model Result")
    
    report.append("## Observed Data")
    report.append(f"**Row:** {', '.join([str(int(x)) for x in row_data])}")
    report.append(f"**Column:** {', '.join([str(int(x)) for x in col_data])}")
    
    report.append("## Marginal Probabilities")
    report.append(f"**Row:** {', '.join([f'{x:.4f}' for x in row_prob])}")
    report.append(f"**Column:** {', '.join([f'{x:.4f}' for x in col_prob])}")
    
    report.append("## (Optimal) Joint Probabilities")
    joint_df = pd.DataFrame(
        np.round(joint_prob, 4),
        index=row_labels,
        columns=col_labels
    )
    report.append(joint_df)
    
    report.append("## Row-wise Proportions")
    report.append("(Each row sums to 1 - shows how each row category distributes across column categories)")
    row_wise_df = pd.DataFrame(
        np.round(row_wise_prop, 4),
        index=row_labels,
        columns=col_labels
    )
    report.append(row_wise_df)
    
    report.append("## Column-wise Proportions")
    report.append("(Each column sums to 1 - shows how each column category distributes across row categories)")
    col_wise_df = pd.DataFrame(
        np.round(col_wise_prop, 4),
        index=row_labels,
        columns=col_labels
    )
    report.append(col_wise_df)
    
    return report
