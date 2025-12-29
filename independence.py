import numpy as np
import pandas as pd
import scipy
import scipy.stats
import streamlit as st
import utils
import pdb
from scipy.stats import chi2

model_names = ['Independence Model', 'ANOAS Model', 'Predictor\'s Proportion',
                   'Log-Odds Z-Test', 'Collapse Cho-Sq Test']

################################ Independence model
def expected_count(df):
    row_total = df.sum(axis=1)
    col_total = df.sum(axis=0)
    total = row_total.sum()
    table = pd.DataFrame(np.round(np.outer(row_total, col_total)/total,2))
    table.index = np.arange(1,df.shape[0]+1)
    table.columns = np.arange(1, df.shape[1]+1)
    return table

def goodman_kruskal_tau(m, by_col_given_row=True):
    if not by_col_given_row:
        m = m.T
    
    row_total = np.array(m.sum(axis=1))
    col_total = np.array(m.sum(axis=0))
    total = row_total.sum()
    
    Q_col = 1-np.sum((col_total/total)**2)
    Q_col_given_row = np.sum((1 - np.sum((m / row_total[:, None]) ** 2, axis=1)) * (row_total / total))
    
    return 1-Q_col_given_row/Q_col

def pearson_chi_squared(observed, expected):
    observed = np.array(observed)
    expected = np.array(expected)
    return ((observed - expected) ** 2 / expected).sum()

def l_sq_chi_sq(observed, expected):
    observed = np.array(observed)
    expected = np.array(expected)
    return 2*(observed*np.log(observed/expected)).sum()

def d_o_f(df):
    return (df.shape[0]-1)*(df.shape[1]-1)

def p_val(l_sq, dof):
    return scipy.stats.chi2.sf(x=l_sq, df=dof)
    
def standardized_residuals(observed, expected, threshold=1.64):
    residual_matrix = (observed - expected) / np.sqrt(expected)
    n_residuals = (np.abs(residual_matrix) > threshold).sum().sum()
    residual_matrix = pd.DataFrame(residual_matrix, index=observed.index, columns=observed.columns)
    return (residual_matrix, n_residuals)
    
def styled_significant(val):
    color = 'yellow' if abs(val) > 1.64 else ''
    return f'background-color: {color}'


def organize_groups(groups, total_num_element):
    membership = np.zeros(total_num_element, dtype=int)
    g = len(groups) + 1
    
    for idx, group in enumerate(groups, start=1):
        membership[group] = idx
        
    for i in range(total_num_element):
        if membership[i] == 0:
            membership[i] = g
            g += 1
    
    organized_membership = np.zeros(total_num_element, dtype=int)
    unique_ordering = np.unique(membership)
    
    organized_groups = []
    for i, label in enumerate(unique_ordering, start=1):
        organized_membership[membership == label] = i
        organized_groups.append(np.where(organized_membership == i)[0])
    
    return organized_groups

def collapse_contingency_table(contingency_table, groups, by_row=True):
    num_groups = len(groups)
    if by_row:
        organized_groups = organize_groups(groups, contingency_table.shape[0])
    else:
        organized_groups = organize_groups(groups, contingency_table.shape[1])
    
    contingency_table_per_group = []
    non_single_groups = []
    
    for group in organized_groups:
        if len(group) > 1:
            if by_row:
                contingency_table_per_group.append(contingency_table[group, :])
            else:
                contingency_table_per_group.append(contingency_table[:, group])
            non_single_groups.append(group)
    
    # Collapse the contingency table
    if by_row:
        collapsed_contingency_table = np.zeros((num_groups, contingency_table.shape[1]))
        for i, group in enumerate(organized_groups):
            collapsed_contingency_table[i, :] = contingency_table[group, :].sum(axis=0)
    else:
        collapsed_contingency_table = np.zeros((contingency_table.shape[0], num_groups))
        for i, group in enumerate(organized_groups):
            collapsed_contingency_table[:, i] = contingency_table[:, group].sum(axis=1)
    
    return {
        'groups': non_single_groups,
        'contingency_table_per_group': contingency_table_per_group,
        'collapsed_contingency_table': collapsed_contingency_table
    }

def independence_model_computation(contingency_table):
    expected_df = expected_count(contingency_table)
    tau_given_x = goodman_kruskal_tau(contingency_table)
    tau_given_y = goodman_kruskal_tau(contingency_table, by_col_given_row=False)
    chi_sq = pearson_chi_squared(contingency_table,expected_df)
    l_sq = l_sq_chi_sq(contingency_table, expected_df)
    df = d_o_f(contingency_table)
    pval = p_val(l_sq, df)
    residual_matrix, n_residuals = standardized_residuals(contingency_table, expected_df)
    return {
        'contingency_table': contingency_table,
        'expected_df': expected_df,
        'tau_given_x': tau_given_x,
        'tau_given_y': tau_given_y,
        'chi_sq': chi_sq,
        'l_sq': l_sq,
        'd_o_f': df,
        'p_val': pval,
        'residual_matrix': residual_matrix,
        'n_residuals': n_residuals
    }
    
import pandas as pd

def independence_model_report(contingency_table):
    computed = independence_model_computation(contingency_table)
    result1 = ""
    result1 += '# Independence Model Result\n\n'
    result1 += '## Observed Data Matrix\n'
    table1 = pd.DataFrame(computed['contingency_table'])
    result2 = '\n'
    result2 += '## Expected Data Matrix\n'
    table2 = pd.DataFrame(computed['expected_df'])
    result3 = ' \n'
    result3 += f'τ(Y|X) = {utils.signif(computed["tau_given_x"],3)}, τ(X|Y) = {utils.signif(computed["tau_given_y"],3)}\n'
    result3 += f'χ² = {utils.signif(computed["chi_sq"],3)}, L² = {utils.signif(computed["l_sq"],3)}, D.O.F = {computed["d_o_f"]}\n\n'
    result3 += f'**p-value = {utils.signif(computed["p_val"],3)}**\n'
    if computed['p_val'] < 0.05:
        result3 += '**Model does not fit the data**\n\n'
    else:
        result3 += '**Model fits the data**\n\n'
    result3 += '## Standardized Residuals\n'
    table3 = utils.signif(pd.DataFrame(computed['residual_matrix']),3)
    result4 = f'Number of significant residuals: {computed["n_residuals"]}\n\n'
    
    return [result1, table1, result2, table2, result3, table3, result4]

def indep_computation2(contingency_table, p_value_threshold=0.05):
    observed = contingency_table
    expected = expected_count(contingency_table)
    
    tau_given_row = goodman_kruskal_tau(observed, by_col_given_row=True)
    tau_given_col = goodman_kruskal_tau(observed, by_col_given_row=False)
    
    chi_sq_stat = pearson_chi_squared(observed, expected)
    l_sq_stat = l_sq_chi_sq(observed, expected)
    
    d_of = (contingency_table.shape[0] - 1) * (contingency_table.shape[1] - 1)
    p_value = chi2.sf(l_sq_stat, df=d_of)
    
    model_is_fit = p_value > p_value_threshold
    
    std_residuals, num_sig_residuals = standardized_residuals(pd.DataFrame(observed), pd.DataFrame(expected))
    
    return {
        'observed': observed,
        'expected': expected,
        'tau_given_row': tau_given_row,
        'tau_given_col': tau_given_col,
        'chi_sq_stat': chi_sq_stat,
        'l_sq_stat': l_sq_stat,
        'd_of': d_of,
        'p_value': p_value,
        'model_is_fit': model_is_fit,
        'std_residuals': std_residuals,
        'num_sig_residuals': num_sig_residuals
    }    

###################################### ANOAS Model

# Use the unified help text from utils
# Note: anoas_help_text kept for backward compatibility but should use utils.RANGE_INPUT_HELP
anoas_help_text = "Enter ranges using format: '1-3;4,5' (use '-' for ranges, ',' to separate items, ';' to separate groups)"

def collapse_for_anoas(data, groups, orientation):
    is_row = orientation == 'Row'
    # Create labels using group_label
    labels = utils.group_label(data.shape[0] if is_row else data.shape[1], groups)
    # Create group lists
    group_list = []
    for i in range(1, max(labels) + 1):
        group_list.append(np.where(np.array(labels) == i)[0])
    
    non_single_data = []
    non_single_group = []
    
    for g in group_list:
        if len(g) > 1:
            if is_row:
                non_single_data.append(data.iloc[g, :] if isinstance(data, pd.DataFrame) else data[g, :])
            else:
                non_single_data.append(data.iloc[:, g] if isinstance(data, pd.DataFrame) else data[:, g])
            non_single_group.append(g)
    
    if is_row:
        collapsed_data = utils.collapse_data(data, row_group=group_list)
    else:
        collapsed_data = utils.collapse_data(data, col_group=group_list)
    
    return {
        'non_single_data': non_single_data,
        'non_single_group': non_single_group,
        'collapsed_data': collapsed_data
    }

def anoas_model_computation(contingency_table, groups, orientation):
    is_row = orientation == 'Row'
    original = contingency_table
    contingency_table[contingency_table==0] = 0.5
    
    full_computed = independence_model_computation(contingency_table)
    collapsed = collapse_for_anoas(contingency_table,groups,orientation)
    group_computed = []
    for m in collapsed['non_single_data']:
        # pdb.set_trace()
        group_computed.append(independence_model_computation(m))
    collapsed_computed = independence_model_computation(collapsed['collapsed_data'])
    collapsed_original = collapse_for_anoas(original, groups, orientation)['collapsed_data']
    
    change_in_l_sq = full_computed['l_sq'] - collapsed_computed['l_sq']
    change_in_dof = full_computed['d_o_f'] - collapsed_computed['d_o_f']
    information_p_value = scipy.stats.chi2.sf(x=change_in_l_sq, df=change_in_dof)
    information_is_lost = information_p_value <= 0.05
    
    return{
        'contingency_table': contingency_table,
        'is_row': is_row,
        'full_computed': full_computed,
        'non_single_group': collapsed['non_single_group'],
        'group_computed': group_computed,
        'collapsed_computed': collapsed_computed,
        'collapsed_original': collapsed_original,
        'change_in_l_sq': change_in_l_sq,
        'change_in_dof': change_in_dof,
        'information_p_value': utils.signif(information_p_value,3),
        'information_is_lost': information_is_lost
    }


def anoas_computation2(contingency_table, original_contingency_table, groups, by_row=True, information_p_value_threshold=0.05):
    # Compute independence for the full contingency table
    full_result = indep_computation2(contingency_table)
    
    # Collapse the contingency table according to the specified groups
    more_contingency_tables = collapse_contingency_table(contingency_table, groups, by_row)
    
    # Compute independence for each group's contingency table
    groups_result = []
    for group_table in more_contingency_tables['contingency_table_per_group']:
        result = indep_computation2(group_table)
        groups_result.append(result)
    
    # Compute independence for the collapsed contingency table
    collapsed_result = indep_computation2(more_contingency_tables['collapsed_contingency_table'])
    
    # Calculate the changes in the chi-square statistic and degrees of freedom
    change_in_l_sq = full_result['l_sq_stat'] - collapsed_result['l_sq_stat']
    change_in_d_of = full_result['d_of'] - collapsed_result['d_of']
    
    # Calculate the p-value for the information loss test
    information_p_value = chi2.sf(change_in_l_sq, df=change_in_d_of)
    
    # Determine if significant information is lost
    information_is_lost = information_p_value <= information_p_value_threshold
    
    # Collapse the original contingency table
    collapsed_original_contingency_table = collapse_contingency_table(original_contingency_table, groups, by_row)['collapsed_contingency_table']
    
    return {
        'full_result': full_result,
        'groups': more_contingency_tables['groups'],
        'groups_result': groups_result,
        'collapsed_result': collapsed_result,
        'collapsed_original_contingency_table': collapsed_original_contingency_table,
        'change_in_l_sq': change_in_l_sq,
        'change_in_d_of': change_in_d_of,
        'information_p_value': information_p_value,
        'information_is_lost': information_is_lost
    }



def anoas_model_report_decomposition_table(computed):
    num_group = len(computed['non_single_group'])
    rows = num_group + 2
    cols = 6
    temp = np.full((rows, cols), "", dtype=object)
    
    # Fill in group data
    for g in range(num_group):
        temp[g, 0] = f"S{g + 1}"
        temp[g, 1] = np.round(computed['group_computed'][g]['l_sq'], 3)
        temp[g, 2] = np.round(computed['group_computed'][g]['chi_sq'], 3)
        temp[g, 3] = computed['group_computed'][g]['d_o_f']
        temp[g, 4] = utils.signif(computed['group_computed'][g]['p_val'], 3)
        temp[g, 5] = f"within {{{', '.join(map(str, computed['non_single_group'][g]+1))}}}"

    # Fill in collapsed data
    temp[num_group, 0] = "Mk"
    temp[num_group, 1] = np.round(computed['collapsed_computed']['l_sq'], 3)
    temp[num_group, 2] = np.round(computed['collapsed_computed']['chi_sq'], 3)
    temp[num_group, 3] = computed['collapsed_computed']['d_o_f']
    temp[num_group, 4] = utils.signif(computed['collapsed_computed']['p_val'], 3)
    temp[num_group, 5] = "between subsets"

    # Fill in full data
    temp[num_group + 1, 0] = "N"
    temp[num_group + 1, 1] = np.round(computed['full_computed']['l_sq'], 3)
    temp[num_group + 1, 2] = np.round(computed['full_computed']['chi_sq'], 3)
    temp[num_group + 1, 3] = computed['full_computed']['d_o_f']
    temp[num_group + 1, 4] = utils.signif(computed['full_computed']['p_val'], 3)
    temp[num_group + 1, 5] = "overall"

    # Convert to DataFrame - do NOT set Sub as index so it stays as a visible column
    df = pd.DataFrame(temp, columns=["Sub", "L²", "χ²", "DOF", "p-value", "Interpretation"])

    return df


import pandas as pd

def anoas_model_report(contingency_table, groups, orientation):
    computed = anoas_model_computation(contingency_table, groups, orientation)
    
    result1 = ""
    result1 += '# ANOAS Model Result\n\n'
    result1 += '## Observed Data Matrix\n'
    table1 = pd.DataFrame(computed['contingency_table'])
    result2 = f'τ(Y|X) = {utils.signif(computed["full_computed"]["tau_given_x"],3)}, τ(X|Y) = {utils.signif(computed["full_computed"]["tau_given_y"],3)}\n\n'
    
    if computed['is_row']:
        result2 += '## Association of Rows:\n\n'
    else:
        result2 += '## Association of Columns:\n\n'
    
    # Decomposition Table
    decomposition_table = anoas_model_report_decomposition_table(computed)
    table2 = decomposition_table
    
    # Information Loss
    result3 = '## Information Loss:\n\n'
    result3 += f'L²(N) - L²(Mk) = {utils.signif(computed["change_in_l_sq"],3)}\n'
    result3 += f'Information loss D.O.F = {computed["change_in_dof"]}\n\n'
    result3 += f'**Information p-value = {computed["information_p_value"]}**\n'
    
    if computed['information_is_lost']:
        result3 += '**Collapsing leads to loss of information**\n\n'
    else:
        result3 += '**Collapsing does not lead to loss of information**\n\n'
    
    result3 += '## Mk Collapsed Matrix\n'
    table3 = pd.DataFrame(computed['collapsed_original'])
    result4 = '## Expected Collapsed Matrix\n'
    table4 = pd.DataFrame(computed['collapsed_computed']['expected_df'])
    result5 = '## Standardized Residuals\n'
    table5 = pd.DataFrame(utils.signif(computed['collapsed_computed']['residual_matrix'],3))
    result6 = f'Number of significant residuals: {computed["collapsed_computed"]["n_residuals"]}\n\n'
    
    return [result1, table1, result2, table2, result3, table3, result4, table4, result5, table5, result6]


############################### Predictor's proportions


def predictors_proportion_report(contingency_table):
    result1 = '# Predictor\'s Proportion Result\n\n'
    result1 += '## Observed Data Matrix\n'
    table1 = pd.DataFrame(contingency_table)
    # Observed Proportion (Per Row)
    observed_row_proportion = pd.DataFrame(contingency_table.div(contingency_table.sum(axis=1), axis=0))
    observed_row_proportion = utils.signif(observed_row_proportion, 3)
    result2 = '## Observed Proportion (Per Row)\n'
    table2 = observed_row_proportion
    
    # Observed Proportion (Per Column)
    observed_col_proportion = pd.DataFrame(contingency_table.div(contingency_table.sum(axis=0), axis=1))
    observed_col_proportion = utils.signif(observed_col_proportion, 3)
    result3 = '## Observed Proportion (Per Column)\n'
    table3 = observed_col_proportion
    
    return [result1, table1, result2, table2, result3, table3]




################################# Log Odds Z-test

def ztest_read_pair(contingency_table, orientation):
    import numpy as np
    
    # Read input from the user
    user_input = st.sidebar.text_input(f"pair of {orientation.lower()}s to test ", help=f'enter two {orientation.lower()} numbers separarated by a comma')
    if user_input:
        # Split and try to convert to integers
        try:
            numbers = list(map(int, user_input.split(',')))
        except ValueError:
            st.sidebar.warning("Invalid input: Please enter two integers separated by a comma.")
            return None
        
        # Check if we have exactly two numbers
        if len(numbers) != 2:
            st.sidebar.warning("Invalid input: Please enter exactly two numbers separated by a comma.")
            return None
        
        # Check the orientation and validate the numbers
        # Note: User enters 1-based indices, so valid range is 1 to num_rows/num_cols (inclusive)
        if orientation == 'Row':
            num_rows = contingency_table.shape[0]
            if any(num < 1 or num > num_rows for num in numbers):
                st.sidebar.warning(f"Invalid input: Both numbers must be between 1 and {num_rows}.")
                return None
        else:
            num_cols = contingency_table.shape[1]
            if any(num < 1 or num > num_cols for num in numbers):
                st.sidebar.warning(f"Invalid input: Both numbers must be between 1 and {num_cols}.")
                return None
        return np.array(numbers)-1


def ztest_computation(contingency_table, pair, orientation):
    original = contingency_table
    contingency_table[contingency_table==0] = 0.5
    by_row = orientation == 'Row'
    
    # Find highest and lowest residuals in the two relevant rows (columns)
    indep_computed = independence_model_computation(contingency_table)
    high_index = [0,0]
    high_n = [0,0]
    low_index = [0,0]
    low_n = [0,0]
    
    
    if by_row:
        for i in range(2):
            high_index[i] = np.argmax(indep_computed['residual_matrix'].iloc[pair[i]])
            high_n[i] = contingency_table.iloc[pair[i], high_index[i]]
            low_index[i] = np.argmin(indep_computed['residual_matrix'].iloc[pair[i]])
            low_n[i] = contingency_table.iloc[pair[i], low_index[i]]
    else:
        for i in range(2):
            high_index[i] = np.argmax(indep_computed['residual_matrix'].iloc[:,pair[i]])
            high_n[i] = contingency_table.iloc[high_index[i], pair[i]]
            low_index[i] = np.argmin(indep_computed['residual_matrix'].iloc[:,pair[i]])
            low_n[i] = contingency_table.iloc[low_index[i], pair[i]]
    
    # Compute odds and z-value
    high_n = np.array(high_n)
    low_n = np.array(low_n)
    odd = high_n / low_n
    z_value = np.abs(np.diff(np.log(odd))) / np.sqrt(np.sum(1/high_n) + np.sum(1/low_n))
    z_value = utils.signif(z_value, 3).astype(float)
    is_significant = z_value > 1.645
    
    return{
        'original': original,
        'contingency_table': contingency_table,
        'pair': pair,
        'indep_computed': indep_computed,
        'high_index': np.array(high_index),
        'high_n': high_n,
        'low_index': np.array(low_index),
        'low_n': low_n,
        'odd': odd,
        'z_value': z_value,
        'is_significant': is_significant
    }
    
def ztest_odds_table(computed, pair, orientation):
    if orientation=='Row':
        row1_largest_residual = computed['indep_computed']['residual_matrix'].iloc[pair[0], computed['high_index'][0]]
        row2_largest_residual = computed['indep_computed']['residual_matrix'].iloc[pair[1], computed['high_index'][1]]
        row1_largest_column = computed['high_index'][0] + 1
        row2_largest_column = computed['high_index'][1] + 1 
        row1_largest_observed = computed['original'].iloc[pair[0], computed['high_index'][0]]
        row2_largest_observed = computed['original'].iloc[pair[1], computed['high_index'][1]]
        row1_odd = computed['odd'][0]
        row2_odd = computed['odd'][1]
        row1_smallest_residual = computed['indep_computed']['residual_matrix'].iloc[pair[0], computed['low_index'][0]]
        row2_smallest_residual = computed['indep_computed']['residual_matrix'].iloc[pair[1], computed['low_index'][1]]
        row1_smallest_column = computed['low_index'][0] + 1
        row2_smallest_column = computed['low_index'][1] + 1
        row1_smallest_observed = computed['original'].iloc[pair[0], computed['low_index'][0]]
        row2_smallest_observed = computed['original'].iloc[pair[1], computed['low_index'][1]]
    
        odds_table = {
        "Largest residual": [row1_largest_residual, row2_largest_residual],
        "Column (largest)": [row1_largest_column, row2_largest_column],
        "Observed data (Largest)": [row1_largest_observed, row2_largest_observed],
        "Smallest residual": [row1_smallest_residual, row2_smallest_residual],
        "Column (smallest)": [row1_smallest_column, row2_smallest_column],
        "Observed data (smallest)": [row1_smallest_observed, row2_smallest_observed],
        "Odd": [row1_odd, row2_odd]
    }
    
    elif orientation=='Column':
        col1_largest_residual = computed['indep_computed']['residual_matrix'].iloc[computed['high_index'][0], pair[0]]
        col2_largest_residual = computed['indep_computed']['residual_matrix'].iloc[computed['high_index'][1], pair[1]]
        col1_largest_row = computed['high_index'][0] + 1
        col2_largest_row = computed['high_index'][1] + 1
        col1_largest_observed = computed['original'].iloc[computed['high_index'][0], pair[0]]
        col2_largest_observed = computed['original'].iloc[computed['high_index'][1], pair[1]]
        col1_odd = computed['odd'][0]
        col2_odd = computed['odd'][1]
        col1_smallest_residual = computed['indep_computed']['residual_matrix'].iloc[computed['low_index'][0], pair[0]]
        col2_smallest_residual = computed['indep_computed']['residual_matrix'].iloc[computed['low_index'][1], pair[1]]
        col1_smallest_row = computed['low_index'][0] + 1
        col2_smallest_row = computed['low_index'][1] + 1
        col1_smallest_observed = computed['original'].iloc[computed['low_index'][0], pair[0]]
        col2_smallest_observed = computed['original'].iloc[computed['low_index'][1], pair[1]]
    
        odds_table = {
        "Largest residual": [col1_largest_residual, col2_largest_residual],
        "Row (largest)": [col1_largest_row, col2_largest_row],
        "Observed data (Largest)": [col1_largest_observed, col2_largest_observed],
        "Smallest residual": [col1_smallest_residual, col2_smallest_residual],
        "Row (smallest)": [col1_smallest_row, col2_smallest_row],
        "Observed data (smallest)": [col1_smallest_observed, col2_smallest_observed],
        "Odd": [col1_odd, col2_odd]
    }
    
    odds_df = pd.DataFrame(odds_table, index=[f"{orientation} {pair[0]+1}", f"{orientation} {pair[1]+1}"])
    return odds_df


def ztest_report(contingency_table, pair, orientation):
    computed = ztest_computation(contingency_table, pair, orientation)
    
    result1 = '# Log-Odds Z-Test Result\n\n'
    
    result1 += '## Observed Data Matrix\n'
    table1 = pd.DataFrame(computed['contingency_table'])
    
    # Process expected matrix
    expected_df = pd.DataFrame(computed['indep_computed']['expected_df'])
    result2 = '## Expected Matrix\n\n'
    table2 = expected_df
    
    # Process standardized residuals
    residuals_df = pd.DataFrame(computed['indep_computed']['residual_matrix'])
    result3 = '## Standardized Residuals\n\n'
    table3 = residuals_df
    
    # Process odds
    odds_df = pd.DataFrame(ztest_odds_table(computed=computed, pair=pair, orientation=orientation))
    result4 = '## Odds\n\n'
    table4 = odds_df
    
    # Add Log-Odds Z-Test result
    result5 = '## Test Result\n\n'
    result5 += f'Z-value: {computed["z_value"][0]}\n\n'
    
    # Add significance result
    if computed['is_significant']:
        result5 += '**The two odds differ significantly**\n'
    else:
        result5 += '**The two odds do not differ significantly**\n'
    
    return [result1, table1, result2, table2, result3, table3, result4, table4, result5]


###############################   CHi-Square test


def chisq_test_computation(contingency_table, row_group, col_group):
    full_computed = independence_model_computation(contingency_table)
    collapsed = utils.collapse_data(contingency_table, row_group, col_group)
    collapsed_computed = independence_model_computation(collapsed)
    
    return {
        'full_computed': full_computed,
        'collapsed_computed': collapsed_computed
    }
    
def chisq_test_report(contingency_table, row_group, col_group):
    computed = chisq_test_computation(contingency_table, row_group, col_group)
    result1 = '# Collapse Chi-Sq Test Result\n\n'
    
    # Observed matrix (collapsed)
    observed_df = pd.DataFrame(computed['full_computed']['contingency_table'])
    result1 += '## Observed Data Matrix (Original)\n\n'
    table1 = observed_df
    
    # Expected matrix (collapsed)
    expecteds_df = pd.DataFrame(computed['full_computed']['expected_df'])
    result2 = '## Expected Data Matrix (Original)\n\n'
    table2 = expecteds_df
    
    # model report
    result3 = f'τ(Y|X) = {np.round(computed["full_computed"]["tau_given_x"],3)}, τ(X|Y) = {np.round(computed["full_computed"]["tau_given_y"],3)}\n'
    # Information Loss 
    result3 += f'χ² = {np.round(computed["full_computed"]["chi_sq"],3)}, L² = {np.round(computed["full_computed"]["l_sq"],3)}, D.O.F = {computed["full_computed"]["d_o_f"]}\n\n'
    result3 += f'**p-value = {utils.signif(computed["full_computed"]["p_val"],3)}**\n\n'

     # Observed matrix (collapsed)
    observed_df_collapsed = pd.DataFrame(computed['collapsed_computed']['contingency_table'])
    result3 += '## Observed Data Matrix (Collapsed)\n\n'
    table3 = observed_df_collapsed
    
    # Expected matrix (collapsed)
    expected_df_collapsed = pd.DataFrame(computed['collapsed_computed']['expected_df'])
    result4 = '## Expected Data Matrix (Collapsed)\n\n'
    table4 = expected_df_collapsed
    
    # model report
    result5 = f'τ(Y|X) = {np.round(computed["collapsed_computed"]["tau_given_x"],3)}, τ(X|Y) = {np.round(computed["collapsed_computed"]["tau_given_y"],3)}\n'
    result5 += f'χ² = {np.round(computed["collapsed_computed"]["chi_sq"],3)}, L² = {np.round(computed["collapsed_computed"]["l_sq"],3)}, D.O.F = {computed["collapsed_computed"]["d_o_f"]}\n\n'
    result5 += f'**p-value = {utils.signif(computed["collapsed_computed"]["p_val"],3)}**\n\n'

    # Standardized residuals (collapsed)
    result5 += '## Standardized Residuals\n'
    table5 = pd.DataFrame(computed['collapsed_computed']['residual_matrix'])
    result6 = f'Number of significant residuals: {computed["collapsed_computed"]["n_residuals"]}\n'
    
    # Chisq test for loss of information
    diff_l_sq_stat = computed['full_computed']['l_sq'] - computed['collapsed_computed']['l_sq']
    diff_dof = computed['full_computed']['d_o_f'] - computed['collapsed_computed']['d_o_f']
    p_val = scipy.stats.chi2.sf(x=float(diff_l_sq_stat), df=int(diff_dof))
    result6 += f'Diff in L² = {np.round(diff_l_sq_stat,3)}, Diff in D.O.F = {diff_dof} \n\n'
    result6 += f'**p-value = {utils.signif(p_val,3)}** \n'
    if p_val > 0.05:
        result6 += '**The collapsing does not entail a loss of information**'
    else:
        result6 += '**The collapsing entails a loss of information**'
    
    return [result1, table1, result2, table2, result3, table3, result4, table4, result5, table5, result6]