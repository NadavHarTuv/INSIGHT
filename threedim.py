import utils
import streamlit as st
import independence
import numpy as np
import pandas as pd
from scipy.stats import chi2
import statsmodels.api as sm
from statsmodels.formula.api import glm
import pdb
import matplotlib.pyplot as plt
import plotly.express as px



def get_target_variable(three_dim_data):
    options = []
    for col in three_dim_data.columns:
        if len(three_dim_data[col].unique())==2:
            options.append(col)
    target_variable = st.sidebar.selectbox('Select target varible', options)
    return target_variable


def transform_to_3d_array(grouped_data):
    required_columns = ['X', 'Y', 'Z']
    for col in required_columns:
        if col not in grouped_data.columns:
            return None
    # Determine the shape of the 3D array
    max_x = grouped_data['X'].max() 
    max_y = grouped_data['Y'].max() 
    max_z = grouped_data['Z'].max() 
    
    # Initialize the 3D array with zeros (correct shape: X, Y, Z)
    data_array = np.zeros((max_x, max_y, max_z))
    
    # Fill the array with the values from the DataFrame
    for _, row in grouped_data.iterrows():
        x, y, z, value = int(row['X'])-1, int(row['Y'])-1, int(row['Z'])-1, row['value']
        data_array[x, y, z] = value
    
    return data_array

import statsmodels.api as sm
from itertools import product

def array_to_dataframe(a):
    """
    Convert a 3-dimensional numpy array to a DataFrame with columns for the indices and values.

    Parameters:
    a (numpy.ndarray): The input 3-dimensional array.

    Returns:
    pd.DataFrame: A DataFrame with columns X, Y, Z, and value representing the indices and values of the array elements.
    """
    # Get the shape of the array
    shape = a.shape

    # Create lists to store the indices and values
    X, Y, Z, values = [], [], [], []

    # Loop over all elements in the array
    for x in range(shape[0]):
        for y in range(shape[1]):
            for z in range(shape[2]):
                # Append the indices and value to the lists
                X.append(x + 1)
                Y.append(y + 1)
                Z.append(z + 1)
                values.append(a[x, y, z])

    # Create a DataFrame from the lists
    df = pd.DataFrame({'X': X, 'Y': Y, 'Z': Z, 'value': values})
    df.index+=1

    return df


def loglin(data, which_x):
    import numpy as np
    import statsmodels.api as sm
    from itertools import product
    # Construct a DataFrame of indices.
    ndim = data.ndim
    indices = list(product(*[range(dim) for dim in data.shape]))
    df_indices = pd.DataFrame(indices, columns=[f'dim{i}' for i in range(ndim)])
    df_indices['count'] = data.flatten()
    
    # Build formula terms based on which_x.
    terms = []
    for term in which_x:
        if isinstance(term, int):
            terms.append(f'C(dim{term})')
        elif isinstance(term, list):
            # For interaction terms.
            interaction = " * ".join([f'C(dim{d})' for d in term])
            terms.append(interaction)
    terms = list(dict.fromkeys(terms))
    formula = 'count ~ ' + ' + '.join(terms)
    model = sm.GLM.from_formula(formula, data=df_indices, family=sm.families.Poisson()).fit()
    expected = model.mu.reshape(data.shape)
    lrt = 2 * (model.llf - model.llnull)
    observed = df_indices['count'].values
    pearson_chi2 = np.sum((observed - model.mu) ** 2 / model.mu)
    df_resid = model.df_resid
    # lambda_coeffs = dict(model.params)
    return {
        'expected': expected,
        'lrt': lrt,
        'df': df_resid,
        'params': model.params,
        'pearson_chi2': pearson_chi2,
        # 'lambda_coef': lambda_coeffs
    }

def compute_lambda_coeffs(expected, data, which_x):

    """
    Compute lambda coefficients from the fitted values.
    """
    log_fit = np.log(expected)
    n_dims = len(data.shape)
    varnames = [f'dim{i}' for i in range(n_dims)]
    lambda_coeffs = {}

    # Compute intercept
    intercept = np.mean(log_fit)
    lambda_coeffs['Intercept'] = intercept

    # Subtract the intercept
    log_fit -= intercept
    # print("Which_X: ", which_x)
    # Compute main effects
    for i in range(len(which_x)):
        if isinstance(which_x[i], list) and len(which_x[i]) == 1:
            var = which_x[i][0]
            levels = data.shape[var]
            lambda_coeffs[varnames[var]] = np.zeros(levels)
            for level in range(levels):
                indices = np.where(np.indices(data.shape)[var] == level)
                lambda_coeffs[varnames[var]][level] = np.mean(log_fit[indices])

    # Compute interactions
    for interaction in which_x:
        if isinstance(interaction, list) and len(interaction) > 1:
            interaction_name = '.'.join([varnames[i] for i in interaction])
            interaction_levels = [data.shape[i] for i in interaction]
            lambda_coeffs[interaction_name] = np.zeros(interaction_levels)
            for levels in product(*[range(l) for l in interaction_levels]):
                indices = np.where(np.all([np.indices(data.shape)[i] == level for i, level in zip(interaction, levels)], axis=0))
                lambda_coeffs[interaction_name][levels] = np.mean(log_fit[indices])
    # Ensure each row and column sum to zero
            for axis in range(len(interaction_levels)):
                for idx in range(interaction_levels[axis]):
                    slice_indices = tuple(slice(None) if i != axis else idx for i in range(len(interaction_levels)))
                    mean = np.mean(lambda_coeffs[interaction_name][slice_indices])
                    lambda_coeffs[interaction_name][slice_indices] -= mean

    return lambda_coeffs

def loglin_computation(data, which_x, p_value_threshold=0.05):
    import numpy as np
    # If no interaction terms are provided, use a simple expected value.
    observed = data
    if len(which_x) == 0:
        expected = np.full(observed.shape, np.mean(observed))
        l_sq_stat = 2 * np.sum(observed * np.log(observed / expected + 1e-10))
        d_o_f = np.prod([(dim - 1) for dim in observed.shape])
        lambda_coef = {'Intercept': np.log(np.mean(observed)) - np.log(np.sum(observed))}
    else:
        computed = loglin(data, which_x)
        expected = computed['expected']
        # l_sq_stat = computed['lrt']
        l_sq_stat = 2 * np.sum(observed * np.log(observed / expected + 1e-10))
        pearson_chi2 = computed['pearson_chi2']
        # d_o_f = computed['df']
        # st.write(observed)
        # st.write(f'observed.shape: {observed.shape}')
        d_o_f = np.prod([(dim - 1) for dim in observed.shape])
    p_value = np.exp(np.log(chi2.sf(l_sq_stat, d_o_f) + 1e-10))
    log_p_value = np.log(p_value + 1e-10)
    model_is_fit = p_value > p_value_threshold
    residual_matrix = (observed - expected) / np.sqrt(expected + 1e-10)
    num_signif_residual = np.sum(np.abs(residual_matrix) > 1.64)
    lambda_coef = compute_lambda_coeffs(expected, data, which_x)
    return {
        'observed': observed,
        'expected': expected,
        'l_sq_stat': l_sq_stat,
        'd_o_f': d_o_f,
        'p_value': p_value,
        'log_p_value': log_p_value,
        'model_is_fit': model_is_fit,
        'std_residual': residual_matrix,
        'num_signif_residual': num_signif_residual,
        'lambda_coef': lambda_coef,
        'pearson_chi2': pearson_chi2
    }
    
def all_coef_computation(data):
    data[data == 0] = 0.5
    models = [
        loglin_computation(data, [[0, 1], [0, 2], [1, 2]]),
        loglin_computation(data, [[0, 1], [0, 2]]),
        loglin_computation(data, [[0, 1], [1, 2]]),
        loglin_computation(data, [[0, 2], [1, 2]]),
        loglin_computation(data, [[2], [0, 1]]),
        loglin_computation(data, [[1], [0, 2]]),
        loglin_computation(data, [[0], [1, 2]]),
        loglin_computation(data, [0, 1, 2])
    ]
    
    coef_texts = [
        "X Y Z (X,Y) (X,Z) (Y,Z)",
        "X Y Z (X,Y) (X,Z)",
        "X Y Z (X,Y) (Y,Z)",
        "X Y Z (X,Z) (Y,Z)",
        "X Y Z (X,Y)",
        "X Y Z (X,Z)",
        "X Y Z (Y,Z)",
        "X Y Z"
    ]
    
    return {
        'models': models,
        'coef_texts': coef_texts
    }

def all_coef_report(computed):
    report = "#3-Dimensional Model Report \n\n"
    report = "## List of possible models \n \n"
    dof_space = 6 
    l_sq_space = 10
    indent = " "*4
    tiers = [3,2,2,2,1,1,0]
    models = []
    for i in range(8):
        report += f"**{computed['coef_texts'][i]}** \n\n"
        report += f"      D.O.F = {computed['models'][i]['d_o_f']}    L² = {np.round(computed['models'][i]['l_sq_stat'],2)}    p-value = {utils.signif(computed['models'][i]['p_value'],2)}\n"
        if computed['models'][i]['p_value'] > 0.05:
            report += '    Model fits the data \n\n'
        else:
            report += '    Model does not fit the data \n\n'

    return [report]

def select_coefs():
    for key in ['X', 'Y', 'Z', 'XY', 'XZ', 'YZ']:
        if key not in st.session_state:
            st.session_state[key] = False
    def update_xy():
        if st.session_state.XY:
            st.session_state.X = True
            st.session_state.Y = True

    def update_xz():
        if st.session_state.XZ:
            st.session_state.X = True
            st.session_state.Z = True

    def update_yz():
        if st.session_state.YZ:
            st.session_state.Y = True
            st.session_state.Z = True

    def update_x():
        if not st.session_state.X:
            st.session_state.XY = False
            st.session_state.XZ = False

    def update_y():
        if not st.session_state.Y:
            st.session_state.XY = False
            st.session_state.YZ = False

    def update_z():
        if not st.session_state.Z:
            st.session_state.XZ = False
            st.session_state.YZ = False

    st.sidebar.checkbox('Intercept', key='Intercept', value=True, disabled=True)
    st.sidebar.checkbox('X', key='X', on_change=update_x)
    st.sidebar.checkbox('Y', key='Y', on_change=update_y)
    st.sidebar.checkbox('Z', key='Z', on_change=update_z)

    st.sidebar.checkbox('X Y', key='XY', on_change=update_xy)
    st.sidebar.checkbox('X Z', key='XZ', on_change=update_xz)
    st.sidebar.checkbox('Y Z', key='YZ', on_change=update_yz)

    selected = []
    if st.session_state.X:
        selected.append("X")
    if st.session_state.Y:
        selected.append("Y")
    if st.session_state.Z:
        selected.append("Z")
    if st.session_state.XY:
        selected.append("X Y")
    if st.session_state.XZ:
        selected.append("X Z")
    if st.session_state.YZ:
        selected.append("Y Z")

    return selected


def parse_selected_coefs(selected):
    """
    Parses the selected coefficients and returns a list of dimensions (which_x).
    
    Args:
        selected (list): A list of selected coefficients in the format ['X', 'Y', 'X Y', 'X Z', etc.].
    
    Returns:
        list: A list of dimensions in the form of which_x.
    """
    which_x = []
    for coef in selected:
        if ' ' in coef:
            dims = [ord(dim) - ord('X') for dim in coef.split()]
            which_x.append(dims)
        else:
            which_x.append([ord(coef) - ord('X')])
    return which_x


def select_coef_computation(data, which_x):
    original = data
    data[data==0] = 0.5
    
    model = loglin_computation(data, which_x)
    
    return{
        'original': original,
        'data': data,
        'which_x': which_x,
        'model': model
    }
    
    
def coef_string(coef):
    """Generate a formatted string for the coefficients."""
    s = []
    coef_name = ["X", "Y", "Z"]
    for g in coef:
        if isinstance(g, int):
            g = [g]  # Make single integers iterable
        s_g = ",".join([coef_name[i] for i in g])
        if len(g) > 1:
            s_g = f"({s_g})"
        s.append(s_g)
    return " ".join(s)

    
def select_coef_report(computed):
    # Section 1: Report Header
    report1 = '# 3-Dimensional Model Report\n\n'
    report1 += '## Results for the selected model:\n\n'
    report1 += '### Observed Data Matrix:\n\n' 
    
    # DataFrame for Observed Data Matrix
    original_df = array_to_dataframe(computed['original'])
    original_df.rename(columns={original_df.columns[-1]: 'count'}, inplace=True)
    
    # Section 2: Expected Data Matrix
    report2 = '### Expected Data Matrix:\n\n'
    expected_df = array_to_dataframe(np.round(computed['model']['expected'], 2))
    expected_df.rename(columns={expected_df.columns[-1]: 'expected count'}, inplace=True)
    
    # Section 3: Model Statistics
    report3 = f"D.O.F = {computed['model']['d_o_f']}    L² = {np.round(computed['model']['pearson_chi2'], 2)}    p-value = {utils.signif(computed['model']['p_value'], 2)}\n"
    report3 += '\n### Information on selected model\n'
    report3 += f"**Lambda coefficients of the selected model: Intercept {coef_string(computed['which_x'])}**\n\n"
    
    lambda_coefs = computed['model']['lambda_coef']
    
    for key, value in lambda_coefs.items():
        if key == 'Intercept':
            report3 += f'Coefficient of "Intercept":\n1 : {value:.4f}\n\n'
        else:
            if 'dim0' in key:
                dim_name = 'X'
            elif 'dim1' in key:
                dim_name = 'Y'
            elif 'dim2' in key:
                dim_name = 'Z'
            
            if '.' in key:
                # Interaction term
                dim_names = key.split('.')
                dim_names = [dim_name if 'dim0' in name else ('Y' if 'dim1' in name else 'Z') for name in dim_names]
                dim_key = f'({",".join(dim_names)})'
                report3 += f'Coefficient of "{dim_key}":\n'
                for idx, val in np.ndenumerate(value):
                    indices = " ".join(str(i + 1) for i in idx)
                    report3 += f'{indices} : {val:.4f}\n'
            else:
                report3 += f'Coefficient of "{dim_name}":\n'
                for idx, val in enumerate(value):
                    report3 += f'{idx + 1} : {val:.4f}\n'
            report3 += '\n'
    
    report3 += '**Model Diagnostics**\n\n'
    if computed['model']['model_is_fit']:
        report3 += 'Model fits the observed data\n'
    else:
        report3 += 'Model does not fit the observed data\n\n'
    report3 += '**Standardized residuals**\n\n'
    
    # DataFrame for Standardized Residuals
    resid_df = array_to_dataframe(np.round(computed['model']['std_residual'], 2))
    resid_df.rename(columns={resid_df.columns[-1]: 'residual'}, inplace=True)
    
    # Section 4: Number of Significant Residuals
    report4 = f"Number of significant residuals = {computed['model']['num_signif_residual']}\n"
    
    return [report1, original_df, report2, expected_df, report3, resid_df, report4]

import numpy as np

def target_computation(data, target):
    """
    Perform three-dimensional target computation.
    """
    target = {'X': 0, 'Y': 1, 'Z': 2}[target]
    
    original = data.copy()
    data[data == 0] = 0.5

    which_x = [[0], [1], [2], [0, 1], [0, 2], [1, 2]]
    model = loglin_computation(data, which_x)
    
    # Compute Mu
    target_lambda = model['lambda_coef'][f'dim{target}']
    target_mu = -np.diff(target_lambda)

    # Compute Mu of pairs
    explanatory_variables = [i for i in range(3) if i != target]
    explanatory_mu = []

    for v in range(2):
        this_explanatory_variable = explanatory_variables[v]
        pair = sorted([this_explanatory_variable, target])
        pair_lambda_coef = model['lambda_coef'][f'dim{pair[0]}.dim{pair[1]}']
        if this_explanatory_variable < target:
            pair_mu = pair_lambda_coef[:, 0] - pair_lambda_coef[:, 1]
        else:
            pair_mu = pair_lambda_coef[0, :] - pair_lambda_coef[1, :]
        explanatory_mu.append(pair_mu)
    
    # Odds matrix
    # In R: target.mu + outer(explanatory.mu[[1]], explanatory.mu[[2]], "+")
    # target_mu is a scalar (single value from -diff)
    # explanatory_mu[0] and explanatory_mu[1] are 1D arrays
    odd_matrix = np.exp(target_mu + 
                       np.add.outer(explanatory_mu[0], explanatory_mu[1]))
    propensity_matrix = np.round(odd_matrix / (odd_matrix + 1), 4)
    epsilon = 1e-4
    propensity_matrix[propensity_matrix < epsilon] = epsilon
    propensity_matrix[propensity_matrix > 1 - epsilon] = 1 - epsilon
    
    # Frequency matrix: sum over the target dimension
    # In R: apply(original, explanatory.variables, sum)
    # This sums over all dimensions EXCEPT the explanatory variables
    freq_matrix = np.sum(original, axis=target)

    return {
        'original': original,
        'data': data,
        'which_x': which_x,
        'model': model,
        'target': target,
        'target_mu': target_mu,
        'explanatory_variables': explanatory_variables,
        'explanatory_mu': explanatory_mu,
        'odd_matrix': odd_matrix,
        'propensity_matrix': propensity_matrix,
        'freq_matrix': freq_matrix
    }

def target_report(computed):
    report1 = ''
    report1 += "# 3-Dimensional Model Result\n\n"
    report1 += ' ## Results for the selected model: \n\n'
    report1 += ' ### Observed Data Matrix: \n\n'
    original_df = array_to_dataframe(computed['original'])
    original_df.rename(columns={original_df.columns[-1]: 'count'}, inplace=True)

    report2 = '\n'
    report2 += '### Expected Data Matrix: \n\n'
    expected_df = array_to_dataframe(np.round(computed['model']['expected'], 2))
    expected_df.rename(columns={expected_df.columns[-1]: 'expected count'}, inplace=True)

    report3 = '\n'
    report3 += f"D.O.F = {computed['model']['d_o_f']}    L² = {np.round(computed['model']['pearson_chi2'], 2)}    p-value = {utils.signif(computed['model']['p_value'], 2)}\n"
    report3 += '\n'
    report3 += '### Information on selected model \n'
    report3 += f"**Lambda coefficients of the selected model: Intercept {coef_string(computed['which_x'])}**\n\n"
    lambda_coefs = computed['model']['lambda_coef']
    for key, value in lambda_coefs.items():
        if key == 'Intercept':
            report3 += f'Coefficient of "Intercept":\n1 : {value:.4f}\n\n'
        else:
            if 'dim0' in key:
                dim_name = 'X'
            elif 'dim1' in key:
                dim_name = 'Y'
            elif 'dim2' in key:
                dim_name = 'Z'
            
            if '.' in key:
                # Interaction term
                dim_names = key.split('.')
                dim_names = [dim_name if 'dim0' in name else ('Y' if 'dim1' in name else 'Z') for name in dim_names]
                dim_key = f'({",".join(dim_names)})'
                report3 += f'Coefficient of "{dim_key}":\n'
                for idx, val in np.ndenumerate(value):
                    indices = " ".join(str(i + 1) for i in idx)
                    report3 += f'{indices} : {val:.4f}\n'
            else:
                report3 += f'Coefficient of "{dim_name}":\n'
                for idx, val in enumerate(value):
                    report3 += f'{idx + 1} : {val:.4f}\n'
            report3 += '\n'
    
    # mu
    variable_names = ['X', 'Y', 'Z']
    report3 += f"Target variable: {variable_names[computed['target']]}\n\n"
    report3 += '### μ\'s for target variable: \n\n'
    report3 += f'**μ(Intercept):** {np.round(computed["target_mu"][0],3)} \n\n'
    
    # mus of pairs
    for v in range(2):
        this_explanatory_variable = computed['explanatory_variables'][v]
        pair = sorted([this_explanatory_variable, computed['target']])
        for i in range(len(computed['explanatory_mu'][v])):
            report3 += f"**μ({variable_names[pair[0]]}, {variable_names[pair[1]]}({variable_names[computed['explanatory_variables'][v]]}={i}))** = {np.round(computed['explanatory_mu'][v][i],3)} \n"
        report3 += '\n'
    
    report3 += '### Odds Matrix'
    # Index corresponds to first explanatory variable (rows), columns to second (cols)
    odds_df = pd.DataFrame(computed['odd_matrix'],
                           index=[f'{variable_names[computed["explanatory_variables"][0]]}={i+1}' for i in range(computed['odd_matrix'].shape[0])],
                           columns=[f'{variable_names[computed["explanatory_variables"][1]]}={i+1}' for i in range(computed['odd_matrix'].shape[1])])
    report4 = "### Propensity Matrix \n\n"
    propensity_df = pd.DataFrame(computed['propensity_matrix'],
                           index=[f'{variable_names[computed["explanatory_variables"][0]]}={i+1}' for i in range(computed['propensity_matrix'].shape[0])],
                           columns=[f'{variable_names[computed["explanatory_variables"][1]]}={i+1}' for i in range(computed['propensity_matrix'].shape[1])])
    
    report5 = '### Frequency Matrix \n\n'
    frequency_df = pd.DataFrame(computed['freq_matrix'],
                           index=[f'{variable_names[computed["explanatory_variables"][0]]}={i+1}' for i in range(computed['freq_matrix'].shape[0])],
                           columns=[f'{variable_names[computed["explanatory_variables"][1]]}={i+1}' for i in range(computed['freq_matrix'].shape[1])])
    
    report6 = '### Model Diagnostics\n\n'
    if computed['model']['model_is_fit']:
        report6 += 'Model fits the observed data\n\n'
    else:
        report6 += 'Model does not fit the observed data\n\n'
    
    report6 += '### Standardized residuals \n\n'
    residual_df = array_to_dataframe(np.round(computed['model']['std_residual'],3))
    residual_df.rename(columns={residual_df.columns[-1]: 'residual'}, inplace=True)

    return [report1,original_df,report2,expected_df,report3, odds_df, report4, propensity_df, report5, frequency_df, report6,residual_df]

def model_value_computation(data, explanatory_variables, propensity_matrix, threshold, tp_reward, tn_reward, fp_reward, fn_reward):
    # Determine the target variable based on the indices not used in explanatory_variables
    target = set(range(3)) - set(explanatory_variables)
    target = list(target)[0]  # Assuming there's only one target variable

    # Select the observed data based on the target
    if target == 0:
        obs_one = data[0, :, :]
        obs_two = data[1, :, :]
    elif target == 1:
        obs_one = data[:, 0, :]
        obs_two = data[:, 1, :]
    elif target == 2:
        obs_one = data[:, :, 0]
        obs_two = data[:, :, 1]

    # Compute predictions based on the threshold
    predict_is_one = propensity_matrix >= threshold
    
    # Calculate true positives, false positives, false negatives, true negatives
    num_tp = np.sum(obs_one[predict_is_one])
    num_fp = np.sum(obs_two[predict_is_one])
    num_fn = np.sum(obs_one[~predict_is_one])
    num_tn = np.sum(obs_two[~predict_is_one])
        
    
    # Calculate sensitivity and specificity
    sensitivity = num_tp / (num_tp + num_fn) if (num_tp + num_fn) != 0 else 0
    specificity = num_tn / (num_fp + num_tn) if (num_fp + num_tn) != 0 else 0
    
    # Compute the overall model value
    model_value = (tp_reward * num_tp) + (tn_reward * num_tn) + (fp_reward * num_fp) + (fn_reward * num_fn)
    
    return {
        'num_tp': num_tp,
        'num_fp': num_fp,
        'num_fn': num_fn,
        'num_tn': num_tn,
        'sensitivity': sensitivity,
        'specificity': specificity,
        'model_value': model_value
    }

def compute_and_plot_model_values(data, explanatory_variables, propensity_matrix, tp_reward, tn_reward, fp_reward, fn_reward):
    try:
        tp_reward = float(tp_reward)
        tn_reward = float(tn_reward)
        fp_reward = float(fp_reward)
        fn_reward = float(fn_reward)
    except:
        st.warning('All rewards must be numeric')
        return None
    # Compute unique sorted thresholds from the propensity matrix
    thresholds = np.sort(np.unique(propensity_matrix.flatten()))
    
    # Compute model values for each threshold
    model_values = []
    for threshold in thresholds:
        model_result = model_value_computation(
            data=data,
            explanatory_variables=explanatory_variables,
            propensity_matrix=propensity_matrix,
            threshold=threshold,
            tp_reward=tp_reward,
            tn_reward=tn_reward,
            fp_reward=fp_reward,
            fn_reward=fn_reward
        )
        model_values.append(model_result['model_value'])
        
    df = pd.DataFrame({
        'Threshold': thresholds,
        'Model Value': model_values
    })

    # Create interactive plot with Plotly
    fig = px.scatter(
        df,
        x='Threshold',
        y='Model Value',
        title='Model Values',
        labels={'Threshold': 'Propensity Threshold', 'Model Value': 'Model Value'},
        hover_data={'Threshold': True, 'Model Value': True},
        template='plotly_white'
    )
    fig.update_layout(
        title={'text': 'Model Values', 'y': 0.95, 'x': 0.5, 'xanchor': 'center', 'yanchor': 'top'},
        margin=dict(l=40, r=40, t=40, b=40)
    )
    
    # Add subtitle with maximum value
    max_value = np.max(model_values)
    max_index = np.argmax(model_values)
    optimal_threshold = thresholds[max_index]
    fig.add_annotation(
        x=optimal_threshold, y=max_value,
        text=f'Max value: {max_value:.2f} at threshold {optimal_threshold:.2f}',
        showarrow=True,
        arrowhead=1
    )
    
    return fig


def compute_and_plot_model_accuracies(data, explanatory_variables, propensity_matrix, tp_reward, tn_reward, fp_reward, fn_reward):
    try:
        tp_reward = float(tp_reward)
        tn_reward = float(tn_reward)
        fp_reward = float(fp_reward)
        fn_reward = float(fn_reward)
    except:
        return None
    
    thresholds = np.sort(np.unique(propensity_matrix.flatten()))
    accuracies = []
    for threshold in thresholds:
        model_result = model_value_computation(
            data=data,
            explanatory_variables=explanatory_variables,
            propensity_matrix=propensity_matrix,
            threshold=threshold,
            tp_reward=tp_reward,
            tn_reward=tn_reward,
            fp_reward=fp_reward,
            fn_reward=fn_reward
        )
        accuracy = (model_result['sensitivity'] + model_result['specificity']) / 2
        accuracies.append(accuracy)
    
    # Create DataFrame for plotting
    df = pd.DataFrame({
        'Threshold': thresholds,
        'Balanced Accuracy': accuracies
    })

    # Create interactive plot with Plotly
    fig = px.scatter(
        df,
        x='Threshold',
        y='Balanced Accuracy',
        title='Model Balanced Accuracies',
        labels={'Threshold': 'Propensity Threshold', 'Balanced Accuracy': 'Model Balanced Accuracy'},
        hover_data={'Threshold': True, 'Balanced Accuracy': True},
        template='plotly_white'
    )
    fig.update_layout(
        title={
            'text': 'Model Balanced Accuracies',
            'y':.95,
            'x':0.5,
            'xanchor': 'center',
            'yanchor': 'top'
        },
        margin=dict(l=40, r=40, t=40, b=40)
    )
    
    # Add subtitle with maximum accuracy
    max_accuracy = np.max(accuracies)
    max_index = np.argmax(accuracies)
    optimal_threshold = thresholds[max_index]
    fig.add_annotation(
        x=optimal_threshold, y=max_accuracy,
        text=f'Max accuracy: {max_accuracy:.4f} at threshold {optimal_threshold:.4f}',
        showarrow=True,
        arrowhead=1
    )
    
    return fig

def compute_and_display_results(data, explanatory_variables, propensity_matrix, tp_reward, tn_reward, fp_reward, fn_reward, threshold):
    try:
        tp_reward = float(tp_reward)
        tn_reward = float(tn_reward)
        fp_reward = float(fp_reward)
        fn_reward = float(fn_reward)
    except:
        st.warning('All rewards must be numeric')
        return None
    try:
        threshold = float(threshold)
    except:
        st.warning('Threshold must be numeric')
        return None
    if threshold < 0 or threshold > 1:
        st.warning('Threshold must be a number between 0 and 1')
    
    computed = model_value_computation(
        data=data,
        explanatory_variables=explanatory_variables,
        propensity_matrix=propensity_matrix,
        threshold=threshold,
        tp_reward=tp_reward,
        tn_reward=tn_reward,
        fp_reward=fp_reward,
        fn_reward=fn_reward
    )
    
    reward_matrix = pd.DataFrame({
        '': ['P = 1', 'P = 2'],
        'O = 1': [tp_reward, fn_reward],
        'O = 2': [fp_reward, tn_reward]
    })

    # Setting the index for proper display
    reward_matrix.set_index('', inplace=True)
    reward_matrix = reward_matrix.T

    c_matrix = pd.DataFrame({
        '': ['P = 1', 'P = 2'],
        'O = 1': [computed['num_tp'], computed['num_fn']],
        'O = 2': [computed['num_fp'], computed['num_tn']]
    })

    # Setting the index for the C-Matrix as well
    c_matrix.set_index('', inplace=True)
    c_matrix = c_matrix.T
    results = [
        "Model Value Result",
        reward_matrix,
        f"Propensity threshold = {threshold}",
        f"Sensitivity = {computed['sensitivity']:.4f}",
        f"Specificity = {computed['specificity']:.4f}",
        f"Accuracy = {(computed['num_tp']+computed['num_tn'])/(computed['num_tp']+computed['num_tn']+computed['num_fp']+computed['num_fn']):.4f}",
        f"Balanced Accuracy = {(computed['sensitivity']+computed['specificity'])/2:.4f}",
        "C-Matrix (O = observed, P = predicted)",
        c_matrix,
        f"Model value = {computed['model_value']:.2f}"
    ]
    
    return results
    
    



def model_value_tab(threedim_result):
    
    st.markdown("""
    <style>
    .wrapper {
        background-color: #f0f0f0; /* Light grey background */
        padding: 10px;
        border-radius: 10px;
        margin-bottom: 20px;
    }
    </style>
    """, unsafe_allow_html=True)

    # Model Value Plot Section
    with st.container():
        st.markdown('<div class="wrapper">', unsafe_allow_html=True)
        st.write("Model Value Plot")
        st.write("**Reward (O = observed, P = predicted)**")

        col1, col2, col3 = st.columns([1, 1, 1])
        with col1:
            st.write("")

        with col2:
            st.write("P = 1")

        with col3:
            st.write("P = 2")

        col1, col2, col3 = st.columns([1, 1, 1])
        with col1:
            st.write("O = 1")

        with col2:
            reward_o1_p1 = st.text_input("Reward O1P1", value="1", key=f'o1p1 {threedim_result["name"]}')

        with col3:
            reward_o1_p2 = st.text_input("Reward O1P2", value="-1", key=f'o1p2 {threedim_result["name"]}')

        col1, col2, col3 = st.columns([1, 1, 1])
        with col1:
            st.write("O = 2")

        with col2:
            reward_o2_p1 = st.text_input("Reward O2P1", value="-1", key=f'o2p1 {threedim_result["name"]}')

        with col3:
            reward_o2_p2 = st.text_input("Reward O2P2", value="1", key=f'o2p2 {threedim_result["name"]}')
            

        if st.button("Plot!", key=f"Plot! {threedim_result['name']}"):
            computed = threedim_result['computed']
            model_value_plot = compute_and_plot_model_values(
                data = computed['data'],
                explanatory_variables=computed['explanatory_variables'],
                propensity_matrix= computed['propensity_matrix'],
                tp_reward=reward_o1_p1,
                fn_reward=reward_o1_p2,
                fp_reward=reward_o2_p1,
                tn_reward=reward_o2_p2
            )
            if model_value_plot:
                st.plotly_chart(model_value_plot, use_container_width=True)
            else:
                pass
            
            model_accuracy_plot = compute_and_plot_model_accuracies(
                data = computed['data'],
                explanatory_variables=computed['explanatory_variables'],
                propensity_matrix= computed['propensity_matrix'],
                tp_reward=reward_o1_p1,
                fn_reward=reward_o1_p2,
                fp_reward=reward_o2_p1,
                tn_reward=reward_o2_p2
            )
            if model_accuracy_plot:
                st.plotly_chart(model_accuracy_plot, use_container_width=True)
            else:
                pass
        st.markdown('</div>', unsafe_allow_html=True)

        # Sensitivity and Specificity Section
        st.markdown('<div class="wrapper">', unsafe_allow_html=True)
        st.write("Sensitivity and Specificity")
        propensity_threshold = st.text_input("Propensity Threshold", key = f'propensity threshold {threedim_result["name"]}')
        if st.button("Compute!",key = f"Compute! {threedim_result['name']}"):
            computed = threedim_result['computed']
            results = compute_and_display_results(
                data = computed['data'],
                explanatory_variables=computed['explanatory_variables'],
                propensity_matrix= computed['propensity_matrix'],
                tp_reward=reward_o1_p1,
                fn_reward=reward_o1_p2,
                fp_reward=reward_o2_p1,
                tn_reward=reward_o2_p2,
                threshold=propensity_threshold)
            for result in results:
                if isinstance(result, pd.DataFrame):
                    st.dataframe(result)
                else:
                    st.text(result)
                    
        st.markdown('</div>', unsafe_allow_html=True)

import numpy as np
import pandas as pd
from scipy.stats import chi2

# These helper functions need to be defined (or imported) in your codebase.
# For example:
# from independence import l_sq_chi_sq, expected_count
# from loglin import three_dimensional_loglin_computation

import numpy as np
import pandas as pd
from scipy.stats import chi2

# It is assumed that the following helper functions are defined:
# - l_sq_chi_sq(observed, expected)
# - expected_count(observed)
# - three_dimensional_loglin_computation(observed, coeff)

def sort_explanatory_variables(data, target_col, explanatory_cols):
    """
    Sort explanatory variables by their importance in predicting the target variable.
    
    Parameters:
      data: DataFrame with the full dataset.
      target_col: The column index representing the target.
      explanatory_cols: A list of column indices or a nested list (we'll handle both).
      
    Returns:
      A list of explanatory column indices sorted by selection order.
    """
    import numpy as np
    import pandas as pd
    from scipy.stats import chi2
    
    # Handle nested list if provided (make it flat)
    if isinstance(explanatory_cols, list) and len(explanatory_cols) > 0 and isinstance(explanatory_cols[0], list):
        explanatory_cols = explanatory_cols[0]
    
    # Number of explanatory variables
    num_explanatory = len(explanatory_cols)
    
    # Trivial case - return the single explanatory column
    if num_explanatory == 1:
        return explanatory_cols
    
    # Initialize arrays to track selection
    selected_col = np.zeros(num_explanatory)
    log_p_values = np.zeros(num_explanatory)
    
    # INITIAL SELECTION: Compute chi-square and p-value for each explanatory variable
    for i in range(num_explanatory):
        # Create 2D contingency table between target and this explanatory variable
        obs = pd.crosstab(data.iloc[:, target_col], data.iloc[:, explanatory_cols[i]])

        # Compute likelihood ratio statistic
        l_sq_stat = independence.l_sq_chi_sq(obs, independence.expected_count(obs))

        d_o_f = np.prod(np.array(obs.shape) - 1)

        log_p_values[i] = chi2.logsf(l_sq_stat, df=d_o_f)

    
    # Select the variable with the smallest log p-value (most significant)
    init_index = np.argmin(log_p_values)
    selected_col[init_index] = 1
    
    # Start with the first selected variable
    encoded_var = data.iloc[:, explanatory_cols[init_index]].copy()
    # st.write('encoded var 1:')
    # st.write(encoded_var)
    
    # Coefficient structure for the three-dimensional log-linear model
    # Matches the R implementation's coeff <- list(1, 2, 3, c(1,2), c(1,3), c(2,3))
    coeff = [[0, 1], [0, 2], [1, 2]]
    
    # STEPWISE SELECTION: Iteratively add remaining variables
    for t in range(2, num_explanatory + 1):
        log_p_values = np.full(num_explanatory, -np.inf)
        
        for i in range(num_explanatory):
            if selected_col[i] > 0:
                continue  # Skip already selected variables
            
            # NEW CODE: Enforce complete category levels for a proper 3D contingency table
            candidate_series = data.iloc[:, explanatory_cols[i]]
            encoded_levels = np.arange(1, int(encoded_var.max()) + 1)
            target_levels = np.arange(1, int(data.iloc[:, target_col].max()) + 1)
            candidate_levels = np.arange(1, int(candidate_series.max()) + 1)
            
            combined_data = pd.DataFrame({
                'encoded': pd.Categorical(encoded_var, categories=encoded_levels),
                'target': pd.Categorical(data.iloc[:, target_col], categories=target_levels),
                'candidate': pd.Categorical(candidate_series, categories=candidate_levels)
            })
            
            contingency_table = pd.crosstab(
                index=[combined_data['encoded'], combined_data['target']],
                columns=combined_data['candidate'],
                dropna=False
            )
            
            encoded_unique = len(encoded_levels)
            target_unique = len(target_levels)
            candidate_unique = len(candidate_levels)
            
            full_index = pd.MultiIndex.from_product([encoded_levels, target_levels], names=['encoded', 'target'])
            contingency_table = contingency_table.reindex(index=full_index, columns=candidate_levels, fill_value=0)
            observed_3d = contingency_table.values.reshape(encoded_unique, target_unique, candidate_unique)
            
            log_lin_result = loglin_computation(observed_3d, coeff)
            log_p_values[i] = log_lin_result['log_p_value']
        
        # Select the variable with the highest log p-value (best fit)
        next_index = np.argmax(log_p_values)
        selected_col[next_index] = t
        
        # Update the composite encoded variable if more iterations remain
        if t < num_explanatory:
            selected_var = data.iloc[:, explanatory_cols[next_index]]
            # Match the R code exactly: encoded.var <- encoded.var * (num.new.category + 1) + selected.var
            num_new_category = selected_var.max()
            encoded_var = encoded_var * (num_new_category + 1) + selected_var
            # st.write('encoded var 2')
            # st.write(encoded_var)
    
    # Return explanatory columns in order of importance
    # This matches the R code: return(explanatory.col[order(selected.col)])
    return [explanatory_cols[i] for i in np.argsort(selected_col)][::-1]
