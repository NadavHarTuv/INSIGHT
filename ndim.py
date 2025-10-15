import numpy as np
import pandas as pd
import streamlit as st
import utils
import threedim
from scipy.stats import chi2
from itertools import combinations
import re
from itertools import product, combinations
import plotly.express as px

def read_input(ndim_data):
    predictors = None
    target = None
    constraints = None
    
    predictors_text = st.sidebar.text_input("Predictor variable(s)", help="select the predictor variables as groups joined by '-' or ',' and separated by ';")
    target_text = threedim.get_target_variable(ndim_data)
    constraits_text = st.sidebar.text_input("Client constraints")
    
    # Extract digits from target_text using regex
    digits = re.findall(r'\d+', target_text)
    if digits:
        target = int(digits[0]) - 1
    else:
        st.sidebar.error("Could not extract a valid target index from the column name.")
        target = None

    if predictors_text:
        predictors = utils.parse_text_groups(predictors_text)
        for subgroup in predictors:
            if target in subgroup:
                st.sidebar.warning('Target variable cannot be a predictor')
                predictors = None
    if constraits_text:
        constraints = utils.parse_text_groups(constraits_text)        
    return predictors, target, constraints

   
def contingency_array_multivariate(df):
    """
    Create a contingency array from a DataFrame.
    Optimized version using numpy for better performance.
    """
    # Get unique values for each column (sorted for consistency)
    unique_levels = [sorted(df[col].unique()) for col in df.columns]
    shape = [len(levels) for levels in unique_levels]
    
    # Initialize contingency array
    contingency_array = np.zeros(shape, dtype=np.float64)
    
    # Convert dataframe to numpy array for faster iteration
    data_array = df.values
    
    # Create mapping from values to indices for each column
    value_to_idx = []
    for col_idx, levels in enumerate(unique_levels):
        mapping = {val: idx for idx, val in enumerate(levels)}
        value_to_idx.append(mapping)
    
    # Fill contingency array (vectorized where possible)
    for row in data_array:
        try:
            indices = tuple(value_to_idx[i][int(row[i])] for i in range(len(row)))
            contingency_array[indices] += 1
        except (KeyError, IndexError):
            continue
    
    return contingency_array
    
    
def unpack_numbers(X):
    # Check if the input is a list of lists
    if all(isinstance(i, list) for i in X):
        # Flatten the list of lists
        flattened_list = [item for sublist in X for item in sublist]
    else:
        # If it's already a flat list, return it as is
        flattened_list = X

    return flattened_list


def two_way_computation(data, predictor_variable, target_variable):
    # Sort predictor and all variables
    predictor_variable = sorted(unpack_numbers(predictor_variable))
    all_variable = sorted(predictor_variable + [target_variable])
    
    df = data.copy()
    df.columns = np.arange(df.shape[1])
    
    # Reorder data
    df = df[all_variable]
    
    p = len(all_variable)
    
    contingency_array = contingency_array_multivariate(df)
   
    # Define the covariates to fit (two-way interactions only)
    # IPF preserves the highest-order margins; lower-order margins are implied
    covariate_to_fit = []
    for i in range(p):
        for j in range(i+1, p):
            covariate_to_fit.append([i, j])

    # Fit the log-linear model using loglin
    model = threedim.loglin_computation(contingency_array, covariate_to_fit)

    
    pearson_chi2 = model['pearson_chi2']
    d_o_f = model['d_o_f']
    p_value = chi2.sf(pearson_chi2, df=d_o_f)
    lambda_coef = model['lambda_coef']
    lambda_coef['Intercept'] -= np.log(np.sum(contingency_array))
    
     # Compute Mu
    target_idx = all_variable.index(target_variable)
    target_lambda = model['lambda_coef'][f'dim{target_idx}']
    target_mu = -np.diff(target_lambda)

    # Compute Mu of pairs
    explanatory_variables = [i for i in range(p) if i != target_idx]
    explanatory_mu = []

    for v in range(p - 1):
        this_explanatory_variable = explanatory_variables[v]
        pair = sorted([this_explanatory_variable, target_idx])
        pair_lambda_coef = model['lambda_coef'][f'dim{pair[0]}.dim{pair[1]}']
        if this_explanatory_variable < target_idx:
            pair_mu = pair_lambda_coef[:, 0] - pair_lambda_coef[:, 1]
        else:
            pair_mu = pair_lambda_coef[0, :] - pair_lambda_coef[1, :]
        explanatory_mu.append(pair_mu)
        
    return {
        'data': df,
        'all_variable': all_variable,
        'predictor_variable': predictor_variable,
        'target_variable': target_variable,
        'target_idx': target_idx,
        'p': p,
        'pearson_chi2': pearson_chi2,
        'd_o_f': d_o_f,
        'p_value': p_value,
        'lambda_coef': lambda_coef,
        'mu': explanatory_mu,
        'target_mu': target_mu
    }


import numpy as np
import pandas as pd
from scipy.stats import chi2
from itertools import combinations, product

def select_model_computation(data, predictor_variable, target_variable, which_x, p_value_threshold=0.05, contingency_array=None):
    df = data.copy()
    df.columns = np.arange(data.shape[1])
    predictor_variable = sorted(predictor_variable)
    all_variable = predictor_variable + [target_variable]
    target_idx = all_variable.index(target_variable)

    df = df[all_variable]

    p = df.shape[1]

    # Create contingency array (or use cached version)
    if contingency_array is None:
        contingency_array = contingency_array_multivariate(df)

    
    model = threedim.loglin_computation(contingency_array, which_x)
    pearson_chi2 = model['pearson_chi2']
    d_o_f = model['d_o_f']
    p_value = chi2.sf(pearson_chi2, df=d_o_f)
    model_is_fit = p_value > p_value_threshold
    lambda_coef = model['lambda_coef']
    lambda_coef['Intercept'] -= np.log(np.sum(contingency_array))

    lambda_coef_names = lambda_coef.keys()
    # print(f'lambda_coef: {lambda_coef}')

    # Compute the mu's
    mu = {}
    for key in lambda_coef_names:
        if key == f'dim{target_idx}':
            # For main effect, take the difference along the target dimension
            if lambda_coef[key] is not None and len(lambda_coef[key]) > 0:
                mu[key] = -np.diff(lambda_coef[key])
            else:
                # If lambda_coef is missing or empty, skip
                continue
        elif '.' in key:
            # For interaction terms
            indices = [int(x[-1]) for x in key.split('.')]
            if target_idx in indices and lambda_coef[key] is not None:
                dim_target = indices.index(target_idx)

                # Apply diff along the target dimension
                diff_result = -np.apply_along_axis(np.diff, dim_target, lambda_coef[key])

                # Calculate the new shape: same as the original but without the target dimension
                original_shape = lambda_coef[key].shape
                new_shape = list(original_shape)
                new_shape.pop(dim_target)

                # Flatten if one-dimensional
                if len(new_shape) == 1:
                    mu[key] = diff_result.flatten()
                else:
                    # Reshape to match the correct multi-dimensional shape
                    mu[key] = diff_result.reshape(new_shape)

                # Ensure each row/column sums to zero along all axes
                for axis in range(len(new_shape)):
                    axis_sums = mu[key].sum(axis=axis)
                    if not np.allclose(axis_sums, 0):
                        correction = axis_sums / new_shape[axis]
                        mu[key] -= np.expand_dims(correction, axis=axis)

    


    # Compute the propensities
    explanatory_mu = {k: v for k, v in mu.items() if k != f'dim{target_idx}'}

    # Check if target mu exists
    if f'dim{target_idx}' not in mu:
        # If target mu is missing, propensity cannot be computed
        sequences = [list(range(len(df.iloc[:,i].unique()))) for i in range(p-1)]
        cases = np.array(list(product(*sequences)))
        propensity_array = np.full((len(cases), 1), np.nan)
        odd_array = np.full((len(cases), 1), np.nan)
    else:
        sequences = [list(range(len(df.iloc[:,i].unique()))) for i in range(p-1)]
        cases = np.array(list(product(*sequences)))

        sum_mu_array = []
        for case in cases:
            try:
                odd = mu[f'dim{target_idx}'].copy()
                for tmp_mu_key in explanatory_mu:

                    # Extract indices from tmp_mu_key
                    indices = [int(x[-1]) for x in tmp_mu_key.split('.')]
                    indices = indices[:-1]  # Remove the last index (as in the R code)
                    
                    # Convert the case indices into a tuple of indices
                    idx_tuple = tuple(case[np.array(indices)])
                    
                    # Use the tuple to index into the multi-dimensional array
                    tmp_odd = explanatory_mu[tmp_mu_key][idx_tuple]
                    
                    # If you need to extract a single element in the case of 1D
                    if isinstance(tmp_odd, np.ndarray) and tmp_odd.size == 1:
                        tmp_odd = tmp_odd.item()  # Convert to scalar if necessary
                    
                    odd += tmp_odd
                sum_mu_array.append(odd[0] if isinstance(odd, np.ndarray) else odd)
            except (KeyError, IndexError, TypeError) as e:
                sum_mu_array.append(np.nan)
        
        sum_mu_array = np.array(sum_mu_array)
        odd_array = np.exp(sum_mu_array)
        propensity_array = odd_array / (odd_array + 1)

    odd_array = np.round(odd_array, 4)
    propensity_array = np.round(propensity_array, 4)
    odd_array = np.hstack((cases, odd_array.reshape(-1, 1)))
    propensity_array = np.hstack((cases, propensity_array.reshape(-1, 1)))
    
    odds_df = pd.DataFrame(odd_array)
    odds_df.columns = [f'X{v+1}' for v in predictor_variable]+['odd']
    odds_df.index += 1
    odds_df.iloc[:, :-1] = odds_df.iloc[:, :-1] + 1
    
    propensity_df = pd.DataFrame(propensity_array)
    propensity_df.columns = [f'X{v+1}' for v in predictor_variable]+['odd']
    propensity_df.index += 1
    propensity_df.iloc[:, :-1] = propensity_df.iloc[:, :-1] + 1
    


    return {
        'data': data,
        'all_variable': all_variable,
        'predictor_variable': predictor_variable,
        'target_variable': target_variable,
        'target_idx': target_idx,
        'p': p,
        'pearson_chi2': pearson_chi2,
        'd_o_f': d_o_f,
        'p_value': p_value,
        'model_is_fit': model_is_fit,
        'lambda_coef': lambda_coef,
        'mu': mu,
        'odd': odds_df,
        'propensity': propensity_df
    }
    
def select_model_report(model):
    """
    Returns a list of [string, DataFrame, string, DataFrame]:
      1) A Markdown string for the main text portion
      2) A DataFrame for the odds (with min/max annotated)
      3) A Markdown string for the heading above the propensity table
      4) A DataFrame for the propensity (with min/max annotated)
    """

    # -----------------------------------------------------
    # 1) Build the initial Markdown (r1)
    # -----------------------------------------------------
    lines = []
    lines.append("## N-Dimensional Model Result\n")

    # Predictor and target variables
    predictor_list = [f"X{var+1}" for var in model['predictor_variable']]
    if predictor_list:
        lines.append(f"**Predictor variables:** {', '.join(predictor_list)}")
    else:
        lines.append("**Predictor variables:** (none)")
    lines.append(f"**Target variable:** X{model['target_variable']+1}")
    lines.append("")

    # L², DOF, p-value
    lines.append(
        f"L-Square = {np.round(model['pearson_chi2'], 2)}  "
        f"D.O.F. = {model['d_o_f']}  "
        f"p-value = {model['p_value']:.4g}"
    )
    lines.append("")

    # Mu’s for target variable
    lines.append("### Mu’s for Target Variable")
    lines.append("---")

    target_dim = f"dim{model['target_idx']}"
    intercept = model['mu'][target_dim][0]
    lines.append(f"**Mu(Intercept)** = {intercept:.4f}\n")

    # Additional Mu terms
    explanatory_mu_keys = list(model['mu'].keys())
    mu = model['mu']
    # Exclude the target dimension from the keys
    explanatory_mu = {k: mu[k] for k in explanatory_mu_keys if k != target_dim}

    for tmp_mu in explanatory_mu.keys():
        # e.g. "dim2.dim3"
        # Extract dimension indices from the key
        tmp_mu_as_vector = [int(x) for x in re.findall(r"\d+", tmp_mu)]
        # Your code typically drops the last index (the target dimension) if needed
        tmp_mu_as_vector = tmp_mu_as_vector[:-1]

        # Build sequences for each dimension
        sequences = []
        for idx in tmp_mu_as_vector:
            col = model['all_variable'][idx]  # The actual column index in the data
            num_cats = len(np.unique(model['data'].iloc[:, col]))
            sequences.append(range(1, num_cats + 1))

        import itertools
        cases = list(itertools.product(*sequences))

        for case in cases:
            arr_index = tuple(c - 1 for c in case)
            val = explanatory_mu[tmp_mu][arr_index]
            # e.g. "Mu(X12, X9=1, X5=2) = 0.234"
            line = f"Mu(X{model['target_variable']+1}"
            for j, cval in enumerate(case):
                line += f", X{model['predictor_variable'][tmp_mu_as_vector[j]]+1}={cval}"
            line += f") = {val:.4f}"
            lines.append(line)
        lines.append("")

    # End of the first text portion
    r1 = "\n".join(lines)

    # -----------------------------------------------------
    # 2) Annotate the "odd" DataFrame with min/max
    # -----------------------------------------------------
    df_odd = model["odd"].copy()
    df_odd["Indicator"] = ""

    # We assume the last numeric column is "odd"
    # If the user named it "odd", let's find it:
    odd_col_name = "odd"
    if odd_col_name not in df_odd.columns:
        # Fallback if "odd" not found
        odd_col_name = df_odd.columns[-2]  # second-last column

    min_idx = df_odd[odd_col_name].idxmin()
    max_idx = df_odd[odd_col_name].idxmax()
    df_odd.loc[min_idx, "Indicator"] = "<- Minimum"
    df_odd.loc[max_idx, "Indicator"] = "<- Maximum"

    # -----------------------------------------------------
    # 3) A small heading for the "propensity" portion (r2)
    # -----------------------------------------------------
    r2 = "### Propensity Values\nBelow is the table of propensity with min/max indicated.\n"

    # -----------------------------------------------------
    # 4) Annotate the "propensity" DataFrame with min/max
    # -----------------------------------------------------
    df_prop = model["propensity"].copy()
    df_prop["Indicator"] = ""

    # Similarly assume the last numeric column is the "odd" or "propensity"
    prop_col_name = "odd"
    # If your code uses "propensity" as a column name, do this:
    if "propensity" in df_prop.columns:
        prop_col_name = "propensity"
    elif prop_col_name not in df_prop.columns:
        prop_col_name = df_prop.columns[-2]

    min_idx_prop = df_prop[prop_col_name].idxmin()
    max_idx_prop = df_prop[prop_col_name].idxmax()
    df_prop.loc[min_idx_prop, "Indicator"] = "<- Minimum"
    df_prop.loc[max_idx_prop, "Indicator"] = "<- Maximum"

    # Return the final list: [text portion, odd DataFrame, text portion, propensity DataFrame]
    return [r1, df_odd, r2, df_prop]



def coefficients_names(data_col_names, coefficients):
    pairs = [coef for coef in coefficients if '.' in coef]

    # Intercept and single terms
    single_terms = [data_col_names[int(re.findall(r'\d+', coef)[0])] for coef in coefficients if '.' not in coef and coef != 'Intercept']
    
    # Pair terms
    pair_terms = []
    for pair in pairs:
        indices = list(map(int, re.findall(r'\d+', pair)))
        pair_terms.append(f"({', '.join(data_col_names[idx] for idx in indices)})")

    # Combine all terms
    terms = ['Intercept'] + single_terms + pair_terms

    return ' '.join(terms)


def target_variable_computation(data, predictor_variables, target_variable, client_constraints, level=2):
    # Flatten if nested
    if isinstance(predictor_variables, list) and len(predictor_variables) > 0 and isinstance(predictor_variables[0], list):
        predictor_variables = predictor_variables[0]
    predictor_variables = sorted(predictor_variables)
    all_variables = predictor_variables + [target_variable]
    target_idx = all_variables.index(target_variable)
    original_data = data.copy()
    sorted_by_predictiveness = threedim.sort_explanatory_variables(original_data, target_variable, predictor_variables)

    # Handle case where client_constraints is None
    if client_constraints is None:
        client_constraints = []
    else:
        client_constraints = list(client_constraints)

    if len(client_constraints) > 0:
        model_predictor_variables = client_constraints[0]
    else:
        model_predictor_variables = sorted_by_predictiveness[:2]
    
    remaining_variables = list(set(sorted_by_predictiveness) - set(model_predictor_variables))

    model1 = two_way_computation(data=original_data, predictor_variable=model_predictor_variables, target_variable=target_variable)
    p0 = model1['p_value']

    if len(remaining_variables) > 0:
        for var in remaining_variables:
            temp_model_predictor_variables = model_predictor_variables + [var]
            model = two_way_computation(data=original_data, predictor_variable=temp_model_predictor_variables, target_variable=target_variable)
            if 0.05 < model['p_value'] <= p0:
                model1 = model
                model_predictor_variables = temp_model_predictor_variables
                p0 = model1['p_value']
            else:
                break

    p = len(model_predictor_variables) + 1

    # Pre-compute contingency array once for efficiency
    df_for_contingency = original_data.copy()
    df_for_contingency.columns = np.arange(original_data.shape[1])
    all_vars_for_model = sorted(model_predictor_variables + [target_variable])
    df_selected = df_for_contingency[all_vars_for_model]
    cached_contingency_array = contingency_array_multivariate(df_selected)

    model_list = []
    levels = []
    
    

    for k in range(p-1, 1, -1):
        # Generate all k-combinations of p elements (matching R's combn)
        # R: combinations = combn(seq(p), k)
        # This gives us all k-way interaction terms directly
        covariates_to_fit = [list(comb) for comb in combinations(range(p), k)]
        
        model = select_model_computation(data=original_data, predictor_variable=model_predictor_variables, target_variable=target_variable, which_x=covariates_to_fit, contingency_array=cached_contingency_array)
        if model['p_value'] > 0.05 and np.all(~np.isnan(model['propensity'])):
            model_list.append(model)
            levels.append(k)
            
#         for comb in combs:
#         # Generate all subterms (including the combination itself)
#             for r in range(1, len(comb) + 1):
#                 subterms = [list(subterm) for subterm in combinations(comb, r)]
#                 covariates_to_fit.extend(subterms)

# # Filter out any covariates that include all predictors (if needed)
#                 covariates_to_fit = [cov for cov in covariates_to_fit if len(cov) < p]

#                 model = select_model_computation(data=original_data, predictor_variable=model_predictor_variables, target_variable=target_variable, which_x=covariates_to_fit)
#                 if model['p_value'] > 0.05 and np.all(~np.isnan(model['propensity'])):
#                     model_list.append(model)
#                     levels.append(k)

    indep_covariates_to_fit = [[i, p-1] for i in range(p-1)]
    indep_covariates_to_fit.extend([[i] for i in range(p)])


    indep_model = select_model_computation(data=original_data, predictor_variable=model_predictor_variables, target_variable=target_variable, which_x=indep_covariates_to_fit, contingency_array=cached_contingency_array)

    if indep_model['p_value'] > 0.05:
        model_list.append(indep_model)

    # data = data.loc[:, model1['all_variable']]
    coef_text = []
    

    if len(model_list) >= 1:
        for model in model_list:
            coef_text.append(coefficients_names(data_col_names=[f"X{var+1}" for var in model['all_variable']], coefficients=model['lambda_coef'].keys()))
        return {
            'original_data': original_data,
            'model': model_list,
            'coef_text': coef_text,
            'sorted_by_predictiveness': sorted_by_predictiveness,
            'levels': levels
        }
    else:
        return None





def target_variable_report(computed):
    if computed is None:
        return ['No model fits the data under the given constraints']
    # st.write(f'model: {computed["model"][0]["pearson_chi2"]}')
    else:
        
        def space(n):
            return ' ' * n
        
        def print_model(s, i):
            # propensity = propensity_computation(computed['model'][i])
            propensity = computed['model'][i]['propensity']
            propensity = np.array(propensity)
            max_propensity = propensity[propensity[:, -1].argmax()]
            min_propensity = propensity[propensity[:, -1].argmin()]

            s += f"Lambda coefficients: {computed['coef_text'][i]}\n"
            s += f"{space(indent)}D.O.F. = {str(computed['model'][i]['d_o_f']).ljust(dof_space)}"
            s += f" L² = {str(np.round(computed['model'][i]['pearson_chi2'], 2)).ljust(l_sq_space)}"
            s += f"p-value = {round(computed['model'][i]['p_value'], 4)}\n"

            if computed['model'][i]['p_value'] > 0.05:
                s += f"{space(indent)}Model fits the observed data.\n"
            else:
                s += f"{space(indent)}Model does not fit the observed data.\n"

            s += "\nProfile of highest propensity: "
            propensity = pd.DataFrame(propensity)
            propensity.columns = [f'X{j+1}' for j in computed["model"][i]["predictor_variable"]]+['propensity']
            # st.write(f'propensity: {propensity}')
            for j in range(propensity.shape[1] - 1):
                s += f"({propensity.columns[j]}={int(max_propensity[j])}) "
            s += f"Propensity: {max_propensity[-1]}\n"

            s += "Profile of lowest propensity: "
            for j in range(propensity.shape[1] - 1):
                s += f"({propensity.columns[j]}={int(min_propensity[j])}) "
            s += f"Propensity: {min_propensity[-1]}\n"

            return s
        
        s = "## n-Dimensional - Model Selection Result\n"
        # s += "==========================\n"
        if computed is None:
            s += "No model fits the data under these constraints"
            return s

        s += "Explanatory variables sorted by predictiveness: "
        for var in computed['sorted_by_predictiveness']:
            s += f"{var+1} "

        omitted_variables = set(computed['sorted_by_predictiveness']) - set(computed['model'][0]['predictor_variable'])
        s += "\n"
        if not omitted_variables:
            s += "No variables omitted"
        else:
            s += "Omitted variables: "
            for var in omitted_variables:
                s += f" {var+1} "
        s += "\n"

        s += "## Models that fit the data by level of complexity:\n"
        # s += "-----------------------\n"
        dof_space = 6
        l_sq_space = 10
        indent = 4
        tiers = [3, 2, 2, 2, 1, 1, 1, 0]
        n = len(computed['coef_text'])
        
        for i, level in enumerate(computed['levels'], 1):
            s += f"**({i}) {level}-way interaction model:**\n"
            s = print_model(s, i - 1)
            s += "\n \n \n "

        if len(computed['model']) > len(computed['levels']):
            s += f"**({len(computed['levels']) + 1}) No interaction model:**\n"
            s = print_model(s, len(computed['levels']))
            s += "\n \n \n \n"

        for i in range(len(computed['model']) - 1, 0, -1):
            l_sq_diff = computed['model'][i]['pearson_chi2'] - computed['model'][0]['pearson_chi2']
            d_o_f_diff = computed['model'][i]['d_o_f'] - computed['model'][0]['d_o_f']
            if l_sq_diff <= chi2.ppf(0.95, d_o_f_diff):
                s += f"L squared difference between model ({i+1}) and model (1) is {round(l_sq_diff, 2)} with {round(d_o_f_diff, 2)} degrees of freedom \n"
                s += f"Model ({i+1}) is the the most parsimonious model that does not entail a loss of information \n"
                break
            else:
                s += f"L squared difference between model ({i+1}) and model (1) is {round(l_sq_diff, 2)} with {round(d_o_f_diff, 2)} degrees of freedom \n"
                s += f"Model ({i+1}) entails a loss of information \n"

        return [s]


_detailed_tab_counter = 0

def detailed_models_tab(ndim_results):
    global _detailed_tab_counter
    _detailed_tab_counter += 1
    
    st.write("Detailed Models: \n")
    # st.write(ndim_results)
    computed = ndim_results['computed']
    if computed is None:
        st.warning('No model fits the data')
    # st.write(f'computed: \n {computed}')
    else:
        number_of_models = len(computed['model'])
        # Use counter to create truly unique keys
        key_base = f'detailed_model_{_detailed_tab_counter}'
        
        model_choice = st.selectbox('Select model to expand',
                                    options=np.arange(1,number_of_models+1),
                                    key = f'{key_base}_selectbox')
        selected_model = computed['model'][model_choice-1]
        if st.button('Show!', key=f'{key_base}_show_button'):
            report = select_model_report(selected_model)
            for r in report:
                st.write(r)



from sklearn.metrics import confusion_matrix

def model_value_computation(data, propensity_matrix, threshold, tp_reward, tn_reward, fp_reward, fn_reward):
    tmp_data = data.copy()
    target_col = tmp_data.columns[-1]
    propensity_df = propensity_matrix.copy()
    # Convert predictor categories to integers
    propensity_df.iloc[:, :-1] = propensity_df.iloc[:, :-1].astype(int)
    
    # Apply the threshold to determine predictions
    propensity_df['prediction'] = propensity_df.iloc[:, -1] >= threshold
    # Create a prediction dictionary
    prediction_dict = {}
    for i, row in propensity_df.iterrows():
        predictor_values = tuple(row[:-2])  # Exclude the last two columns (propensity score and prediction)
        prediction_dict[predictor_values] = row['prediction']
    # Map predictions to data

    tmp_data['prediction'] = tmp_data.iloc[:, :-1].apply(lambda row: prediction_dict[tuple(row)], axis=1)
    
    
    # Compute confusion matrix components
    y_true = tmp_data[target_col] == 1
    y_pred = tmp_data['prediction']
    
    tn, fp, fn, tp = confusion_matrix(y_true, y_pred).ravel()
    
    # Calculate metrics
    sensitivity = tp / (tp + fn) if (tp + fn) != 0 else 0
    specificity = tn / (tn + fp) if (tn + fp) != 0 else 0
    model_value = tp_reward * tp + tn_reward * tn + fp_reward * fp + fn_reward * fn
    
    return {
        'num_tp': tp,
        'num_fp': fp,
        'num_fn': fn,
        'num_tn': tn,
        'sensitivity': sensitivity,
        'specificity': specificity,
        'model_value': model_value
    }
 
def compute_and_plot_model_values(data, propensity_matrix, tp_reward, tn_reward, fp_reward, fn_reward):
    try:
        tp_reward = float(tp_reward)
        tn_reward = float(tn_reward)
        fp_reward = float(fp_reward)
        fn_reward = float(fn_reward)
    except:
        st.warning('All rewards must be numeric')
        return None
    # Compute unique sorted thresholds from the propensity matrix
    thresholds = np.sort(np.unique(propensity_matrix.iloc[:,-1]))

    # Compute model values for each threshold
    model_values = []
    for threshold in thresholds:
        model_result = model_value_computation(
            data=data,
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

def compute_and_plot_model_accuracies(data,  propensity_matrix, tp_reward, tn_reward, fp_reward, fn_reward):
    try:
        tp_reward = float(tp_reward)
        tn_reward = float(tn_reward)
        fp_reward = float(fp_reward)
        fn_reward = float(fn_reward)
    except:
        return None
    
    thresholds = np.sort(np.unique(propensity_matrix.iloc[:,-1]))
    accuracies = []
    for threshold in thresholds:
        model_result = model_value_computation(
            data=data,
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


def compute_and_display_results(data, propensity_matrix, tp_reward, tn_reward, fp_reward, fn_reward, threshold):
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


_model_value_tab_counter = 0

def model_value_tab(ndim_results):
    global _model_value_tab_counter
    _model_value_tab_counter += 1
    
    computed = ndim_results['computed']
    if computed is None:
        st.warning('No model fits the data')
    else:
        number_of_models = len(computed['model'])
        model_choice = st.selectbox('Select model',
                                    options=np.arange(1, number_of_models+1),
                                    key=f'model_value_choice_{_model_value_tab_counter}')
        selected_model = computed['model'][model_choice-1]
        
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
                reward_o1_p1 = st.text_input("Reward O1P1", value="1", key=f'o1p1 {ndim_results["name"]}')

            with col3:
                reward_o1_p2 = st.text_input("Reward O1P2", value="-1", key=f'o1p2 {ndim_results["name"]}')

            col1, col2, col3 = st.columns([1, 1, 1])
            with col1:
                st.write("O = 2")

            with col2:
                reward_o2_p1 = st.text_input("Reward O2P1", value="-1", key=f'o2p1 {ndim_results["name"]}')

            with col3:
                reward_o2_p2 = st.text_input("Reward O2P2", value="1", key=f'o2p2 {ndim_results["name"]}')
                

            if st.button("Plot!", key=f"Plot! {ndim_results['name']}"):
                model_value_plot = compute_and_plot_model_values(
                    data = selected_model['data'].iloc[:,selected_model['all_variable']],
                    propensity_matrix= selected_model['propensity'],
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
                    data = selected_model['data'].iloc[:,selected_model['all_variable']],
                    propensity_matrix= selected_model['propensity'],
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
            propensity_threshold = st.text_input("Propensity Threshold", key = f'propensity threshold {ndim_results["name"]}')
            if st.button("Compute!",key = f"Compute! {ndim_results['name']}"):
                results = compute_and_display_results(
                    data = selected_model['data'].iloc[:,selected_model['all_variable']],
                    propensity_matrix= selected_model['propensity'],
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