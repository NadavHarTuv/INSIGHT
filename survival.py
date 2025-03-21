import streamlit as st
import utils
import independence
import numpy as np
from scipy.stats import chi2
import pandas as pd
from scipy.optimize import minimize
from matplotlib import pyplot as plt

def is_consecutive(lst):
    return len(lst) == (max(lst) - min(lst) + 1)


def parse_splining_string(s, num_stage):
    group = utils.parse_text_groups(s)
    if len(group) == 1 and group[0] is None:
        return None
    if len(group) == 0:
        return [[1, num_stage]]
    if not all(is_consecutive(g) for g in group):
        return None
    if max(max(g) for g in group) > num_stage or sum(len(g) for g in group) < num_stage:
        return None
    membership = utils.group_label(num_stage, group)
    num_group = membership[-1]
    splining_interval = []
    for i in range(1, num_group + 1):
        group_idx = [idx + 1 for idx, m in enumerate(membership) if m == i]
        splining_interval.append([group_idx[0], group_idx[-1]])
    return splining_interval


def homogeneous_computation(death_per_period, still_survive, envelope_factor=1.96, p_value_threshold=0.05):

    death_per_period = np.array(death_per_period).flatten()
    total_num_people = np.sum(death_per_period) + still_survive
    total_num_people = np.array(total_num_people)
    num_period = len(death_per_period)


    num_survivor = np.cumsum(np.concatenate((still_survive, death_per_period[::-1]), axis=0))[::-1]
    num_survivor = np.array(num_survivor)
    cond_frac_of_death_per_period = death_per_period / num_survivor[:-1]
    
    # Compute phi
    phi = np.sum(death_per_period) / np.sum(num_survivor[:-1])
    phi_easd = np.sqrt(phi**2 * (1 - phi) / (1 - (1 - phi)**num_period) / total_num_people)
    
    # Compute the expected numbers of deaths per period
    cond_prob_to_die = np.repeat(phi, num_period)
    expected_death_per_period = np.cumprod(np.concatenate(([1], 1 - cond_prob_to_die[:-1]))) * np.outer(cond_prob_to_die, total_num_people).flatten()
    expected_still_survive = np.outer(np.prod(1 - cond_prob_to_die), total_num_people).flatten()
    
    # Compute observed and expected (unconditional) survival probabilities
    uncond_survival_freq = num_survivor / total_num_people
    temp_p = death_per_period / total_num_people
    uncond_survival_freq_envelope = np.concatenate(([0], envelope_factor * np.sqrt(temp_p * (1 - temp_p) / num_survivor[1:])))
    
    expected_uncond_survival_prob = np.cumprod(np.concatenate(([1], 1 - cond_prob_to_die)))
    temp_n = total_num_people - np.cumsum(np.concatenate(([0], expected_death_per_period)))
    temp_p = expected_death_per_period / temp_n[:-1]
    expected_uncond_survival_prob_envelope = np.concatenate(([0], envelope_factor * np.sqrt(temp_p * (1 - temp_p) / temp_n[:-1])))
    
    observed = np.concatenate((death_per_period, still_survive))
    expected = np.concatenate((expected_death_per_period, expected_still_survive))
    
    observed = pd.DataFrame(observed)
    expected = pd.DataFrame(expected)
    
    chi_sq_stat = independence.pearson_chi_squared(observed, expected)
    l_sq_stat = independence.l_sq_chi_sq(observed, expected)
    d_o_f = num_period - 1
    p_value = chi2.sf(l_sq_stat, df=d_o_f)
    model_is_fit = p_value > p_value_threshold
    std_residuals, num_sig_residuals = independence.standardized_residuals(observed, expected)
    
    return {
        'observed': observed,
        'expected': expected,
        'cond_frac_of_death_per_period': cond_frac_of_death_per_period,
        'phi': phi,
        'phi_easd': phi_easd,
        'expected_death_per_period': expected_death_per_period,
        'expected_still_survive': expected_still_survive,
        'chi_sq_stat': chi_sq_stat,
        'l_sq_stat': l_sq_stat,
        'd_o_f': d_o_f,
        'p_value': p_value,
        'model_is_fit': model_is_fit,
        'std_residuals': std_residuals,
        'num_sig_residuals': num_sig_residuals,
        'uncond_survival_freq': uncond_survival_freq,
        'uncond_survival_freq_envelope': uncond_survival_freq_envelope,
        'expected_uncond_survival_prob': expected_uncond_survival_prob,
        'expected_uncond_survival_prob_envelope': expected_uncond_survival_prob_envelope
    }
    
    
def homogeneous_report(result, first_stage_idx, test_portion=None):
    num_stage = len(result['observed'])

    # Initial text
    s1 = "Homogeneous Model Result\n"
    s1 += "========================\n\n"
    if test_portion is not None:
        s1 += f"Test portion= {round(test_portion, 2)}%\n\n"
    

    # DataFrame creation
    stages = list(range(first_stage_idx, first_stage_idx + num_stage - 1)) + ["Survive", "Total"]
    observed = result['observed'].iloc[:,-1].tolist()
    observed_sum = np.sum(observed)
    observed.append(observed_sum)
    
    n_k_over_N_k = list(result['cond_frac_of_death_per_period']) + [None, None]
    
    expected_death_per_period_flat = [float(x) if isinstance(x, (int, float)) else float(np.sum(x)) for x in result['expected_death_per_period']]
    expected = expected_death_per_period_flat + [float(result['expected_still_survive']), None]  # Replacing sum with None
    expected[-1] = observed[-1]
    expected = [np.round(x,2) if x is not None else None for x in expected ]


    std_residuals = result['std_residuals'].iloc[:, -1].tolist()
    residuals = std_residuals + [None]
    residuals = [np.round(x,2) if x is not None else None for x in residuals ]


    df = pd.DataFrame({
        "Stage": stages,
        "Observed": observed,
        "n(k)/N(k)": n_k_over_N_k,
        "Expected": expected,
        "Residual": residuals
    })
    df.set_index('Stage', inplace=True)
    
    # Text for Parameter Estimates
    s2 = '\n'
    s2 += "\nParameter Estimate\n"
    s2 += "===================\n\n"
    s2 += '\n' 
    s2 += f"Phi= {np.round(result['phi'], 4)}\n"
    s2 += f"Phi EASD= {np.round(result['phi_easd'][0], 4)}\n"
    s2 += f"X-Square= {np.round(result['chi_sq_stat'], 4)}\n"
    s2 += f"L-Square= {np.round(result['l_sq_stat'], 4)}\n"
    s2 += f"D.O.F.= {np.round(result['d_o_f'], 4)}\n"
    s2 += f"p-value= {utils.signif(result['p_value'], 4)}\n\n"

    # Text for Model Diagnostics
    s2 += "Model Diagnostics\n"
    s2 += "=================\n"
    if result['model_is_fit']:
        s2 += "Model fits the observed data.\n"
    else:
        s2 += "Model does not fit the observed data.\n"
    s2 += f"\nNumber of significant residuals = {result['num_sig_residuals']}\n"

    return [s1, df, s2]



def acc_dc_computation(death_per_period, still_survive, envelope_factor=1.96, p_value_threshold=0.05):
    total_num_people = np.sum(death_per_period) + still_survive
    total_num_people = np.array(total_num_people)

    num_period = len(death_per_period)
    death_per_period = np.array(death_per_period).flatten()
    num_survivor = np.cumsum(np.concatenate((still_survive, death_per_period[::-1]), axis=0))[::-1]
    num_survivor = np.array(num_survivor)    
    
    cond_frac_of_death_per_period = death_per_period / num_survivor[:-1]

    # Negative Log-Likelihood Function
    def neg_log_like(x, death_per_period, num_survivor):
        delta, phi = x
        probs = delta * np.exp(np.arange(1, num_period + 1) * phi)
        temp_s = num_survivor[1:]
        neg_log_like_x = -np.sum(temp_s * np.log(1 - probs)) - np.sum(death_per_period * np.log(probs))
        
        total_death = num_survivor[0] - num_survivor[-1]
        g1 = -np.sum(temp_s * np.exp(np.arange(1, num_period + 1) * phi) / (1 - probs)) + total_death / delta
        g2 = np.sum(np.arange(1, num_period + 1) * (death_per_period - probs / (1 - probs) * temp_s))
        
        h11 = -np.sum(temp_s * np.exp(2 * phi * np.arange(1, num_period + 1)) / (1 - probs)**2) - total_death / delta**2
        h12 = -np.sum(temp_s * np.exp(np.arange(1, num_period + 1) * phi) * np.arange(1, num_period + 1) / (1 - probs)**2)
        h22 = -np.sum((np.arange(1, num_period + 1)**2) * temp_s * probs / (1 - probs)**2)
        
        grad = -np.array([g1, g2])
        hessian = -np.array([[h11, h12], [h12, h22]])
        
        return neg_log_like_x, grad, hessian

    # Initialize phi and delta
    phi = np.sum(np.log(death_per_period[1:] / death_per_period[:-1] * num_survivor[:-2] / num_survivor[1:-1])) / (num_period - 1)
    delta = (np.prod(death_per_period / num_survivor[:num_period])**(1 / num_period)) * np.exp(-phi * (1 + num_period) / 2)

    # Optimization
    def func_to_optimize(x):
        f, grad, hess = neg_log_like(x, death_per_period, num_survivor)
        return f

    def grad_to_optimize(x):
        f, grad, hess = neg_log_like(x, death_per_period, num_survivor)
        return grad

    def hess_to_optimize(x):
        f, grad, hess = neg_log_like(x, death_per_period, num_survivor)
        return hess

    result = minimize(func_to_optimize, x0=[delta, phi], jac=grad_to_optimize, hess=hess_to_optimize, method='trust-ncg')
    delta, phi = result.x

    # Compute the EASD
    hessian = result.hess
    det_hessian = np.linalg.det(hessian)
    if det_hessian == 0:
        return {"computation_has_singularity": True}
    
    delta_easd = np.sqrt(abs(hessian[1, 1] / det_hessian) / total_num_people)
    phi_easd = np.sqrt(abs(hessian[0, 0] / det_hessian) / total_num_people)

    # Compute the expected numbers of deaths per period
    cond_prob_to_die = delta * np.exp(np.arange(1, num_period + 1) * phi)
    if any(cond_prob_to_die <= 0) or any(cond_prob_to_die >= 1):
        return {"computation_has_singularity": True}

    expected_death_per_period = np.cumprod(np.concatenate(([1], 1 - cond_prob_to_die[:-1]))) * np.outer(cond_prob_to_die , total_num_people).flatten()
    expected_still_survive = np.prod(1 - cond_prob_to_die) * total_num_people

    # Compute observed and expected (unconditional) survival probabilities
    uncond_survival_freq = num_survivor / total_num_people
    temp_p = death_per_period / total_num_people
    uncond_survival_freq_envelope = np.concatenate(([0], envelope_factor * np.sqrt(temp_p * (1 - temp_p) / num_survivor[1:])))

    expected_uncond_survival_prob = np.cumprod(np.concatenate(([1], 1 - cond_prob_to_die)))

    temp_n = total_num_people - np.cumsum(np.concatenate(([0], expected_death_per_period)))
    temp_p = expected_death_per_period / temp_n[:-1]
    expected_uncond_survival_prob_envelope = np.concatenate(([0], envelope_factor * np.sqrt(temp_p * (1 - temp_p) / temp_n[1:])))

    observed = np.concatenate((death_per_period, still_survive))
    expected = np.concatenate((expected_death_per_period, expected_still_survive))
    
    chi_sq_stat = np.sum((observed - expected)**2 / expected)
    l_sq_stat = 2 * np.sum(observed * np.log(observed / expected))
    d_of = num_period - 2
    p_value = chi2.sf(l_sq_stat, df=d_of)
    model_is_fit = p_value > p_value_threshold
    std_residuals = (observed - expected) / np.sqrt(expected)
    num_sig_residuals = np.sum(np.abs(std_residuals) > 1.96)

    return {
        "computation_has_singularity": False,
        "observed": observed,
        "expected": expected,
        "cond_frac_of_death_per_period": cond_frac_of_death_per_period,
        "delta": delta,
        "phi": phi,
        "delta_easd": delta_easd,
        "phi_easd": phi_easd,
        "expected_death_per_period": expected_death_per_period,
        "expected_still_survive": expected_still_survive,
        "chi_sq_stat": chi_sq_stat,
        "l_sq_stat": l_sq_stat,
        "d_o_f": d_of,
        "p_value": p_value,
        "model_is_fit": model_is_fit,
        "std_residuals": std_residuals,
        "num_sig_residuals": num_sig_residuals,
        "uncond_survival_freq": uncond_survival_freq,
        "uncond_survival_freq_envelope": uncond_survival_freq_envelope,
        "expected_uncond_survival_prob": expected_uncond_survival_prob,
        "expected_uncond_survival_prob_envelope": expected_uncond_survival_prob_envelope
    }
    

def acc_dc_report(result, first_stage_idx):
    output = []
    
        # Initial text
    s = "ACC/DC Model Result\n"
    s += "========================\n\n"
    output.append(s)

    
    if result.get('computation_has_singularity', False):
        return "The ACC/DC computation reached a singularity and stopped."

    num_stage = len(result['observed'])

    # Initialize the DataFrame for the stage-wise data
    stages = list(range(first_stage_idx, first_stage_idx + num_stage - 1)) + ["Survive", "Total"]
    observed = list(result['observed'])
    observed.append(sum(observed))
    n_k_over_N_k = list(result['cond_frac_of_death_per_period']) + [None, None]
    n_k_over_N_k = [np.round(x,2) if x is not None else None for x in n_k_over_N_k ]    

    
    expected_death_per_period_flat = [float(x) if isinstance(x, (int, float)) else float(np.sum(x)) for x in result['expected_death_per_period']]
    expected = expected_death_per_period_flat + [float(result['expected_still_survive']), observed[-1]]  # Replacing sum with None
    expected[-1] = observed[-1]
    expected = [np.round(x,2) if x is not None else None for x in expected ]    
    
    std_residuals = list(result['std_residuals']) + [None]
    std_residuals = [np.round(x,2) if x is not None else None for x in std_residuals ]  

    df = pd.DataFrame({
        "Stage": stages,
        "Observed": observed,
        "n(k)/N(k)": n_k_over_N_k,
        "Expected": expected,
        "Residual": std_residuals
    })
    df.set_index('Stage', inplace=True)
    
    output.append(df)

    # Adding textual information
    s = "\nParameter Estimate\n========================\n\n"
    s += f"Delta= {result['delta']:.4g}\n"
    s += f"Delta EASD= {utils.signif(result['delta_easd'][0],4)}\n"
    s += f"Phi= {result['phi']:.4g}\n"
    s += f"Phi EASD= {utils.signif(result['phi_easd'][0],4)}\n"

    s += f"X-Square= {result['chi_sq_stat']:.4g}\n"
    s += f"L-Square= {result['l_sq_stat']:.4g}\n"
    s += f"D.O.F.= {result['d_o_f']}\n"
    s += f"p-value= {utils.signif(result['p_value'],4)}\n"
    
    output.append(s)

    output.append("Model Diagnostics\n=================\n")
    if result['model_is_fit']:
        output.append("Model fits the observed data.\n")
    else:
        output.append("Model does not fit the observed data.\n")

    output.append(f"\nNumber of significant residuals = {result['num_sig_residuals']}\n")

    return output



from scipy.interpolate import interp1d

def margin_from_prob(x, fx, upper_envelope, y):
    # Interpolate to find the new x corresponding to y in the fx function
    interp_fx_to_x = interp1d(fx, x, fill_value="extrapolate")
    x_new = interp_fx_to_x(y)
    
    # Interpolate the upper envelope at the new x value
    interp_x_to_upper_envelope = interp1d(x, upper_envelope, fill_value="extrapolate")
    upper_envelope_at_x_new = interp_x_to_upper_envelope(x_new)
    
    # Calculate and return the margin
    return upper_envelope_at_x_new - y


def plot_tab(survival_result):
    computed = survival_result['computed']
    first_idx = survival_result['first_idx'] - 1
    unique_key = f'probability_value_{survival_result["name"]}_{survival_result["first_idx"]}'
    
    # Initialize probability value in session state if not present
    if 'probability_value' not in st.session_state:
        st.session_state.probability_value = 0.5  # default value
    
    # Input for probability value
    probability_value = st.number_input("Probability value", value=st.session_state.probability_value, key = unique_key)
    
    # Store the updated probability value in session state
    st.session_state.probability_value = probability_value
    
    # Empirical Plot Data
    data_empirical = pd.DataFrame({
        'stage': np.array(range(first_idx, first_idx + len(computed['uncond_survival_freq']))),
        'lower': computed['uncond_survival_freq'] - computed['uncond_survival_freq_envelope'],
        'value': computed['uncond_survival_freq'],
        'upper': computed['uncond_survival_freq'] + computed['uncond_survival_freq_envelope']
    })
    
    # Model Plot Data
    data_model = pd.DataFrame({
        'stage': np.array(range(first_idx, first_idx + len(computed['expected_uncond_survival_prob']))),
        'lower': computed['expected_uncond_survival_prob'] - computed['expected_uncond_survival_prob_envelope'],
        'value': computed['expected_uncond_survival_prob'],
        'upper': computed['expected_uncond_survival_prob'] + computed['expected_uncond_survival_prob_envelope']
    })
    
    col1, col2 = st.columns([1,2])

    with col2:
        # Empirical Plot
        fig_empirical, ax_empirical = plt.subplots(figsize=(10, 6))
        ax_empirical.plot(data_empirical['stage'], data_empirical['lower'], color='#0072B2', linestyle='--', label='Lower Bound')
        ax_empirical.plot(data_empirical['stage'], data_empirical['value'], color='#D55E00', label='Survival Frequency')
        ax_empirical.plot(data_empirical['stage'], data_empirical['upper'], color='#0072B2', linestyle='--', label='Upper Bound')
        ax_empirical.axhline(probability_value, color='grey', linestyle='-', label=f'Probability = {probability_value}')
        ax_empirical.set_ylim(0, 1)
        ax_empirical.set_xlabel("Stage")
        ax_empirical.set_ylabel("Frequency")
        ax_empirical.set_title("Empirical Plot", fontsize=16)
        ax_empirical.legend()

        # Model Plot
        fig_model, ax_model = plt.subplots(figsize=(10, 6))
        ax_model.plot(data_model['stage'], data_model['lower'], color='#0072B2', linestyle='--', label='Lower Bound')
        ax_model.plot(data_model['stage'], data_model['value'], color='#D55E00', label='Model Probability')
        ax_model.plot(data_model['stage'], data_model['upper'], color='#0072B2', linestyle='--', label='Upper Bound')
        ax_model.axhline(probability_value, color='grey', linestyle='-', label=f'Probability = {probability_value}')
        ax_model.set_ylim(0, 1)
        ax_model.set_xlabel("Stage")
        ax_model.set_ylabel("Probability")
        ax_model.set_title("Model Plot", fontsize=16)
        ax_model.legend()

        # Display the plots
        st.pyplot(fig_empirical)
        st.pyplot(fig_model)
    
    with col1:
        # Interpolation and Margins Calculation
        n = len(computed['uncond_survival_freq'])
        stages = np.arange(n)
        
        # For Empirical
        empirical_l = interp1d(computed['uncond_survival_freq'] - computed['uncond_survival_freq_envelope'], stages, fill_value="extrapolate")(probability_value)
        empirical_m = interp1d(computed['uncond_survival_freq'], stages, fill_value="extrapolate")(probability_value)
        empirical_u = interp1d(computed['uncond_survival_freq'] + computed['uncond_survival_freq_envelope'], stages, fill_value="extrapolate")(probability_value)
        empirical_w = margin_from_prob(stages, 
                                            computed['uncond_survival_freq'], 
                                            computed['uncond_survival_freq'] + 
                                            computed['uncond_survival_freq_envelope'], 
                                            probability_value)        
        # For Model
        model_l = interp1d(computed['expected_uncond_survival_prob'] - computed['expected_uncond_survival_prob_envelope'], stages, fill_value="extrapolate")(probability_value)
        model_m = interp1d(computed['expected_uncond_survival_prob'], stages, fill_value="extrapolate")(probability_value)
        model_u = interp1d(computed['expected_uncond_survival_prob'] + computed['expected_uncond_survival_prob_envelope'], stages, fill_value="extrapolate")(probability_value)
        model_w = margin_from_prob(stages, 
                                        computed['expected_uncond_survival_prob'], 
                                        computed['expected_uncond_survival_prob'] + 
                                        computed['expected_uncond_survival_prob_envelope'], 
                                        probability_value)        
        # Generate Text Output
        result_text = []
        result_text.append("Plot Envelope Result \n====================")
        result_text.append("")
        result_text.append("Empirical Envelope \n------------------")
        result_text.append(f"Left stage =      {np.round(empirical_l, 2)}")
        result_text.append(f"Middle stage =    {np.round(empirical_m, 2)}")
        result_text.append(f"Right stage =     {np.round(empirical_u, 2)}")
        result_text.append(f"Vertical margin = {np.round(empirical_w, 4)}")
        result_text.append("")
        result_text.append("Model Envelope \n--------------")
        result_text.append(f"Left stage =      {np.round(model_l, 2)}")
        result_text.append(f"Middle stage =    {np.round(model_m, 2)}")
        result_text.append(f"Right stage =     {np.round(model_u, 2)}")
        result_text.append(f"Vertical margin = {np.round(model_w, 4)}")

        # Display the result in Streamlit
        for line in result_text:
            st.text(line)



def oos_sampling(death_per_period, still_survive, training_fraction, reproducible=True):
    death_per_period = np.array(death_per_period)
    training_fraction = training_fraction/100

    max_stage = len(death_per_period)
    x = np.concatenate([np.repeat(np.arange(1, max_stage + 1), death_per_period), np.repeat(-1, still_survive)])
    n = np.sum(death_per_period) + still_survive
    num_training = int(max(np.round(n * training_fraction)[0], 1))

    if reproducible:
        np.random.seed(1)
    
    # Do sampling
    training_data = np.random.choice(x, num_training, replace=False)
    training_counts = pd.Series(training_data).value_counts().sort_index()
    survive_count = training_counts.iloc[0]  # Get the count of the first element
    training_counts = training_counts.iloc[1:]
    training_counts = pd.concat([training_counts, pd.Series([survive_count], index=[training_counts.index[-1] + 1])])
    training_vector = training_counts
    test_vector = np.append(death_per_period,still_survive) - np.array(training_vector)
    return {
        "training_vector": training_vector,
        "test_vector": test_vector
    }
    

def oos_training_computation(death_per_period, still_survive):
    total_num_people = np.sum(death_per_period) + still_survive
    num_period = len(death_per_period)
    
    # Calculate the number of survivors
    num_survivor = np.cumsum(np.concatenate((np.atleast_1d(still_survive), death_per_period[::-1])))[::-1]
    
    # Compute conditional fraction of deaths per period
    cond_frac_of_death_per_period = death_per_period / num_survivor[:-1]
    
    # Replace NaN values with None (similar to NA in R)
    cond_frac_of_death_per_period = np.where(np.isnan(cond_frac_of_death_per_period), None, cond_frac_of_death_per_period)
    
    observed = np.append(death_per_period, still_survive)
    
    return {
        "observed": observed,
        "cond_frac_of_death_per_period": cond_frac_of_death_per_period
    }

def oos_training_report(result, training_portion):
    output = []

    # Text portion
    text_part = ""
    text_part += "Out-of-Sample Training Result\n"
    text_part += "=============================\n\n"
    text_part += f"Training portion= {round(training_portion, 2)}%\n\n"

    # Prepare the DataFrame
    stages = list(range(1, len(result['observed']))) + ["Survive", "Total"]
    observed = list(result['observed']) + [sum(result['observed'])]
    cond_frac = list(result['cond_frac_of_death_per_period']) + [None, None]

    df = pd.DataFrame({
        "Stage": stages,
        "Observed": observed,
        "n(k)/N(k)": cond_frac
    })

    # Append text and DataFrame to output list
    output.append(text_part)
    output.append(df)

    return output

def oos_testing_report_grouped(sampling_result, training_fraction, g=None, show_plot=False):
    """
    Generate an Out-of-Sample Testing report, grouping the test data into
    subsets (if g is provided) and computing the homogeneous and ACC/DC models
    on each subset. Returns a list of dictionaries (one per group).
    
    sampling_result: dict from oos_sampling with keys "training_vector" and "test_vector".
    training_fraction: percentage used for training (so test fraction = 100 - training_fraction).
    g: optional splining groups in the same format as in create_report (e.g. [[1,3], [4,6]]).
    show_plot: if True, include plot metadata.
    """
    test_fraction = 100 - training_fraction
    test_vector = sampling_result["test_vector"]
    # Assume test_vector structure: [death counts..., still_survive]
    test_death_per_period = test_vector[:-1]
    test_still_survive = np.atleast_1d(test_vector[-1])
    
    # Define groups: if no grouping provided, use the entire test data.
    if g is None:
        groups = [(1, len(test_death_per_period))]
    else:
        groups = [(group[0], group[1]) for group in g]
    
    group_results = []
    header_text = f"Out-of-Sample Testing Result\nTest portion = {round(test_fraction, 2)}%\n\n"
    
    for (start, end) in groups:
        # Slice the test death counts according to the group. (Adjusting for 1-indexing)
        dp_group = test_death_per_period[start-1:end]
        # Use the same test_still_survive for each group (if appropriate)
        ss = test_still_survive
        
        homo_computed = homogeneous_computation(dp_group, ss)
        homo_report = homogeneous_report(homo_computed, first_stage_idx=start)
        
        acc_dc_computed = acc_dc_computation(dp_group, ss)
        acc_dc_report_ = acc_dc_report(acc_dc_computed, first_stage_idx=start)
        
        group_dict = {
            "group": (start, end),
            "header": header_text,
            "homogeneous": homo_report,
            "acc_dc": acc_dc_report_
        }
        if show_plot:
            group_dict["homogeneous_plot"] = [{
                "mode": "plot",
                "name": f"Homogeneous Plot for stages {start}-{end}",
                "computed": homo_computed,
                "first_idx": start
            }]
            group_dict["acc_dc_plot"] = [{
                "mode": "plot",
                "name": f"ACC/DC Plot for stages {start}-{end}",
                "computed": acc_dc_computed,
                "first_idx": start
            }]
        group_results.append(group_dict)
    
    return group_results




def add_target_column(df, survival_column, stages):
    """
    Add a new column to df with a value of 1 if the value of df.iloc[:, survival_column] is in stages, and 2 otherwise.
    
    Parameters:
    df (pd.DataFrame): The DataFrame to modify.
    survival_column (int): The index of the column to check against the stages.
    stages (list): A list of stage values to compare against.
    
    Returns:
    pd.DataFrame: The DataFrame with the new target column added.
    """
    df_with_new_target_col = df.copy()  # Make a copy of the DataFrame to avoid modifying the original
    df_with_new_target_col['Target'] = df.iloc[:, survival_column].apply(lambda x: 1 if x in stages else 2)
    return df_with_new_target_col

def create_report(death_per_period, still_survive, show_plot, g=None):
    """
    If g is provided, it should be a list of groups like [[1,3], [4,6]].
    This function splits the data according to each group and computes
    the homogeneous and ACC/DC models for each subset.
    Returns a list of dictionaries, one per group.
    """
    # If no splining groups are provided, treat entire data as one group.
    if g is None:
        groups = [(1, len(death_per_period))]
    else:
        groups = [(group[0], group[1]) for group in g]

    group_results = []
    for (start, end) in groups:
        # Slice the death counts (adjusting for 1-indexed input)
        dp_group = death_per_period[start-1:end]
        # You might want to adjust still_survive for groups—but here we use it as is.
        ss = still_survive

        # Compute Homogeneous and ACC/DC results on the subset.
        homo_computed = homogeneous_computation(dp_group, ss)
        homo_report = homogeneous_report(homo_computed, first_stage_idx=start)

        acc_dc_computed = acc_dc_computation(dp_group, ss)
        acc_dc_report_ = acc_dc_report(acc_dc_computed, first_stage_idx=start)

        # Build a dictionary for this group.
        group_dict = {
            "group": (start, end),
            "homogeneous": homo_report,  # list of textual/DF items
            "acc_dc": acc_dc_report_
        }
        if show_plot:
            group_dict["homogeneous_plot"] = [{
                "mode": "plot",
                "name": f"Homogeneous Plot",
                "computed": homo_computed,
                "first_idx": start
            }]
            group_dict["acc_dc_plot"] = [{
                "mode": "plot",
                "name": f"ACC/DC Plot",
                "computed": acc_dc_computed,
                "first_idx": start
            }]
        group_results.append(group_dict)
    return group_results
