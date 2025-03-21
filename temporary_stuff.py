# import statsmodels.api as sm
# from itertools import product

# import numpy as np
# import pandas as pd


# def loglin(data, which_x):
#     # Create a DataFrame with all the indices and the counts
#     indices = list(product(*[range(dim) for dim in data.shape]))
#     df = pd.DataFrame(indices, columns=[f'dim{i}' for i in range(data.ndim)])
#     df['count'] = data.flatten()

#     # Debug: Print the DataFrame
#     print("DataFrame:")
#     print(df)

#     terms = []

#     # Add terms based on which_x
#     for pair in which_x:
#         if isinstance(pair, int):  # Single dimension term
#             terms.append(f'C(dim{pair})')
#         elif isinstance(pair, list) and len(pair) == 1:  # Single dimension term in a list
#             terms.append(f'C(dim{pair[0]})')
#         else:  # Interaction terms
#             interaction_term = ')*C(dim'.join(map(str, pair)).join(['C(dim', ')'])
#             terms.append(interaction_term)

#     # Remove duplicate terms
#     terms = list(dict.fromkeys(terms))

#     formula = 'count ~ ' + ' + '.join(terms)
#     # Debug: Print the formula
#     print("Formula:", formula)

#     # Fit the model
#     model = sm.GLM.from_formula(formula, data=df, family=sm.families.Poisson()).fit()
    
#     # pdb.set_trace()

#     # Debug: Print model summary
#     print("Model Summary:")
#     print(model.summary())

#     expected = model.mu.reshape(data.shape)

#     # Manually calculate LRT
#     lrt = 2 * (model.llf - model.llnull)
    
#     # Manually calculate Pearson Chi-squared
#     observed = df['count'].values
#     pearson_chi2 = np.sum((observed - model.mu) ** 2 / model.mu)

#     # Degrees of freedom
#     df_resid = model.df_resid
#     params = dict(model.params)

#     # Debug: Print outputs
#     print("Expected Counts:")
#     print(expected)
#     print("LRT:", lrt)
#     print("Pearson Chi-squared:", pearson_chi2)
#     print("Degrees of Freedom:", df_resid)
#     print("Parameters:")
#     print(params)

#     # Return four values: expected, lrt, pearson_chi2, df_resid
#     return expected, lrt, df_resid, params, pearson_chi2



# def loglin(data, which_x):
#     # Create a DataFrame with all the indices and the counts
#     indices = list(product(*[range(dim) for dim in data.shape]))
#     df = pd.DataFrame(indices, columns=[f'dim{i}' for i in range(data.ndim)])
#     df['count'] = data.flatten()

#     # Debug: Print the DataFrame
#     print("DataFrame:")
#     print(df)

#     terms = []

#     # Add terms based on which_x
#     for pair in which_x:
#         if isinstance(pair, int):  # Single dimension term
#             terms.append(f'C(dim{pair})')
#         elif isinstance(pair, list) and len(pair) == 1:  # Single dimension term in a list
#             terms.append(f'C(dim{pair[0]})')
#         else:  # Interaction terms
#             interaction_term = '):C(dim'.join(map(str, pair)).join(['C(dim', ')'])
#             terms.append(interaction_term)

#     # Remove duplicate terms
#     terms = list(dict.fromkeys(terms))

#     formula = 'count ~ ' + ' + '.join(terms)
#     # Debug: Print the formula
#     print("Formula:", formula)

#     # Fit the model
#     model = sm.GLM.from_formula(formula, data=df, family=sm.families.Poisson()).fit()
    
#     # Debug: Print model summary
#     print("Model Summary:")
#     print(model.summary())

#     expected = model.mu.reshape(data.shape)

#     # Manually calculate LRT
#     lrt = 2 * (model.llf - model.llnull)
    
#     # Manually calculate Pearson Chi-squared
#     observed = df['count'].values
#     pearson_chi2 = np.sum((observed - model.mu) ** 2 / model.mu)

#     # Degrees of freedom
#     df_resid = model.df_resid
#     params = dict(model.params)

#     # Debug: Print outputs
#     print("Expected Counts:")
#     print(expected)
#     print("LRT:", lrt)
#     print("Pearson Chi-squared:", pearson_chi2)
#     print("Degrees of Freedom:", df_resid)
#     print("Parameters:")
#     print(params)

#     # Calculate lambda coefficients from beta parameters
#     lambda_coeffs = {}

#     # Process main effects
#     for term, beta in params.items():
#         if term == 'Intercept':
#             continue
#         if ':' not in term:  # Main effects
#             variable, level = term.split('[')[0], term.split('T.')[-1][:-1]
#             level = int(level)
#             if variable not in lambda_coeffs:
#                 lambda_coeffs[variable] = {}
#             lambda_coeffs[variable][level] = beta
    
#     # Center main effects to sum to zero
#     for variable in lambda_coeffs:
#         levels = sorted(lambda_coeffs[variable].keys())
#         mean_lambda = np.mean([lambda_coeffs[variable][level] for level in levels])
#         for level in levels:
#             lambda_coeffs[variable][level] -= mean_lambda
    
#     # Process interaction terms
#     for term, beta in params.items():
#         if ':' in term:  # Interaction effects
#             variables = term.split(':')
#             levels = [int(v.split('T.')[-1][:-1]) for v in variables]
#             variables = [v.split('[')[0] for v in variables]
#             interaction = tuple(zip(variables, levels))
#             if interaction not in lambda_coeffs:
#                 lambda_coeffs[interaction] = beta

#     # Debug: Print lambda coefficients
#     print("Lambda Coefficients:")
#     for variable, levels in lambda_coeffs.items():
#         if isinstance(variable, tuple):  # Interaction terms
#             print(f'{variable} = {levels:.4f}')
#         else:
#             for level, value in levels.items():
#                 print(f'{variable}[T.{level}] = {value:.4f}')

#     # Return expected, lrt, df_resid, params, pearson_chi2, lambda_coeffs
#     return expected, lrt, df_resid, params, pearson_chi2, lambda_coeffs
