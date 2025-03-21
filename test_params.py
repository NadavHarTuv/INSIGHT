import ranking
import numpy as np
from scipy import stats

# Use the same contingency table from the test_crosstab.py
ct = np.array([
    [35, 40, 30, 51, 51, 60, 50],
    [87, 108, 80, 14, 18, 15, 19],
    [35, 44, 31, 50, 60, 55, 65],
    [107, 103, 85, 16, 15, 19, 25],
    [92, 88, 95, 20, 15, 16, 15]
])

print("Running exponential spacing computation with explicit polynomial degrees...")
# Pass the number of rows-1 and columns-1 as the polynomial degrees
num_rows, num_cols = ct.shape
result = ranking.exponential_spacing_computation(
    ct,
    poly_deg_row=num_rows-1,
    poly_deg_col=num_cols-1,
    debug=True
)

print("\nFinal parameters:")
print(f"Alpha: {result['alpha'].round(4)}")
print(f"Beta: {result['beta'].round(4)}")
print(f"Mu: {result['mu'].round(4)}")
print(f"Nu: {result['nu'].round(4)}")
print(f"Phi: {result['phi']:.6f}")

print("\nModel fit statistics:")
print(f"G-squared: {result['g_squared']:.4f}")
print(f"df: {result['df']}")
print(f"p-value: {result['p_value']:.6f}")
print(f"Dissimilarity index: {result['dissimilarity']:.4f}")

print("\nComparison of observed vs expected:")
for i in range(ct.shape[0]):
    print(f"Row {i+1}: ")
    print(f"  Observed: {ct[i,:]}")
    print(f"  Expected: {result['expected'][i,:].round(2)}")
    
print("\nRow margins:")
observed_row_sums = ct.sum(axis=1)
expected_row_sums = result['expected'].sum(axis=1)
print(f"Observed: {observed_row_sums}")
print(f"Expected: {expected_row_sums.round(2)}")
print(f"Difference: {(observed_row_sums - expected_row_sums).round(4)}")

print("\nColumn margins:")
observed_col_sums = ct.sum(axis=0)
expected_col_sums = result['expected'].sum(axis=0)
print(f"Observed: {observed_col_sums}")  
print(f"Expected: {expected_col_sums.round(2)}")
print(f"Difference: {(observed_col_sums - expected_col_sums).round(4)}")

# Standard errors
print("\nStandard errors:")
print(f"Mu SE: {result['mu_se'].round(6)}")
print(f"Nu SE: {result['nu_se'].round(6)}")
print(f"Phi SE: {result['phi_se']:.6f}")

# Calculate Z-values and p-values for parameters
print("\nParameter significance:")
mu_z = result['mu'] / result['mu_se']
mu_p = 2 * (1 - stats.norm.cdf(abs(mu_z)))
print("Mu parameters:")
for i in range(len(result['mu'])):
    print(f"  Mu[{i}] = {result['mu'][i]:.4f} (SE={result['mu_se'][i]:.4f}, Z={mu_z[i]:.4f}, p={mu_p[i]:.4f})")

nu_z = result['nu'] / result['nu_se']
nu_p = 2 * (1 - stats.norm.cdf(abs(nu_z)))
print("Nu parameters:")
for i in range(len(result['nu'])):
    print(f"  Nu[{i}] = {result['nu'][i]:.4f} (SE={result['nu_se'][i]:.4f}, Z={nu_z[i]:.4f}, p={nu_p[i]:.4f})")

phi_z = result['phi'] / result['phi_se']
phi_p = 2 * (1 - stats.norm.cdf(abs(phi_z)))
print(f"Phi = {result['phi']:.6f} (SE={result['phi_se']:.6f}, Z={phi_z:.4f}, p={phi_p:.6f})") 