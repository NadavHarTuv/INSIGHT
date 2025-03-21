import numpy as np
import pandas as pd
import sys
import traceback
from ranking import confirmatory_report

# Load the data
df = pd.read_csv('explanatory_data.csv', header=None)
print(f'Original data shape: {df.shape}')

# Use the proper transformation logic from utils.load_data
brand_col = 1  # 2nd column (0-indexed)
ranking_col = 3  # 4th column (0-indexed)
contingency_table = pd.crosstab(df.iloc[:, brand_col], df.iloc[:, ranking_col])

# Ensure columns are in order (as done in utils.load_data)
contingency_table = contingency_table.reindex(sorted(contingency_table.columns), axis=1)
print(f'Contingency table shape: {contingency_table.shape}')
print(f'Contingency table values:\n{contingency_table}')

# Try running confirmatory analysis with Debug mode
try:
    result = confirmatory_report(contingency_table, satisfaction_constraint='None', debug=True)
    print('Success!')
    
    # Print some of the results to verify correctness
    print("\nNumber of report items:", len(result))
    
    # Look for degrees of freedom in the result
    for i, item in enumerate(result):
        if isinstance(item, str) and "Degrees of Freedom" in item:
            print(f"\nFound degrees of freedom at item {i+1}:")
            print(item)
        elif isinstance(item, str) and "Model Fit Statistics" in item:
            # Print the next few items which likely contain the degrees of freedom
            print(f"\nFound Model Fit Statistics at item {i+1}:")
            for j in range(i+1, min(i+5, len(result))):
                print(f"Item {j+1}: {result[j]}")
    
    # Print strings from the result
    string_items = [item for item in result if isinstance(item, str)]
    print("\nSample of string items:")
    for item in string_items[:5]:  # Print first 5 string items
        print(item)
    
    # Print DataFrames from the result
    df_items = [item for item in result if isinstance(item, pd.DataFrame)]
    print("\nNumber of DataFrame items:", len(df_items))
    if df_items:
        print("\nFirst DataFrame:")
        print(df_items[0])
        
except Exception as e:
    print(f'Error: {e}')
    traceback.print_exc() 