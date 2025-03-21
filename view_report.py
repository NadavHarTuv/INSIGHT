import numpy as np
import pandas as pd
from ranking import confirmatory_report

# Use the same contingency table from test_params.py
ct = np.array([
    [35, 40, 30, 51, 51, 60, 50],
    [87, 108, 80, 14, 18, 15, 19],
    [35, 44, 31, 50, 60, 55, 65],
    [107, 103, 85, 16, 15, 19, 25],
    [92, 88, 95, 20, 15, 16, 15]
])

# Run confirmatory report
print("Generating confirmatory report...")
result = confirmatory_report(ct, debug=False)

# Display all items in the report
print("\n===== FULL REPORT =====\n")
for i, item in enumerate(result):
    if isinstance(item, pd.DataFrame):
        print(f"Item {i+1} (DataFrame):")
        print(item)
        print()
    else:
        print(f"Item {i+1}: {item}") 