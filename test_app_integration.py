import numpy as np
import traceback
import ranking
import pandas as pd

def test_app_integration():
    try:
        # Create a contingency table as a DataFrame to simulate app.py's behavior
        data = pd.DataFrame(
            [[10, 20, 30, 40], 
             [40, 30, 20, 10], 
             [25, 25, 25, 25]],
            index=["Brand 1", "Brand 2", "Brand 3"],
            columns=["Level 1", "Level 2", "Level 3", "Level 4"]
        )
        
        print("Data shape:", data.shape)
        print("Data type:", type(data))
        
        # Define variables as they would be in app.py
        upper_polarity_idx = None
        satisfaction_constraint = None
        debug = True
        
        print("\nRunning confirmatory computation...")
        confirmatory_comp_result = ranking.confirmatory_computation(
            data, 
            upper_polarity_idx=upper_polarity_idx, 
            satisfaction_constraint=satisfaction_constraint, 
            debug=debug
        )
        
        print("Computation completed. Brand cluster:", confirmatory_comp_result["brand_cluster"])
        
        print("\nRunning confirmatory report generation...")
        confirmatory_report_result = ranking.confirmatory_report(
            data, 
            confirmatory_comp_result, 
            upper_polarity_idx=upper_polarity_idx, 
            satisfaction_constraint=satisfaction_constraint, 
            debug=debug
        )
        
        print("Report generation completed.")
        print("Number of report items:", len(confirmatory_report_result["report_items"]))
        
        # Simulate what app.py does with the results
        results = confirmatory_report_result["report_items"]
        print("Type of results:", type(results))
        
        # Demonstrate handling in app.py
        print("\nSimulating app.py behavior...")
        for i, item in enumerate(results[:5]):
            print(f"Item {i+1}: {type(item)}")
            if isinstance(item, pd.DataFrame):
                print("  DataFrame shape:", item.shape)
            else:
                print("  String value:", item)
        
        return True
    
    except Exception as e:
        print("\nERROR OCCURRED:")
        print(str(e))
        traceback.print_exc()
        return False

if __name__ == "__main__":
    success = test_app_integration()
    if success:
        print("\nTest completed successfully!")
    else:
        print("\nTest failed!") 