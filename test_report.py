import numpy as np
import traceback
import ranking

def test_confirmatory_report():
    # Create a simple contingency table
    ct = np.array([[10, 20, 30, 40], 
                   [40, 30, 20, 10], 
                   [25, 25, 25, 25]])
    
    try:
        print("Running confirmatory computation...")
        comp_result = ranking.confirmatory_computation(ct)
        
        print("Computation completed. Keys in result:", list(comp_result.keys()))
        print("Brand cluster:", comp_result['brand_cluster'])
        print("Satisfaction partition:", comp_result['satisfaction_partition'])
        
        print("\nTrying to generate report...")
        report_result = ranking.confirmatory_report(ct, comp_result)
        
        print("Report generation successful!")
        print("Number of report items:", len(report_result['report_items']))
        print("First few report items:")
        
        # Print the first 5 report items or less if there are fewer
        for i, item in enumerate(report_result['report_items'][:5]):
            print(f"Item {i+1}: {type(item)}, Value: {item}")
        
        return True
    
    except Exception as e:
        print("Error occurred:")
        print(str(e))
        traceback.print_exc()
        return False

if __name__ == "__main__":
    success = test_confirmatory_report()
    if success:
        print("\nTest completed successfully!")
    else:
        print("\nTest failed!") 