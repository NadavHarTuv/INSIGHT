import numpy as np
import traceback
import pandas as pd
import ranking

def test_pmf_calculation():
    """Test specifically the PMF calculation in the confirmatory computation function"""
    
    # Create a simple contingency table
    ct = np.array([[10, 20, 30, 40], 
                   [40, 30, 20, 10], 
                   [25, 25, 25, 25]])
    
    print("Starting PMF calculation test...")
    
    try:
        # Setup similar to confirmatory_computation
        working_table = ct.copy()
        working_table[working_table == 0] = 0.5
        n_rows, n_cols = working_table.shape
        
        print(f"Working table shape: {working_table.shape}")
        print(f"Working table: \n{working_table}")
        
        # Test row_sums calculation
        row_sums = np.sum(working_table, axis=1, keepdims=True)
        print(f"Row sums shape: {row_sums.shape}")
        print(f"Row sums: {row_sums.flatten()}")
        
        # Test PMF calculation
        pmf = working_table / row_sums
        print(f"PMF shape: {pmf.shape}")
        print(f"PMF: \n{pmf}")
        
        # Test full_pmf calculation
        full_pmf = working_table / np.sum(working_table, axis=1, keepdims=True)
        print(f"Full PMF shape: {full_pmf.shape}")
        print(f"Full PMF: \n{full_pmf}")
        
        # Test ranking calculations
        avg_satisfaction = np.sum(working_table * np.arange(1, n_cols + 1).reshape(1, -1), axis=1) / np.sum(working_table, axis=1)
        print(f"Average satisfaction: {avg_satisfaction}")
        
        rank_by_avg = np.argsort(-avg_satisfaction)
        print(f"Rank by avg: {rank_by_avg}")
        
        rank_by_best = np.argsort(-full_pmf[:, -1])
        print(f"Rank by best: {rank_by_best}")
        
        rank_by_best_two = np.argsort(-np.sum(full_pmf[:, [-2, -1]], axis=1))
        print(f"Rank by best two: {rank_by_best_two}")
        
        return True
    except Exception as e:
        print("Error occurred:")
        print(str(e))
        traceback.print_exc()
        return False

if __name__ == "__main__":
    success = test_pmf_calculation()
    if success:
        print("\nPMF calculation test completed successfully!")
    else:
        print("\nPMF calculation test failed!") 