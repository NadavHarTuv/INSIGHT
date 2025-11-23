import argparse
import numpy as np
import pandas as pd

import loyalty


def build_contingency_table(path: str, first_purchase_col: int, second_purchase_col: int) -> np.ndarray:
    """Return contingency table aligned with loyalty models (1-indexed columns in CSV)."""
    df = pd.read_csv(path, header=None)
    first = df.iloc[:, first_purchase_col]
    second = df.iloc[:, second_purchase_col]

    brands = sorted(set(first.dropna().astype(int)).union(second.dropna().astype(int)))
    contingency = (
        pd.crosstab(first, second)
        .reindex(index=brands, columns=brands, fill_value=0)
        .sort_index(axis=0)
        .sort_index(axis=1)
    )
    return contingency.to_numpy(dtype=float), brands


def summarize_model(name: str, result: dict) -> None:
    print(f"\n===== {name} Model =====")
    print("phi:", np.round(result["phi"], 6))
    print("delta:", result.get("delta"))
    print("cov_matrix:\n", np.round(result["cov_matrix"], 6))
    print("diag_cov:", np.round(np.diag(result["cov_matrix"]), 6))
    print("significance_stat:\n", np.round(result["significance_stat"], 4))
    print("significance_table:\n", result["significance_table"])


def main():
    parser = argparse.ArgumentParser(description="Run loyalty M/Q models on a CSV sample.")
    parser.add_argument("--csv", default="INSIGHT Streamlit example.csv")
    parser.add_argument("--first-col", type=int, default=1, help="0-indexed column for first purchase (default=1).")
    parser.add_argument("--second-col", type=int, default=2, help="0-indexed column for second purchase (default=2).")
    parser.add_argument("--threshold", type=float, default=1.64)
    args = parser.parse_args()

    contingency, brands = build_contingency_table(args.csv, args.first_col, args.second_col)
    print(f"Loaded contingency table for brands: {brands}")
    print(contingency.astype(int))

    m_res = loyalty.m_model_computation(contingency, significance_threshold=args.threshold)
    summarize_model("M", m_res)

    q_res = loyalty.q_model_computation(contingency, significance_threshold=args.threshold)
    summarize_model("Q", q_res)


if __name__ == "__main__":
    main()

