import streamlit as st
import pandas as pd
import numpy as np
import independence
import threedim
import ndim
import survival
import loyalty
import ranking
import utils

st.markdown(
    """
    <style>
    div[data-testid="stMarkdownContainer"] p {
      white-space: pre-wrap;
    }
    </style>
    """,
    unsafe_allow_html=True
)


def display_result_items(result_items):
    """Display a list of result items, using st.dataframe for DataFrames and st.markdown for strings."""
    if isinstance(result_items, list):
        for item in result_items:
            if isinstance(item, pd.DataFrame):
                st.dataframe(utils.clean_df(item))
            elif isinstance(item, str):
                # Break long strings at newlines for better readability
                if '\n' in item:
                    # For debug logs with newlines, use code block formatting
                    st.code(item)
                else:
                    st.markdown(item)
            else:
                st.write(item)
    else:
        st.write(result_items)


def display_analysis_with_plots(result_items, analysis_type, contingency_table, compute_results=None):
    """
    Display analysis results with plots in subtabs.
    
    Args:
        result_items: The result items to display in the main tab
        analysis_type: Either 'exploratory' or 'confirmatory'
        contingency_table: The contingency table used for the analysis
        compute_results: The result dictionary from the computation
    """
    # Create tabs for results and plots
    tabs = st.tabs(["Analysis Results", "Average Satisfaction", "Distribution by Brand Groups"])
    
    # Results tab - show the original report
    with tabs[0]:
        if isinstance(result_items, list):
            for item in result_items:
                try:
                    if isinstance(item, pd.DataFrame):
                        st.dataframe(utils.clean_df(item))
                    elif isinstance(item, str):
                        # Break long strings at newlines for better readability
                        if '\n' in item:
                            # For debug logs with newlines, use code block formatting
                            st.code(item)
                        else:
                            st.markdown(item)
                    else:
                        # For any other type, convert to string
                        st.write(str(item))
                except Exception as e:
                    st.error(f"Error displaying item: {str(e)}")
                    st.code(f"Item type: {type(item)}, Value: {str(item)}")
        else:
            try:
                st.write(str(result_items))
            except Exception as e:
                st.error(f"Error displaying result_items: {str(e)}")
                st.code(f"Type: {type(result_items)}")
    
    # If we don't have compute results, we can't display plots
    if compute_results is None:
        for i in range(1, 3):
            with tabs[i]:
                st.warning("No plot data available. Run analysis again to see plots.")
        return
    
    # Average Satisfaction Plot tab
    with tabs[1]:
        try:
            if analysis_type == 'exploratory':
                fig = ranking.plot_exploratory_brand_clusters(compute_results)
                st.pyplot(fig)
            else:  # confirmatory
                # Use the computation results directly
                fig = ranking.plot_confirmatory_brand_clusters(compute_results)
                st.pyplot(fig)
        except Exception as e:
            st.error(f"Error generating average satisfaction plot: {str(e)}")
            st.exception(e)
    
    # Distribution by Brand Groups Plot tab
    with tabs[2]:
        try:
            if analysis_type == 'exploratory':
                fig = ranking.plot_exploratory_brand_distribution(compute_results, contingency_table)
                st.pyplot(fig)
            else:  # confirmatory
                # Use the computation results directly
                fig = ranking.plot_confirmatory_brand_distribution(compute_results)
                st.pyplot(fig)
        except Exception as e:
            st.error(f"Error generating distribution plot: {str(e)}")
            st.exception(e)


analysis_methods = utils.analysis_methods
model_names = utils.model_names

st.title('INSIGHT')


# Sidebar: Select analysis method
selected_method = st.sidebar.selectbox("Select an Analysis Method", analysis_methods)

# Initialize session state for data and results across all analysis methods
if 'raw_data' not in st.session_state:
    st.session_state['raw_data'] = None
if 'transformed_data' not in st.session_state:
    st.session_state['transformed_data'] = None
if 'results' not in st.session_state:
    st.session_state['results'] = {}
if 'comp_results' not in st.session_state:
    st.session_state['comp_results'] = {}
if 'tabs' not in st.session_state:
    st.session_state['tabs'] = {}
    


# Sidebar: File uploader
uploaded_file = st.sidebar.file_uploader("Upload data file", type='csv', key='file_uploader')

# Load/transform data only if a file is uploaded
if uploaded_file is not None:
    new_raw_data, new_transformed_data = utils.load_data(uploaded_file, selected_method)
    if new_raw_data is not None and new_transformed_data is not None:
        # Store the transformed data (contingency table) in session state
        st.session_state['raw_data'] = new_raw_data
        st.session_state['transformed_data']=new_transformed_data
        # Set up storage for results and tabs specific to this method.
key = selected_method.lower().replace(" ", "_")
if key not in st.session_state['results']:
    st.session_state['results'][key] = []
if key not in st.session_state['tabs']:
    st.session_state['tabs'][key] = ['Data']

# If we have a contingency table, display model selection and a "Go!" button
if selected_method == 'Independence' and st.session_state.get('transformed_data') is not None:
    key = 'independence'
    data = st.session_state['transformed_data']
    selected_model = st.sidebar.selectbox("Select Model", model_names['Independence'])
    
    # ------------------------
    # 1) ANOAS-Specific Inputs
    # ------------------------
    orientation_anoas = None
    group_string_anoas = None
    groups_anoas = None
    if selected_model == 'ANOAS Model':
        orientation_anoas = st.sidebar.radio("Select orientation", ["Row", "Column"])
        group_string_anoas = st.sidebar.text_input("ANOAS groups", "", help=independence.anoas_help_text)
        if group_string_anoas.strip():
            groups_anoas = utils.parse_text_groups(group_string_anoas)
            if groups_anoas is None:
                st.sidebar.warning("Invalid group string. Check the format.")
    # -----------------------------------
    # 2) Log-Odds Z-Test Specific Inputs
    # -----------------------------------
    orientation_ztest = None
    pair = None
    if selected_model == "Log-Odds Z-Test":
        orientation_ztest = st.sidebar.radio("Select orientation", ["Row", "Column"])
        # This helper will read the pair from a text_input and return either the
        # 0-based indices or None if invalid.
        pair = independence.ztest_read_pair(
            st.session_state['indep_contingency_table'],
            orientation_ztest
        )

    # -----------------------------------
    # 3) Chi Square Specific Inputs
    # -----------------------------------    
    row_groups = None
    col_groups = None
    if selected_model == "Collapse Chi-Sq Test":
        row_groups_str = st.sidebar.text_input("Row groups", help="Enter row groups (e.g., '1-3;4,5')")
        col_groups_str = st.sidebar.text_input("Column groups", help="Enter column groups (e.g., '1,2;3-4')")
        
        if row_groups_str.strip():
            row_groups = utils.parse_text_groups(row_groups_str)
            if row_groups is None:
                st.sidebar.warning("Invalid row groups string. Please check the format.")
        if col_groups_str.strip():
            col_groups = utils.parse_text_groups(col_groups_str)
            if col_groups is None:
                st.sidebar.warning("Invalid column groups string. Please check the format.")
    
    
    if st.sidebar.button("Go!"):
        if selected_model == 'Independence Model':
            result = independence.independence_model_report(data)
            
        elif selected_model == 'ANOAS Model':
            if groups_anoas is None:
                result = "Invalid or missing group string. Cannot compute ANOAS."
            else:
                result = independence.anoas_model_report(data, groups_anoas, orientation_anoas)
                
        elif selected_model == "Predictors' Proportion":
            result = independence.predictors_proportion_report(data)
            
        elif selected_model == "Log-Odds Z-Test":
            if pair is None:
                result = "Invalid pair of rows/columns. Cannot compute Log-Odds Z-Test."
            else:  
                result = independence.ztest_report(contingency_table=data, pair=pair,orientation=orientation_ztest)
            
        elif selected_model == "Collapse Chi-Sq Test":
            if row_groups is None or col_groups is None:
                result = "Invalid group string(s) provided. Cannot compute Collapse Chi-Sq Test."
            else:
                result = independence.chisq_test_report(data, row_groups, col_groups)
            
        else:
            result = "Selected model not implemented."
        
        st.session_state['results'][key].append(result)
        new_tab_label = f"Result {len(st.session_state['results'][key])}"
        st.session_state['tabs'][key].append(new_tab_label)

elif selected_method == '3-Dimensional' and st.session_state['transformed_data'] is not None:
    key = selected_method.lower().replace(' ', '_')
    data = st.session_state['transformed_data']
    data_for_computations = threedim.transform_to_3d_array(data)
    
    selected_model = st.sidebar.selectbox("Model Name", model_names['3-Dimensional'])
    
    # Then the user picks which type of 3D computation to do:
    computation_choice = st.sidebar.radio(
        "Computation Type",
        ["All coefficients", "Select coefficients", "Target variable"]
    )
    
    # We'll store the final result in computed_result
    computed_result = None
    
    # ---------------------------
    # A) All coefficients
    # ---------------------------
    if computation_choice == "All coefficients":
        st.sidebar.write("Compute all possible 3D models.")
        # When user clicks "Go!", do the all_coef_computation => all_coef_report
        if st.sidebar.button("Go!"):
            comp = threedim.all_coef_computation(data_for_computations)
            computed_result = threedim.all_coef_report(comp)
            # Save in session state
            st.session_state['results'][key].append(computed_result)
            new_tab_label = f"Result {len(st.session_state['results'][key])}"
            st.session_state['tabs'][key].append(new_tab_label)
    
    # ---------------------------
    # B) Select coefficients
    # ---------------------------
    elif computation_choice == "Select coefficients":
        # Show hierarchical checkboxes
        st.sidebar.write("Select which coefficients you want in the model.")
        selected_coefs = threedim.select_coefs()  # This calls your function that updates X, Y, Z if X Y is chosen, etc.
        
        # Then we parse those coefs into which_x
        if st.sidebar.button("Go!"):
            which_x = threedim.parse_selected_coefs(selected_coefs)
            comp = threedim.select_coef_computation(data_for_computations, which_x)
            computed_result = threedim.select_coef_report(comp)
            # Save in session state
            st.session_state['results'][key].append(computed_result)
            new_tab_label = f"Result {len(st.session_state['results'][key])}"
            st.session_state['tabs'][key].append(new_tab_label)

    # ---------------------------
    # C) Target variable
    # ---------------------------
    else:  # "Target variable"
        # Let user pick from columns that have exactly 2 unique values
        st.sidebar.write("Select which variable is the target (must be binary).")
        target_var = threedim.get_target_variable(data)
        
        if st.sidebar.button("Go!"):
            comp = threedim.target_computation(data_for_computations, target_var)
            
            # For target variable, we want TWO sub-tabs:
            #   1) The standard result
            #   2) The "Model Value" dynamic UI
            # We'll store them in a single dictionary so we can display them together.
            
            # The normal output
            standard_report = threedim.target_report(comp)  # list of text/DataFrames
            
            # We'll create a "model_value" dictionary with everything needed for model_value_tab
            # For example, model_value_tab expects a dict like {"name": "...", "computed": comp}
            # so let's store something like that:
            model_value_info = {
                "name": f"Target={target_var}",
                "computed": comp
            }
            
            # We'll store them as a dict with two "sub-tabs"
            # so we can easily handle them in the main code
            computed_result = {
                "mode": "target_variable",      # a little flag to let us know
                "report": standard_report,      # the normal list
                "model_value": model_value_info # the dictionary for dynamic UI
            }
            
            st.session_state['results'][key].append(computed_result)
            new_tab_label = f"Target variable {len(st.session_state['results'][key])}"
            st.session_state['tabs'][key].append(new_tab_label)
     
# ----------------------------
# N-Dimensional Analysis Section
# ----------------------------
elif selected_method == 'N-Dimensional' and st.session_state.get('raw_data') is not None:
    key = selected_method.lower().replace(' ','_')
    data = st.session_state['raw_data']
    data=utils.clean_df(data)
    data.columns = [f'X{i+1}' for i in range(data.shape[1])]
    data.index = data.index + 1
    
    # Read user input: predictors, target, and constraints.
    # read_input() should display sidebar inputs for "Predictor variable(s)" and "Client constraints"
    # and use a function (like threedim.get_target_variable) to offer only binary options as targets.
    predictors, target, constraints = ndim.read_input(data)
    
    # # Optionally, display what the user has entered:
    # st.sidebar.write("Predictor variable(s):", predictors)
    # st.sidebar.write("Target variable:", target)
    # st.sidebar.write("Client constraints:", constraints)
    
    # The N-dimensional analysis uses variable selection (the only option), so we have one button.
    if st.sidebar.button("Go!"):
        # Run the target variable computation.
        # This returns a dictionary with keys like 'original_data', 'model', etc.
        ndim_results = ndim.target_variable_computation(data, predictors, target, constraints, level=2)
        
        # For target variable mode, we want two sub-tabs:
        #   1) The standard output report
        #   2) The dynamic "Model Value" UI.
        standard_report = ndim.target_variable_report(ndim_results)  # returns a list of text and DataFrames
        model_value_info = {
            "name": f"Target=X{target+1}",
            "computed": ndim_results
        }
        computed_result = {
            "mode": "target_variable",   # flag for target-variable mode
            "report": standard_report,
            "model_value": model_value_info,
            'detailed': {
                'computed': ndim_results,
                'name': f"Target=X{target+1}"
            }
        }
        st.session_state['results'][key].append(computed_result)
        new_tab_label = f"Result {len(st.session_state['results'][key])}"
        st.session_state['tabs'][key].append(new_tab_label)
            
elif selected_method == 'Survival' and st.session_state.get('transformed_data') is not None:
    key = selected_method.lower().replace(' ','_')
    data = st.session_state['transformed_data']
    
    death_per_period = data['Count'].iloc[:-1].values
    still_survive = data['Count'].iloc[-1]
    still_survive = np.array([still_survive])
    
    # Initialize storage for survival results and tabs
    if key not in st.session_state['results']:
        st.session_state['results'][key] = []
    if key not in st.session_state['tabs']:
        st.session_state['tabs'][key] = ['Data']
        
    # Sidebar: Select the survival model type (mimicking R Shiny design)
    selected_model = st.sidebar.selectbox("Select Survival Model", 
                                                 ["Homogeneous, ACC/DC", 
                                                  "Out-of-Sample Splining", 
                                                  "Explanatory Variable"])
    
    if selected_model == "Homogeneous, ACC/DC":
        spline_groups = None
        splining_str = st.sidebar.text_input("(Optional) Splining", value="")
        if splining_str:
            spline_groups = survival.parse_splining_string(splining_str, num_stage=len(death_per_period))
        show_plot = st.sidebar.checkbox("Show plot?")
    
        if st.sidebar.button('Go!'):
            group_results = survival.create_report(death_per_period, still_survive, show_plot, g=spline_groups)
            st.session_state['results'][key].extend(group_results)
            for group in group_results:
                start, end = group['group']
                new_tab_label = f"Stages {start}-{end}"
                st.session_state['tabs'][key].append(new_tab_label)
                
    elif selected_model == "Out-of-Sample Splining":
        training_portion = st.sidebar.text_input("Training portion (in %)", value="50")
        reproducible = st.sidebar.checkbox("Reproducible?", value=True)
        if st.sidebar.button("Go Training!"):
            # Compute the training results using the existing functions in survival.py
            oos_result = survival.oos_training_computation(death_per_period, still_survive)
            report = survival.oos_training_report(oos_result, float(training_portion))
            st.session_state['results'][key].append(report)
            new_tab_label = f"Result {len(st.session_state['results'][key])}"
            st.session_state['tabs'][key].append(new_tab_label)
        
        testing_splining_str = st.sidebar.text_input("(Optional) Testing Splining", value="")
        show_plot_testing = st.sidebar.checkbox("Show plot for testing?")
        testing_groups=None
        
        if st.sidebar.button("Go Testing!", key="go_testing"):
            training_fraction = float(training_portion)
            sampling_result = survival.oos_sampling(death_per_period, still_survive, training_fraction, reproducible)
            # If a testing splining string is provided, parse it:
            if testing_splining_str:
                testing_groups = survival.parse_splining_string(testing_splining_str, num_stage=len(death_per_period))
            else:
                testing_groups = None
            group_results = survival.oos_testing_report_grouped(sampling_result, training_fraction, g=testing_groups, show_plot=show_plot_testing)
            # For each group, append its result and create an outer tab with a meaningful label:
            for group in group_results:
                st.session_state['results'][key].append(group)
                start, end = group["group"]
                new_tab_label = f"Stages {start}-{end} (Test)"
                st.session_state['tabs'][key].append(new_tab_label)
    
    elif selected_model == "Explanatory Variable":
        # Use raw_data for this analysis
        data_expl = st.session_state['raw_data']
        # Retrieve the survival column from session_state (saved by utils.load_data)
        survival_col = st.session_state.get('survival_col')
        if survival_col is None:
            st.error("Survival column not found. Please re-upload your data.")
        else:
            st.sidebar.subheader("Explanatory Variable Options")
            # Input for death stages (comma-separated or ranges, e.g. "1,3-5")
            death_stages_str = st.sidebar.text_input(
                "Enter death stages (range, e.g. 3-5)",
                value="",
                key="expl_death_stages"
            )
            # Input for explanatory variable indices as a string (1-based, comma-separated or ranges)
            expl_vars_str = st.sidebar.text_input(
                "Enter explanatory variable indices (comma-separated or ranges, e.g. 2,4-6)",
                value="",
                key="expl_vars_str"
            )
            sort_explanatory = st.sidebar.checkbox("Sort explanatory variables?", value=True, key="sort_expl")
            
            
            # For death stages, we use the same parser.
            def parse_death_stages(s):
                return utils.parse_indices_string(s)
            
            if st.sidebar.button("Create explanatory data and sort variables", key="go_explanatory"):
                # Parse death stages and explanatory variable indices.
                death_stages = parse_death_stages(death_stages_str)
                expl_indices = utils.parse_indices_string(expl_vars_str)
                # Convert 1-based indices to 0-based.
                expl_indices = [i - 1 for i in expl_indices]
                # Map these indices to column names (if in range)
                explanatory_vars_list = [
                    data_expl.columns[i] for i in expl_indices if i < len(data_expl.columns)
                ]
                
                if not death_stages:
                    st.sidebar.error("Please specify at least one valid death stage.")
                else:
                    # Create the new data by adding a target column.
                    # In add_target_column, rows in the survival column matching any death stage get target=1; others get target=2.
                    new_data = survival.add_target_column(data_expl, survival_col, death_stages)
                    new_data.columns = [i+1 for i in range(len(new_data.columns))]
                    new_data.index = new_data.index+1
                    
                    # If the user opts to sort explanatory variables and has provided indices,
                    # use threedim.sort_explanatory_variables.
                    if sort_explanatory and explanatory_vars_list:
                        # Note: threedim.sort_explanatory_variables expects the data,
                        # the target column, and a list of explanatory columns. In our case,
                        # we provide a list inside a list (to match its usage in your code).
                        sorted_order = threedim.sort_explanatory_variables(new_data, survival_col, [explanatory_vars_list])
                        # sorted_order is likely a list of column indices. Convert to names.
                        sorted_expl_vars = [data_expl.columns[i]+1 for i in sorted_order]
                        # Print nicely using markdown.
                        sorted_order_str = ", ".join(map(str, sorted_expl_vars))
                        st.markdown(f"**Sorted explanatory variables:** {sorted_order_str}")
                    
                    # Save the new data in session state (like your other models)
                    st.session_state['results'][key].append(new_data)
                    new_tab_label = f"Explanatory Result {len(st.session_state['results'][key])}"
                    st.session_state['tabs'][key].append(new_tab_label)
                    
                    # Display the new data and provide a download button.
                    st.write(new_data)
                    csv = new_data.to_csv(index=False, header=False).encode('utf-8')
                    st.download_button("Download New CSV", data=csv, file_name="explanatory_data.csv", mime="text/csv")

elif selected_method == 'Loyalty' and st.session_state.get('transformed_data') is not None:
    # Use the transformed data for Loyalty (i.e. the contingency table computed in utils.load_data)
    key = selected_method.lower().replace(' ', '_')
    data = st.session_state['transformed_data']
    
    # Initialize storage for Loyalty results and tabs if not already done
    if key not in st.session_state['results']:
        st.session_state['results'][key] = []
    if key not in st.session_state['tabs']:
        st.session_state['tabs'][key] = ['Data']
    
    # Sidebar: Choose the Loyalty model
    selected_loyalty_model = st.sidebar.selectbox("Select Loyalty Model", 
                                                   ["M Model", "Q Model", "Explanatory Variable"])
    
    # ---- M Model Branch ----
    if selected_loyalty_model == "M Model":
        if st.sidebar.button("Go!", key="loyalty_m_go"):
            result = loyalty.m_model_computation(data)
            report = loyalty.m_model_report(result)  # report: list of items (text and DataFrames)
            fig = loyalty.bra_brl_plot(result['bra'], result['brl'])
            # Package result as a group dictionary with report and plot
            group_result = {
                "mode": "loyalty_m",
                "report": report,
                "plot": {"name": "M Model Plot", "figure": fig}
            }
            st.session_state['results'][key].append(group_result)
            new_tab_label = f"M Model {len(st.session_state['results'][key])}"
            st.session_state['tabs'][key].append(new_tab_label)
    
    # ---- Model Report Branch ----
    elif selected_loyalty_model == "Q Model":
        if st.sidebar.button("Go!", key="loyalty_q_go"):
            result = loyalty.q_model_computation(data)
            report = loyalty.q_model_report(result)
            fig = loyalty.bra_brl_plot(result['bra'], result['brl'])
            group_result = {
                "mode": "loyalty_q",
                "report": report,
                "plot": {"name": "Q Model Plot", "figure": fig}
            }
            st.session_state['results'][key].append(group_result)
            new_tab_label = f"Q Model {len(st.session_state['results'][key])}"
            st.session_state['tabs'][key].append(new_tab_label)
    
    # ---- Explanatory Variable Branch ----
    elif selected_loyalty_model == "Explanatory Variable":
        # The loyalty transformation for explanatory variables is handled by loyalty.explanatory_input and compute_explanatory_df.
        loyalty.explanatory_input()
        
        expl_vars_str = st.sidebar.text_input(
         "Enter explanatory variable indices (e.g. 2,4-6)",
         value="",
         key="loyalty_ex_expl_vars"
    )
        sort_explanatory = st.sidebar.checkbox(
         "Sort explanatory variables?",
         value=True,
         key="loyalty_ex_sort"
    )
        if st.sidebar.button("Go!", key="loyalty_ex_go"):
            # Compute the new DataFrame using the purchase conditions already entered.
            new_df = loyalty.compute_explanatory_df()
            if new_df is not None:
                # If sorting is requested and the user provided explanatory indices,
                # parse them and sort the explanatory variables.
                if sort_explanatory and expl_vars_str:
                    expl_indices = utils.parse_indices_string(expl_vars_str)
                    # Convert 1-based indices to 0-based
                    expl_indices = [i - 1 for i in expl_indices]
                    # Map these indices to column names from new_df.
                    explanatory_vars_list = [
                        new_df.columns[i] for i in expl_indices if i < len(new_df.columns)
                    ]
                    if sort_explanatory and explanatory_vars_list:
                        # threedim.sort_explanatory_variables expects: (data, target_col, [explanatory_cols])
                        sorted_order = threedim.sort_explanatory_variables(new_df, st.session_state.get('survival_col'), [explanatory_vars_list])
                        # Convert sorted indices to names (assuming new_df columns correspond)
                        sorted_expl_vars = [new_df.columns[i]+1 for i in sorted_order]
                        sorted_order_str = ", ".join(map(str, sorted_expl_vars))
                        st.markdown(f"**Sorted explanatory variables:** {sorted_order_str}")
                
                st.session_state['results'][key].append(new_df)
                new_tab_label = f"Explanatory Result {len(st.session_state['results'][key])}"
                st.session_state['tabs'][key].append(new_tab_label)
                st.write(new_df)
                csv = new_df.to_csv(index=False, header=False).encode('utf-8')
                st.download_button("Download New CSV", data=csv, file_name="loyalty_explanatory.csv", mime="text/csv")

    
    
# Create tabs if we have a contingency table
if selected_method == 'Independence' and st.session_state.get('transformed_data') is not None:
    key='independence'
    tabs = st.tabs(st.session_state['tabs'][key])
    
    # First tab: show the contingency table
    with tabs[0]:
        st.subheader("Contingency Table")
        # Optionally show a brief message
        st.write("Data loaded and transformed:")
        
        st.dataframe(utils.clean_df(st.session_state['transformed_data']))
    
    # Additional tabs: each analysis result
    for idx, res in enumerate(st.session_state['results'][key], start=1):
        with tabs[idx]:
            # st.subheader(st.session_state['tabs'][key][idx])
            if isinstance(res, list):
                for item in res:
                    if isinstance(item, pd.DataFrame):
                        st.dataframe(utils.clean_df(item))
                    else:
                        st.write(item)
            else:
                st.write(res)
                
elif selected_method == '3-Dimensional' and st.session_state.get('transformed_data') is not None:
    key = selected_method.lower().replace(' ', '_')
    tabs = st.tabs(st.session_state['tabs'][key])
    
    # First tab: show the contingency table
    with tabs[0]:
        st.subheader("Contingency Table")
        # Optionally show a brief message
        st.write("Data loaded and transformed:")
        st.dataframe(st.session_state['transformed_data'])
    
    # Additional tabs for results
    for idx, res in enumerate(st.session_state['results'][key], start=1):
        with tabs[idx]:
            # If it's a normal result (list of strings/DF), we just display
            # If it's the target variable result, we have 2 sub-tabs inside
            if isinstance(res, dict) and res.get("mode") == "target_variable":
                # Sub-tabs: "Output" and "Model Value"
                sub_tabs = st.tabs(["Output", "Model Value"])
                with sub_tabs[0]:
                    display_result_items(res["report"])
                with sub_tabs[1]:
                    # We call your model_value_tab(...) function
                    threedim.model_value_tab(res["model_value"])
            else:
                # It's a normal list of text/DF
                if isinstance(res, list):
                    display_result_items(res)
                else:
                    # We have more results than tabs - this shouldn't happen but we handle it gracefully
                    st.error(f"Error: More results than tabs. Missing tab for result {idx}")
                    # Create a new tab for this result
                    st.session_state['tabs'][key].append(f"Result {idx}")
                    st.warning("Tab has been created. Please refresh the page to see it.")

elif selected_method == 'N-Dimensional' and st.session_state.get('raw_data') is not None:
    key = selected_method.lower().replace(' ', '_')
    tabs = st.tabs(st.session_state['tabs'][key])
    
    # First tab: Display the raw data (or a summary thereof)
    with tabs[0]:
        st.subheader("Data")
        st.dataframe(data)
    
    # Additional tabs: for each computed result
    for idx, res in enumerate(st.session_state['results'][key], start=1):
        with tabs[idx]:
            # For target-variable mode, create two sub-tabs: "Output" and "Model Value"
            if isinstance(res, dict) and res.get("mode") == "target_variable":
                sub_tabs = st.tabs(["Result",'Detailed Models', "Model Value"])
                with sub_tabs[0]:
                    display_result_items(res["report"])
                with sub_tabs[1]:
                    ndim.detailed_models_tab(res['detailed'])
                with sub_tabs[2]:
                    ndim.model_value_tab(res["model_value"])
            else:
                # For other result formats (if any), simply display them.
                display_result_items(res)

elif selected_method=='Survival' and st.session_state['transformed_data'] is not None:
    key = selected_method.lower().replace(' ', '_')
    tabs = st.tabs(st.session_state['tabs'][key])
    
    with tabs[0]:
        st.subheader('Data')
        st.dataframe(data)
        
    group_results = st.session_state['results'][key]
    for idx, result_item in enumerate(st.session_state['results'][key], start=1):
        with tabs[idx]:
            # Check if the result_item is a dictionary with a "group" key
            if isinstance(result_item, dict) and "group" in result_item:
                start, end = result_item["group"]
                st.subheader(f"Stages {start}-{end}")
                sub_tab_names = ["Homogeneous", "ACC/DC"]
                if "homogeneous_plot" in result_item:
                    sub_tab_names.append("Homogeneous Plot")
                if "acc_dc_plot" in result_item:
                    sub_tab_names.append("ACC/DC Plot")
                sub_tabs = st.tabs(sub_tab_names)
                with sub_tabs[0]:
                    for item in result_item["homogeneous"]:
                        if isinstance(item, pd.DataFrame):
                            st.dataframe(item)
                        else:
                            st.write(item)
                with sub_tabs[1]:
                    for item in result_item["acc_dc"]:
                        if isinstance(item, pd.DataFrame):
                            st.dataframe(item)
                        else:
                            st.write(item)
                if "homogeneous_plot" in result_item:
                    with sub_tabs[2]:
                        for item in result_item["homogeneous_plot"]:
                            st.write(f"### {item['name']}")
                            survival.plot_tab(item)
                if "acc_dc_plot" in result_item:
                    # Adjust index accordingly if both plot tabs exist.
                    plot_index = 3 if "homogeneous_plot" in result_item else 2
                    with sub_tabs[plot_index]:
                        for item in result_item["acc_dc_plot"]:
                            st.write(f"### {item['name']}")
                            survival.plot_tab(item)
            else:
                # For flat results (e.g. from branches that don't use splining)
                if isinstance(result_item, list):
                    for item in result_item:
                        if isinstance(item, pd.DataFrame):
                            st.dataframe(item)
                        else:
                            st.write(item)
                else:
                    st.write(result_item)

elif selected_method=='Loyalty' and st.session_state['transformed_data'] is not None:
    key = selected_method.lower().replace(' ', '_')
    data = st.session_state['transformed_data']
    tabs = st.tabs(st.session_state['tabs'][key])
     # First tab: Display the raw transformed data
    with tabs[0]:
        st.subheader("Data")
        st.write("Data loaded and transformed:")
        st.dataframe(utils.clean_df(data))
    
    # Additional tabs: Display each Loyalty result
    for idx, res in enumerate(st.session_state['results'][key], start=1):
        with tabs[idx]:
            # For M Model and Model Report, we expect a group dictionary with "mode", "report", and "plot"
            if isinstance(res, dict) and res.get("mode") in ["loyalty_m", "loyalty_q"]:
                sub_tabs = st.tabs(["Report", "Plot"])
                with sub_tabs[0]:
                    for item in res["report"]:
                        if isinstance(item, pd.DataFrame):
                            st.dataframe(item)
                        else:
                            st.write(item)
                with sub_tabs[1]:
                    st.write(f"### {res['plot']['name']}")
                    st.pyplot(res['plot']['figure'])
            else:
                # For explanatory variable results (which are DataFrames)
                if isinstance(res, pd.DataFrame):
                    st.dataframe(res)
                else:
                    st.write(res)

elif selected_method == 'Ranking' and st.session_state.get('transformed_data') is not None:
    key = selected_method.lower().replace(' ', '_')
    data = st.session_state['transformed_data']
    
    # Initialize storage for Ranking results and tabs if not already done
    if key not in st.session_state['results']:
        st.session_state['results'][key] = []
    if key not in st.session_state['tabs']:
        st.session_state['tabs'][key] = ['Data']
    if key not in st.session_state['comp_results']:
        st.session_state['comp_results'][key] = []
    
    # Model selection for Ranking analysis
    ranking_model = st.sidebar.selectbox(
        "Select Ranking Model",
        ["Exploratory", "Confirmatory", "Explanatory Variable"],
        key="ranking_model_selection"
    )
    
    # Only show satisfaction constraint and upper polarity for Exploratory and Confirmatory
    if ranking_model in ["Exploratory", "Confirmatory"]:
        # User input for satisfaction constraint
        satisfaction_constraint = st.sidebar.selectbox(
            "Satisfaction constraint",
            ["None", "First", "Last"],
            key="ranking_sat_constraint"
        )
        
        # User input for upper polarity index
        upper_polarity_str = st.sidebar.text_input(
            "Enter upper polarity index (e.g., '4-7' for levels 4 to 7, or comma-separated numbers)",
            value="",
            key="ranking_upper_polarity"
        )
        
        # Parse the upper polarity indices if provided
        def parse_upper_polarity(s):
            try:
                # Check if it's a range format like "4-7"
                if "-" in s and s.count("-") == 1 and "," not in s:
                    start, end = map(int, s.split("-"))
                    return list(range(start, end + 1))
                else:
                    # Otherwise, treat as comma-separated values
                    parts = [p.strip() for p in s.split(",") if p.strip()]
                    return [int(p) for p in parts]
            except Exception as e:
                st.sidebar.error(f"Invalid upper polarity index input: {str(e)}")
                return None
        
        upper_polarity_idx = parse_upper_polarity(upper_polarity_str) if upper_polarity_str else None
    
    # For Explanatory Variable, show different inputs
    elif ranking_model == "Explanatory Variable":
        # Use raw_data for this analysis
        data_expl = st.session_state['raw_data']
        
        # Check if we have the necessary column information
        brand_col = st.session_state.get('ranking_brand_col')
        ranking_col = st.session_state.get('ranking_level_col')

        
        if brand_col is None or ranking_col is None:
            st.error("Please select the brand and ranking columns in the data format section first.")
        else:
            st.sidebar.subheader("Explanatory Variable Options")
            
            # Input for brands to consider
            brands_str = st.sidebar.text_input(
                "Enter brand indices (comma-separated or ranges, e.g. 1,3-5)",
                value="",
                key="ranking_expl_brands"
            )
            
            # Input for satisfaction level range
            ranking_range_str = st.sidebar.text_input(
                "Enter satisfaction level range (e.g. 4-5 for levels 4 to 5)",
                value="",
                key="ranking_expl_range"
            )
            
            # Input for explanatory variable indices
            expl_vars_str = st.sidebar.text_input(
                "Enter explanatory variable indices (comma-separated or ranges, e.g. 2,4-6)",
                value="",
                key="ranking_expl_vars"
            )
            
            sort_explanatory = st.sidebar.checkbox(
                "Sort explanatory variables?",
                value=True,
                key="ranking_expl_sort"
            )

            if st.sidebar.button("Go!", key="ranking_expl_go"):
                try:
                    # Parse brands and ranking range
                    brands = utils.parse_indices_string(brands_str)
                    ranking_range = utils.parse_indices_string(ranking_range_str)
                    
                    if not brands or not ranking_range or len(ranking_range) != 2:
                        st.sidebar.error("Please specify valid brand indices and satisfaction level range.")
                    else:
                        # Parse explanatory variable indices
                        expl_indices = utils.parse_indices_string(expl_vars_str)
                        # Convert 1-based indices to 0-based
                        expl_indices = [i - 1 for i in expl_indices]
                        # Map these indices to column names
                        explanatory_vars_list = [
                            data_expl.columns[i] for i in expl_indices if i < len(data_expl.columns)
                        ]
                        
                        # Create new data with target column using the correct columns
                        new_data = ranking.add_target_column_ranking(
                            data_expl, 
                            brands, 
                            ranking_range,
                            brand_col=brand_col,
                            ranking_col=ranking_col
                        )
                        
                        if new_data is not None:
                            # Rename columns to match expected format
                            new_data.columns = [i+1 for i in range(len(new_data.columns))]
                            new_data.index = new_data.index + 1
                            
                            # Sort explanatory variables if requested
                            if sort_explanatory and explanatory_vars_list:
                                # Get the target column index (should be the last column)
                                target_col = len(new_data.columns)-1
                                # Sort the explanatory variables
                                sorted_order = threedim.sort_explanatory_variables(new_data, target_col, [explanatory_vars_list])
                                # Convert sorted indices to names
                                sorted_expl_vars = [data_expl.columns[i]+1 for i in sorted_order]
                                # Display sorted order
                                sorted_order_str = ", ".join(map(str, sorted_expl_vars))
                                st.markdown(f"**Sorted explanatory variables:** {sorted_order_str}")
                            
                            # Save results and create new tab
                            st.session_state['results'][key].append(new_data)
                            new_tab_label = f"Explanatory Result {len(st.session_state['results'][key])}"
                            st.session_state['tabs'][key].append(new_tab_label)
                            
                            # Display data and provide download option
                            st.write(new_data)
                            csv = new_data.to_csv(index=False, header=False).encode('utf-8')
                            st.download_button("Download New CSV", data=csv, file_name="ranking_explanatory.csv", mime="text/csv")
                        else:
                            st.error("Error creating target column. Please check your inputs.")
                except Exception as e:
                    st.error(f"Error in explanatory analysis: {str(e)}")
                    import traceback
                    st.code(traceback.format_exc())
                
    
    # Display Ranking results in tabs.
    tabs = st.tabs(st.session_state['tabs'][key])
    with tabs[0]:
        st.subheader("Data")
        st.dataframe(utils.clean_df(data))
    
    # Ensure we don't try to access tabs that don't exist
    num_tabs = len(st.session_state['tabs'][key])
    for idx, res in enumerate(st.session_state['results'][key], start=1):
        if idx < num_tabs:  # Make sure the tab index exists
            with tabs[idx]:
                # For result items from confirmatory_report function
                if isinstance(res, list):
                    # Create three sub-tabs instead of displaying results directly
                    sub_tabs = st.tabs(["Analysis Results", "Average Satisfaction", "Distribution by Brand Groups"])
                    
                    # First tab: Display the report items
                    with sub_tabs[0]:
                        display_result_items(res)
                    
                    # Second and third tabs: Display plots if comp_results is available
                    if 'comp_results' in st.session_state and key in st.session_state['comp_results']:
                        if idx <= len(st.session_state['comp_results'][key]):
                            comp_data = st.session_state['comp_results'][key][idx-1]
                            if comp_data and 'analysis_type' in comp_data:
                                # Second tab: Average satisfaction plot
                                with sub_tabs[1]:
                                    try:
                                        fig = ranking.plot_confirmatory_brand_clusters(comp_data['results'])
                                        st.pyplot(fig)
                                    except Exception as e:
                                        st.error(f"Error generating brand clusters plot: {str(e)}")
                                
                                # Third tab: Distribution plot
                                with sub_tabs[2]:
                                    try:
                                        fig = ranking.plot_confirmatory_brand_distribution(comp_data['results'])
                                        st.pyplot(fig)
                                    except Exception as e:
                                        st.error(f"Error generating distribution plot: {str(e)}")
                            else:
                                # Handle case where comp_data doesn't have analysis_type
                                for i in range(1, 3):
                                    with sub_tabs[i]:
                                        st.warning("Plot data not available.")
                        else:
                            # Handle case where idx > len(comp_results)
                            for i in range(1, 3):
                                with sub_tabs[i]:
                                    st.warning("Plot data not available for this result.")
                    else:
                        # Handle case where comp_results is not available
                        for i in range(1, 3):
                            with sub_tabs[i]:
                                st.warning("No plot data available. Run analysis again to see plots.")
                
                # For old-style group results with mode="ranking_confirmatory"
                elif isinstance(res, dict) and res.get("mode") in ["ranking_confirmatory", "ranking_exploratory", "ranking_explanatory", "ranking_explanatory_var"]:
                    sub_tabs = st.tabs(["Report", "Plots"])
                    with sub_tabs[0]:
                        display_result_items(res.get("report", []))
                    with sub_tabs[1]:
                        if "plots" in res:
                            for plot_name, plot in res["plots"].items():
                                st.markdown(f"#### {plot_name}")
                                st.pyplot(plot)
                        else:
                            st.warning("No plots available for this analysis.")
                else:
                    # Fallback for any other type of result
                    st.write(res)
        else:
            # We have more results than tabs - this shouldn't happen but we handle it gracefully
            st.error(f"Error: More results than tabs. Missing tab for result {idx}")
            # Create a new tab for this result
            st.session_state['tabs'][key].append(f"Result {idx}")
            st.warning("Tab has been created. Please refresh the page to see it.")
