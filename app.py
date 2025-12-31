import streamlit as st
import pandas as pd
import numpy as np
import independence
import threedim
import ndim
import survival
import loyalty
import ranking
import spacing
import scale_conversion
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
                st.dataframe(utils.clean_df(item), hide_index=True)
            elif isinstance(item, str):
                # Check if it's markdown content (contains markdown headers or formatting)
                if item.strip().startswith('#') or '**' in item or '*' in item:
                    # Use markdown for formatted text
                    st.markdown(item)
                elif '\n' in item and not (item.strip().startswith('#') or '**' in item):
                    # For debug logs with newlines but no markdown, use code block formatting
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
                st.plotly_chart(fig, use_container_width=True)
            else:  # confirmatory
                # Use the computation results directly
                fig = ranking.plot_confirmatory_brand_clusters(compute_results)
                st.plotly_chart(fig, use_container_width=True)
        except Exception as e:
            st.error(f"Error generating average satisfaction plot: {str(e)}")
            st.exception(e)
    
    # Distribution by Brand Groups Plot tab
    with tabs[2]:
        try:
            if analysis_type == 'exploratory':
                fig = ranking.plot_exploratory_brand_distribution(compute_results, contingency_table)
                st.plotly_chart(fig, use_container_width=True)
            else:  # confirmatory
                # Use the computation results directly
                fig = ranking.plot_confirmatory_brand_distribution(compute_results)
                st.plotly_chart(fig, use_container_width=True)
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
if 'last_method' not in st.session_state:
    st.session_state['last_method'] = None
if 'results' not in st.session_state:
    st.session_state['results'] = {}
if 'comp_results' not in st.session_state:
    st.session_state['comp_results'] = {}
if 'tabs' not in st.session_state:
    st.session_state['tabs'] = {}

# Pre-initialize ALL widget session states for model value tabs to prevent tab jumping
# This must happen BEFORE any tabs are created
for method_key in ['n-dimensional', '3-dimensional']:
    if method_key in st.session_state.get('results', {}):
        for idx, res in enumerate(st.session_state['results'][method_key], start=1):
            if isinstance(res, dict) and res.get("mode") == "target_variable":
                if method_key == 'n-dimensional':
                    result_name = res["model_value"]["name"]
                    # Initialize widgets for ALL models (not just one)
                    computed = res.get('detailed', {}).get('computed')
                    if computed is not None:
                        number_of_models = len(computed.get('model', []))
                        for model_idx in range(1, number_of_models + 1):
                            unique_key = f"{result_name}_model_{model_idx}"
                            # Number inputs for rewards for each model
                            if f'o1p1 {unique_key}' not in st.session_state:
                                st.session_state[f'o1p1 {unique_key}'] = 1.0
                            if f'o1p2 {unique_key}' not in st.session_state:
                                st.session_state[f'o1p2 {unique_key}'] = -1.0
                            if f'o2p1 {unique_key}' not in st.session_state:
                                st.session_state[f'o2p1 {unique_key}'] = -1.0
                            if f'o2p2 {unique_key}' not in st.session_state:
                                st.session_state[f'o2p2 {unique_key}'] = 1.0
                            if f'propensity threshold {unique_key}' not in st.session_state:
                                st.session_state[f'propensity threshold {unique_key}'] = 0.5
                            # Plot and compute results storage for each model
                            if f'plots_{unique_key}' not in st.session_state:
                                st.session_state[f'plots_{unique_key}'] = None
                            if f'compute_{unique_key}' not in st.session_state:
                                st.session_state[f'compute_{unique_key}'] = None
                elif method_key == '3-dimensional':
                    result_name = res["model_value"]["name"]
                    unique_key = f"{result_name}_{idx}"
                    # Plot and compute results storage
                    if f'plots_{unique_key}' not in st.session_state:
                        st.session_state[f'plots_{unique_key}'] = None
                    if f'compute_results_{unique_key}' not in st.session_state:
                        st.session_state[f'compute_results_{unique_key}'] = None
                    # Number inputs for rewards
                    if f'o1p1 {unique_key}' not in st.session_state:
                        st.session_state[f'o1p1 {unique_key}'] = 1.0
                    if f'o1p2 {unique_key}' not in st.session_state:
                        st.session_state[f'o1p2 {unique_key}'] = -1.0
                    if f'o2p1 {unique_key}' not in st.session_state:
                        st.session_state[f'o2p1 {unique_key}'] = -1.0
                    if f'o2p2 {unique_key}' not in st.session_state:
                        st.session_state[f'o2p2 {unique_key}'] = 1.0
                    if f'propensity threshold {unique_key}' not in st.session_state:
                        st.session_state[f'propensity threshold {unique_key}'] = 0.5

# Pre-initialize Spacing Models widget session states to prevent tab jumping
if 'spacing_models' in st.session_state.get('results', {}):
    for idx, res in enumerate(st.session_state['results']['spacing_models'], start=1):
        if isinstance(res, dict) and res.get("mode") == "spacing_specific_model":
            unique_key = f"spacing_or_{idx}"
            model_result = res.get("result", {})
            num_row = len(model_result.get('mu', []))
            num_col = len(model_result.get('nu', []))
            
            # Pre-initialize number inputs for odd ratio computation
            if f"{unique_key}_row1" not in st.session_state:
                st.session_state[f"{unique_key}_row1"] = 1
            if f"{unique_key}_row2" not in st.session_state:
                st.session_state[f"{unique_key}_row2"] = min(2, num_row) if num_row > 1 else 1
            if f"{unique_key}_col1" not in st.session_state:
                st.session_state[f"{unique_key}_col1"] = 1
            if f"{unique_key}_col2" not in st.session_state:
                st.session_state[f"{unique_key}_col2"] = min(2, num_col) if num_col > 1 else 1

# Check if method changed and reset transformed_data if needed
if st.session_state.get('last_method') is not None and st.session_state['last_method'] != selected_method:
    # Method has changed since last run
    st.session_state['transformed_data'] = None
    # We keep raw_data since it might be usable across methods

# Update the last method
st.session_state['last_method'] = selected_method


# Sidebar: File uploader
uploaded_file = st.sidebar.file_uploader("Upload data file", type='csv', key='file_uploader')

# Load/transform data when a file is uploaded
# ALWAYS call load_data to show the column selection UI, regardless of existing data
if uploaded_file is not None:
    # Check if this is a different file than before
    is_new_file = st.session_state.get('uploaded_file_name') != uploaded_file.name
    
    # If it's a new file, reset everything to allow fresh column selection
    if is_new_file:
        st.session_state['transformed_data'] = None
        st.session_state['uploaded_file_name'] = uploaded_file.name
        st.session_state['data_modified'] = False
        st.session_state['last_column_selection'] = None
    
    # Always call load_data to display the column selection UI
    new_raw_data, new_transformed_data = utils.load_data(uploaded_file, selected_method)
    
    # Update raw_data always
    if new_raw_data is not None:
        st.session_state['raw_data'] = new_raw_data
    
    # Only update transformed_data if:
    # 1. We don't have any yet (first load), OR
    # 2. Data hasn't been modified (collapsed) and new data is available
    # 3. Column selection actually changed (detected by comparing data shapes or a hash)
    if new_transformed_data is not None:
        # Check if this is initial load or if we should update
        current_data = st.session_state.get('transformed_data')
        should_update = (
            current_data is None or  # No data yet
            (not st.session_state.get('data_modified', False) and  # Data not collapsed
             (current_data.shape != new_transformed_data.shape or  # Different shape means different selection
              not current_data.equals(new_transformed_data)))  # Or different content
        )
        
        if should_update:
            st.session_state['transformed_data'] = new_transformed_data
            st.session_state['data_modified'] = False
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
    # Store the contingency table for future reference
    st.session_state['indep_contingency_table'] = data
    
    # ------------------------
    # Collapse Data Option (collapsible)
    # ------------------------
    with st.sidebar.expander("Permanent Data Collapsing", expanded=False):
        # Row groups input
        row_groups_str = st.text_input(
            "Row groups to collapse (optional)", 
            value="",
            help=utils.RANGE_INPUT_HELP,
            key="collapse_row_groups"
        )
        
        # Column groups input  
        col_groups_str = st.text_input(
            "Column groups to collapse (optional)", 
            value="",
            help=utils.RANGE_INPUT_HELP,
            key="collapse_col_groups"
        )
        
        # Collapse and Revert buttons side by side
        col1, col2 = st.columns(2)
        
        # Revert button - only show if data has been modified (collapsed)
        with col2:
            if st.session_state.get('data_modified', False):
                if st.button("↩ Revert", key="revert_collapse_btn", help="Restore original uncollapsed data"):
                    st.session_state['data_modified'] = False
                    st.session_state['transformed_data'] = None  # Force reload from original
                    st.rerun()
        
        # Collapse button
        with col1:
            collapse_clicked = st.button("Collapse", key="collapse_data_btn")
        
        if collapse_clicked:
            row_groups = None
            col_groups = None
            
            # Parse row groups if provided
            if row_groups_str.strip():
                try:
                    row_groups = utils.parse_text_groups(row_groups_str)
                    if row_groups is None:
                        st.error("Invalid row groups format. Please check the format.")
                except Exception as e:
                    st.error(f"Error parsing row groups: {str(e)}")
                    row_groups = None
            
            # Parse column groups if provided
            if col_groups_str.strip():
                try:
                    col_groups = utils.parse_text_groups(col_groups_str)
                    if col_groups is None:
                        st.error("Invalid column groups format. Please check the format.")
                except Exception as e:
                    st.error(f"Error parsing column groups: {str(e)}")
                    col_groups = None
            
            # Perform collapse if at least one group is specified
            if row_groups is not None or col_groups is not None:
                try:
                    collapsed_data = utils.collapse_data(data, row_groups, col_groups, as_data_metrix=True)
                    
                    # Update the transformed data in session state 
                    st.session_state['transformed_data'] = collapsed_data
                    st.session_state['indep_contingency_table'] = collapsed_data
                    st.session_state['data_modified'] = True  # Mark data as modified to prevent overwriting
                    
                    st.rerun()  # Rerun to show the Revert button immediately
                except Exception as e:
                    st.error(f"Error collapsing data: {str(e)}")
                    import traceback
                    st.code(traceback.format_exc())
            else:
                st.warning("Please specify at least one group (row or column) to collapse.")
    
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
        # Use the current transformed data (which may have been collapsed)
        current_data = st.session_state['transformed_data']
        pair = independence.ztest_read_pair(current_data, orientation_ztest)

    # -----------------------------------
    # 3) Chi Square Specific Inputs
    # -----------------------------------    
    row_groups = None
    col_groups = None
    if selected_model == "Collapse Chi-Sq Test":
        row_groups_str = st.sidebar.text_input("Row groups", help=utils.RANGE_INPUT_HELP)
        col_groups_str = st.sidebar.text_input("Column groups", help=utils.RANGE_INPUT_HELP)
        
        if row_groups_str.strip():
            row_groups = utils.parse_text_groups(row_groups_str)
            if row_groups is None:
                st.sidebar.warning("Invalid row groups string. Please check the format.")
        if col_groups_str.strip():
            col_groups = utils.parse_text_groups(col_groups_str)
            if col_groups is None:
                st.sidebar.warning("Invalid column groups string. Please check the format.")
    
    
    if st.sidebar.button("Go!"):
        # Always use the current session state data (which may have been collapsed)
        current_data = st.session_state['transformed_data']
        
        if selected_model == 'Independence Model':
            result = independence.independence_model_report(current_data)
            
        elif selected_model == 'ANOAS Model':
            if groups_anoas is None:
                result = "Invalid or missing group string. Cannot compute ANOAS."
            else:
                result = independence.anoas_model_report(current_data, groups_anoas, orientation_anoas)
                
        elif selected_model == "Predictors' Proportion":
            result = independence.predictors_proportion_report(current_data)
            
        elif selected_model == "Log-Odds Z-Test":
            if pair is None:
                result = "Invalid pair of rows/columns. Cannot compute Log-Odds Z-Test."
            else:  
                result = independence.ztest_report(contingency_table=current_data, pair=pair,orientation=orientation_ztest)
            
        elif selected_model == "Collapse Chi-Sq Test":
            if row_groups is None or col_groups is None:
                result = "Invalid group string(s) provided. Cannot compute Collapse Chi-Sq Test."
            else:
                result = independence.chisq_test_report(current_data, row_groups, col_groups)
            
        else:
            result = "Selected model not implemented."
        
        st.session_state['results'][key].append(result)
        new_tab_label = f"Result {len(st.session_state['results'][key])}"
        st.session_state['tabs'][key].append(new_tab_label)
        st.success(f"✅ Analysis complete! Select '{new_tab_label}' from the dropdown below.")

elif selected_method == '3-Dimensional' and st.session_state['transformed_data'] is not None:
    key = selected_method.lower().replace(' ', '_')
    data = st.session_state['transformed_data']
    
    try:
        # Try to transform data to 3D array
        data_for_computations = threedim.transform_to_3d_array(data)
        
        if data_for_computations is None:
            st.warning("The data format is not suitable for 3D analysis. Please select X, Y, and Z columns to continue.")
        else:
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
                    st.success(f"✅ Analysis complete! Select '{new_tab_label}' from the dropdown below.")
            
            # ---------------------------
            # B) Select coefficients
            # ---------------------------
            elif computation_choice == "Select coefficients":
                # Show hierarchical checkboxes
                st.sidebar.write("Select which coefficients you want in the model.")
                selected_coefs = threedim.select_coefs()  # This calls your function that updates X, Y, Z if X Y is chosen, etc.
                
                # Then we parse those coefs into which_x
                if st.sidebar.button("Go!"):
                    st.write(f'data_for_computations.shape: {data_for_computations.shape}')
                    which_x = threedim.parse_selected_coefs(selected_coefs)
                    comp = threedim.select_coef_computation(data_for_computations, which_x)
                    computed_result = threedim.select_coef_report(comp)
                    # Save in session state
                    st.session_state['results'][key].append(computed_result)
                    new_tab_label = f"Result {len(st.session_state['results'][key])}"
                    st.session_state['tabs'][key].append(new_tab_label)
                    st.success(f"✅ Analysis complete! Select '{new_tab_label}' from the dropdown below.")

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
                    new_tab_label = f"Result {len(st.session_state['results'][key])}"
                    st.session_state['tabs'][key].append(new_tab_label)
                    st.success(f"✅ Analysis complete! Select '{new_tab_label}' from the dropdown below.")
    except Exception as e:
        st.error(f"Error processing data for 3D analysis: {str(e)}")
        import traceback
        st.code(traceback.format_exc())
        st.warning("Please ensure you've selected X, Y, and Z columns in the file uploader options. The current data might be from a different analysis method.")
        st.info("Upload your data file again and select the appropriate columns for 3D analysis.")

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
        st.success(f"✅ Analysis complete! Select '{new_tab_label}' from the dropdown below.")
            
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
            st.success(f"✅ Analysis complete! Select results from the dropdown below.")
                
    elif selected_model == "Out-of-Sample Splining":
        training_portion = st.sidebar.number_input("Training portion (in %)", min_value=0.0, max_value=100.0, value=50.0, step=1.0)
        reproducible = st.sidebar.checkbox("Reproducible?", value=True)
        if st.sidebar.button("Go Training!"):
            # Compute the training results using the existing functions in survival.py
            oos_result = survival.oos_training_computation(death_per_period, still_survive)
            report = survival.oos_training_report(oos_result, float(training_portion))
            st.session_state['results'][key].append(report)
            new_tab_label = f"Result {len(st.session_state['results'][key])}"
            st.session_state['tabs'][key].append(new_tab_label)
            st.success(f"✅ Training complete! Select '{new_tab_label}' from the dropdown below.")
        
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
            st.success(f"✅ Testing complete! Select results from the dropdown below.")
    
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
                "Enter death stages",
                value="",
                help=utils.RANGE_INPUT_HELP,
                key="expl_death_stages"
            )
            # Input for explanatory variable indices as a string (1-based, comma-separated or ranges)
            expl_vars_str = st.sidebar.text_input(
                "Enter explanatory variable indices",
                value="",
                help=utils.RANGE_INPUT_HELP,
                key="expl_vars_str"
            )
            sort_explanatory = st.sidebar.checkbox("Sort explanatory variables?", value=True, key="sort_expl")
            
            
            # For death stages, we use the same parser.
            def parse_death_stages(s):
                return utils.parse_indices_string(s)
            
            if st.sidebar.button("Go!", key="go_explanatory"):
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
                    
                    # Sort explanatory variables if requested
                    if sort_explanatory and explanatory_vars_list:
                        # Get the target column index (should be the last column)
                        target_col = len(new_data.columns) - 1  # Last column is target
                        # Sort the explanatory variables
                        sorted_indices = threedim.sort_explanatory_variables(new_data, target_col, [explanatory_vars_list])
                        # The sorted_indices are the actual explanatory column indices from our list
                        # Convert these to 1-based user-friendly indices
                        sorted_expl_vars = [index + 1 for index in sorted_indices]
                        # Display sorted order
                        sorted_order_str = ", ".join(map(str, sorted_expl_vars))
                        st.markdown(f"**Sorted explanatory variables:** {sorted_order_str}")
                    
                    # Save the new data in session state (like your other models)
                    st.session_state['results'][key].append(new_data)
                    new_tab_label = f"Explanatory Result {len(st.session_state['results'][key])}"
                    st.session_state['tabs'][key].append(new_tab_label)
                    st.success(f"✅ Data created! Select '{new_tab_label}' from the dropdown below.")
                    
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
            st.success(f"✅ M Model complete! Select '{new_tab_label}' from the dropdown below.")
    
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
            st.success(f"✅ Q Model complete! Select '{new_tab_label}' from the dropdown below.")
    
    # ---- Explanatory Variable Branch ----
    elif selected_loyalty_model == "Explanatory Variable":
        # The loyalty transformation for explanatory variables is handled by loyalty.explanatory_input and compute_explanatory_df.
        loyalty.explanatory_input()
        
        expl_vars_str = st.sidebar.text_input(
         "Enter explanatory variable indices",
         value="",
         help=utils.RANGE_INPUT_HELP,
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
                    if expl_indices:
                        # The target is the last column ('new_column') which is at index len(new_df.columns) - 1
                        target_col_idx = len(new_df.columns) - 1
                        # threedim.sort_explanatory_variables expects integer indices
                        sorted_order = threedim.sort_explanatory_variables(new_df, target_col_idx, expl_indices)
                        # Convert sorted indices to 1-based for display
                        sorted_expl_vars = [i + 1 for i in sorted_order]
                        sorted_order_str = ", ".join(map(str, sorted_expl_vars))
                        st.markdown(f"**Sorted explanatory variables (by column index):** {sorted_order_str}")
                
                st.session_state['results'][key].append(new_df)
                new_tab_label = f"Explanatory Result {len(st.session_state['results'][key])}"
                st.session_state['tabs'][key].append(new_tab_label)
                st.success(f"✅ Data created! Select '{new_tab_label}' from the dropdown below.")
                st.write(new_df)
                csv = new_df.to_csv(index=False, header=False).encode('utf-8')
                st.download_button("Download New CSV", data=csv, file_name="loyalty_explanatory.csv", mime="text/csv")

    
    
# Create navigation if we have a contingency table
if selected_method == 'Independence' and st.session_state.get('transformed_data') is not None:
    key='independence'
    
    # Use selectbox for main navigation to preserve selection across reruns
    tab_options = st.session_state['tabs'][key]
    
    # Initialize tab selection in session state
    tab_select_key = f"{key}_tab_select"
    if tab_select_key not in st.session_state:
        st.session_state[tab_select_key] = tab_options[0] if tab_options else "Data"
    
    # Ensure current selection is valid
    if st.session_state[tab_select_key] not in tab_options:
        st.session_state[tab_select_key] = tab_options[0] if tab_options else "Data"
    
    selected_tab = st.selectbox(
        "Select Result",
        tab_options,
        key=tab_select_key,
        label_visibility="collapsed"
    )
    
    st.divider()
    
    # Find the index of the selected tab
    selected_idx = tab_options.index(selected_tab) if selected_tab in tab_options else 0
    
    # Display content based on selection
    if selected_idx == 0:
        # Data tab
        st.subheader("Contingency Table")
        st.write("Data loaded and transformed:")
        st.dataframe(utils.clean_df(st.session_state['transformed_data']))
    else:
        # Result tabs
        res = st.session_state['results'][key][selected_idx - 1]
        
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
    
    # Use selectbox for main navigation to preserve selection across reruns
    tab_options = st.session_state['tabs'][key]
    
    # Initialize tab selection in session state
    tab_select_key = f"{key}_tab_select"
    if tab_select_key not in st.session_state:
        st.session_state[tab_select_key] = tab_options[0] if tab_options else "Data"
    
    # Ensure current selection is valid
    if st.session_state[tab_select_key] not in tab_options:
        st.session_state[tab_select_key] = tab_options[0] if tab_options else "Data"
    
    selected_tab = st.selectbox(
        "Select Result",
        tab_options,
        key=tab_select_key,
        label_visibility="collapsed"
    )
    
    st.divider()
    
    # Find the index of the selected tab
    selected_idx = tab_options.index(selected_tab) if selected_tab in tab_options else 0
    
    # Display content based on selection
    if selected_idx == 0:
        # Data tab
        st.subheader("Contingency Table")
        st.write("Data loaded and transformed:")
        st.dataframe(utils.clean_df(st.session_state['transformed_data']))
    else:
        # Result tabs
        res = st.session_state['results'][key][selected_idx - 1]
        
        if isinstance(res, dict) and res.get("mode") == "target_variable":
            # Use radio buttons for sub-view selection
            view_key = f"3dim_view_{selected_idx}"
            if view_key not in st.session_state:
                st.session_state[view_key] = "Output"
            
            selected_view = st.radio(
                "Select View",
                ["Output", "Model Value"],
                key=view_key,
                horizontal=True,
                label_visibility="collapsed"
            )
            
            st.divider()
            
            if selected_view == "Output":
                display_result_items(res["report"])
            else:
                threedim.model_value_tab(res["model_value"], result_idx=selected_idx)
        else:
            # Normal list of text/DF
            if isinstance(res, list):
                display_result_items(res)
            else:
                st.write(res)

elif selected_method == 'N-Dimensional' and st.session_state.get('raw_data') is not None:
    key = selected_method.lower().replace(' ', '_')
    
    # Use selectbox for main navigation to preserve selection across reruns
    tab_options = st.session_state['tabs'][key]
    
    # Initialize tab selection in session state
    tab_select_key = f"{key}_tab_select"
    if tab_select_key not in st.session_state:
        st.session_state[tab_select_key] = tab_options[0] if tab_options else "Data"
    
    # Ensure current selection is valid
    if st.session_state[tab_select_key] not in tab_options:
        st.session_state[tab_select_key] = tab_options[0] if tab_options else "Data"
    
    selected_tab = st.selectbox(
        "Select Result",
        tab_options,
        key=tab_select_key,
        label_visibility="collapsed"
    )
    
    st.divider()
    
    # Find the index of the selected tab
    selected_idx = tab_options.index(selected_tab) if selected_tab in tab_options else 0
    
    # Display content based on selection
    if selected_idx == 0:
        # Data tab
        st.subheader("Data")
        st.dataframe(utils.clean_df(data))
    else:
        # Result tabs
        res = st.session_state['results'][key][selected_idx - 1]
        
        if isinstance(res, dict) and res.get("mode") == "target_variable":
            # Use radio buttons for sub-view selection
            view_key = f"ndim_view_{selected_idx}"
            if view_key not in st.session_state:
                st.session_state[view_key] = "Result"
            
            selected_view = st.radio(
                "Select View",
                ["Result", "Detailed Models", "Model Value"],
                key=view_key,
                horizontal=True,
                label_visibility="collapsed"
            )
            
            st.divider()
            
            if selected_view == "Result":
                display_result_items(res["report"])
            elif selected_view == "Detailed Models":
                ndim.detailed_models_tab(res['detailed'])
            else:
                ndim.model_value_tab(res["model_value"])
        else:
            # For other result formats (if any), simply display them.
            display_result_items(res)

elif selected_method=='Survival' and st.session_state['transformed_data'] is not None:
    key = selected_method.lower().replace(' ', '_')
    
    # Use selectbox for main navigation to preserve selection across reruns
    tab_options = st.session_state['tabs'][key]
    
    # Initialize tab selection in session state
    tab_select_key = f"{key}_tab_select"
    if tab_select_key not in st.session_state:
        st.session_state[tab_select_key] = tab_options[0] if tab_options else "Data"
    
    # Ensure current selection is valid
    if st.session_state[tab_select_key] not in tab_options:
        st.session_state[tab_select_key] = tab_options[0] if tab_options else "Data"
    
    selected_tab = st.selectbox(
        "Select Result",
        tab_options,
        key=tab_select_key,
        label_visibility="collapsed"
    )
    
    st.divider()
    
    # Find the index of the selected tab
    selected_idx = tab_options.index(selected_tab) if selected_tab in tab_options else 0
    
    # Display content based on selection
    if selected_idx == 0:
        # Data tab
        st.subheader('Data')
        # Clean the data, then rename rows to Stage 1, Stage 2, ..., Survive
        survival_display = utils.clean_df(data.copy())
        n_rows = len(survival_display)
        stage_names = [f'Stage {i}' for i in range(1, n_rows)] + ['Survive']
        survival_display.index = stage_names
        survival_display.index.name = 'Stage'
        st.dataframe(survival_display)
    else:
        # Result tabs
        result_item = st.session_state['results'][key][selected_idx - 1]
        idx = selected_idx
        
        # Check if the result_item is a dictionary with a "group" key
        if isinstance(result_item, dict) and "group" in result_item:
            start, end = result_item["group"]
            st.subheader(f"Stages {start}-{end}")
            
            # Build sub-view options
            sub_view_options = ["Homogeneous", "ACC/DC"]
            if "homogeneous_plot" in result_item:
                sub_view_options.append("Homogeneous Plot")
            if "acc_dc_plot" in result_item:
                sub_view_options.append("ACC/DC Plot")
            
            # Use radio for sub-view selection
            view_key = f"survival_view_{idx}"
            if view_key not in st.session_state:
                st.session_state[view_key] = "Homogeneous"
            
            selected_view = st.radio(
                "Select View",
                sub_view_options,
                key=view_key,
                horizontal=True,
                label_visibility="collapsed"
            )
            
            st.divider()
            
            if selected_view == "Homogeneous":
                for item in result_item["homogeneous"]:
                    if isinstance(item, pd.DataFrame):
                        st.dataframe(utils.clean_df(item))
                    else:
                        st.write(item)
            elif selected_view == "ACC/DC":
                for item in result_item["acc_dc"]:
                    if isinstance(item, pd.DataFrame):
                        st.dataframe(utils.clean_df(item))
                    else:
                        st.write(item)
            elif selected_view == "Homogeneous Plot" and "homogeneous_plot" in result_item:
                for item in result_item["homogeneous_plot"]:
                    st.write(f"### {item['name']}")
                    survival.plot_tab(item)
            elif selected_view == "ACC/DC Plot" and "acc_dc_plot" in result_item:
                for item in result_item["acc_dc_plot"]:
                    st.write(f"### {item['name']}")
                    survival.plot_tab(item)
        else:
            # For flat results (e.g. from branches that don't use splining)
            if isinstance(result_item, list):
                for item in result_item:
                    if isinstance(item, pd.DataFrame):
                        st.dataframe(utils.clean_df(item))
                    else:
                        st.write(item)
            else:
                st.write(result_item)

elif selected_method=='Loyalty' and st.session_state['transformed_data'] is not None:
    key = selected_method.lower().replace(' ', '_')
    data = st.session_state['transformed_data']
    
    # Use selectbox for main navigation to preserve selection across reruns
    tab_options = st.session_state['tabs'][key]
    
    # Initialize tab selection in session state
    tab_select_key = f"{key}_tab_select"
    if tab_select_key not in st.session_state:
        st.session_state[tab_select_key] = tab_options[0] if tab_options else "Data"
    
    # Ensure current selection is valid
    if st.session_state[tab_select_key] not in tab_options:
        st.session_state[tab_select_key] = tab_options[0] if tab_options else "Data"
    
    selected_tab = st.selectbox(
        "Select Result",
        tab_options,
        key=tab_select_key,
        label_visibility="collapsed"
    )
    
    st.divider()
    
    # Find the index of the selected tab
    selected_idx = tab_options.index(selected_tab) if selected_tab in tab_options else 0
    
    # Display content based on selection
    if selected_idx == 0:
        # Data tab
        st.subheader("Data")
        st.write("Data loaded and transformed:")
        st.dataframe(utils.clean_df(data))
    else:
        # Result tabs
        res = st.session_state['results'][key][selected_idx - 1]
        idx = selected_idx
        
        # For M Model and Model Report, we expect a group dictionary with "mode", "report", and "plot"
        if isinstance(res, dict) and res.get("mode") in ["loyalty_m", "loyalty_q"]:
            # Use radio for sub-view selection
            view_key = f"loyalty_view_{idx}"
            if view_key not in st.session_state:
                st.session_state[view_key] = "Report"
            
            selected_view = st.radio(
                "Select View",
                ["Report", "Plot"],
                key=view_key,
                horizontal=True,
                label_visibility="collapsed"
            )
            
            st.divider()
            
            if selected_view == "Report":
                for item in res["report"]:
                    if isinstance(item, pd.DataFrame):
                        st.dataframe(utils.clean_df(item))
                    else:
                        st.write(item)
            else:  # Plot
                st.write(f"### {res['plot']['name']}")
                st.plotly_chart(res['plot']['figure'], use_container_width=True)
        else:
            # For explanatory variable results (which are DataFrames)
            if isinstance(res, pd.DataFrame):
                st.dataframe(utils.clean_df(res))
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
            "Enter upper polarity index",
            value="",
            help=utils.RANGE_INPUT_HELP,
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
    
        # Add Go! button for Exploratory and Confirmatory models
        if st.sidebar.button("Go!", key=f"ranking_{ranking_model.lower()}_go"):
            try:
                # Process satisfaction constraint
                sat_constraint = None
                if satisfaction_constraint == "First":
                    sat_constraint = "first"
                elif satisfaction_constraint == "Last":
                    sat_constraint = "last"
                
                # Run the appropriate analysis based on selected model
                if ranking_model == "Exploratory":
                    results = ranking.exploratory_computation(data, upper_polarity_idx, sat_constraint)
                    report = ranking.exploratory_report(results)
                    
                    # Store the computation results for plotting
                    comp_data = {
                        'analysis_type': 'exploratory',
                        'results': results,
                        'contingency_table': data
                    }
                    st.session_state['comp_results'][key].append(comp_data)
                    
                    # Save report in results
                    st.session_state['results'][key].append(report)
                    
                    # Create new tab
                    new_tab_label = f"Exploratory {len(st.session_state['results'][key])}"
                    st.session_state['tabs'][key].append(new_tab_label)
                    
                    st.success(f"✅ Analysis complete! Select '{new_tab_label}' from the dropdown below.")
                    
                elif ranking_model == "Confirmatory":
                    results = ranking.confirmatory_computation(data, upper_polarity_idx, sat_constraint)
                    report = ranking.confirmatory_report(results)
                    
                    # Store computation results for plotting
                    comp_data = {
                        'analysis_type': 'confirmatory',
                        'results': results,
                        'contingency_table': data
                    }
                    st.session_state['comp_results'][key].append(comp_data)
                    
                    # Save report in results
                    st.session_state['results'][key].append(report)
                    
                    # Create new tab
                    new_tab_label = f"Confirmatory {len(st.session_state['results'][key])}"
                    st.session_state['tabs'][key].append(new_tab_label)
                    
                    st.success(f"✅ Analysis complete! Select '{new_tab_label}' from the dropdown below.")
                    
            except Exception as e:
                st.error(f"Error in {ranking_model} analysis: {str(e)}")
                import traceback
                st.sidebar.code(traceback.format_exc())
    
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
                "Enter brand indices",
                value="",
                help=utils.RANGE_INPUT_HELP,
                key="ranking_expl_brands"
            )
            
            # Input for satisfaction level range
            ranking_range_str = st.sidebar.text_input(
                "Enter satisfaction level range",
                value="",
                help=utils.RANGE_INPUT_HELP,
                key="ranking_expl_range"
            )
            
            # Input for explanatory variable indices
            expl_vars_str = st.sidebar.text_input(
                "Enter explanatory variable indices",
                value="",
                help=utils.RANGE_INPUT_HELP,
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
                                target_col = len(new_data.columns) - 1  # Last column is target
                                # Sort the explanatory variables
                                sorted_indices = threedim.sort_explanatory_variables(new_data, target_col, [explanatory_vars_list])
                                # The sorted_indices are the actual explanatory column indices from our list
                                # Convert these to 1-based user-friendly indices
                                sorted_expl_vars = [index + 1 for index in sorted_indices]
                                # Display sorted order
                                sorted_order_str = ", ".join(map(str, sorted_expl_vars))
                                st.markdown(f"**Sorted explanatory variables:** {sorted_order_str}")
                            
                            # Save results and create new tab
                            st.session_state['results'][key].append(new_data)
                            new_tab_label = f"Explanatory Result {len(st.session_state['results'][key])}"
                            st.session_state['tabs'][key].append(new_tab_label)
                            st.success(f"✅ Data created! Select '{new_tab_label}' from the dropdown below.")
                            
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
    
    # Use selectbox for main navigation to preserve selection across reruns
    tab_options = st.session_state['tabs'][key]
    
    # Initialize tab selection in session state
    tab_select_key = f"{key}_tab_select"
    if tab_select_key not in st.session_state:
        st.session_state[tab_select_key] = tab_options[0] if tab_options else "Data"
    
    # Ensure current selection is valid
    if st.session_state[tab_select_key] not in tab_options:
        st.session_state[tab_select_key] = tab_options[0] if tab_options else "Data"
    
    selected_tab = st.selectbox(
        "Select Result",
        tab_options,
        key=tab_select_key,
        label_visibility="collapsed"
    )
    
    st.divider()
    
    # Find the index of the selected tab
    selected_idx = tab_options.index(selected_tab) if selected_tab in tab_options else 0
    
    # Display content based on selection
    if selected_idx == 0:
        # Data tab
        st.subheader("Data")
        st.dataframe(utils.clean_df(data))
    else:
        # Result tabs
        res = st.session_state['results'][key][selected_idx - 1]
        idx = selected_idx
        
        # For result items from confirmatory_report function
        if isinstance(res, list):
            # Use radio for sub-view selection
            view_key = f"ranking_view_{idx}"
            if view_key not in st.session_state:
                st.session_state[view_key] = "Analysis Results"
            
            selected_view = st.radio(
                "Select View",
                ["Analysis Results", "Average Satisfaction", "Distribution by Brand Groups"],
                key=view_key,
                horizontal=True,
                label_visibility="collapsed"
            )
            
            st.divider()
            
            if selected_view == "Analysis Results":
                display_result_items(res)
            elif selected_view == "Average Satisfaction":
                if 'comp_results' in st.session_state and key in st.session_state['comp_results']:
                    if idx <= len(st.session_state['comp_results'][key]):
                        comp_data = st.session_state['comp_results'][key][idx-1]
                        if comp_data and 'analysis_type' in comp_data:
                            try:
                                fig = ranking.plot_confirmatory_brand_clusters(comp_data['results'])
                                st.plotly_chart(fig, use_container_width=True)
                            except Exception as e:
                                st.error(f"Error generating brand clusters plot: {str(e)}")
                        else:
                            st.warning("Plot data not available.")
                    else:
                        st.warning("Plot data not available for this result.")
                else:
                    st.warning("No plot data available. Run analysis again to see plots.")
            else:  # Distribution by Brand Groups
                if 'comp_results' in st.session_state and key in st.session_state['comp_results']:
                    if idx <= len(st.session_state['comp_results'][key]):
                        comp_data = st.session_state['comp_results'][key][idx-1]
                        if comp_data and 'analysis_type' in comp_data:
                            try:
                                fig = ranking.plot_confirmatory_brand_distribution(comp_data['results'])
                                st.plotly_chart(fig, use_container_width=True)
                            except Exception as e:
                                st.error(f"Error generating distribution plot: {str(e)}")
                        else:
                            st.warning("Plot data not available.")
                    else:
                        st.warning("Plot data not available for this result.")
                else:
                    st.warning("No plot data available. Run analysis again to see plots.")
        
        # For old-style group results with mode="ranking_confirmatory"
        elif isinstance(res, dict) and res.get("mode") in ["ranking_confirmatory", "ranking_exploratory", "ranking_explanatory", "ranking_explanatory_var"]:
            # Use radio for sub-view selection
            view_key = f"ranking_view_{idx}"
            if view_key not in st.session_state:
                st.session_state[view_key] = "Report"
            
            selected_view = st.radio(
                "Select View",
                ["Report", "Plots"],
                key=view_key,
                horizontal=True,
                label_visibility="collapsed"
            )
            
            st.divider()
            
            if selected_view == "Report":
                display_result_items(res.get("report", []))
            else:  # Plots
                if "plots" in res:
                    for plot_name, plot in res["plots"].items():
                        st.markdown(f"#### {plot_name}")
                        st.plotly_chart(plot, use_container_width=True)
                else:
                    st.warning("No plots available for this analysis.")
        else:
            # Fallback for any other type of result
            st.write(res)
            st.warning("Tab has been created. Please refresh the page to see it.")

# ----------------------------
# Spacing Models Analysis Section
# ----------------------------
elif selected_method == 'Spacing Models' and st.session_state.get('transformed_data') is not None:
    key = selected_method.lower().replace(' ', '_')
    data = st.session_state['transformed_data']
    
    # Initialize storage for Spacing Models results and tabs if not already done
    if key not in st.session_state['results']:
        st.session_state['results'][key] = []
    if key not in st.session_state['tabs']:
        st.session_state['tabs'][key] = ['Data']
    
    # Sidebar: Model Name selection
    spacing_model_name = st.sidebar.selectbox(
        "Model Name",
        ["Exponential Spacing", "Canonical Correlation"],
        key="spacing_model_name"
    )
    
    # Sidebar: Model Type selection
    spacing_model_type = st.sidebar.selectbox(
        "Model Type",
        ["Model Selection", "RC Model", "R Model", "C Model", "U Model"],
        key="spacing_model_type"
    )
    
    # ------------------------
    # Permanent Data Collapsing (collapsible)
    # ------------------------
    with st.sidebar.expander("Permanent Data Collapsing", expanded=False):
        # Row groups input
        row_groups_str = st.text_input(
            "Row groups to collapse (optional)", 
            value="",
            help=utils.RANGE_INPUT_HELP,
            key="spacing_collapse_row_groups"
        )
        
        # Column groups input  
        col_groups_str = st.text_input(
            "Column groups to collapse (optional)", 
            value="",
            help=utils.RANGE_INPUT_HELP,
            key="spacing_collapse_col_groups"
        )
        
        # Collapse and Revert buttons side by side
        col1, col2 = st.columns(2)
        
        # Revert button - only show if data has been modified (collapsed)
        with col2:
            if st.session_state.get('data_modified', False):
                if st.button("↩ Revert", key="spacing_revert_collapse_btn", help="Restore original uncollapsed data"):
                    st.session_state['data_modified'] = False
                    st.session_state['transformed_data'] = None  # Force reload from original
                    st.rerun()
        
        # Collapse button
        with col1:
            collapse_clicked = st.button("Collapse", key="spacing_collapse_data_btn")
        
        if collapse_clicked:
            row_groups = None
            col_groups = None
            
            # Parse row groups if provided
            if row_groups_str.strip():
                try:
                    row_groups = utils.parse_text_groups(row_groups_str)
                    if row_groups is None:
                        st.error("Invalid row groups format. Please check the format.")
                except Exception as e:
                    st.error(f"Error parsing row groups: {str(e)}")
                    row_groups = None
            
            # Parse column groups if provided
            if col_groups_str.strip():
                try:
                    col_groups = utils.parse_text_groups(col_groups_str)
                    if col_groups is None:
                        st.error("Invalid column groups format. Please check the format.")
                except Exception as e:
                    st.error(f"Error parsing column groups: {str(e)}")
                    col_groups = None
            
            # Perform collapse if at least one group is specified
            if row_groups is not None or col_groups is not None:
                try:
                    collapsed_data = utils.collapse_data(data, row_groups, col_groups, as_data_metrix=True)
                    
                    # Update the transformed data in session state 
                    st.session_state['transformed_data'] = collapsed_data
                    st.session_state['data_modified'] = True  # Mark data as modified to prevent overwriting
                    
                    st.rerun()  # Rerun to show the Revert button immediately
                except Exception as e:
                    st.error(f"Error collapsing data: {str(e)}")
                    import traceback
                    st.code(traceback.format_exc())
            else:
                st.warning("Please specify at least one group (row or column) to collapse.")
    
    # Go button
    if st.sidebar.button("Go!", key="spacing_go"):
        try:
            # Get the current data (may have been collapsed)
            current_data = st.session_state['transformed_data']
            num_row = current_data.shape[0]
            num_col = current_data.shape[1]
            
            if spacing_model_type == "Model Selection":
                # Run model selection
                selection_result = spacing.model_selection(current_data, spacing_model_name)
                report = spacing.model_selection_report(selection_result)
                
                result_data = {
                    "mode": "spacing_model_selection",
                    "report": report,
                    "selection_result": selection_result
                }
                
                st.session_state['results'][key].append(result_data)
                new_tab_label = f"Model Selection {len(st.session_state['results'][key])}"
                st.session_state['tabs'][key].append(new_tab_label)
                st.success(f"✅ Model Selection complete! Select '{new_tab_label}' from the dropdown below.")
                
            else:
                # Specific model selected
                # Determine polynomial degrees based on model type
                if spacing_model_type == "RC Model":
                    poly_deg_row = num_row - 1
                    poly_deg_col = num_col - 1
                elif spacing_model_type == "R Model":
                    poly_deg_row = num_row - 1
                    poly_deg_col = 1
                elif spacing_model_type == "C Model":
                    poly_deg_row = 1
                    poly_deg_col = num_col - 1
                elif spacing_model_type == "U Model":
                    poly_deg_row = 1
                    poly_deg_col = 1
                else:
                    poly_deg_row = num_row - 1
                    poly_deg_col = num_col - 1
                
                # Run computation
                if spacing_model_name == "Exponential Spacing":
                    result = spacing.exponential_spacing_computation(current_data, poly_deg_row, poly_deg_col)
                else:
                    result = spacing.canonical_correlation_computation(current_data, poly_deg_row, poly_deg_col)
                
                # Generate report
                report = spacing.spacing_model_report(result, spacing_model_type, spacing_model_name)
                
                result_data = {
                    "mode": "spacing_specific_model",
                    "report": report,
                    "result": result,
                    "model_type": spacing_model_type,
                    "model_name": spacing_model_name
                }
                
                st.session_state['results'][key].append(result_data)
                new_tab_label = f"{spacing_model_type} {len(st.session_state['results'][key])}"
                st.session_state['tabs'][key].append(new_tab_label)
                st.success(f"✅ Analysis complete! Select '{new_tab_label}' from the dropdown below.")
                
        except Exception as e:
            st.error(f"Error in Spacing Models analysis: {str(e)}")
            import traceback
            st.sidebar.code(traceback.format_exc())
    
    # Use selectbox for main navigation to preserve selection across reruns
    tab_options = st.session_state['tabs'][key]
    
    # Initialize tab selection in session state
    tab_select_key = f"{key}_tab_select"
    if tab_select_key not in st.session_state:
        st.session_state[tab_select_key] = tab_options[0] if tab_options else "Data"
    
    # Ensure current selection is valid
    if st.session_state[tab_select_key] not in tab_options:
        st.session_state[tab_select_key] = tab_options[0] if tab_options else "Data"
    
    selected_tab = st.selectbox(
        "Select Result",
        tab_options,
        key=tab_select_key,
        label_visibility="collapsed"
    )
    
    st.divider()
    
    # Find the index of the selected tab
    selected_idx = tab_options.index(selected_tab) if selected_tab in tab_options else 0
    
    # Display content based on selection
    if selected_idx == 0:
        # Data tab
        st.subheader("Data")
        st.write("Contingency table for analysis:")
        st.dataframe(utils.clean_df(data))
    else:
        # Result tabs
        res = st.session_state['results'][key][selected_idx - 1]
        idx = selected_idx  # For compatibility with existing code
        
        if isinstance(res, dict):
            if res.get("mode") == "spacing_model_selection":
                # Display model selection results
                display_result_items(res.get("report", []))
                
            elif res.get("mode") == "spacing_specific_model":
                # Display specific model results with radio buttons instead of tabs
                model_result = res.get("result", {})
                model_name = res.get("model_name", "Exponential Spacing")
                
                # Use radio buttons to preserve selection across reruns
                view_key = f"spacing_view_{idx}"
                if view_key not in st.session_state:
                    st.session_state[view_key] = "Report"
                
                selected_view = st.radio(
                    "Select View",
                    ["Report", "Parameters Plot", "Odd Ratio Computation"],
                    key=view_key,
                    horizontal=True,
                    label_visibility="collapsed"
                )
                
                st.divider()
                
                if selected_view == "Report":
                    display_result_items(res.get("report", []))
                
                elif selected_view == "Parameters Plot":
                        # Plot mu and nu parameters with Plotly for hover support
                        try:
                            import plotly.graph_objects as go
                            from plotly.subplots import make_subplots
                            
                            # Get data
                            mu = model_result.get('mu', [])
                            mu_easd = model_result.get('mu_easd', [])
                            nu = model_result.get('nu', [])
                            nu_easd = model_result.get('nu_easd', [])
                            
                            x_mu = list(range(1, len(mu) + 1))
                            x_nu = list(range(1, len(nu) + 1))
                            
                            # Create subplots
                            fig = make_subplots(rows=1, cols=2, 
                                              subplot_titles=('Row Spacing Parameters (Mu)', 
                                                            'Column Spacing Parameters (Nu)'))
                            
                            # Mu plot - confidence band
                            if len(mu_easd) > 0 and not np.all(np.isnan(mu_easd)):
                                mu_lower = mu - 1.96 * np.nan_to_num(mu_easd, nan=0)
                                mu_upper = mu + 1.96 * np.nan_to_num(mu_easd, nan=0)
                                fig.add_trace(go.Scatter(
                                    x=x_mu + x_mu[::-1],
                                    y=list(mu_upper) + list(mu_lower)[::-1],
                                    fill='toself',
                                    fillcolor='rgba(0,100,255,0.2)',
                                    line=dict(color='rgba(255,255,255,0)'),
                                    hoverinfo='skip',
                                    showlegend=False,
                                    name='95% CI'
                                ), row=1, col=1)
                            
                            # Mu plot - main line with hover
                            mu_hover_text = [
                                f"Mu[{i}]<br>Estimate: {mu[i-1]:.4f}<br>E.A.S.D.: {mu_easd[i-1]:.4f}" 
                                if not np.isnan(mu_easd[i-1]) else f"Mu[{i}]<br>Estimate: {mu[i-1]:.4f}<br>E.A.S.D.: NA"
                                for i in x_mu
                            ]
                            fig.add_trace(go.Scatter(
                                x=x_mu, y=mu,
                                mode='lines+markers',
                                marker=dict(size=10, color='blue'),
                                line=dict(color='blue', width=2),
                                name='Mu',
                                hovertext=mu_hover_text,
                                hoverinfo='text'
                            ), row=1, col=1)
                            
                            # Mu plot - zero line
                            fig.add_hline(y=0, line_dash="dash", line_color="gray", opacity=0.5, row=1, col=1)
                            
                            # Nu plot - confidence band
                            if len(nu_easd) > 0 and not np.all(np.isnan(nu_easd)):
                                nu_lower = nu - 1.96 * np.nan_to_num(nu_easd, nan=0)
                                nu_upper = nu + 1.96 * np.nan_to_num(nu_easd, nan=0)
                                fig.add_trace(go.Scatter(
                                    x=x_nu + x_nu[::-1],
                                    y=list(nu_upper) + list(nu_lower)[::-1],
                                    fill='toself',
                                    fillcolor='rgba(255,100,100,0.2)',
                                    line=dict(color='rgba(255,255,255,0)'),
                                    hoverinfo='skip',
                                    showlegend=False,
                                    name='95% CI'
                                ), row=1, col=2)
                            
                            # Nu plot - main line with hover
                            nu_hover_text = [
                                f"Nu[{i}]<br>Estimate: {nu[i-1]:.4f}<br>E.A.S.D.: {nu_easd[i-1]:.4f}" 
                                if not np.isnan(nu_easd[i-1]) else f"Nu[{i}]<br>Estimate: {nu[i-1]:.4f}<br>E.A.S.D.: NA"
                                for i in x_nu
                            ]
                            fig.add_trace(go.Scatter(
                                x=x_nu, y=nu,
                                mode='lines+markers',
                                marker=dict(size=10, color='red'),
                                line=dict(color='red', width=2),
                                name='Nu',
                                hovertext=nu_hover_text,
                                hoverinfo='text'
                            ), row=1, col=2)
                            
                            # Nu plot - zero line
                            fig.add_hline(y=0, line_dash="dash", line_color="gray", opacity=0.5, row=1, col=2)
                            
                            # Update layout
                            fig.update_xaxes(title_text="Row Index", row=1, col=1)
                            fig.update_xaxes(title_text="Column Index", row=1, col=2)
                            fig.update_yaxes(title_text="Mu Value", row=1, col=1)
                            fig.update_yaxes(title_text="Nu Value", row=1, col=2)
                            fig.update_layout(
                                height=450,
                                showlegend=False,
                                hovermode='closest'
                            )
                            
                            st.plotly_chart(fig, use_container_width=True)
                            
                            # Display phi value
                            phi = model_result.get('phi', 0)
                            phi_easd = model_result.get('phi_easd', np.nan)
                            st.markdown(f"**Association Parameter (Phi):** {phi:.4f}")
                            if not np.isnan(phi_easd):
                                st.markdown(f"**Phi E.A.S.D.:** {phi_easd:.4f}")
                                
                        except Exception as e:
                            st.error(f"Error generating plot: {str(e)}")
                
                else:  # selected_view == "Odd Ratio Computation"
                    # Odd Ratio Computation - dynamic section
                    st.subheader("Odd Ratio Computation")
                    st.write("Select two rows and two columns to compute the odd ratio.")
                    
                    mu = model_result.get('mu', [])
                    nu = model_result.get('nu', [])
                    phi = model_result.get('phi', 0)
                    num_row = len(mu)
                    num_col = len(nu)
                    
                    # Create unique keys for this result
                    unique_key = f"spacing_or_{idx}"
                    
                    # Ensure session state is initialized (should already be done at top of script)
                    if f"{unique_key}_row1" not in st.session_state:
                        st.session_state[f"{unique_key}_row1"] = 1
                    if f"{unique_key}_row2" not in st.session_state:
                        st.session_state[f"{unique_key}_row2"] = min(2, num_row) if num_row > 1 else 1
                    if f"{unique_key}_col1" not in st.session_state:
                        st.session_state[f"{unique_key}_col1"] = 1
                    if f"{unique_key}_col2" not in st.session_state:
                        st.session_state[f"{unique_key}_col2"] = min(2, num_col) if num_col > 1 else 1
                    
                    # Input fields in columns - don't pass 'value' when using 'key' with session state
                    col1, col2 = st.columns(2)
                    
                    with col1:
                        st.markdown("**Rows**")
                        row1 = st.number_input("Row 1", min_value=1, max_value=num_row, step=1, key=f"{unique_key}_row1")
                        row2 = st.number_input("Row 2", min_value=1, max_value=num_row, step=1, key=f"{unique_key}_row2")
                    
                    with col2:
                        st.markdown("**Columns**")
                        col1_val = st.number_input("Column 1", min_value=1, max_value=num_col, step=1, key=f"{unique_key}_col1")
                        col2_val = st.number_input("Column 2", min_value=1, max_value=num_col, step=1, key=f"{unique_key}_col2")
                    
                    # Compute button
                    if st.button("Compute Odd Ratio", key=f"{unique_key}_compute"):
                        # Validate inputs
                        error_msg = None
                        if row1 == row2:
                            error_msg = "Row 1 and Row 2 should be different."
                        elif col1_val == col2_val:
                            error_msg = "Column 1 and Column 2 should be different."
                        
                        if error_msg:
                            st.error(error_msg)
                        else:
                            # Convert to 0-based indices
                            r1, r2 = int(row1) - 1, int(row2) - 1
                            c1, c2 = int(col1_val) - 1, int(col2_val) - 1
                            
                            # Compute odd ratio based on model type
                            if model_name == "Exponential Spacing":
                                # For Exponential Spacing: OR = exp(phi * (mu[r1] - mu[r2]) * (nu[c1] - nu[c2]))
                                odd_ratio = np.exp(phi * (mu[r1] - mu[r2]) * (nu[c1] - nu[c2]))
                            else:
                                # For Canonical Correlation: compute from expected values
                                expected = model_result.get('expected', None)
                                if expected is not None:
                                    odd_ratio = (expected[r1, c1] * expected[r2, c2]) / (expected[r1, c2] * expected[r2, c1])
                                else:
                                    odd_ratio = np.nan
                            
                            # Store the result in session state
                            st.session_state[f"{unique_key}_result"] = odd_ratio
                    
                    # Display result if available
                    if f"{unique_key}_result" in st.session_state:
                        odd_ratio = st.session_state[f"{unique_key}_result"]
                        st.success(f"**Odd Ratio:** {odd_ratio:.6f}")
                        
                        # Interpretation
                        if odd_ratio > 1:
                            st.info(f"The odds in row {row1} vs row {row2} are {odd_ratio:.4f} times higher for column {col1_val} compared to column {col2_val}.")
                        elif odd_ratio < 1:
                            st.info(f"The odds in row {row1} vs row {row2} are {1/odd_ratio:.4f} times lower for column {col1_val} compared to column {col2_val}.")
                        else:
                            st.info("The odds ratio is 1, indicating no association between the selected rows and columns.")
            else:
                # Fallback display
                display_result_items(res.get("report", []))
        else:
            # Fallback for any other type of result
            st.write(res)

# Scale Conversion Analysis Section
elif selected_method == 'Scale Conversion' and st.session_state.get('transformed_data') is not None:
    key = selected_method.lower().replace(' ', '_')
    data = st.session_state['transformed_data']
    
    # Initialize storage for Scale Conversion results and tabs if not already done
    if key not in st.session_state['results']:
        st.session_state['results'][key] = []
    if key not in st.session_state['tabs']:
        st.session_state['tabs'][key] = ['Data']
    
    # The data has been transformed in utils.py to contain frequency distributions
    # Columns: Row_Scale, Col_Scale, Row_Labels, Col_Labels
    if st.sidebar.button("Go!", key="scale_go"):
        try:
            # Extract the frequency data
            row_data = data['Row_Scale'].dropna().values.astype(float)
            col_data = data['Col_Scale'].dropna().values.astype(float)
            row_labels = data['Row_Labels'].dropna().values
            col_labels = data['Col_Labels'].dropna().values
            
            # Validate data
            if len(row_data) == 0:
                st.error("Row scale contains no valid values.")
            elif len(col_data) == 0:
                st.error("Column scale contains no valid values.")
            elif np.sum(row_data) == 0 or np.sum(col_data) == 0:
                st.error("At least one value must be positive in each scale.")
            else:
                # Generate report with labels
                report = scale_conversion.scale_conversion_report(row_data, col_data, row_labels, col_labels)
                
                # Store result
                st.session_state['results'][key].append(report)
                new_tab_label = f"Result {len(st.session_state['results'][key])}"
                st.session_state['tabs'][key].append(new_tab_label)
                st.success(f"✅ Analysis complete! Select '{new_tab_label}' from the dropdown below.")
                
        except Exception as e:
            st.error(f"Error in Scale Conversion analysis: {str(e)}")
            import traceback
            st.sidebar.code(traceback.format_exc())
    
    # Use selectbox for main navigation to preserve selection across reruns
    tab_options = st.session_state['tabs'][key]
    
    # Initialize tab selection in session state
    tab_select_key = f"{key}_tab_select"
    if tab_select_key not in st.session_state:
        st.session_state[tab_select_key] = tab_options[0] if tab_options else "Data"
    
    # Ensure current selection is valid
    if st.session_state[tab_select_key] not in tab_options:
        st.session_state[tab_select_key] = tab_options[0] if tab_options else "Data"
    
    selected_tab = st.selectbox(
        "Select Result",
        tab_options,
        key=tab_select_key,
        label_visibility="collapsed"
    )
    
    st.divider()
    
    # Find the index of the selected tab
    selected_idx = tab_options.index(selected_tab) if selected_tab in tab_options else 0
    
    # Display content based on selection
    if selected_idx == 0:
        # Data tab - show a clean two-column table like the R app
        st.subheader("Data")
        
        # Create a simple Row/Column table
        row_counts = data['Row_Scale'].dropna().values
        col_counts = data['Col_Scale'].dropna().values
        
        # Make both same length (pad with empty if needed)
        max_len = max(len(row_counts), len(col_counts))
        display_df = pd.DataFrame({
            'Row': list(row_counts) + [''] * (max_len - len(row_counts)),
            'Column': list(col_counts) + [''] * (max_len - len(col_counts))
        })
        display_df.index = range(1, len(display_df) + 1)
        st.dataframe(display_df)
    else:
        # Result tabs
        res = st.session_state['results'][key][selected_idx - 1]
        display_result_items(res)
