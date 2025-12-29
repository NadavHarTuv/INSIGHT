import streamlit as st
import pandas as pd
import numpy as np
import re

# from st_aggrid import AgGrid


analysis_methods = ['Independence', '3-Dimensional', 'N-Dimensional', 'Survival', 
                    'Loyalty', 'Ranking', 'Spacing Models', 'Scale Conversion']

# Unified help text for range/group inputs across all modules
RANGE_INPUT_HELP = "Enter ranges using format: '1-3;4,5' (use '-' for ranges, ',' to separate items, ';' to separate groups)"

model_names = {
    'Independence': ["Independence Model",
                        "ANOAS Model",
                        "Predictors' Proportion",
                        "Log-Odds Z-Test",
                        "Collapse Chi-Sq Test"],
    '3-Dimensional': ['Log-Linear Model'],
    'Loyalty': ["M Model", "Q Model", "Explore Model",
                    "Brand Sensitivity", "Brand Omission", "Explanatory Variable"],
    'Survival': ["Homogeneous, ACC/DC", "Out-of-Sample Splining", "Explanatory Variable"],
    'Ranking': ['Exploratory',
                'Confirmatory',
                'Brand Sensitivity',
                'Brand Omission',
                'Explanatory Variable']
}

def validate_column_input(column_text, max_col_idx):
    """
    Validates if the column_text is a valid integer and checks if it's within 
    the bounds of the dataframe's columns.
    """
    try:
        col_idx = int(column_text) - 1  # Convert to 0-based index
        if 0 <= col_idx < max_col_idx:
            return col_idx, None
        else:
            return None, "Column index is out of range."
    except ValueError:
        return None, "Column input is not a valid number."


def create_contingency_table_from_observations(raw_data):
    # Inputs for columns (1-based index)
    brand_column_text = st.sidebar.text_input("Which column in file for brand?")
    ranking_column_text = st.sidebar.text_input("Which column in file for ranking?")
    
    max_col_idx = raw_data.shape[1]  # Number of columns in the data

    # Validate brand column input
    brand_col_idx = None
    if brand_column_text:  # User has entered something in brand column field
        brand_col_idx, brand_error = validate_column_input(brand_column_text, max_col_idx)
        if brand_error:
            st.sidebar.error(f"Brand column error: {brand_error}")

    # Validate ranking column input
    ranking_col_idx = None
    if ranking_column_text:  # User has entered something in ranking column field
        ranking_col_idx, ranking_error = validate_column_input(ranking_column_text, max_col_idx)
        if ranking_error:
            st.sidebar.error(f"Ranking column error: {ranking_error}")

    # Proceed with creating the contingency table only if both inputs are valid
    if brand_col_idx is not None and ranking_col_idx is not None:
        # Get the relevant columns from raw_data
        brands = raw_data.iloc[:, brand_col_idx]
        rankings = raw_data.iloc[:, ranking_col_idx]

        # Create the contingency table
        contingency_table = pd.crosstab(brands, rankings)

        # Rename the index and columns to match the format in the example
        contingency_table.index.name = 'Brand'
        contingency_table.columns.name = 'Level'

        # Optionally, display the table in Streamlit
        # st.write("Contingency Table")
        # st.dataframe(contingency_table)

        return raw_data, contingency_table

    # If input fields are present but not valid, do nothing (no output or return)
    return None, None

def transform_data(raw_data, X_int, Y_int, Z_int):
    grouped_data = raw_data.groupby([raw_data.iloc[:, X_int], raw_data.iloc[:, Y_int], raw_data.iloc[:, Z_int]]).size().reset_index(name='value')
    grouped_data.columns = ['X', 'Y', 'Z', 'value']
    return (raw_data, grouped_data)

def load_data(uploaded_file, analysis_method):
    if uploaded_file is not None:
        raw_data = pd.read_csv(uploaded_file, header=None)
        
        if analysis_method == 'Independence':
            file_formats = ['Rows of subjects', 'Contingency table']
            selected_format = st.sidebar.radio(label='File format',options=file_formats)
            
            if selected_format == 'Rows of subjects':
                row_column = st.sidebar.selectbox('Which column in file for rows?', np.arange(1,raw_data.shape[1]+1), index=None, placeholder='Select column')
                column_column = st.sidebar.selectbox('Which column in file for columns?', np.arange(1,raw_data.shape[1]+1), index=None, placeholder='Select column')
                
                if row_column and column_column:
                    row_column = row_column - 1
                    column_column = column_column - 1
                    contingency_table = pd.crosstab(raw_data.iloc[:,row_column].astype(int),
                                                    raw_data.iloc[:,column_column].astype(int))

                    return (raw_data, contingency_table)
            else:
                return (raw_data, raw_data)
        
        elif analysis_method == '3-Dimensional':
            X = st.sidebar.text_input('Which column in file for variable X?',value=None)
            X_int, Y_int, Z_int = None, None, None
            if X:
                try:
                    X_int = int(X)-1
                    if X_int <= 0 or X_int > raw_data.shape[1]:
                        X_int = None
                        st.sidebar.warning(f'X must be a number beteween 1 and {raw_data.shape[1]}')
                except:
                    st.sidebar.warning(f'X must be an integer')
            Y = st.sidebar.text_input('Which column in file for variable Y?',value=None)
            if Y:
                try:
                    Y_int = int(Y)-1
                    if Y_int <= 0 or Y_int > raw_data.shape[1]:
                        Y_int = None
                        st.sidebar.warning(f'Y must be a number beteween 1 and {raw_data.shape[1]}')
                except:
                    st.sidebar.warning(f'Y must be an integer')
            Z = st.sidebar.text_input('Which column in file for variable Z?', value=None)
            if Z:
                try:
                    Z_int = int(Z)-1
                    if Z_int <= 0 or Z_int > raw_data.shape[1]:
                        st.sidebar.warning(f'X must be a number beteween 1 and {raw_data.shape[1]}')
                except:
                    st.sidebar.warning(f'Y must be an integer')

            
            if X_int is not None and Y_int is not None and Z_int is not None:
                # data = raw_data.groupby([raw_data.iloc[:, X_int], raw_data.iloc[:, Y_int], raw_data.iloc[:, Z_int]]).size().unstack(fill_value=0).reset_index()
                # data.columns = ['X', 'Y'] + [f'Z={val}' for val in data.columns[2:]]
                # return (raw_data, data)
                return transform_data(raw_data, X_int, Y_int, Z_int)

        elif analysis_method == 'N-Dimensional':
            return raw_data, raw_data
        
        elif analysis_method == 'Survival':
            file_formats = ['Rows of subjects', 'Contingency table']
            selected_format = st.sidebar.radio(label='File format',options=file_formats)
            
            if selected_format == 'Rows of subjects':
                st.session_state['survival_col'] = st.sidebar.text_input(label='Which column in file?')
                if st.session_state['survival_col']:
                    try:
                        st.session_state['survival_col'] = int(st.session_state['survival_col'])-1
                        if st.session_state['survival_col'] > raw_data.shape[1]-1 or st.session_state['survival_col'] < 0:
                            st.sidebar.warning(f'Column must be a number between 1 and {raw_data.shape[1]}')
                    except:
                        st.sidebar.warning('Column must be an integer')
                    
                    counts = raw_data.iloc[:,st.session_state['survival_col']].value_counts()
                    
                    # Sort by index to get stages in order (1, 2, 3, ..., -1)
                    # Separate positive stages and -1 (survive)
                    positive_indices = sorted([i for i in counts.index if i > 0])
                    
                    # Build the ordered DataFrame
                    rows = []
                    for i in positive_indices:
                        rows.append({'Stage': f'Stage {i}', 'Count': counts[i]})
                    
                    # Add Survive row at the end if -1 exists
                    if -1 in counts.index:
                        rows.append({'Stage': 'Survive', 'Count': counts[-1]})
                    
                    counts_df = pd.DataFrame(rows)
                    counts_df.index = counts_df['Stage']
                    counts_df = counts_df.drop('Stage', axis=1)
                    return (raw_data, counts_df)
            else:
                return (raw_data, raw_data)
        
        elif analysis_method == 'Loyalty':
            file_formats = ['Rows of subjects', 'Contingency table']
            selected_format = st.sidebar.radio(label='File format',options=file_formats)
            if selected_format == 'Rows of subjects':
                first_column_text = st.sidebar.text_input("Which column in file for 1st purchase?")
                second_column_text = st.sidebar.text_input("Which column in file for 2nd purchase?")
                
                if first_column_text and second_column_text:
                    try:
                        first_column_int = int(first_column_text) - 1 
                        second_column_int = int(second_column_text) - 1
                        if first_column_int < 0 or first_column_int > raw_data.shape[1] or second_column_int < 0 or second_column_int > raw_data.shape[1] or second_column_int==first_column_int:
                            st.sidebar.warning('Both columns should be in the data and different from eachother')
                            return (raw_data, None)
                        else:
                            st.session_state['loyalty_col_1'] = first_column_text
                            st.session_state['loyalty_col_2'] = second_column_text
                            contingency_table = pd.crosstab(raw_data.iloc[:,first_column_int].astype(int),
                                                        raw_data.iloc[:,second_column_int].astype(int))
                            return (raw_data, contingency_table)
                    except:
                        st.sidebar.warning("Both columns must be integers")
                        return (raw_data, None)
                else:
                    # Waiting for both inputs
                    return (raw_data, None)
            else:
                # Contingency table format
                return (raw_data, raw_data)
            
        elif analysis_method == 'Ranking':
            file_formats = ['Rows of subjects', 'Contingency table']
            selected_format = st.sidebar.radio(label='File format', options=file_formats)
            
            if selected_format == 'Rows of subjects':
                brand_col = st.sidebar.selectbox('Which column in file for brand?', np.arange(1, raw_data.shape[1]+1), index=None, placeholder='Select column')
                ranking_col = st.sidebar.selectbox('Which column in file for ranking?', np.arange(1, raw_data.shape[1]+1), index=None, placeholder='Select column')
                
                if brand_col and ranking_col:
                    brand_col = brand_col - 1
                    ranking_col = ranking_col - 1
                    st.session_state['ranking_brand_col'] = brand_col
                    st.session_state['ranking_level_col'] = ranking_col
                    contingency_table = pd.crosstab(raw_data.iloc[:, brand_col],
                                                  raw_data.iloc[:, ranking_col])
                    # Ensure columns are in order
                    contingency_table = contingency_table.reindex(sorted(contingency_table.columns), axis=1)
                    return (raw_data, contingency_table)
            else:
                # For contingency table format, ensure numeric data and sort columns
                contingency_table = raw_data.astype(float)
                return (raw_data, contingency_table)
        
        elif analysis_method == 'Spacing Models':
            file_formats = ['Rows of subjects', 'Contingency table']
            selected_format = st.sidebar.radio(label='File format', options=file_formats, key='spacing_file_format')
            
            if selected_format == 'Rows of subjects':
                row_column = st.sidebar.selectbox('Which column in file for rows?', np.arange(1, raw_data.shape[1]+1), index=None, placeholder='Select column', key='spacing_row_col')
                column_column = st.sidebar.selectbox('Which column in file for columns?', np.arange(1, raw_data.shape[1]+1), index=None, placeholder='Select column', key='spacing_col_col')
                
                if row_column and column_column:
                    row_column = row_column - 1
                    column_column = column_column - 1
                    contingency_table = pd.crosstab(raw_data.iloc[:, row_column].astype(int),
                                                    raw_data.iloc[:, column_column].astype(int))
                    return (raw_data, contingency_table)
            else:
                return (raw_data, raw_data)
        
        elif analysis_method == 'Scale Conversion':
            # Scale conversion needs two categorical columns to compute frequency distributions
            row_column = st.sidebar.selectbox('Which column for Row scale?', np.arange(1, raw_data.shape[1]+1), index=None, placeholder='Select column', key='scale_row_col')
            col_column = st.sidebar.selectbox('Which column for Column scale?', np.arange(1, raw_data.shape[1]+1), index=None, placeholder='Select column', key='scale_col_col')
            
            if row_column and col_column:
                row_column = row_column - 1
                col_column = col_column - 1
                
                # Compute frequency distributions for each column
                row_counts = raw_data.iloc[:, row_column].value_counts().sort_index()
                col_counts = raw_data.iloc[:, col_column].value_counts().sort_index()
                
                # Create a DataFrame with the frequency distributions
                # Pad shorter series with zeros to make them same length if needed
                max_len = max(len(row_counts), len(col_counts))
                
                scale_data = pd.DataFrame({
                    'Row_Scale': list(row_counts.values) + [0] * (max_len - len(row_counts)),
                    'Col_Scale': list(col_counts.values) + [0] * (max_len - len(col_counts)),
                    'Row_Labels': list(row_counts.index) + [None] * (max_len - len(row_counts)),
                    'Col_Labels': list(col_counts.index) + [None] * (max_len - len(col_counts))
                })
                
                return (raw_data, scale_data)
            return (raw_data, None)
                
            
    return (None, None)

def clean_df(df):
    # Reset the index so it's a default RangeIndex
    df = df.reset_index(drop=True)
    df.index = df.index+1
    # Convert column names to strings
    df.columns = [str(col) for col in df.columns]
    # Convert every element to a native Python type if it's a NumPy type
    df = df.applymap(lambda x: int(x) if isinstance(x, np.integer) else x)
    # Clear any extra attributes that might not be serializable
    df.attrs = {}
    return df


def data_report(contingency_table):
    result = "Contingency Table: \n\n"
    result += pd.DataFrame(contingency_table).to_markdown(index=True)
    
    return result

def parse_stages_to_collapse(stages_text):
    parsed_stages = []
    for part in stages_text.split(','):
        part = part.strip()
        if '-' in part:
            start, end = part.split('-')
            parsed_stages.extend(list(range(int(start), int(end) + 1)))
        else:
            parsed_stages.append(int(part))
    return parsed_stages


def parse_text_groups(text_input):
    # Remove all whitespace
    text = re.sub(r'\s+', '', text_input)
    
    # Split into groups separated by ';'
    groups = text.split(';')
    
    parsed_groups = []
    all_numbers = set()
    
    for group in groups:
        group_numbers = set()
        segments = group.split(',')
        
        for segment in segments:
            if '-' in segment:
                start, end = map(int, segment.split('-'))
                # Generate range of numbers inclusive
                numbers = set(range(start-1, end))
            else:
                numbers = {int(segment)-1}
            
            # Check for duplicate numbers across groups
            if not group_numbers.isdisjoint(numbers):
                raise ValueError(f"Error: Number {numbers.intersection(group_numbers)} is included in multiple segments of the same group.")
            
            if not all_numbers.isdisjoint(numbers):
                raise ValueError(f"Error: Number {numbers.intersection(all_numbers)} is included in multiple groups.")
            
            group_numbers.update(numbers)
        
        parsed_groups.append(sorted(group_numbers))
        all_numbers.update(group_numbers)
    
    return parsed_groups

def parse_row_or_column_groups(text_input, orientation, contingency_table):
    parsed_numbers = parse_text_groups(text_input)
    
    if isinstance(parsed_numbers, str):  # If there was an error in parsing, return the error message
        st.error(parsed_numbers)
        return None
    
    if orientation == 'Row':
        max_allowed = contingency_table.shape[0]
    elif orientation == 'Column':
        max_allowed = contingency_table.shape[1]

    for group in parsed_numbers:
        if max(group) > max_allowed:
            st.error(f"Error: Number {max(group)} exceeds the maximum allowed index {max_allowed} for {orientation.lower()}s.")
            return None
    
    return parsed_numbers

def group_label(n, groups):
    # Ensure that all indices are within bounds
    for g in groups:
        for idx in g:
            if idx < 0 or idx >= n:
                raise IndexError(f"Index {idx} is out of bounds for an array of size {n}")

    label = [0] * n
    for g in range(len(groups)):
        for idx in groups[g]:
            label[idx] = g + 1
    
    num_missing = label.count(0)
    next_label = len(groups) + 1
    for i in range(n):
        if label[i] == 0:
            label[i] = next_label
            next_label += 1
    
    output = [0] * n
    group_order = []
    seen = set()
    for l in label:
        if l not in seen:
            group_order.append(l)
            seen.add(l)
    
    for i, g in enumerate(group_order):
        for j in range(n):
            if label[j] == g:
                output[j] = i + 1
    
    return output


def collapse_data(data, row_group=None, col_group=None, as_data_metrix=False):
    if isinstance(data, pd.DataFrame):
        m = data.values
    else:
        m = data
    
    is_collapsed = False
    
    if row_group:
        row_label = group_label(m.shape[0], row_group)
        if any(np.array(row_label) != np.arange(1, m.shape[0] + 1)):
            num_label = max(row_label)
            m_new = np.zeros((num_label, m.shape[1]))
            for i in range(1, num_label + 1):
                m_new[i - 1, :] = np.sum(m[np.array(row_label) == i, :], axis=0)
            m = m_new
            is_collapsed = True
    
    if col_group:
        col_label = group_label(m.shape[1], col_group)
        if any(np.array(col_label) != np.arange(1, m.shape[1] + 1)):
            num_label = max(col_label)
            m_new = np.zeros((m.shape[0], num_label))
            for j in range(1, num_label + 1):
                m_new[:, j - 1] = np.sum(m[:, np.array(col_label) == j], axis=1)
            m = m_new
            is_collapsed = True
    
    if is_collapsed:
        if isinstance(data, pd.DataFrame) or as_data_metrix:
            data = pd.DataFrame(m, index=np.arange(1,m.shape[0]+1), columns=np.arange(1,m.shape[1]+1))
        else:
            data = m
    
    
    return data

            
def signif(x, digits):
    def format_number(n, digits):
        if n == 0:
            return "0"
        else:
            return f"{n:.{digits}g}"
    
    if isinstance(x, (list, np.ndarray, pd.Series)):
        return np.vectorize(lambda n: format_number(n, digits))(x)
    elif isinstance(x, pd.DataFrame):
        return x.applymap(lambda n: format_number(n, digits))
    else:  # scalar
        return format_number(x, digits)
    

def array_data(a):
    if np.isscalar(a):
        # Handle single scalar value
        return pd.DataFrame({'Value': [a]}, index=['0'])
    
    a = np.asarray(a)
    if a.ndim == 1:
        # Handle 1D array
        return pd.DataFrame(a, columns=['Value'], index=map(str, range(len(a))))
    
    # Handle multi-dimensional arrays
    shape = a.shape
    df = pd.DataFrame(a.flatten(), columns=['Value'])
    indices = np.indices(shape).reshape(len(shape), -1).T
    index_names = [",".join(map(str, idx + 1)) for idx in indices]
    df.index = index_names
    
    return df

def array_string(a):
    df = array_data(a)
    s = ""
    for i, row in df.iterrows():
        s += f"{i} : {row['Value']}\n\n"
    return s

def space(n):
    """
    Return a string of spaces of length n.
    """
    return ' ' * int(n)

def matrix_string(m, spacing=2, left_justified=True):
    """
    Converts a matrix to a nicely formatted string with specified spacing.
    """
    m = np.array(m, dtype=str)
    m[m == 'nan'] = "NA"
    
    # Debug print statements
    print("Matrix shape:", m.shape)
    print("Matrix contents:\n", m)

    # Determine the longest entry in each column
    col_widths = [max(len(entry) for entry in m[:, j]) for j in range(m.shape[1])]
    
    # Format each entry in the matrix
    for j in range(m.shape[1]):
        for i in range(m.shape[0]):
            if left_justified:
                m[i, j] = m[i, j] + space(col_widths[j] - len(m[i, j]))
            else:
                m[i, j] = space(col_widths[j] - len(m[i, j])) + m[i, j]
    
    # Add spacing between columns
    for j in range(m.shape[1] - 1):
        for i in range(m.shape[0]):
            m[i, j] = m[i, j] + space(spacing)
    
    # Join rows and columns to create the final string
    return "\n".join(["".join(row) for row in m])

def parse_indices_string(s):
                        indices = set()
                        for part in s.split(","):
                            part = part.strip()
                            if "-" in part:
                                try:
                                    start, end = part.split("-")
                                    start = int(start.strip())
                                    end = int(end.strip())
                                    indices.update(range(start, end + 1))
                                except Exception as e:
                                    st.sidebar.error(f"Invalid range format in '{part}'.")
                            elif part.isdigit():
                                indices.add(int(part))
                            elif part:
                                st.sidebar.error(f"Invalid input: '{part}'.")
                        return sorted(indices)