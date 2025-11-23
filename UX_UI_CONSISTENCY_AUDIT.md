# UX/UI Consistency Audit Report
## INSIGHT Streamlit Application

**Priority Guidelines:**
1. Don't break anything that works
2. Consistent with R phrasing (keep "Go!", "Go Testing!", etc.)
3. Internal consistency across different sections

---

## CRITICAL INCONSISTENCIES FOUND

### 1. **INPUT TYPES FOR SIMILAR PURPOSES**

#### Column Selection Inconsistencies
**Problem:** Same purpose (selecting a column index) uses different input types across modules

- **Independence**: Uses `st.sidebar.selectbox` with numeric options
  ```python
  # Line 101-102 in app.py
  row_column = st.sidebar.selectbox('Which column for rows?', np.arange(1,raw_data.shape[1]), ...)
  column_column = st.sidebar.selectbox('Which column for columns?', np.arange(1,raw_data.shape[1]), ...)
  ```

- **3-Dimensional**: Uses `st.sidebar.text_input` requiring manual integer entry
  ```python
  # Line 115-134 in app.py
  X = st.sidebar.text_input('Which column in file for variable X?', value=None)
  Y = st.sidebar.text_input('Which column in file for variable Y?', value=None)
  Z = st.sidebar.text_input('Which column for variable Z?', value=None)
  ```

- **Survival**: Uses `st.sidebar.text_input`
  ```python
  # Line 158 in utils.py
  st.session_state['survival_col'] = st.sidebar.text_input(label='Which column in file?')
  ```

- **Loyalty**: Uses `st.sidebar.text_input`
  ```python
  # Line 191-192 in utils.py
  first_column_text = st.sidebar.text_input("Which column in file for 1st purchase?")
  second_column_text = st.sidebar.text_input("Which column in file for 2nd purchase?")
  ```

- **Ranking**: Uses `st.sidebar.selectbox` (correct!)
  ```python
  # Line 222-223 in utils.py
  brand_col = st.sidebar.selectbox('Which column for brand?', np.arange(1, raw_data.shape[1]), ...)
  ranking_col = st.sidebar.selectbox('Which column for ranking?', np.arange(1, raw_data.shape[1]), ...)
  ```

**Recommendation:** Standardize to `st.sidebar.selectbox` for all column selections (like Independence and Ranking do). This prevents user input errors and provides clearer feedback.

---

#### Range/Group Input Inconsistencies
**Problem:** Multiple different formats for entering ranges/groups of indices

- **Independence - Data Collapse**: `st.sidebar.text_input` with help text
  ```python
  # Lines 194-207 in app.py
  row_groups_str = st.sidebar.text_input("Row groups to collapse (optional)", 
      help="Enter row groups to collapse (e.g., '1-3;4,5'...")
  col_groups_str = st.sidebar.text_input("Column groups to collapse (optional)", 
      help="Enter column groups to collapse...")
  ```

- **Independence - ANOAS**: `st.sidebar.text_input` with DIFFERENT help text
  ```python
  # Line 264 in app.py
  group_string_anoas = st.sidebar.text_input("ANOAS groups", "", help=independence.anoas_help_text)
  ```

- **Independence - Collapse Chi-Sq**: `st.sidebar.text_input` with SHORT help text
  ```python
  # Lines 288-289 in app.py
  row_groups_str = st.sidebar.text_input("Row groups", help="Enter row groups (e.g., '1-3;4,5')")
  col_groups_str = st.sidebar.text_input("Column groups", help="Enter column groups (e.g., '1,2;3-4')")
  ```

- **Survival - Splining**: `st.sidebar.text_input` with NO label consistency
  ```python
  # Line 505 in app.py
  splining_str = st.sidebar.text_input("(Optional) Splining", value="")
  ```

- **Survival - Testing Splining**: Different label again
  ```python
  # Line 529 in app.py
  testing_splining_str = st.sidebar.text_input("(Optional) Testing Splining", value="")
  ```

- **Survival - Death Stages**: Yet another format
  ```python
  # Lines 559-562 in app.py
  death_stages_str = st.sidebar.text_input(
      "Enter death stages (range, e.g. 3-5)", value="", key="expl_death_stages")
  ```

- **Ranking - Upper Polarity**: Separate help text
  ```python
  # Lines 913-916 in app.py
  upper_polarity_str = st.sidebar.text_input(
      "Enter upper polarity index (e.g., '4-7' for levels 4 to 7, or comma-separated numbers)",
      value="", key="ranking_upper_polarity")
  ```

**Recommendation:** Create unified help text constant and label pattern:
```python
# In utils.py
RANGE_INPUT_HELP = "Enter ranges using format: '1-3;4,5' (ranges with '-', groups with ',', separate groups with ';')"
```

---

### 2. **BUTTON LABELS**

**Problem:** While R phrasing should be maintained, button labeling is inconsistent

Current buttons found:
- `"Go!"` - Used in: Independence (line 301), 3D (lines 364, 382, 399), N-Dim (line 457), Survival (lines 510, 521, 533), Loyalty (lines 637, 653, 681, 1036), Ranking (line 937)
- `"Go Training!"` - Survival (line 521)
- `"Go Testing!"` - Survival (line 533, with `key="go_testing"`)
- `"Create explanatory data and sort variables"` - Survival (line 577)
- `"Plot!"` - threedim.py (line 1012), ndim.py (line 961)
- `"Compute!"` - threedim.py (line 1054), ndim.py (line 993)
- `"Collapse Data"` - Independence (line 210)
- `"Show!"` - ndim.py (line 663)
- `"Download New CSV"` - Multiple places (lines 618, 706, 1090)

**Recommendation:** Keep R convention but standardize related actions:
- Analysis/computation: `"Go!"` ✓
- Sub-analysis for training: `"Go Training!"` ✓
- Sub-analysis for testing: `"Go Testing!"` ✓
- Plotting: Standardize to `"Plot!"`
- Computing metrics: Standardize to `"Compute!"`
- Data operations: Use descriptive labels like `"Collapse Data"`

**Action:** The only changes needed:
1. Ensure all keys are unique (currently some "Go!" buttons may conflict)
2. Standardize "Plot!" vs "plot!" (capitalize consistently)

---

### 3. **CHECKBOX vs RADIO vs SELECTBOX**

**Problem:** Binary choices use different widgets

**Checkboxes used for binary choices:**
- Survival: `show_plot = st.sidebar.checkbox("Show plot?")` (line 508)
- Survival: `reproducible = st.sidebar.checkbox("Reproducible?", value=True)` (line 520)
- Survival: `show_plot_testing = st.sidebar.checkbox("Show plot for testing?")` (line 530)
- Survival: `sort_explanatory = st.sidebar.checkbox("Sort explanatory variables?", value=True, key="sort_expl")` (line 570)
- Loyalty: Multiple sort checkboxes (lines 676, 1030)

**Radio buttons used for binary choices:**
- Independence ANOAS: `orientation_anoas = st.sidebar.radio("Select orientation", ["Row", "Column"])` (line 263)
- Independence Z-Test: `orientation_ztest = st.sidebar.radio("Select orientation", ["Row", "Column"])` (line 275)
- 3D: `computation_choice = st.sidebar.radio("Computation Type", ["All coefficients", "Select coefficients", "Target variable"])` (line 350) - 3 options, OK
- Survival: `selected_model = st.sidebar.selectbox("Select Survival Model", ["Homogeneous, ACC/DC", "Out-of-Sample Splining", "Explanatory Variable"])` (line 498)
- Utils: `selected_format = st.sidebar.radio(label='File format', options=file_formats)` (lines 98, 155, 189, 219)

**Selectbox with 2 options:**
- Loyalty explanatory: `first_purchase_with = st.selectbox(label="1st purchase is", options=['with', 'without'], ...)` (line 677-682 in loyalty.py)

**Recommendation (following standard UX guidelines):**
- **Checkboxes**: For boolean yes/no toggles (show/hide, enable/disable, sort/don't sort)
  - ✓ Keep: "Show plot?", "Reproducible?", "Sort explanatory variables?"
  
- **Radio buttons**: For categorical choices where user MUST select exactly one (typically 2-4 options, mutually exclusive)
  - ✓ Keep: "Row" vs "Column" orientation (these are truly categorical, not boolean)
  - ✓ Keep: File format choices
  - ✓ Keep: 3D computation type (3 options)
  
- **Selectbox**: For 3+ options or when space is limited
  - ✓ Keep: Model selection dropdowns

**Current usage is CORRECT!** No changes needed here.

---

### 4. **TEXT INPUT vs NUMBER INPUT**

**Problem:** Numeric inputs use text_input instead of number_input

**Text inputs used for numbers:**
- Training portion: `training_portion = st.sidebar.text_input("Training portion (in %)", value="50")` (line 519)
- Propensity threshold: `propensity_threshold = st.text_input("Propensity Threshold", ...)` (lines 1053, 992)
- All column indices
- All explanatory variable indices
- All reward values in Model Value tabs (lines 996-1009 in threedim.py, 945-958 in ndim.py)

**Recommendation:** Use `st.number_input` for:
- Training portion percentage (with min=0, max=100, step=1)
- Propensity threshold (with min=0.0, max=1.0, step=0.01)
- Reward values (with step=0.1 or 1)

**DO NOT change column/index inputs** - these are better as selectbox (see recommendation #1)

---

### 5. **OUTPUT DISPLAY INCONSISTENCIES**

#### Section Headers
**Problem:** Different markdown levels and formats for similar content

- Independence: `'## Independence Model Results\n\n'` then `'### Observed Data Matrix \n'` (lines 146-150 in independence.py)
- ANOAS: `'# ANOAS Model Result\n\n'` then `'### Observed Data Matrix \n'` (lines 359-360)
- 3D all_coef: `"#3-Dimensional Model Report \n\n"` then `"## List of possible models \n \n"` (lines 383-384)
- 3D select_coef: `'# 3-Dimensional Model Report\n\n'` then `'## Results for the selected model:\n\n'` (lines 510-511)
- 3D target: `"# 3-Dimensional Model Result\n\n"` then `' ## Results for the selected model: \n\n'` (lines 636-637)
- N-Dim: `"## n-Dimensional - Model Selection Result\n"` (line 590)
- Survival Homogeneous: `"Homogeneous Model Result\n"` then `"========================\n\n"` (lines 103-104)
- Survival ACC/DC: `"ACC/DC Model Result\n"` then `"========================\n\n"` (lines 281-282)
- Loyalty M Model: `"M Model Result"` then `"=============="` (lines 320-321)
- Log-Odds Z-Test: `'# Log-odds Z-Test Result \n\n'` (line 561)

**Recommendation:** Standardize to:
```python
# Top-level: Use single # with model name
"# {Model Name} Result\n\n"
# Second-level sections: Use ##
"## Observed Data Matrix\n\n"
# Third-level: Use ###
"### Parameter Estimates\n\n"
```

Remove separator lines like `"===========\n"` - markdown handles formatting.

---

#### DataFrame Display
**Problem:** Mix of direct display, utils.clean_df(), and manual formatting

**Current approaches:**
- `st.dataframe(utils.clean_df(item))` (line 29, 65, 721, 860, 1103)
- `st.dataframe(item)` (lines 730, 820, 826, 845, 1072, etc.)
- `st.dataframe(result)` without cleaning
- Some tabs display raw DataFrames in reports

**Recommendation:** ALWAYS use `st.dataframe(utils.clean_df(df))` when displaying DataFrames from results. Update `display_result_items()`:

```python
def display_result_items(result_items):
    if isinstance(result_items, list):
        for item in result_items:
            if isinstance(item, pd.DataFrame):
                st.dataframe(utils.clean_df(item))  # ✓ Already correct
            elif isinstance(item, str):
                # ... rest of logic
```

---

#### Tab Naming Conventions
**Problem:** Inconsistent tab naming patterns

- Independence: `"Data"`, `"Result {n}"`
- 3D: `"Data"`, `"Target variable {n}"`
- N-Dim: `"Data"`, `"Result {n}"`
- Survival: `"Data"`, `"Stages {start}-{end}"`, `"Stages {start}-{end} (Test)"`
- Loyalty: `"Data"`, `"M Model {n}"`, `"Q Model {n}"`, `"Explanatory Result {n}"`
- Ranking: `"Data"`, `"Exploratory {n}"`, `"Confirmatory {n}"`, `"Explanatory Result {n}"`

**Recommendation:** Standardize to: `"{Model Type} {n}"` or `"{Description} {n}"`
- Independence: `"Data"`, `"Result {n}"` ✓
- 3D: `"Data"`, `"Result {n}"` (change from "Target variable {n}")
- Survival: `"Data"`, `"Stages {start}-{end}"` ✓ (descriptive is better here)
- Others: Keep current pattern ✓

---

### 6. **HELP TEXT / PLACEHOLDER INCONSISTENCIES**

**Problem:** Same input types have different or missing help text

**Column inputs:**
- Some: `index=None, placeholder='Select column'`
- Some: No placeholder
- Some: Text input with no guidance

**Range inputs:**
- Some: Detailed help text with examples
- Some: Brief help
- Some: No help text at all

**Recommendation:** Add consistent help text using a helper function:

```python
# In utils.py
def render_column_selector(label, raw_data, key):
    """Standardized column selector"""
    return st.sidebar.selectbox(
        label, 
        options=np.arange(1, raw_data.shape[1]), 
        index=None, 
        placeholder='Select column',
        key=key
    )

def render_range_input(label, help_text=None, key=None):
    """Standardized range input"""
    if help_text is None:
        help_text = RANGE_INPUT_HELP
    return st.sidebar.text_input(label, value="", help=help_text, key=key)
```

---

### 7. **REWARD INPUT TABLES (Model Value)**

**Problem:** Reward inputs duplicated across threedim.py and ndim.py with IDENTICAL code

**Locations:**
- `threedim.py` lines 955-1076 (`model_value_tab`)
- `ndim.py` lines 897-1008 (`model_value_tab`)

Code is 95% identical - only difference is data structure access.

**Recommendation:** Extract to shared component in utils.py:

```python
# In utils.py
def render_model_value_inputs(result_name, computed_data, propensity_matrix, explanatory_variables, 
                               compute_fn_values, compute_fn_accuracies, display_fn):
    """Shared Model Value tab rendering"""
    # ... shared logic ...
```

---

### 8. **CRITICAL: MISSING UNIQUE KEYS**

**Problem:** Multiple "Go!" buttons without unique keys can cause Streamlit conflicts

**Buttons needing unique keys:**
- Independence "Go!" (line 301) - ✓ No key (only one per model selection)
- 3D "Go!" (lines 364, 382, 399) - ✓ Only one shown at a time based on radio selection
- N-Dim "Go!" (line 457) - ✓ No key
- Survival "Go!" (line 510) - ✓ No key
- Survival "Go Training!" (line 521) - ✓ No key
- Survival "Go Testing!" (line 533) - ✓ Has key="go_testing"
- Survival "Create explanatory..." (line 577) - ✓ Has key="go_explanatory"
- Loyalty M/Q "Go!" (lines 637, 653) - ✓ Has key="loyalty_m_go", "loyalty_q_go"
- Loyalty Explanatory "Go!" (line 681) - ✓ Has key="loyalty_ex_go"
- Ranking "Go!" (line 937) - ✓ Has key built from f"ranking_{ranking_model.lower()}_go"
- Ranking Explanatory "Go!" (line 1036) - ✓ Has key="ranking_expl_go"

**Status:** All buttons that need unique keys already have them! ✓

---

## SUMMARY OF REQUIRED CHANGES

### HIGH PRIORITY (Breaks Consistency)

1. **Standardize column selection to selectbox**
   - Change 3D X/Y/Z to selectbox (3 changes)
   - Change Survival column to selectbox (1 change)
   - Change Loyalty purchase columns to selectbox (2 changes)

2. **Unify range input help text**
   - Create RANGE_INPUT_HELP constant in utils.py
   - Apply to all range inputs (~12 locations)

3. **Standardize section headers in reports**
   - Use `#` for top-level, `##` for sections, `###` for subsections
   - Remove manual separator lines
   - (~15 report functions)

4. **Always use utils.clean_df() for DataFrames**
   - Ensure all DataFrame displays are cleaned
   - (~10 locations)

### MEDIUM PRIORITY (Improves UX)

5. **Convert numeric text_input to number_input**
   - Training portion (1 change)
   - Propensity threshold (2 changes)
   - Reward values (8 changes)

6. **Standardize tab naming**
   - 3D: Change "Target variable {n}" to "Result {n}"
   - (1 location)

### LOW PRIORITY (Code Quality)

7. **Extract shared Model Value rendering**
   - Create shared component in utils.py
   - Refactor threedim and ndim to use it
   - (~200 lines of deduplication)

---

## ITEMS THAT ARE CORRECT (No Changes Needed)

✓ Button labels maintain R phrasing convention
✓ Checkbox vs Radio usage follows UX best practices
✓ All buttons have unique keys where needed
✓ File format selection is consistent
✓ Download button labels are descriptive

---

## IMPLEMENTATION PLAN

### Phase 1: Input Standardization (Priority 1-2)
1. Create helper functions in utils.py
2. Update column selectors across all modules
3. Unify help text

### Phase 2: Output Standardization (Priority 3-4)
1. Standardize report headers
2. Ensure clean_df() usage

### Phase 3: UX Polish (Priority 5-6)
1. Convert to number_input where appropriate
2. Unify tab naming

### Phase 4: Code Quality (Priority 7)
1. Extract shared components
2. Document patterns for future development


