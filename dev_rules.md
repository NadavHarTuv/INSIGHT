# Development Rules & Principles

## Error Handling Philosophy: Fail Fast

1. **NO silent failures or fallbacks** - Code should throw clear exceptions rather than degrading to partial functionality.

2. **NO defensive returns** - Never return "basic versions" of outputs when proper data is missing. If a function can't do its job properly, it should fail clearly.

3. **NO hiding of root causes** - Exceptions should propagate so the source of the problem is visible. Don't catch exceptions just to return a simplified output.

4. **NO hiding error details** - Error messages should include the complete context about what went wrong.

5. **YES to clear error reporting** - All errors should report exactly where they occurred and why.

6. **YES to early validation** - Check requirements at the beginning of functions and fail immediately if they can't be met.

## Implementation Guidelines

When modifying code:

- Remove try/except blocks that catch broad exceptions
- Remove fallback logic that returns partial or default results
- Add explicit validation for required inputs at function start
- Replace default values with proper error propagation
- Ensure error messages identify exactly what's missing or invalid

## Why This Matters

Hiding errors through fallbacks makes debugging harder by obscuring the real issues. It's better to have code fail immediately and clearly when problems occur rather than producing invalid or incomplete results.

Errors are valuable information that should be exposed, not suppressed. 