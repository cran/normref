# normref 0.1.1
- Reduced vignette build time by precomputing results and loading them during checks

# normref 0.1.0

## New features
- Added `sample_size_poly()` for sample size planning in continuous norming using distribution free polynomial regression
- Patched `fb_select` to support families with a single distributional parameter

---

# normref 0.0.0.1

- Initial release
- Includes functions for shaping data, estimating reliability, and creating norm tables
- Functions to shape data for GAMLSS modeling `shape_data()` and `composite_shape()`
- Forward–backward selection with `fb_select()`
- Norm table creation with `normtable_create()`
- Reliability estimation with `reliability_window()` and `different_rel()`
- Plotting and visualization functions included: `plot_normtable()`, `plot_Drel()`

