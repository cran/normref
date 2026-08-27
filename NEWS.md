# normref 0.1.2

- Patched `normtable_create()` to include age grid in `cdf_matrix`.
- Patched spline handling in `fb_select()`. 
- Change default continuity correction to `TRUE` for discrete families in `normtable_create()`.
- Patched newdata handling with continuity correction in `normtable_create()`.
- Speed up `fb_select` with a cache, and remove parallel processing option.
- `fb_select` now returns model selection history. 
- Documented model selection history feature in the vignette.
- Add `weights` option to `composite_shape`.

# normref 0.1.1

- Added `sample_size_poly()` for sample size planning in continuous norming using distribution free polynomial regression
- Patched `fb_select` to support families with a single distributional parameter

---

# normref 0.0.0.1

- Initial release
- Includes functions for shaping data, estimating reliability, and creating norm tables
- Functions to shape data for GAMLSS modeling `shape_data()` and `composite_shape()`
- Forward-backward selection with `fb_select()`
- Norm table creation with `normtable_create()`
- Reliability estimation with `reliability_window()` and `different_rel()`
- Plotting and visualization functions included: `plot_normtable()`, `plot_Drel()`

