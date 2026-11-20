# normref 1.0.0

- **Added bootstrap functionality** through `boot_subtest()` and `boot_composite()` for post-hoc nonparametric bootstrap estimation of normed-score precision for subtest and composite scores. Precision can be compared against benchmarks based on traditional norming guidelines. Parametric bootstrap estimates are also available for subtest sample planning.
- **Added bootstrap visualization functions**: `boot_heatmap()`, `boot_centiles()`, and `boot_sample()` for visualizing bootstrap results.
- **Added a vignette** demonstrating the new bootstrap functionality and its applications.
- **Added scaled scores (`SS`)** as a supported `norm_type` in `normtable_create()`.
- **Improved `normtable_create()`** to support `gamlss` model objects fitted directly, rather than only models returned by `fb_select()`.
- **Updated `normtable_create()`**: added `newdata` for evaluating norms on new data while preserving the model's training data; deprecated the boolean `new_data` argument.
- **Added warm starts to model selection in `fb_select()`** using `start.from`. This may result in minor differences in selected models or parameter estimates compared with previous versions, as different starting values can lead to slightly different convergence endpoints.
- **Updated package metadata** to include the grant acknowledgement and funding information.

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

