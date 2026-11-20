
#' Create a norm table based on a GAMLSS fitted model
#'
#' @description 
#' `normtable_create()` creates a norm table based on a fitted \code{GAMLSS} model.
#'
#' @details
#' If `excel = TRUE`, results are written to an Excel file via the \pkg{openxlsx2} package.  
#' If the package is not installed, a message is printed and the function continues
#' without writing an Excel file. By default, the file is written to a temporary path 
#' (see [tempfile()]); if you want to keep the file permanently, provide your own file 
#' name via the `excel_name` argument (e.g., `"norms.xlsx"`).
#' 
#' @param model a GAMLSS fitted model, for example the result of [fb_select()].
#' @param data data.frame. The sample on which the model was fitted. If omitted,
#'   it is retrieved directly from \code{model}; must contain the score variable (with name given in \code{score_name}) and age variable 
#'   (with name given in \code{age_name}).
#' @param age_name string. Name of the age variable.
#' @param score_name string. Name of the score variable.
#' @param datarel data.frame or numeric. If a data.frame, must contain columns `age` and `rel`, 
#'   with estimated test reliability per age. If numeric, a constant reliability is assumed 
#'   for all ages (optional, only needed for confidence intervals).
#' @param normtype string. Norm score type: `"Z"` (N(0,1); default), `"T"` (N(50,10)), `"IQ"` (N(100,15)), or `"SS"` (N(10,3)).
#' @param min_age numeric. Lowest age value in the norm table; default is the first integer 
#'   below the minimum observed age.
#' @param max_age numeric. Highest age value in the norm table; default is the first integer 
#'   above the maximum observed age.
#' @param min_score numeric. Lowest score value in the norm table; default is the minimum 
#'   observed score.
#' @param max_score numeric. Highest score value in the norm table; default is the maximum 
#'   observed score.
#' @param step_size_score numeric. Increment of the scores in the norm table; default is 1.
#' @param step_size_age numeric. Increment of the ages in the norm table; defaults to 
#'   approximately 100 ages in total.
#' @param cont_cor Logical. If \code{TRUE}, apply a continuity correction for discrete families.
#'   Default is \code{TRUE}.
#' @param ci_level numeric. Confidence interval level (if \code{datarel} is provided). 
#'   Default is 0.95.
#' @param trim numeric. Trim norm scores at ± \code{trim} standard deviations. 
#'   Default is 3.
#' @param excel logical. If `TRUE`, attempt to write results to an Excel file. Default is `FALSE`.
#' @param excel_name character. Path to the Excel file. Defaults to a temporary file.
#'   Ignored if `excel = FALSE`.
#' @param new_data logical. Deprecated; kept for backward compatibility. Use `newdata` instead.
#' @param newdata data.frame. Optional new data on which sample-level norm scores 
#'   are evaluated. If provided, full norm matrices are omitted.
#' @param warn_extrapolation logical. If \code{TRUE} (default), a warning is issued when values in
#'   \code{newdata} fall outside the range of the normative sample. Set to \code{FALSE} in internal
#'   resampling or bootstrap routines.
#'
#' @return A list of class \code{NormTable} containing:
#' * \code{norm_sample}: Estimated norm scores (\code{normtype}) in the sample, trimmed at \code{trim}.
#' * \code{norm_sample_lower}, \code{norm_sample_upper}: Lower and upper \code{ci_level} confidence bounds of \code{norm_sample}.
#' * \code{norm_matrix}: Norm scores (\code{normtype}) by age.
#' * \code{norm_matrix_lower}, \code{norm_matrix_upper}: Lower and upper \code{ci_level} bounds of \code{norm_matrix}.
#' * \code{znorm_sample}: Estimated Z scores in the sample.
#' * \code{cdf_sample}: Estimated percentiles in the sample.
#' * \code{cdf_matrix}: Percentile table by age.
#' * \code{data}, \code{age_name}, \code{score_name}: Copies of respective function arguments.
#' * \code{pop_age}: Evaluated ages in the norm table.
#' 
#' @importFrom Rdpack reprompt
#' 
#' @references
#' \insertRef{timmerman2021tutorial}{normref}
#' 
#' @seealso [fb_select()], [plot_normtable()]
#'
#' @examples
#' \donttest{
#' # Load example data
#' invisible(data("ids_data"))
#' 
#' # Prepare data for modeling
#' mydata_BB_y14 <- shape_data(
#'   data = ids_data,
#'   age_name = "age",
#'   score_name = "y14",
#'   family = "BB"
#' )
#' 
#' # Fit model using BIC as selection criterion
#' mod_BB_y14 <- fb_select(
#'   data = mydata_BB_y14,
#'   age_name = "age",
#'   score_name = "shaped_score",
#'   family = "BB",
#'   selcrit = "BIC"
#' )
#' 
#' # Create norm table from fitted model
#' norm_mod_BB_y14 <- normtable_create(
#'   model = mod_BB_y14,
#'   data = mydata_BB_y14,
#'   age_name = "age",
#'   score_name = "shaped_score"
#' )
#' 
#' # Calculate norms for a new sample using reliability data
#' invisible(data("ids_rel_data"))
#' newdata <- ids_data[1:5, c("age", "y14")]
#' 
#' norm_mod_BB_newdata <- normtable_create(
#'   model = mod_BB_y14,
#'   newdata = newdata,
#'   age_name = "age",
#'   score_name = "y14",
#'   datarel = ids_rel_data
#' )
#' }
#'
#' @export
normtable_create <- function(model,
                             data = NULL,
                             age_name,
                             score_name,
                             datarel = NULL,
                             normtype = "Z",
                             min_age = NULL,
                             max_age = NULL,
                             min_score = NULL,
                             max_score = NULL,
                             step_size_score = 1,
                             step_size_age = NULL,
                             cont_cor = TRUE,
                             ci_level = 0.95,
                             trim = 3,
                             excel = FALSE,
                             excel_name = tempfile("norms", fileext = ".xlsx"),
                             new_data = NULL,
                             newdata = NULL,
                             warn_extrapolation = TRUE) {
  # Benign GAMLSS warnings are muffled during the entire computation
  .muffle_benign_warnings(
    .normtable_create_impl(
      model = model, data = data, age_name = age_name, score_name = score_name,
      datarel = datarel, normtype = normtype, min_age = min_age, max_age = max_age,
      min_score = min_score, max_score = max_score, step_size_score = step_size_score,
      step_size_age = step_size_age, cont_cor = cont_cor, ci_level = ci_level,
      trim = trim, excel = excel, excel_name = excel_name, new_data = new_data,
      newdata = newdata, warn_extrapolation = warn_extrapolation
    )
  )
}

#' Implementation of normtable_create()
#' @inheritParams normtable_create
#' @noRd
.normtable_create_impl <- function(model, data, age_name, score_name, datarel, normtype,
                                   min_age, max_age, min_score, max_score,
                                   step_size_score, step_size_age, cont_cor, ci_level,
                                   trim, excel, excel_name, new_data, newdata,
                                   warn_extrapolation) {
  
  # 1. Backward compatibility check for `new_data`
  if (isTRUE(new_data)) {
    newdata <- data
    data <- NULL
  }
  
  # 2. Resolve fitting data and evaluation sample
  model_data <- tryCatch({
    if (!is.null(model$data)) as.data.frame(model$data) else as.data.frame(get_model_data(model))
  }, error = function(e) NULL)
  
  fitdata <- .resolve_fit_data(data, model_data, age_name, score_name)
  
  is_newdata <- !is.null(newdata)
  eval_data  <- if (is_newdata) as.data.frame(newdata[c(age_name, score_name)]) else fitdata
  
  # Ensure flat 1D score columns
  if (!is.null(dim(eval_data[[score_name]]))) eval_data[[score_name]] <- eval_data[[score_name]][, 1]
  if (!is.null(dim(fitdata[[score_name]])))   fitdata[[score_name]]   <- fitdata[[score_name]][, 1]
  
  if (is_newdata && isTRUE(warn_extrapolation)) {
    .warn_extrapolation(fitdata, eval_data, age_name, score_name)
  }
  
  if (is.null(model$data)) {
    model$data <- fitdata
    model$call$data <- fitdata
  }
  
  # 3. Family object & continuity correction
  fname <- model$family[1]
  family_obj <- find_fun(fname)()
  cont_cor <- isTRUE(cont_cor) && (family_obj$type == "Discrete")
  
  min_score_fit <- min(fitdata[[score_name]], na.rm = TRUE)
  max_score_fit <- max(fitdata[[score_name]], na.rm = TRUE)
  
  # 4. Population norm matrix
  if (is.null(min_age)) min_age <- min(fitdata[[age_name]], na.rm = TRUE)
  if (is.null(max_age)) max_age <- max(fitdata[[age_name]], na.rm = TRUE)
  if (is.null(min_score)) min_score <- min_score_fit
  if (is.null(max_score)) max_score <- max_score_fit
  
  pop_age <- if (is.null(step_size_age)) {
    data.frame(age = seq(min_age, max_age, length.out = 100))
  } else {
    data.frame(age = seq(min_age, max_age, by = step_size_age))
  }
  names(pop_age) <- age_name
  
  q <- seq(from = min_score, to = max_score, by = step_size_score)
  scores_c <- rep(1, nrow(pop_age)) %*% t(q)
  
  if (!is.null(datarel) && !is.data.frame(datarel) && length(datarel) == 1) {
    datarel <- data.frame(
      rel = rep(datarel, 20),
      age = seq(min_age, max_age, length.out = 20)
    )
  }
  
  has_rel <- !is.null(datarel)
  
  pop <- .cdf_to_norm(
    estimateCDF(model, ages = pop_age, scores = scores_c, data = fitdata),
    q = q, pop_age = pop_age, trim = trim, normtype = normtype, cont_cor = cont_cor
  )
  cdf_matrix  <- pop$cdf
  norm_matrix <- pop$norm
  
  if (has_rel) {
    uplow <- intervals(
      model, datarel,
      score_min = min_score_fit, score_max = max_score_fit,
      normingdata = fitdata, scores_c = scores_c,
      pop_age = pop_age, age_name = age_name,
      score_name = score_name, ci_level = ci_level
    )
    bound_norm <- function(bound_matrix) {
      .cdf_to_norm(
        estimateCDF(model, ages = pop_age, scores = bound_matrix, data = fitdata),
        q = q, pop_age = pop_age, trim = trim, normtype = normtype, cont_cor = cont_cor
      )$norm
    }
    norm_matrix_lower <- bound_norm(uplow$lower_matrix)
    norm_matrix_upper <- bound_norm(uplow$upper_matrix)
  } else {
    norm_matrix_lower <- norm_matrix_upper <- NA
  }
  
  # 5. Sample norm scores and confidence bounds (evaluated on eval_data)
  sample_q <- data.frame(eval_data[[score_name]])
  sample_age <- data.frame(eval_data[[age_name]])
  colnames(sample_q) <- score_name
  colnames(sample_age) <- age_name
  
  sample_cdf <- function(scores_df) {
    .cdf_sample(model, scores_df, sample_age, fitdata, eval_data,
                cont_cor = cont_cor, min_score_fit = min_score_fit, score_name = score_name)
  }
  
  if (has_rel) {
    uplow_sample <- intervals(
      model, datarel,
      score_min = min_score_fit, score_max = max_score_fit,
      normingdata = eval_data, scores_c = NULL,
      pop_age = NULL, age_name = age_name,
      score_name = score_name, ci_level = ci_level
    )
    sample_bound_norm <- function(bound) {
      bound_df <- data.frame(bound)
      colnames(bound_df) <- score_name
      z_bound <- CDFtotrimZ(sample_cdf(bound_df), trim)
      cbind(age = eval_data[[age_name]], ZtoScale(z_bound, normtype))
    }
    norm_sample_lower <- sample_bound_norm(uplow_sample$lower_sample)
    norm_sample_upper <- sample_bound_norm(uplow_sample$upper_sample)
  } else {
    norm_sample_lower <- norm_sample_upper <- NA
  }
  
  cdf_sample <- sample_cdf(sample_q)
  z_sample <- CDFtotrimZ(cdf_sample, trim)
  
  znorm_sample <- cbind(eval_data[[age_name]], z_sample)
  colnames(znorm_sample) <- c(age_name, "Z")
  
  norm_sample <- cbind(eval_data[[age_name]], ZtoScale(z_sample, normtype))
  colnames(norm_sample) <- c(age_name, normtype)
  
  # 6. Excel export
  if (excel) {
    .write_norm_excel(
      list(
        "norm matrix" = norm_matrix,
        "norm matrix lower" = norm_matrix_lower,
        "norm matrix upper" = norm_matrix_upper,
        "norm sample" = norm_sample,
        "norm sample lower" = norm_sample_lower,
        "norm sample upper" = norm_sample_upper
      ),
      excel_name
    )
  }
  
  out <- list(
    norm_sample = norm_sample,
    norm_sample_lower = norm_sample_lower,
    norm_sample_upper = norm_sample_upper,
    norm_matrix = norm_matrix,
    norm_matrix_lower = norm_matrix_lower,
    norm_matrix_upper = norm_matrix_upper,
    znorm_sample = znorm_sample,
    cdf_sample = cdf_sample,
    cdf_matrix = cdf_matrix,
    data = eval_data,
    age_name = age_name,
    score_name = score_name,
    pop_age = pop_age
  )
  class(out) <- "NormTable"
  out
}

#' Resolve the fitting data and check it against the model's own data
#' @noRd
.resolve_fit_data <- function(data, model_data, age_name, score_name) {
  req_cols <- c(age_name, score_name)
  
  if (is.null(data)) {
    if (is.null(model_data)) {
      stop("Could not locate the data used to fit `model`. Please supply the original fitting data via `data`.")
    }
    return(model_data)
  }
  
  fitdata <- as.data.frame(data)
  
  # Check whether age and score columns match model's internal data
  if (!is.null(model_data) && all(req_cols %in% names(model_data))) {
    if (!all(req_cols %in% names(fitdata))) {
      stop(sprintf("Supplied `data` must contain '%s' and '%s'.", age_name, score_name))
    }
    
    matches <- c(
      isTRUE(all.equal(fitdata[[age_name]],   model_data[[age_name]])),
      isTRUE(all.equal(fitdata[[score_name]], model_data[[score_name]]))
    )
    if (!all(matches)) {
      stop(sprintf(
        "The supplied `data` does not correspond to the data found in `model$data` for column(s): %s.",
        paste(sprintf("'%s'", req_cols[!matches]), collapse = ", ")
      ))
    }
  }
  fitdata
}

#' Warn when newdata fall outside the range of the normative sample
#' @noRd
.warn_extrapolation <- function(fitdata, eval_data, age_name, score_name, tol = 1e-7) {
  describe_oob <- function(nm) {
    fit_rng <- range(fitdata[[nm]], na.rm = TRUE)
    new_rng <- range(eval_data[[nm]], na.rm = TRUE)
    if (new_rng[1] < fit_rng[1] - tol || new_rng[2] > fit_rng[2] + tol) {
      sprintf("'%s' (observed: [%.2f, %.2f], new: [%.2f, %.2f])",
              nm, fit_rng[1], fit_rng[2], new_rng[1], new_rng[2])
    } else {
      NULL
    }
  }
  
  out_of_bounds <- c(describe_oob(age_name), describe_oob(score_name))
  if (length(out_of_bounds) > 0) {
    warning(
      "Values in `newdata` fall outside the range of the normative sample for: ",
      paste(out_of_bounds, collapse = ", "), ".\n",
      "Age and score values should fall within the range of the normative sample. ",
      "Extrapolating outside this range risks producing invalid norm score estimates.",
      call. = FALSE
    )
  }
}

#' Turn a CDF matrix (ages x scores) into a (trimmed) norm score table
#' @return list(cdf, norm); `cdf` is continuity-corrected if `cont_cor`.
#' @noRd
.cdf_to_norm <- function(cdf, q, pop_age, trim, normtype, cont_cor) {
  if (cont_cor) {
    cdf <- 0.5 * cdf + 0.5 * cbind(0, cdf[, -ncol(cdf)])
  }
  z <- CDFtotrimZ(cdf, trim = trim)
  colnames(z) <- q
  colnames(cdf) <- q
  list(cdf = cdf, norm = cbind(pop_age, ZtoScale(z, normtype)))
}

#' CDF at sample scores, optionally continuity-corrected for discrete families
#' @noRd
.cdf_sample <- function(model, scores_df, ages, fitdata, eval_data,
                        cont_cor, min_score_fit, score_name) {
  cdf <- estimateCDF(model, ages = ages, scores = scores_df, data = fitdata, newdata = eval_data)
  
  if (cont_cor) {
    floor_df <- data.frame(scores_df[[1]] - 1)
    colnames(floor_df) <- score_name
    
    cdf_floor <- estimateCDF(model, ages = ages, scores = floor_df, data = fitdata, newdata = eval_data)
    cdf_floor[floor_df[[1]] < min_score_fit, ] <- 0
    
    cdf <- 0.5 * cdf + 0.5 * cdf_floor
  }
  cdf
}

#' Write norm table components to an Excel file (if openxlsx2 is available)
#' @noRd
.write_norm_excel <- function(sheets, excel_name) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    message("Package 'openxlsx2' is required for Excel export but is not installed. Skipping Excel output.")
    return(invisible(NULL))
  }
  openxlsx2::write_xlsx(sheets, excel_name)
  message("Norm table written to: ", excel_name)
}

#' Estimate test reliability by age using a sliding window
#'
#' @description
#' Estimates reliability across age using a sliding window approach, either at fixed age points
#' or per individual.
#'
#' @param data data.frame containing the item scores and age variable.
#' @param age_name string. Name of the age variable.
#' @param item_variables numeric or character vector. Column indices or names of the item variables.
#' @param window_width numeric. Width of the sliding window used to group individuals by age.
#' @param window_version string. Type of windowing:
#'   - `"step"` (default): Estimate reliability at fixed age intervals.
#'   - `"window_per_person"`: Estimate reliability for each individual.
#' @param min_agegroup numeric. Minimum age to include. Defaults to the floor of the minimum age in the data.
#' @param max_agegroup numeric. Maximum age to include. Defaults to the ceiling of the maximum age in the data.
#' @param step_agegroup numeric. Step size between evaluated ages. Used only when `window_version = "step"`.
#' @param complete.obs logical. If `TRUE` (default), uses listwise deletion; if `FALSE`, uses pairwise deletion.
#'
#' @return A data.frame with:
#' * `rel`: Reliability estimates
#' * `age`: Corresponding age values
#' * `window_width`: The width of the sliding window
#' * `window_per`: Description of age step or observation unit
#'
#' This output can be used as the `datarel` argument in [normtable_create()].
#' 
#' @importFrom Rdpack reprompt
#' 
#' @references
#' \insertRef{heister2024item}{normref}
#' 
#' @seealso [normtable_create()]
#'
#' @examples
#' invisible(data("ids_kn_data"))
#' rel_est <- reliability_window(
#'   data = ids_kn_data,
#'   age_name = "age_years",
#'   item_variables = colnames(ids_kn_data),
#'   window_width = 2
#' )
#'
#' @export
reliability_window <- function(data,
                               age_name,
                               item_variables,
                               window_width,
                               window_version = "step",
                               min_agegroup = NULL,
                               max_agegroup = NULL,
                               step_agegroup = 1,
                               complete.obs = TRUE) {
  
  # Set default min and max age groups
  if (is.null(min_agegroup)) {
    min_agegroup <- floor(min(data[[age_name]]))
  }
  if (is.null(max_agegroup)) {
    max_agegroup <- ceiling(max(data[[age_name]]))
  }
  
  # Generate sequence of age steps
  if (!is.na(step_agegroup)) {
    age_steps <- seq(min_agegroup, max_agegroup, by = step_agegroup)
  }
  
  # Apply listwise deletion if requested
  if (complete.obs) {
    data <- data[rowSums(is.na(data[, item_variables])) == 0, ]
  }
  
  N <- length(item_variables)
  
  # Helper function to compute Cronbach's alpha
  compute_alpha <- function(temp_data) {
    var_mat <- stats::var(temp_data[, item_variables], use = "pairwise.complete.obs")
    cov_avg <- (sum(var_mat) - sum(diag(var_mat))) / (N * (N - 1))
    var_avg <- mean(diag(var_mat))
    (N * cov_avg) / (var_avg + (N - 1) * cov_avg)
  }
  
  if (window_version == "window_per_person") {
    rel <- numeric(nrow(data))
    
    for (i in seq_len(nrow(data))) {
      age_i <- data[i, age_name]
      temp_data <- data[data[[age_name]] >= age_i - window_width / 2 &
                          data[[age_name]] <  age_i + window_width / 2, ]
      rel[i] <- compute_alpha(temp_data)
    }
    
    reliability <- data.frame(
      rel = rel,
      age = data[[age_name]],
      window_width = window_width,
      window_per = "all obs"
    )
    
  } else if (window_version == "step") {
    rel <- numeric(length(age_steps))
    
    for (index in seq_along(age_steps)) {
      age_i <- age_steps[index]
      temp_data <- data[data[[age_name]] >= age_i - window_width / 2 &
                          data[[age_name]] <  age_i + window_width / 2, ]
      rel[index] <- if (nrow(temp_data) > 0) compute_alpha(temp_data) else NA
    }
    
    reliability <- data.frame(
      rel = rel,
      age = age_steps,
      window_width = window_width,
      window_per = if (length(age_steps) > 1) age_steps[2] - age_steps[1] else NA
    )
    
  } else {
    stop("`window_version` must be 'window_per_person' or 'step'")
  }
  
  return(reliability)
}

#' Estimate reliability across multiple window widths and age steps
#'
#' @description
#' Estimates reliability curves across various combinations of window widths and age step sizes,
#' with optional per-individual estimation.
#'
#' @param data data.frame containing item scores and age variable.
#' @param item_variables character vector. Names of the columns with item scores.
#' @param age_name string. Name of the age variable. Default is `"age_years"`.
#' @param step_window numeric vector. Window widths to evaluate.
#' @param min_agegroup numeric. Minimum age to include. Defaults to the floor of the minimum age in the data.
#' @param max_agegroup numeric. Maximum age to include. Defaults to the ceiling of the maximum age in the data.
#' @param step_agegroup numeric vector. Step sizes between evaluated age points.
#' @param include_window_per_person logical. If `TRUE`, also estimates reliability for each individual. Default is `FALSE`.
#' @param complete.obs logical. If `TRUE` (default), uses listwise deletion; if `FALSE`, uses pairwise deletion.
#'
#' @return An object of class \code{Drel} (a data.frame) with:
#' * `rel`: Reliability estimates
#' * `age`: Corresponding evaluated ages
#' * `window_width`: Width of the window used
#' * `age_group_width`: Step size between evaluated age groups
#' * `version`: Type of estimation (`"step"` or `"window_per_person"`)
#'
#' @seealso [plot_drel()]
#'
#' @examples
#' \donttest{
#' invisible(data("ids_kn_data"))
#' rel_int <- different_rel(
#'   data = ids_kn_data,
#'   item_variables = colnames(ids_kn_data),
#'   age_name = "age_years",
#'   step_window = c(0.5, 1, 2, 5, 10, 20),
#'   min_agegroup = 5,
#'   max_agegroup = 20,
#'   step_agegroup = c(0.5, 1, 1.5, 2)
#' )
#' }
#'
#' @export
different_rel <- function(data,
                          item_variables,
                          age_name,
                          step_window,
                          min_agegroup = NULL,
                          max_agegroup = NULL,
                          step_agegroup,
                          include_window_per_person = FALSE,
                          complete.obs = TRUE) {
  
  # Set defaults
  if (is.null(min_agegroup)) {
    min_agegroup <- floor(min(data[[age_name]]))
  }
  if (is.null(max_agegroup)) {
    max_agegroup <- ceiling(max(data[[age_name]]))
  }
  
  # Build settings grid
  settings <- expand.grid(
    w_width = step_window,
    age_step = step_agegroup,
    window_version = "step",
    stringsAsFactors = FALSE
  )
  
  if (include_window_per_person) {
    extra <- expand.grid(
      w_width = step_window,
      age_step = NA,
      window_version = "window_per_person",
      stringsAsFactors = FALSE
    )
    settings <- rbind(settings, extra)
  }
  
  # Run reliability estimation for each configuration
  all_rel <- lapply(seq_len(nrow(settings)), function(i) {
    w <- settings$w_width[i]
    step <- settings$age_step[i]
    version <- settings$window_version[i]
    
    res <- reliability_window(
      data = data,
      age_name = age_name,
      item_variables = item_variables,
      window_width = w,
      window_version = version,
      min_agegroup = min_agegroup,
      max_agegroup = max_agegroup,
      step_agegroup = step,
      complete.obs = complete.obs
    )
    
    res$age_group_width <- as.factor(step)
    res$window_width <- as.factor(w)
    res$version <- version
    res$window_per <- NULL  # Drop legacy field
    
    return(res)
  })
  
  diff_rel <- do.call(rbind, all_rel)
  class(diff_rel) <- "Drel"
  
  return(diff_rel)
}

#' Bootstrap uncertainty for single GAMLSS norm tables
#'
#' @description
#' Computes bootstrap estimates, standard errors (SE), Monte Carlo standard
#' errors of the SE, and percentile confidence intervals for continuous norm
#' tables and sample observations fitted with \code{gamlss}. Supports both
#' nonparametric resampling and parametric simulation for sample size planning.
#'
#' @param model A fitted \code{gamlss} object (e.g. from \code{fb_select()}). The data used for model fitting should be available in the global environment.
#' @param age_name Character. Column name of the continuous independent variable 
#'   (e.g., \code{"age"}, \code{"grade"}). Default is \code{"age"}.
#' @param score_name Optional character. Label for the score variable in output tables 
#'   and plots. If \code{NULL} (default), \code{"Subtest Score"} is used.
#' @param boot_type Character. Either \code{"nonparametric"} (default; resamples empirical observations)
#'   or \code{"parametric"} (simulates new raw scores from the fitted GAMLSS model).
#' @param n_sim Optional integer. Total sample size to simulate when \code{boot_type = "parametric"}.
#'   Defaults to the original sample size.
#' @param age_sampling_frame Optional function or numeric vector defining the continuous age distribution 
#'   when \code{boot_type = "parametric"}.
#' @param normtype Character. One of \code{"Z"}, \code{"T"}, or \code{"IQ"}. Default is \code{"Z"}.
#' @param min_age,max_age Numeric. Grid bounds for the continuous independent variable. 
#'   Defaults to the observed range in model data.
#' @param min_score,max_score Numeric. Grid bounds for the score variable. 
#'   Defaults to the observed range.
#' @param step_size_score Numeric. Increment for the score grid (default 1).
#' @param step_size_age Numeric. Increment for the independent variable grid. 
#'   Defaults to approximately 100 points.
#' @param cont_cor Logical. Whether to apply continuity correction for discrete families.
#' @param ci_level Numeric. Nominal confidence level for bias-corrected percentile CIs (default 0.95).
#' @param B Integer. Number of bootstrap replicates (default 500).
#' @param seed Optional integer seed for reproducibility.
#' @param parallel Logical. If \code{TRUE}, runs replicates in parallel.
#' @param ncores Integer. Number of worker cores when \code{parallel = TRUE}.
#' @param trim Numeric. Truncation threshold for extreme scores in Z scale (default 3).
#' @param keep_boot Logical. Whether to retain raw bootstrap replications.
#' @param reselect Logical. Whether to re-select polynomial degrees per replicate.
#' @param fb_control Named list of options passed to \code{fb_select()}, to be used when \code{reselect = TRUE}.
#'
#' @return An object of class \code{"normref_bootstrap"} containing:
#' \describe{
#'   \item{normtable}{Tidy data.frame of normtable-level bootstrap statistics.}
#'   \item{sample}{Data.frame of observation-level bootstrap statistics with all subtest scores.}
#'   \item{mcse_normtable}{Monte Carlo standard error metrics for the norm table.}
#'   \item{mcse_sample}{Monte Carlo standard error metrics for the sample.}
#'   \item{B, trueB}{Requested and successfully converged replicate counts.}
#'   \item{model_freq_table}{Model selection frequencies across replicates.}
#'   \item{model_selection_history}{Model selection history (if \code{reselect = TRUE}).}
#'   \item{meta}{Metadata including models, data, and normtype.}
#'   \item{grid}{Evaluation grid for the continuous variable and composite scores.}
#'   \item{boot}{Optional raw bootstrap results (if \code{keep_boot = TRUE}).}
#' }
#' 
#' @seealso [boot_composite()], [boot_heatmap()], [boot_centiles()]
#' 
#' @examples
#' \donttest{
#' # # 1. Prepare example data from normref
#' invisible(data("ids_data", package = "normref"))
#' mydata.BB <- shape_data(
#'   ids_data,
#'   age_name = "age",
#'   score_name = "y7",
#'   family = "BB")
#' 
#' # 2. Fit a GAMLSS model
#' mod <- fb_select(
#'   data = mydata.BB,
#'   age_name = "age",
#'   score_name = "shaped_score",
#'   family = "BB",
#'   selcrit = "BIC")
#' 
#' # 2. Alternatively, fit a spline model:
#' # library(gamlss)
#' # mod <- gamlss(shaped_score ~ pb(age), family = "BB", data = mydata.BB, trace = FALSE)
#' 
#' # 3. Run nonparametric bootstrap on subtest norms
#' boot_res <- boot_subtest(
#'   model = mod,
#'   age_name = "age",
#'   normtype = "IQ",
#'   boot_type = "nonparametric",
#'   B = 10, # 1000
#'   min_age = 5,
#'   max_age = 20,
#'   seed = 42,
#'   parallel = FALSE #TRUE
#' )
#' head(boot_res$normtable[1050:1055,]) # inspect results
#' 
#' # 4. Parametric bootstrap for sample size planning (e.g., N = 300)
#' frame_uniform <- function(n) {runif(n, min = 5, max = 20)} # define sampling frame
#' 
#' boot_plan <- boot_subtest(
#'   model = mod,
#'   age_name = "age",
#'   score_name = "y7",
#'   boot_type = "parametric",
#'   n_sim = 300,
#'   age_sampling_frame = frame_uniform,
#'   normtype = "Z",
#'   min_age = 5,
#'   max_age = 20,
#'   B = 10, # 1000,
#'   parallel = FALSE, #TRUE,
#'   seed = 123
#' )
#' 
#' boot_centiles(
#' boot_plan,
#' centiles = c(5,50,95)
#' )
#' }
#' 
#' @export
boot_subtest <- function(
    model,
    age_name = "age",
    score_name = NULL,
    boot_type = c("nonparametric", "parametric"),
    n_sim = NULL,
    age_sampling_frame = NULL,
    normtype = "Z",
    min_age = NULL,
    max_age = NULL,
    min_score = NULL,
    max_score = NULL,
    step_size_score = 1,
    step_size_age = NULL,
    cont_cor = TRUE,
    ci_level = 0.95,
    B = 500,
    seed = NULL,
    parallel = FALSE,
    ncores = NULL,
    trim = 3,
    keep_boot = FALSE,
    reselect = FALSE,
    fb_control = list()
) {
  if (!inherits(model, "gamlss")) stop("'model' must be a fitted gamlss object.")
  
  boot_type <- match.arg(boot_type)
  fit_data  <- as.data.frame(get_model_data(model))
  
  if (!age_name %in% names(fit_data)) stop(sprintf("Independent variable '%s' not found.", age_name))
  model_resp_name <- all.vars(model$mu.formula)[1]
  if (!model_resp_name %in% names(fit_data)) stop(sprintf("Response '%s' not found.", model_resp_name))
  
  score_col_name <- if (!is.null(score_name)) score_name else "Subtest Score"
  normtype <- match.arg(normtype, c("Z", "T", "IQ", "SS"))
  if (!is.null(seed)) set.seed(seed)
  
  fit_data <- fit_data[!is.na(fit_data[[age_name]]), , drop = FALSE]
  fit_data$.normref_id <- seq_len(nrow(fit_data))
  
  raw_score_vec <- .score_vector(fit_data[[model_resp_name]])
  n_target <- if (!is.null(n_sim)) as.integer(n_sim) else nrow(fit_data)
  score_bounds <- c(min(raw_score_vec), max(raw_score_vec))
  
  min_age <- if (is.null(min_age)) min(fit_data[[age_name]]) else min_age
  max_age <- if (is.null(max_age)) max(fit_data[[age_name]]) else max_age
  min_score <- if (is.null(min_score)) min(raw_score_vec) else min_score
  max_score <- if (is.null(max_score)) max(raw_score_vec) else max_score
  
  # Original Norm Table
  orig_norm <- normtable_create(
    model = model, data = fit_data, age_name = age_name, score_name = model_resp_name,
    normtype = normtype, min_age = min_age, max_age = max_age, min_score = min_score,
    max_score = max_score, step_size_score = step_size_score, step_size_age = step_size_age,
    cont_cor = cont_cor, trim = trim
  )
  
  model_meta <- extract_poly_degrees(model)
  family_env <- capture_family_env(model$family[1])
  
  backend <- .boot_backend(parallel, ncores, list(family_env))
  on.exit(backend$cleanup(), add = TRUE)
  `%op%` <- backend$op
  
  b <- NULL
  boot_seeds <- sample.int(1e8, B)
  results <- foreach::foreach(
    b = seq_len(B),
    .packages = c("gamlss", "normref"),
    .errorhandling = "pass",
    .export = c("extract_poly_degrees", "normtable_create", "fit_boot_model", 
                "estimateCDF", "simulate_gamlss_scores", "generate_age_frame",
                ".fit_boot_replicate", ".muffle_benign_warnings")
  ) %op% {
    set.seed(boot_seeds[b])
    tryCatch({
      if (boot_type == "nonparametric") {
        boot_idx <- sample(nrow(fit_data), size = n_target, replace = TRUE)
        boot_sample <- fit_data[boot_idx, , drop = FALSE]
      } else {
        sim_ages <- generate_age_frame(age_sampling_frame, n_target, fit_data[[age_name]])
        sim_df <- data.frame(covar = sim_ages)
        names(sim_df)[1] <- age_name
        sim_scores <- simulate_gamlss_scores(model, sim_df, score_bounds = score_bounds, fallback_data = fit_data)
        sim_df[[model_resp_name]] <- sim_scores
        boot_sample <- sim_df
      }
      
      m_boot <- .fit_boot_replicate(
        model = model, data_boot = boot_sample,
        boot_idx = if (boot_type == "nonparametric") boot_idx else NULL,
        age_name = age_name, score_name = model_resp_name,
        reselect = reselect, poly_degrees = model_meta$poly_degrees,
        spline = model_meta$spline, fb_control = fb_control
      )
      
      if (!isTRUE(m_boot$converged)) stop("Model did not converge.")
      m_boot$data <- boot_sample
      
      nt <- normtable_create(
        model = m_boot, data = boot_sample, newdata = fit_data, age_name = age_name, 
        score_name = model_resp_name, normtype = normtype, min_age = min_age, max_age = max_age, 
        min_score = min_score, max_score = max_score, step_size_score = step_size_score, 
        step_size_age = step_size_age, cont_cor = cont_cor, trim = Inf, warn_extrapolation = FALSE
      )
      
      list(table = nt$norm_matrix, sample = nt$norm_sample, poly = extract_poly_degrees(m_boot)$poly_degrees)
    }, error = function(e) paste0("Replicate ", b, " error: ", conditionMessage(e)))
  }
  
  collected <- .collect_boot_results(results)
  results_success <- collected$success
  trueB <- collected$trueB
  stacked <- .stack_boot_replicates(results_success)
  table_array <- stacked$table_array
  sample_matrix <- stacked$sample_matrix
  
  table_orig_df <- normref_table_to_long(orig_norm$norm_matrix, age_col = age_name, score_col = score_col_name)
  sample_orig_df <- data.frame(
    id = fit_data$.normref_id,
    covar = fit_data[[age_name]],
    score_val = raw_score_vec,
    original = as.numeric(orig_norm$norm_sample[, 2])
  )
  names(sample_orig_df)[2] <- age_name
  names(sample_orig_df)[3] <- score_col_name
  
  normtable_summary <- trim_bootstrap_results(summarise_boot_matrix_or_array(table_array, table_orig_df, ci_level), trim, normtype)
  sample_summary <- trim_bootstrap_results(summarise_boot_matrix_or_array(sample_matrix, sample_orig_df, ci_level), trim, normtype)
  
  mcse_normtable <- cbind(normtable_summary[, c(age_name, score_col_name, "original", "boot_se")], calc_mcse_df(table_array))
  mcse_normtable <- trim_bootstrap_results(mcse_normtable, trim, normtype)
  mcse_sample <- cbind(sample_summary[, c("id", age_name, score_col_name, "original", "boot_se")], calc_mcse_df(sample_matrix))
  mcse_sample <- trim_bootstrap_results(mcse_sample, trim, normtype)
  
  poly_history <- lapply(results_success, `[[`, "poly")
  model_freq_table <- if (reselect) sort(table(sapply(poly_history, paste, collapse = "-")), decreasing = TRUE) else NULL
  
  out <- list(
    normtable = normtable_summary,
    sample = sample_summary,
    mcse_normtable = mcse_normtable,
    mcse_sample = mcse_sample,
    B = B,
    trueB = trueB,
    model_freq_table = model_freq_table,
    model_selection_history = poly_history,
    meta = list(
      type = "single", boot_type = boot_type, n_sim = n_target,
      model = model, data = fit_data, normtype = normtype,
      age_name = age_name, score_name = score_col_name
    ),
    grid = list(covariate = orig_norm$pop_age[[age_name]], scores = colnames(orig_norm$norm_matrix)[-1]),
    boot = if (keep_boot) results_success else NULL,
    call = match.call()
  )
  names(out$grid)[1] <- age_name
  class(out) <- c("normref_bootstrap", "list")
  out
}

#' Bootstrap uncertainty for composite norm tables
#'
#' @description
#' Computes bootstrap estimates, standard errors (SE), Monte Carlo standard
#' errors of the SE, and percentile confidence intervals for composite norm
#' tables and sample observations derived from multiple subtests.
#'
#' @details
#' \code{boot_composite()} operates directly on complete observations across all subtests.
#' Fitting data and response variables are extracted directly from the model data
#' and \code{model$mu.formula} of each subtest model in \code{subtest_models} and \code{composite_model}.
#' 
#' \strong{Important:} All subtest models in \code{subtest_models} must have been
#' fitted on the exact same sample of complete observations with identical row counts
#' and row ordering in their model data.
#'
#' @param subtest_models Named list of fitted \code{gamlss} objects for each subtest. The data used for model fitting should be available in the global environment.
#' @param composite_model Fitted \code{gamlss} object for the composite score.
#' @param age_name Character. Name of the continuous independent variable (e.g. \code{"age"}, \code{"grade"}). 
#'   Default is \code{"age"}.
#' @param composite_score_name Optional character. Label for the composite score variable in output tables. 
#'   If \code{NULL} (default), \code{"Composite Score"} is used.
#' @param subtest_score_names Optional named list or character vector of labels for each subtest score. 
#'   Defaults to \code{"<subtestname>_testscore"}.
#' @param normtype Character. Norm score type: `"Z"` (N(0,1); default), `"T"` (N(50,10)), `"IQ"` (N(100,15)), or `"SS"` (N(10,3)).
#' @param weights Optional numeric weights for the subtests.
#' @param min_age,max_age Numeric. Bounds for the independent variable grid.
#' @param step_size_age Numeric. Increment for the continuous variable grid.
#' @param min_score,max_score Numeric. Score grid bounds for composite score.
#' @param step_size_score Numeric. Increment for composite score grid.
#' @param cont_cor Logical. Apply continuity correction to subtests.
#' @param ci_level Numeric. Nominal confidence level for bias-corrected percentile CIs (default 0.95).
#' @param B Integer. Number of bootstrap replicates (default 500).
#' @param reselect Logical or named list controlling model reselection per subtest / composite.
#' @param seed Optional integer seed for reproducibility.
#' @param parallel Logical. If \code{TRUE}, runs replicates in parallel.
#' @param ncores Integer. Number of worker cores when \code{parallel = TRUE}.
#' @param trim Numeric or named list. Truncation threshold for extreme scores (in Z units). 
#'   If numeric (default \code{3}), subtests remain untrimmed (\code{Inf}) during composite score calculation 
#'   to avoid artificial tail truncation artifacts, and only the final composite norms are trimmed at \code{trim}. 
#'   Alternatively, supply a named list specifying thresholds per subtest and for \code{"composite"}.
#' @param keep_boot Logical. Whether to retain raw bootstrap replicate matrices.
#' @param fb_control Named list of control arguments for \code{fb_select()}, to be used when \code{reselect = TRUE}.
#'
#' @return An object of class \code{"normref_bootstrap"} containing:
#' \describe{
#'   \item{normtable}{Tidy data.frame of normtable-level bootstrap statistics.}
#'   \item{sample}{Data.frame of observation-level bootstrap statistics with all subtest and composite scores.}
#'   \item{mcse_normtable}{Monte Carlo standard error metrics for the norm table.}
#'   \item{mcse_sample}{Monte Carlo standard error metrics for the sample.}
#'   \item{B, trueB}{Requested and successfully converged replicate counts.}
#'   \item{model_freq_table}{Model selection frequencies across replicates.}
#'   \item{model_selection_history}{Model selection history per subtest and composite.}
#'   \item{meta}{Metadata including weights, correlation matrix, models, data, and normtype.}
#'   \item{grid}{Evaluation grid for the continuous variable and composite scores.}
#'   \item{boot}{Optional raw bootstrap results (if \code{keep_boot = TRUE}).}
#' }
#' 
#' @seealso [boot_subtest()], [boot_heatmap()], [boot_centiles()], [boot_sample()]
#' 
#' @examples
#' \donttest{
#' # 1. Prepare example data from normref
#' invisible(data("ids_data", package = "normref"))
#' 
#' # 2. Fit subtest models
#' mydata1 <- shape_data(ids_data, age_name = "age", score_name = "y7",  family = "BB")
#' mod1 <- fb_select(mydata1, age_name = "age", score_name = "shaped_score", family = "BB")
#' mydata2 <- shape_data(ids_data, age_name = "age", score_name = "y14", family = "NO")
#' mod2 <- fb_select(mydata2, age_name = "age", score_name = "shaped_score", family = "NO")
#' 
#' normtable1 <- normtable_create(mod1, mydata1, age_name = "age", score_name = "shaped_score", 
#'                 trim = Inf)
#' normtable2 <- normtable_create(mod2, mydata2, age_name = "age", score_name = "shaped_score", 
#'                 trim = Inf)
#' # 3. Construct composite score
#' comp_data <- composite_shape(list(y7 = normtable1, y14 = normtable2))
#' comp_model <- fb_select(comp_data, age_name = "age", score_name = "z_sum", family = "NO")
#' 
#' # 4. Bootstrap composite score precision
#' boot_comp <- boot_composite(
#'   subtest_models = list(y7 = mod1, y14 = mod2),
#'   composite_model = comp_model,
#'   age_name = "age",
#'   composite_score_name = "z_sum",
#'   step_size_score = 0.1,
#'   B = 10, # 1000
#'   seed = 42,
#'   parallel = FALSE #TRUE
#' )
#' 
#' head(boot_comp$sample)
#' 
#' # Observed sample-level standard error ratios by score bin
#' boot_sample(
#'   boot_comp,
#'   type = "ratio",
#'   benchmark_n = 300,
#'   z_bins = seq(-3, 3, by = 1)
#'   )
#' }
#' @export
boot_composite <- function(
    subtest_models,
    composite_model,
    age_name = "age",
    composite_score_name = NULL,
    subtest_score_names = NULL,
    normtype = "Z",
    weights = NULL,
    min_age = NULL,
    max_age = NULL,
    step_size_age = NULL,
    min_score = NULL,
    max_score = NULL,
    step_size_score = 1,
    cont_cor = TRUE,
    ci_level = 0.95,
    B = 500,
    reselect = FALSE,
    seed = NULL,
    parallel = FALSE,
    ncores = NULL,
    trim = 3,
    keep_boot = FALSE,
    fb_control = NULL
) {
  if (is.null(names(subtest_models))) stop("'subtest_models' must be a named list.")
  if (!inherits(composite_model, "gamlss")) stop("'composite_model' must be a fitted gamlss object.")
  
  # Check identical complete observation counts
  subtest_data <- lapply(subtest_models, function(m) {get_model_data(m)})
  n_obs_vec <- vapply(subtest_data, function(dat) nrow(dat), integer(1))
  if (length(unique(n_obs_vec)) > 1) {
    stop(paste0(
      "All subtest models must be fitted on the exact same complete observations with matching row counts.\n",
      "Observed row counts: ", paste(names(n_obs_vec), n_obs_vec, sep = " = ", collapse = ", ")
    ))
  }
  n_obs <- n_obs_vec[1]
  if (n_obs < 2) stop("Not enough complete observations to perform bootstrap.")
  
  # Check continuous variable in all subtest models
  for (nm in names(subtest_models)) {
    if (!age_name %in% names(subtest_data[[nm]])) {
      stop(sprintf("Independent variable '%s' not found in model data of subtest '%s'.", age_name, nm))
    }
  }
  
  normtype <- match.arg(normtype, c("Z", "T", "IQ", "SS"))
  if (!is.null(seed)) set.seed(seed)
  
  model_names <- c(names(subtest_models), "composite")
  models <- c(subtest_models, list(composite = composite_model))
  
  # Trim: Inf for subtests (untrimmed intermediate sums), target trim for composite
  if (is.list(trim)) {
    trim_list <- stats::setNames(lapply(model_names, function(nm) if (!is.null(trim[[nm]])) trim[[nm]] else Inf), model_names)
    trim_list[["composite"]] <- if (!is.null(trim[["composite"]])) trim[["composite"]] else 3
  } else {
    trim_list <- stats::setNames(as.list(rep(Inf, length(model_names))), model_names)
    trim_list[["composite"]] <- if (is.numeric(trim)) trim else 3
  }
  
  comp_score_col <- if (!is.null(composite_score_name)) composite_score_name else "Composite Score"
  sub_score_cols <- stats::setNames(paste0(names(subtest_models), "_testscore"), names(subtest_models))
  if (!is.null(subtest_score_names)) {
    for (nm in names(subtest_models)) {
      if (!is.null(subtest_score_names[[nm]])) sub_score_cols[nm] <- subtest_score_names[[nm]]
    }
  }
  
  subtest_resp_names <- vapply(subtest_models, function(m) all.vars(m$mu.formula)[1], character(1))
  comp_resp_name     <- all.vars(composite_model$mu.formula)[1]
  
  if (is.null(weights)) {
    weights <- stats::setNames(rep(1, length(subtest_models)), names(subtest_models))
  } else {
    if (!is.numeric(weights) || length(weights) != length(subtest_models)) {
      stop("'weights' must be NULL or a numeric vector with length equal to subtest_models.")
    }
    if (any(!is.finite(weights)) || sum(weights) == 0) {
      stop("'weights' must contain finite values and sum to a non-zero value.")
    }
    weights <- weights / sum(weights)
    names(weights) <- names(subtest_models)
  }
  
  if (isFALSE(reselect)) {
    reselect <- stats::setNames(as.list(rep(FALSE, length(model_names))), model_names)
  } else if (isTRUE(reselect)) {
    reselect <- stats::setNames(as.list(rep(TRUE, length(model_names))), model_names)
  } else {
    missing <- setdiff(model_names, names(reselect))
    reselect[missing] <- FALSE
    reselect <- reselect[model_names]
  }
  
  if (is.null(fb_control)) fb_control <- list()
  missing_fb <- setdiff(model_names, names(fb_control))
  fb_control[missing_fb] <- vector("list", length(missing_fb))
  fb_control <- fb_control[model_names]
  
  family_envs <- lapply(models, function(m) capture_family_env(m$family[1]))
  names(family_envs) <- model_names
  
  model_meta <- lapply(models, extract_poly_degrees)
  poly_degrees <- lapply(model_meta, `[[`, "poly_degrees")
  spline <- lapply(model_meta, `[[`, "spline")
  
  ref_data <- subtest_data[[1]]
  min_age  <- if (is.null(min_age)) min(ref_data[[age_name]]) else min_age
  max_age  <- if (is.null(max_age)) max(ref_data[[age_name]]) else max_age
  if (is.null(step_size_age)) step_size_age <- min(1, (max_age - min_age) / 50)
  
  # 1. Original Point Estimates (subtests use trim = Inf)
  orig_subtest_normtables <- vector("list", length(subtest_models))
  names(orig_subtest_normtables) <- names(subtest_models)
  for (nm in names(subtest_models)) {
    orig_subtest_normtables[[nm]] <- normtable_create(
      model = subtest_models[[nm]], data = subtest_data[[nm]], age_name = age_name,
      score_name = subtest_resp_names[nm], normtype = "Z", cont_cor = cont_cor, 
      trim = trim_list[[nm]]
    )
  }
  
  orig_comp_data <- composite_shape(orig_subtest_normtables, weights = weights)
  orig_comp_data[[age_name]] <- ref_data[[age_name]]
  if (comp_resp_name != "z_sum") orig_comp_data[[comp_resp_name]] <- orig_comp_data$z_sum
  
  if (is.null(min_score)) min_score <- min(orig_comp_data$z_sum, na.rm = TRUE)
  if (is.null(max_score)) max_score <- max(orig_comp_data$z_sum, na.rm = TRUE)
  if (is.null(step_size_score)) step_size_score <- min(1, (max_score - min_score) / 50)
  
  orig_comp_norm <- normtable_create(
    model = composite_model, data = orig_comp_data, age_name = age_name, score_name = comp_resp_name,
    normtype = normtype, cont_cor = FALSE, trim = trim_list[["composite"]], min_age = min_age, max_age = max_age,
    step_size_age = step_size_age, min_score = min_score, max_score = max_score,
    step_size_score = step_size_score
  )
  
  orig_sample <- data.frame(id = seq_len(n_obs), covar = ref_data[[age_name]])
  names(orig_sample)[2] <- age_name
  for (nm in names(subtest_models)) orig_sample[[sub_score_cols[nm]]] <- .score_vector(subtest_data[[nm]][[subtest_resp_names[nm]]])
  orig_sample[[comp_score_col]] <- orig_comp_data$z_sum
  orig_sample$original <- as.numeric(orig_comp_norm$norm_sample[, 2])
  
  orig_table <- normref_table_to_long(orig_comp_norm$norm_matrix, age_col = age_name, score_col = comp_score_col)
  
  z_mat <- sapply(orig_subtest_normtables, function(nt) nt$znorm_sample[, 2])
  P <- stats::cor(z_mat, use = "pairwise.complete.obs")
  
  # 2. Parallel Setup
  b <- NULL
  backend <- .boot_backend(parallel, ncores, family_envs)
  on.exit(backend$cleanup(), add = TRUE)
  `%op%` <- backend$op
  
  boot_seeds <- sample.int(1e8, B)
  results <- foreach::foreach(
    b = seq_len(B),
    .packages = c("gamlss", "normref"),
    .errorhandling = "pass",
    .export = c("extract_poly_degrees", "normtable_create", "fit_boot_model", 
                "estimateCDF", "simulate_gamlss_scores", "generate_age_frame",
                ".fit_boot_replicate", ".muffle_benign_warnings")
  ) %op% {
    set.seed(boot_seeds[b])
    tryCatch({
      boot_idx <- sample(n_obs, replace = TRUE)
      
      subtest_normtables    <- vector("list", length(subtest_models))
      subtest_orig_sample_z <- vector("list", length(subtest_models))
      names(subtest_normtables) <- names(subtest_orig_sample_z) <- names(subtest_models)
      subtest_poly <- vector("list", length(subtest_models))
      names(subtest_poly) <- names(subtest_models)
      
      # Fit subtest bootstrap models
      for (nm in names(subtest_models)) {
        sub_boot_df <- subtest_data[[nm]][boot_idx, , drop = FALSE]
        
        model_boot <- .fit_boot_replicate(
          model = subtest_models[[nm]], data_boot = sub_boot_df, boot_idx = boot_idx,
          age_name = age_name, score_name = subtest_resp_names[nm],
          reselect = reselect[[nm]], poly_degrees = poly_degrees[[nm]],
          spline = spline[[nm]], fb_control = fb_control[[nm]]
        )
        
        if (!isTRUE(model_boot$converged)) stop(sprintf("Subtest '%s' model failed.", nm))
        model_boot$data <- sub_boot_df
        
        subtest_normtables[[nm]] <- normtable_create(
          model = model_boot, data = sub_boot_df, age_name = age_name, 
          score_name = subtest_resp_names[nm], normtype = "Z", cont_cor = cont_cor, 
          trim = trim_list[[nm]]
        )
        
        subtest_orig_sample_z[[nm]] <- normtable_create(
          model = model_boot, data = sub_boot_df, newdata = subtest_data[[nm]], age_name = age_name, 
          score_name = subtest_resp_names[nm], normtype = "Z", cont_cor = cont_cor, 
          trim = trim_list[[nm]], warn_extrapolation = FALSE
        )
        
        subtest_poly[[nm]] <- extract_poly_degrees(model_boot)$poly_degrees
      }
      
      # Fit composite bootstrap model
      comp_boot_df <- composite_shape(subtest_normtables, weights = weights)
      comp_boot_df[[age_name]] <- ref_data[[age_name]][boot_idx]
      if (comp_resp_name != "z_sum") comp_boot_df[[comp_resp_name]] <- comp_boot_df$z_sum
      
      m_comp <- .fit_boot_replicate(
        model = composite_model, data_boot = comp_boot_df, boot_idx = boot_idx,
        age_name = age_name, score_name = comp_resp_name,
        reselect = reselect[["composite"]], poly_degrees = poly_degrees[["composite"]],
        spline = spline[["composite"]], fb_control = fb_control[["composite"]]
      )
      
      if (!isTRUE(m_comp$converged)) stop("Composite model failed.")
      m_comp$data <- comp_boot_df
      subtest_poly$composite <- extract_poly_degrees(m_comp)$poly_degrees
      
      # Evaluation on original sample
      comp_orig_eval_df <- composite_shape(subtest_orig_sample_z, weights = weights)
      comp_orig_eval_df[[age_name]] <- ref_data[[age_name]]
      if (comp_resp_name != "z_sum") comp_orig_eval_df[[comp_resp_name]] <- comp_orig_eval_df$z_sum
      
      nt_comp <- normtable_create(
        model = m_comp, data = comp_boot_df, newdata = comp_orig_eval_df, age_name = age_name, 
        score_name = comp_resp_name, normtype = normtype, cont_cor = FALSE, 
        trim = trim_list[["composite"]], min_age = min_age, max_age = max_age, 
        step_size_age = step_size_age, min_score = min_score, max_score = max_score, 
        step_size_score = step_size_score, warn_extrapolation = FALSE
      )
      
      list(table = nt_comp$norm_matrix, sample = nt_comp$norm_sample, poly = subtest_poly)
    }, error = function(e) paste0("Replicate ", b, " error: ", conditionMessage(e)))
  }
  
  collected <- .collect_boot_results(results)
  results_success <- collected$success
  trueB <- collected$trueB
  stacked <- .stack_boot_replicates(results_success)
  table_array <- stacked$table_array
  sample_array <- stacked$sample_matrix
  
  normtable_summary <- trim_bootstrap_results(summarise_boot_matrix_or_array(table_array, orig_table, ci_level), trim_list[["composite"]], normtype)
  sample_summary <- trim_bootstrap_results(summarise_boot_matrix_or_array(sample_array, orig_sample, ci_level), trim_list[["composite"]], normtype)
  
  mcse_normtable <- cbind(normtable_summary[, c(age_name, comp_score_col)], calc_mcse_df(table_array))
  mcse_sample <- cbind(sample_summary[, c("id", age_name, unname(sub_score_cols), comp_score_col)], calc_mcse_df(sample_array))
  
  poly_history <- lapply(results_success, function(x) x$poly)
  model_selection_history <- lapply(model_names, function(nm) lapply(poly_history, `[[`, nm))
  names(model_selection_history) <- model_names
  
  model_freq_table <- lapply(model_names, function(nm) {
    if (!reselect[[nm]]) return(NULL)
    freq <- table(sapply(model_selection_history[[nm]], paste, collapse = "-"))
    sort(freq, decreasing = TRUE)
  })
  names(model_freq_table) <- model_names
  
  out <- list(
    normtable = normtable_summary,
    sample = sample_summary,
    mcse_normtable = mcse_normtable,
    mcse_sample = mcse_sample,
    B = B,
    trueB = trueB,
    model_freq_table = model_freq_table,
    model_selection_history = model_selection_history,
    meta = list(
      type = "composite", weights = weights, P = P, model = composite_model,
      subtest_models = subtest_models, data = orig_comp_data, normtype = normtype,
      age_name = age_name, score_name = comp_score_col, subtest_score_names = sub_score_cols,
      trim = trim_list
    ),
    grid = list(covariate = orig_comp_norm$pop_age[[age_name]], scores = colnames(orig_comp_norm$norm_matrix)[-1]),
    boot = if (keep_boot) results_success else NULL,
    call = match.call()
  )
  names(out$grid)[1] <- age_name
  class(out) <- c("normref_bootstrap", "list")
  out
}