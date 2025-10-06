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
#' @param data data.frame. The sample on which the model has been fitted, or new data; 
#'   must contain the score variable (with name given in \code{score_name}) and age variable 
#'   (with name given in \code{age_name}).
#' @param age_name string. Name of the age variable.
#' @param score_name string. Name of the score variable.
#' @param datarel data.frame or numeric. If a data.frame, must contain columns `age` and `rel`, 
#'   with estimated test reliability per age. If numeric, a constant reliability is assumed 
#'   for all ages (optional, only needed for confidence intervals).
#' @param normtype string. Norm score type: `"Z"` (N(0,1); default), `"T"` (N(50,10)), or `"IQ"` (N(100,15)).
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
#' @param cont_cor logical. If `TRUE`, apply continuity correction for discrete test scores. 
#'   Default is `FALSE`.
#' @param ci_level numeric. Confidence interval level (if \code{datarel} is provided). 
#'   Default is 0.95.
#' @param trim numeric. Trim norm scores at ± \code{trim} standard deviations. 
#'   Default is 3.
#' @param excel logical. If `TRUE`, attempt to write results to an Excel file. Default is `FALSE`.
#' @param excel_name character. Path to the Excel file. Defaults to a temporary file.
#'   Ignored if `excel = FALSE`.
#' @param new_data logical. If `FALSE` (default), create a full norm table and norm scores. 
#'   If `TRUE`, only return norm scores for the given data.
#'
#' @return A list of class \code{NormTable} containing:
#' * \code{norm_sample}: Estimated norm scores (\code{normtype}) in the sample, trimmed at \code{trim}.
#' * \code{norm_sample_lower}, \code{norm_sample_upper}: Lower and upper \code{ci_level} confidence bounds of \code{norm_sample}.
#' * \code{norm_matrix}: Norm scores (\code{normtype}) by age (only if \code{new_data = FALSE}).
#' * \code{norm_matrix_lower}, \code{norm_matrix_upper}: Lower and upper \code{ci_level} bounds of \code{norm_matrix}.
#' * \code{znorm_sample}: Estimated Z scores in the sample.
#' * \code{cdf_sample}: Estimated percentiles in the sample.
#' * \code{cdf_matrix}: Percentile table by age (only if \code{new_data = FALSE}).
#' * \code{data}, \code{age_name}, \code{score_name}: Copies of respective function arguments.
#' * \code{pop_age}: Evaluated ages in the norm table (only if \code{new_data = FALSE}).
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
#'   data = newdata,
#'   age_name = "age",
#'   score_name = "y14",
#'   new_data = TRUE,
#'   datarel = ids_rel_data
#' )
#' }

#' @export
normtable_create <- function(model,
                             data,
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
                             cont_cor = FALSE,
                             ci_level = 0.95,
                             trim = 3,
                             excel = FALSE,
                             excel_name = tempfile("norms", fileext = ".xlsx"),
                             new_data = FALSE) {
  mydata <- data
  
  # Ensure score is a vector (not matrix)
  if (!is.null(dim(mydata[[score_name]]))) {
    mydata[[score_name]] <- mydata[[score_name]][, 1]
  }
  
  nperson <- nrow(mydata)
  score_max <- floor(max(mydata[[score_name]]))
  sample_q <- data.frame(score = mydata[[score_name]])
  sample_age <- data.frame(age = mydata[[age_name]])
  colnames(sample_q) <- score_name
  colnames(sample_age) <- age_name
  
  # Default ranges
  if (is.null(min_age)) {
    min_age <- floor(min(data[[age_name]]))
  }
  if (is.null(max_age)) {
    max_age <- ceiling(max(mydata[[age_name]]))
  }
  if (is.null(min_score)) {
    min_score <- min(mydata[[score_name]])
  }
  if (is.null(max_score)) {
    max_score <- max(mydata[[score_name]])
  }

  # Convert constant reliability to dataframe
  if (!is.null(datarel) && !is.data.frame(datarel) && length(datarel) == 1) {
    datarel <- data.frame(
      rel = rep(datarel, 20),
      age = seq(min_age, max_age, length.out = 20)
    )
  }
  
  # Construct population ages if needed
  if (!new_data) {
    pop_age <- if (is.null(step_size_age)) {
      data.frame(age = seq(min_age, max_age, length.out = 100))
    } else {
      data.frame(age = seq(min_age, max_age, by = step_size_age))
    }
    names(pop_age) <- age_name
    
    # Score grid
    q <- seq(from = min_score, to = max_score, by = step_size_score)
    scores_c <- rep(1, nrow(pop_age)) %*% t(q)
    
    # Confidence intervals (if reliability given)
    if (!is.null(datarel)) {
      uplow <- intervals(
        model, datarel,
        score_min = min_score, score_max = score_max,
        normingdata = mydata, scores_c = scores_c,
        pop_age = pop_age, age_name = age_name,
        score_name = score_name, ci_level = ci_level
      )
      lower_matrix <- uplow$lower_matrix
      upper_matrix <- uplow$upper_matrix
    }
    
    # Norm estimates
    cdf_matrix <- estimateCDF(model, ages = pop_age, scores = scores_c, score_max = score_max)
    if (cont_cor) {
      cdf_matrix <- 0.5 * cdf_matrix +
        0.5 * cbind(0, cdf_matrix[, -ncol(cdf_matrix)])
    }
    
    z_matrix <- CDFtotrimZ(cdf_matrix, trim = trim)
    colnames(z_matrix) <- q
    colnames(cdf_matrix) <- q
    
    # Confidence interval transformations
    if (!is.null(datarel)) {
      cdf_matrix_lower <- estimateCDF(model, 
                                      ages = pop_age, 
                                      scores = lower_matrix, 
                                      score_max = score_max)
      cdf_matrix_upper <- estimateCDF(model, 
                                      ages = pop_age, 
                                      scores = upper_matrix, 
                                      score_max = score_max)
      if (cont_cor) {
        cdf_matrix_lower <- 0.5 * cdf_matrix_lower +
          0.5 * cbind(0, cdf_matrix_lower[, -ncol(cdf_matrix_lower)])
        cdf_matrix_upper <- 0.5 * cdf_matrix_upper +
          0.5 * cbind(0, cdf_matrix_upper[, -ncol(cdf_matrix_upper)])
      }
      z_matrix_lower <- CDFtotrimZ(cdf_matrix_lower, trim)
      z_matrix_upper <- CDFtotrimZ(cdf_matrix_upper, trim)
      colnames(z_matrix_lower) <- q
      colnames(z_matrix_upper) <- q
    }
  } else {
    cdf_matrix <- norm_matrix <- pop_age <- NA
    norm_matrix_lower <- norm_matrix_upper <- NA
  }
  
  # Sample confidence intervals
  if (!is.null(datarel)) {
    uplow <- intervals(
      model, datarel,
      score_min = min_score, score_max = score_max,
      normingdata = mydata, scores_c = NULL,
      pop_age = NULL, age_name = age_name,
      score_name = score_name, ci_level = ci_level
    )
    lower_sample <- data.frame(uplow$lower_sample)
    upper_sample <- data.frame(uplow$upper_sample)
    
    cdf_sample_lower <- estimateCDF(model, sample_age, lower_sample, score_max)
    cdf_sample_upper <- estimateCDF(model, sample_age, upper_sample, score_max)
    
    if (cont_cor) {
      lower_floor <- data.frame(pmax(min(lower_sample), lower_sample - 1))
      upper_floor <- data.frame(pmax(min(upper_sample), upper_sample - 1))
      cdf_sample_lower <- 0.5 * cdf_sample_lower +
        0.5 * estimateCDF(model, sample_age, lower_floor, score_max)
      cdf_sample_upper <- 0.5 * cdf_sample_upper +
        0.5 * estimateCDF(model, sample_age, upper_floor, score_max)
    }
    
    z_sample_lower <- CDFtotrimZ(cdf_sample_lower, trim)
    z_sample_upper <- CDFtotrimZ(cdf_sample_upper, trim)
  }
  
  # Sample estimates
  cdf_sample <- estimateCDF(model, sample_age, sample_q, score_max)
  if (cont_cor) {
    floor_scores <- data.frame(pmax(min(mydata[[score_name]]), mydata[[score_name]] - 1))
    cdf_sample <- 0.5 * cdf_sample +
      0.5 * estimateCDF(model, sample_age, floor_scores, score_max)
  }
  z_sample <- CDFtotrimZ(cdf_sample, trim)
  znorm_sample <- cbind(mydata[[age_name]], z_sample)
  colnames(znorm_sample) <- c(age_name, "Z")
  
  if (!new_data) {
    norm_matrix <- cbind(pop_age, ZtoScale(z_matrix, normtype))
    if (!is.null(datarel)) {
      norm_matrix_lower <- cbind(pop_age, ZtoScale(z_matrix_lower, normtype))
      norm_matrix_upper <- cbind(pop_age, ZtoScale(z_matrix_upper, normtype))
    }
  }
  
  norm_sample <- cbind(mydata[[age_name]], ZtoScale(z_sample, normtype))
  colnames(norm_sample) <- c(age_name, normtype)
  
  if (!is.null(datarel)) {
    norm_sample_lower <- cbind(age = mydata[[age_name]], ZtoScale(z_sample_lower, normtype))
    norm_sample_upper <- cbind(age = mydata[[age_name]], ZtoScale(z_sample_upper, normtype))
  } else {
    norm_sample_lower <- norm_sample_upper <- norm_matrix_lower <- norm_matrix_upper <- NA
  }
  
  if (excel) {
    if (!requireNamespace("openxlsx2", quietly = TRUE)) {
      message("Package 'openxlsx2' is required for Excel export but is not installed. ",
              "Skipping Excel output.")
    } else {
      l <- list(
        "norm matrix" = norm_matrix,
        "norm matrix lower" = norm_matrix_lower,
        "norm matrix upper" = norm_matrix_upper,
        "norm sample" = norm_sample,
        "norm sample lower" = norm_sample_lower,
        "norm sample upper" = norm_sample_upper
      )
      openxlsx2::write_xlsx(l, excel_name)
      message("Norm table written to: ", excel_name)
    }
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
    data = data,
    age_name = age_name,
    score_name = score_name,
    pop_age = pop_age
  )
  class(out) <- "NormTable"
  out
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

