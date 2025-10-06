#' Shape data as input for [fb_select()]
#'
#' @description 
#' `shape_data()` reshapes the response variable into the right format for the specified 
#' distribution and removes all cases with missing data on the score or age variable. 
#' The result is suitable for use as input to [fb_select()].
#'
#' @inheritParams fb_select
#' @param max_score numeric. Highest possible score in the norm table. 
#'   Defaults to the maximum observed score in the sample.
#' @param verbose logical. If `TRUE`, messages are printed whenever a transformation is applied.
#'
#' @return A data.frame containing the original variables and a new column `shaped_score`, 
#' with the response variable in the correct format for GAMLSS modeling.
#'
#' @importFrom Rdpack reprompt
#'
#' @references
#' \insertRef{voncken2019model}{normref}
#'
#' @details 
#' The function checks whether the response values are valid for the specified 
#' GAMLSS distribution family. If not, transformations are applied to ensure compatibility. 
#' Messages are printed (if `verbose = TRUE`) to describe each transformation.  
#'
#' Unexpected transformations should prompt inspection of the original data.  
#' Note that the function does **not** assess whether the chosen family is appropriate 
#' for the data—it only ensures compatibility.  
#'
#' Compatible with all gamlss distributions, with the exception of distributions in the multinomial family ([gamlss::.gamlss.multin.list]).
#' This includes user-defined distributions, such as truncated distributions.
#'
#' @seealso [fb_select()]
#'
#' @examples
#' invisible(data("ids_data"))
#' mydata_BB <- shape_data(ids_data, age_name = "age", score_name = "y14", family = "BB")
#' mydata_BCPE <- shape_data(ids_data, age_name = "age", score_name = "y14", family = "BCPE")
#'
#' @export
shape_data <- function(data,
                       age_name,
                       score_name,
                       family,
                       max_score = NULL,
                       verbose = TRUE) {
  # Handle missing data
  temp_na <- data[[score_name]] + data[[age_name]]
  data_out <- data[!is.na(temp_na), c(score_name, age_name)]
  y <- data_out[[score_name]]
  
  family <- gamlss.dist::as.gamlss.family(family)
  
  # Case 1: valid responses, nothing to do
  if ((family$y.valid(y) &&
       family$type != "Discrete" &&
       !any(family$family %in% gamlss::.gamlss.bi.list)) ||
      (!any(family$family %in% gamlss::.gamlss.bi.list) &&
       family$type == "Discrete" &&
       all(round(y) == y) &&
       family$y.valid(y))) {
    if (verbose) message("Response values are valid and were not transformed.")
  }
  
  # Case 2: negative values
  if (!family$y.valid(-1) && min(y, na.rm = TRUE) < 0) {
    shift <- abs(min(y, na.rm = TRUE))
    y <- y + shift
    if (verbose) message("Negative values detected. Shifted to ensure minimum is 0.")
  }
  
  # Case 3: non-integers for discrete families
  if (family$type == "Discrete" && !all(round(y) == y)) {
    y <- round(y)
    if (verbose) message("Non-discrete values detected. Rounded to nearest integer.")
  }
  
  # Case 4: zeros not allowed
  if (!family$y.valid(0) && any(y == 0)) {
    if (family$type == "Discrete") {
      y <- y + 1
      if (verbose) message("Zero values detected. Shifted upward by 1.")
    } else {
      y <- pmax(y, 0.001)
      if (verbose) message("Zero values detected. Clipped at 0.001.")
    }
  }
  
  # Case 5: ones not allowed
  if (!family$y.valid(1) && any(y == 1)) {
    y <- pmin(y, 0.999)
    if (verbose) message("Ones detected. Clipped at 0.999.")
  }
  
  # Case 6: binomial families require matrix form
  if (any(family$family %in% gamlss::.gamlss.bi.list)) {
    if (is.null(max_score)) {
      max_score <- max(data[[score_name]], na.rm = TRUE)
      if (verbose) message("No 'max_score' specified. Using maximum observed response.")
    }
    y <- cbind(data_out[[score_name]], max_score - data_out[[score_name]])
    if (verbose) {
      message("Binomial family: converted response to matrix with two columns:\n",
              "  col1 = number correct, col2 = number incorrect.")
    }
  }
  
  # Return cleaned and transformed data
  data_out$shaped_score <- y
  
  if (nrow(data_out) < nrow(data)) {
    warning(sprintf("Missing data detected.\nTotal rows: %d\nRows used: %d",
                    nrow(data), nrow(data_out)))
  }
  
  return(data_out)
}


#' Shape data for a composite scale based on normalized Z-scores
#'
#' @description 
#' `composite_shape()` creates a data.frame with age values and the sum of normalized 
#' z-scores from multiple NormTable objects, suitable for use as input to [fb_select()].
#'
#' @param normtables list of NormTable objects created by [normtable_create()]. 
#'   Each must contain `znorm_sample` and `norm_sample`.
#'
#' @return A data.frame with:
#'   - `age`: Age values from the first NormTable  
#'   - `z_sum`: Unweighted sum of normalized z-scores across all objects
#'
#' @seealso [shape_data()], [fb_select()], [normtable_create()]
#'
#' @examples
#' \donttest{
#' invisible(data("ids_data"))
#' 
#' # Example with two normtables
#' mydata1 <- shape_data(ids_data, age_name = "age", score_name = "y7", family = "BCPE")
#' mod1 <- fb_select(mydata1, age_name = "age", score_name = "shaped_score",
#'                   family = "BCPE", selcrit = "BIC")
#' norm1 <- normtable_create(mod1, mydata1, age_name = "age", score_name = "shaped_score")
#' 
#' mydata2 <- shape_data(ids_data, age_name = "age", score_name = "y14", family = "BCPE")
#' mod2 <- fb_select(mydata2, age_name = "age", score_name = "shaped_score",
#'                   family = "BCPE", selcrit = "BIC")
#' norm2 <- normtable_create(mod2, mydata2, age_name = "age", score_name = "shaped_score")
#' 
#' composite_data <- composite_shape(list(norm1, norm2))}
#'
#' @export

composite_shape <- function(normtables) {
  if (!is.list(normtables) || length(normtables) == 0) {
    stop("'normtables' must be a non-empty list of normtables")
  }
  
  required_components <- c("znorm_sample", "norm_sample")
  for (i in seq_along(normtables)) {
    if (!all(required_components %in% names(normtables[[i]]))) {
      stop(sprintf(
        "Normtable %d is missing required components: %s",
        i,
        paste(setdiff(required_components, names(normtables[[i]])), collapse = ", ")
      ))
    }
  }
  
  # Extract z-scores and sum across normtables
  z_scores <- lapply(normtables, function(x) x$znorm_sample[, 2])
  z_sum <- Reduce(`+`, z_scores)
  
  data.frame(
    age = normtables[[1]]$norm_sample[, 1],
    z_sum = z_sum
  )
}
