#' Free order model selection procedure
#'
#' @description
#' `fb_select()` applies the free order model selection procedure, using forward–backward selection
#' \insertCite{voncken2019model}{normref}.
#' For a given GAMLSS distribution and model selection criterion, it selects the optimal
#' polynomial degrees for all distribution parameters.
#'
#' @details
#' The search history of the free order model selection procedure can be accessed in `search_history`. This allows the user to investigate if exists a more parsimonious model with similar model fit to the selected model.
#'
#' @param data data.frame. Sample on which to fit the distribution; contains the scores and ages.
#' @param age_name string. Name of the age variable.
#' @param score_name string. Name of the score variable.
#' @param family string. For example, `"BB"`, `"BCPE"`, `"NO"`, etc.
#'   See [gamlss.dist::gamlss.family] for more information on available distributions. Also accepts user-defined distributions like truncated distributions, see [gamlss.tr::gen.trun()].
#' @param selcrit string. Model selection criterion: `"AIC"`, `"BIC"` (default), `"GAIC(3)"`, or `"CV"`
#'   (cross-validated global deviance with 10 folds).
#' @param spline logical. If `FALSE` (default), estimate polynomial(s) for \eqn{\mu};
#'   if `TRUE`, estimate a p-spline for \eqn{\mu}.
#' @param method string. Estimation method for [gamlss::gamlss()]. Either `"RS()"`, `"CG()"`, or `"mixed()"`,
#'   with iteration count. Default is `"RS(10000)"`.
#' @param max_poly vector. Maximum polynomial degrees for each parameter.
#' @param min_poly vector. Minimum polynomial degrees for each parameter.
#' @param start_poly vector. Starting polynomial degrees for each parameter.
#' @param trace logical. If `TRUE`, prints progress during selection.
#' @param seed integer. Random seed for cross-validation folds.
#'
#' @return A selected GAMLSS model with the chosen polynomial degrees. This is a gamlss object with additional components
#' * \code{selcrit}: the value of the selection criterion for the chosen model.
#' * \code{search_history}: search history, containing the polynomial degrees for examined models (`poly_deg`) and their selection criteria (`selcrit`).
#'
#' @importFrom Rdpack reprompt
#'
#' @references
#'   \insertRef{voncken2019model}{normref}
#'
#' @seealso [shape_data()], [fb_select()], [normtable_create()]
#'
#' @examples
#' \donttest{
#' invisible(data("ids_data"))
#' mydata <- shape_data(ids_data, age_name = "age", score_name = "y14", family = "BB")
#' mod <- fb_select(mydata, age_name = "age", score_name = "shaped_score",
#'                  family = "BB", selcrit = "BIC")
#' mod$search_history
#' }
#'
#' @export
fb_select <- function(data,
                      age_name,
                      score_name,
                      family,
                      selcrit = "BIC",
                      spline = FALSE,
                      method = "RS(10000)",
                      max_poly = c(5, 5, 2, 2),
                      min_poly = c(0, 0, 0, 0),
                      start_poly = c(2, 1, 0, 0),
                      trace = TRUE,
                      seed = 123) {
  
  # --- 1. Input validation & Setup ---
  max_poly <- .pad_to_length(max_poly, len = 4, fill = 0)
  min_poly <- .pad_to_length(min_poly, len = 4, fill = 0)
  start_poly <- .pad_to_length(start_poly, len = 4, fill = 0)
  
  if (!all(max_poly >= 0)) stop("'max_poly' must contain non-negative integers.")
  if (!all(start_poly >= 0)) stop("'start_poly' must contain non-negative integers.")
  if (!all(min_poly >= 0)) stop("'min_poly' must contain non-negative integers.")
  if (!all(min_poly <= max_poly)) stop("'min_poly' cannot exceed 'max_poly'.")
  if (any(start_poly < min_poly | start_poly > max_poly)) stop("'start_poly' must lie between 'min_poly' and 'max_poly'.")
  if (!all(floor(max_poly) == max_poly)) stop("'max_poly' must contain integers only.")
  if (!all(floor(start_poly) == start_poly)) stop("'start_poly' must contain integers only.")
  if (!all(floor(min_poly) == min_poly)) stop("'min_poly' must contain integers only.")
  
  # --- 2. Handle Family Object ---
  
  family_env <- capture_family_env(fname = family)
  
  if (inherits(family, "gamlss.family")) {
    family_obj <- family
  } else if (is.function(family)) {
    family_obj <- family()
  } else if (is.character(family) && length(family) == 1) {
    family_obj <- find_fun(family, envir = family_env)()
  } else {
    stop("'family' must be a family name, constructor function, or gamlss.family object.")
  }
  
  fname <- family_obj$family[1]
  lpar <- length(family_obj$parameters)
  
  max_poly   <- max_poly[1:lpar]
  min_poly   <- min_poly[1:lpar]
  start_poly <- start_poly[1:lpar]
  data_call <- substitute(data)
  data_name <- ".fb_data"
  
  # --- 3. Create Evaluation Environment ---
  folds <- withr::with_seed(seed, {sample(cut(seq(1, nrow(data)), breaks = 10, labels = FALSE))})
  lookup_env <- rlang::env(
    parent = family_env,
    folds = folds
  )
  assign(data_name, data, envir = lookup_env)

  
  # --- 5. Cache & Selection Logic ---
  memo_cache <- new.env(hash = TRUE, parent = emptyenv())
  
  get_selcrit_cached <- function(poly_deg) {
    key <- paste(poly_deg, collapse = "-")
    if (exists(key, envir = memo_cache)) return(get(key, envir = memo_cache))
    
    val <- get_selcrit(
      poly_deg = poly_deg,
      selcrit = selcrit,
      family_obj = family_obj, # Pass object
      spline = spline,
      method = method,
      data_name = data_name,
      age_name = age_name,
      score_name = score_name,
      env = lookup_env
    )
    assign(key, val, envir = memo_cache)
    return(val)
  }
  
  # --- 6. Optimization Loop ---
  i_poly_deg <- start_poly
  i_selcrit <- get_selcrit_cached(i_poly_deg)
  
  dif_selcrit <- 1
  iter <- 0
  if (trace) .print_iteration(iter, i_poly_deg, spline, selcrit, i_selcrit)
  
  
  if(!(lpar == 1 && spline == TRUE)){
    while (dif_selcrit > 1e-10) {
      new_poly_degs <- get_fb_degs(i_poly_deg, spline, max_poly, min_poly)
      if (length(new_poly_degs) > 0) {
        new_selcrits <- lapply(new_poly_degs, get_selcrit_cached)
        names(new_selcrits) <- seq_along(new_poly_degs)
        new_selcrit <- min(unlist(new_selcrits))
      } else {
        new_selcrit <- i_selcrit
      }
      dif_selcrit <- i_selcrit - new_selcrit
      if (new_selcrit < i_selcrit) {
        id_best <- as.numeric(names(which.min(unlist(new_selcrits))))
        i_poly_deg <- new_poly_degs[[id_best]]
        i_selcrit <- new_selcrit
        iter <- iter + 1
        if (trace) .print_iteration(iter, i_poly_deg, spline, selcrit, i_selcrit)
      }
    }
  }
  
  # --- 7. Final Model Fitting ---
  best_model <- fit_gamlss(
    poly_deg   = i_poly_deg,
    selcrit    = if (selcrit == "CV") "AIC" else selcrit,
    family_obj = family_obj,
    spline     = spline,
    method     = method,
    data_name  = data_name,
    age_name   = age_name,
    score_name = score_name,
    env        = lookup_env
  )
  
  # Metadata
  best_model$selcrit <- i_selcrit
  best_model$data <- data
  best_model$call$data <- data_call
  best_model$call$selcrit <- selcrit
  best_model$call$family <- fname
  
  # Search History
  hist_list <- as.list(memo_cache)
  best_model$search_history <- data.frame(
    poly_deg  = names(hist_list),
    selcrit = unlist(hist_list),
    stringsAsFactors = FALSE
  )[order(unlist(hist_list)), ]
  rownames(best_model$search_history) <- NULL
  
  return(best_model)
}