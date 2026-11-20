#' Free order model selection procedure
#'
#' @description
#' `fb_select()` applies the free order model selection procedure, using forward–backward selection
#' \insertCite{voncken2019model}{normref}.
#' For a given GAMLSS distribution and model selection criterion, it selects the optimal
#' polynomial degrees for all distribution parameters.
#'
#' @details
#' The search history of the free order model selection procedure can be accessed in `search_history`. This allows the user to investigate whether a more parsimonious model with similar model fit to the selected model exists.
#'
#' @param data data.frame. Sample on which to fit the distribution; contains the scores and ages.
#' @param age_name string. Name of the age variable.
#' @param score_name string. Name of the score variable.
#' @param family string. For example, `"BB"`, `"BCPE"`, `"NO"`, etc.
#'   See [gamlss.dist::gamlss.family] for more information on available distributions. Also accepts user-defined distributions like truncated distributions.
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
#' @param start.from gamlss object (optional). A pre-fitted GAMLSS model to serve as the
#'   initial warm start for fitting the starting model (`start_poly`) using the `start.from` argument in [gamlss::gamlss()]. Only utilized when the distribution has 3 or more parameters. Default is `NULL`.
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
                      start.from = NULL,
                      trace = TRUE,
                      seed = 123) {
  
  # --- 1. Input validation & setup ---
  if (!is.null(start.from) && !inherits(start.from, "gamlss")) {
    stop("'start.from' must be a fitted gamlss model object or NULL.")
  }
  
  max_poly   <- .pad_to_length(max_poly, len = 4, fill = 0)
  min_poly   <- .pad_to_length(min_poly, len = 4, fill = 0)
  start_poly <- .pad_to_length(start_poly, len = 4, fill = 0)
  
  is_whole <- function(x) all(floor(x) == x)
  if (!all(max_poly >= 0))   stop("'max_poly' must contain non-negative integers.")
  if (!all(start_poly >= 0)) stop("'start_poly' must contain non-negative integers.")
  if (!all(min_poly >= 0))   stop("'min_poly' must contain non-negative integers.")
  if (!all(min_poly <= max_poly)) stop("'min_poly' cannot exceed 'max_poly'.")
  if (any(start_poly < min_poly | start_poly > max_poly)) stop("'start_poly' must lie between 'min_poly' and 'max_poly'.")
  if (!is_whole(max_poly))   stop("'max_poly' must contain integers only.")
  if (!is_whole(start_poly)) stop("'start_poly' must contain integers only.")
  if (!is_whole(min_poly))   stop("'min_poly' must contain integers only.")
  if (length(selcrit) != 1L || !selcrit %in% c("AIC", "BIC", "GAIC(3)", "CV")) {
    stop("Unknown selection criterion: ", selcrit)
  }
  
  # --- 2. Family object ---
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
  lpar  <- length(family_obj$parameters)
  
  max_poly   <- max_poly[1:lpar]
  min_poly   <- min_poly[1:lpar]
  start_poly <- start_poly[1:lpar]
  data_call  <- substitute(data)
  data_name  <- ".fb_data"
  
  # --- 3. Evaluation environment ---
  folds <- withr::with_seed(seed, {sample(cut(seq(1, nrow(data)), breaks = 10, labels = FALSE))})
  lookup_env <- rlang::env(parent = family_env, folds = folds)
  assign(data_name, data, envir = lookup_env)
  
  # --- 4. Shared state: criterion cache and warning-dedup cache ---
  memo          <- .make_memo_cache()
  warning_cache <- new.env(parent = emptyenv())
  
  # Plain (non-CV) fit used for the returned model. For "CV", gamlssCV does not
  # return a gamlss object, so the final model is always an ordinary fit.
  fit_plain <- function(poly_deg, start_from_mod = NULL) {
    fit_gamlss(
      poly_deg       = poly_deg,
      selcrit        = "AIC",   # anything but "CV" yields a regular gamlss fit
      family_obj     = family_obj,
      spline         = spline,
      method         = method,
      data_name      = data_name,
      age_name       = age_name,
      score_name     = score_name,
      env            = lookup_env,
      start_from_mod = start_from_mod
    )
  }
  
  # Memoised evaluation. Cached hits return mod = NULL so historical models
  # are never retained in memory.
  eval_model <- function(poly_deg, start_from_mod = NULL) {
    if (memo$has(poly_deg)) return(list(val = memo$get(poly_deg), mod = NULL))
    
    res <- get_selcrit(
      poly_deg       = poly_deg,
      selcrit        = selcrit,
      family_obj     = family_obj,
      spline         = spline,
      method         = method,
      data_name      = data_name,
      age_name       = age_name,
      score_name     = score_name,
      env            = lookup_env,
      start_from_mod = start_from_mod,
      warning_cache  = warning_cache
    )
    memo$set(poly_deg, res$val)
    res
  }
  
  # Benign warnings are muffled for the whole selection run, including the
  # refits below that happen outside get_selcrit().
  best_model <- .muffle_benign_warnings({
    
    # --- 5. Initial model ---
    i_poly_deg <- start_poly
    init_fit   <- eval_model(i_poly_deg, start_from_mod = start.from)
    i_selcrit  <- init_fit$val
    best_mod   <- init_fit$mod
    
    iter <- 0
    if (trace) .print_iteration(iter, i_poly_deg, spline, selcrit, i_selcrit)
    
    # --- 6. Forward–backward loop ---
    if (!(lpar == 1 && spline)) {
      repeat {
        cands <- get_fb_degs(i_poly_deg, spline, max_poly, min_poly)
        if (length(cands) == 0L) break
        
        # Candidates are warm-started from the current best model
        results   <- lapply(cands, eval_model, start_from_mod = best_mod)
        crits     <- vapply(results, function(x) x$val, numeric(1))
        best_idx  <- which.min(crits)
        new_crit  <- crits[best_idx]
        
        # No improvement: stop (candidate models are discarded)
        if (!(new_crit < i_selcrit)) {
          gc(verbose = FALSE)
          break
        }
        
        dif_selcrit <- i_selcrit - new_crit
        i_poly_deg  <- cands[[best_idx]]
        i_selcrit   <- new_crit
        iter        <- iter + 1
        new_mod     <- results[[best_idx]]$mod
        rm(results)
        
        # Candidate may come from the cache (no model stored): refit once
        if (is.null(new_mod)) new_mod <- fit_plain(i_poly_deg, start_from_mod = best_mod)
        best_mod <- new_mod
        gc(verbose = FALSE)
        
        if (trace) .print_iteration(iter, i_poly_deg, spline, selcrit, i_selcrit)
        
        # Keep the improving model, but stop once the gain is negligible
        if (!(dif_selcrit > 1e-10)) break
      }
    }
    
    # CV (or a missing model) requires one ordinary final fit
    if (identical(selcrit, "CV") || is.null(best_mod)) {
      best_mod <- fit_plain(i_poly_deg)
    }
    
    best_mod
  })
  
  # --- 7. Final output ---
  best_model <- .strip_envs(best_model)
  best_model$selcrit <- i_selcrit
  best_model$data <- data
  best_model$call$data <- data_call
  best_model$call$selcrit <- selcrit
  best_model$call$family <- fname
  best_model$call$start.from <- NULL
  best_model$search_history <- memo$history()

  best_model
}