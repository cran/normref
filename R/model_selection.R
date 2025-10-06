#' Free order model selection procedure
#'
#' @description 
#' `fb_select()` applies the free order model selection procedure, using forward–backward selection 
#' \insertCite{voncken2019model}{normref}.  
#' For a given GAMLSS distribution and model selection criterion, it selects the optimal 
#' polynomial degrees for all distribution parameters.
#' 
#' @details
#' If `parallel = TRUE`, candidate models are evaluated in parallel using the 
#' \pkg{future} and \pkg{future.apply} packages. If these packages are not installed, 
#' a message is printed and the function continues with sequential evaluation.  
#' Parallelization can reduce elapsed time for large datasets, complex models and cross-validation, 
#' but may be slower than sequential evaluation for smaller problems.
#'
#' @param data data.frame. Sample on which to fit the distribution; contains the scores and ages.
#' @param age_name string. Name of the age variable.
#' @param score_name string. Name of the score variable.
#' @param family string. For example, `"BB"`, `"BCPE"`, `"NO"`, etc.  
#'   See [gamlss.dist::gamlss.family] for more information.  
#' @param selcrit string. Model selection criterion: `"AIC"`, `"BIC"` (default), `"GAIC(3)"`, or `"CV"` 
#'   (cross-validation with 10 folds).
#' @param spline logical. If `FALSE` (default), estimate polynomial(s) for \eqn{\mu}; 
#'   if `TRUE`, estimate a p-spline for \eqn{\mu}.
#' @param method string. Estimation method for [gamlss::gamlss()]. Either `"RS()"`, `"CG()"`, or `"mixed()"`, 
#'   with iteration count. Default is `"RS(10000)"`.
#' @param max_poly vector. Maximum polynomial degrees for each parameter.
#' @param min_poly vector. Minimum polynomial degrees for each parameter.
#' @param start_poly vector. Starting polynomial degrees for each parameter.
#' @param trace logical. If `TRUE`, prints progress during selection.
#' @param seed integer. Random seed for cross-validation folds.
#' @param parallel logical. If `TRUE`, candidate models are evaluated in 
#'   parallel using \pkg{future.apply}. This can reduce elapsed time 
#'   for computationally heavy settings (e.g., large datasets, distributions 
#'   with many parameters, or when using cross-validation as the selection 
#'   criterion). For light models or small datasets, the overhead of 
#'   parallelization may make it slower than sequential evaluation.  
#'   Parallelization is not supported for user-defined distribution families; 
#'   use built-in \pkg{gamlss.dist} families instead. Default is `FALSE`.
#'
#' @return A selected GAMLSS model with the chosen polynomial degrees and the final criterion value.
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
                     seed = 123,
                     parallel = FALSE) {
  # --- input validation ---
  if (!all(max_poly >= 0)) stop("'max_poly' must contain non-negative integers.")
  if (!all(start_poly >= 0)) stop("'start_poly' must contain non-negative integers.")
  if (!all(min_poly >= 0)) stop("'min_poly' must contain non-negative integers.")
  if (!all(min_poly <= max_poly)) stop("'min_poly' cannot exceed 'max_poly'.")
  if (!all(min_poly <= start_poly & start_poly <= max_poly)) {
    stop("'start_poly' must lie between 'min_poly' and 'max_poly'.")
  }
  if (!all(floor(max_poly) == max_poly)) stop("'max_poly' must contain integers only.")
  if (!all(floor(start_poly) == start_poly)) stop("'start_poly' must contain integers only.")
  if (!all(floor(min_poly) == min_poly)) stop("'min_poly' must contain integers only.")
  
  # --- set up ---
  
  family <- gamlss.dist::as.gamlss.family(family)
  fname <- as.character(family$family[1])
  lpar <- length(family$parameters)
  max_poly   <- max_poly[1:lpar]
  min_poly   <- min_poly[1:lpar]
  start_poly <- start_poly[1:lpar]
  data_name <- deparse(substitute(data))
  
  # --- set up CV folds ---
  folds <- withr::with_seed(seed, {sample(cut(seq(1, nrow(data)), breaks = 10, labels = FALSE))})
  env <- rlang::env(folds = folds)
  assign(data_name, data, envir = env)
  for (f in c(paste0("d", fname), paste0("p", fname),
              paste0("q", fname), paste0("r", fname))) {
    assign(f, find_fun(f), envir = env)
  }
  assign(fname, family, env)
  

  # --- initialize ---
  i_poly_deg <- start_poly
  i_selcrit <- get_selcrit(
    i_poly_deg,
    selcrit = selcrit,
    family = fname,
    spline = spline,
    method = method,
    data_name = data_name,
    age_name = age_name,
    score_name = score_name,
    env = env
  )
  dif_selcrit <- 1
  iter <- 0
  
  if (trace) .print_iteration(iter, i_poly_deg, spline, selcrit, i_selcrit)
  
  if (parallel) {
    if (!requireNamespace("future.apply", quietly = TRUE) ||
        !requireNamespace("future", quietly = TRUE)) {
      message("Parallel execution requested, but packages 'future' and 'future.apply' ",
              "are not installed. Falling back to sequential execution.")
      lapply_fun <- lapply
    } else {
      # save and restore plan
      oplan <- future::plan()
      on.exit(future::plan(oplan), add = TRUE)
      
      future::plan(future::multisession,
                   workers = max(1, parallel::detectCores() - 1))
      lapply_fun <- future.apply::future_lapply
    }
  } else {
    lapply_fun <- lapply
  }
  
  # --- forward–backward search ---
  while (dif_selcrit > 1e-10) {
    new_poly_degs <- get_fb_degs(
      poly_deg = i_poly_deg,
      spline = spline,
      max_poly = max_poly,
      min_poly = min_poly
    )
    
    if (length(new_poly_degs) > 0) {
      new_selcrits <- lapply_fun(new_poly_degs, function(x) {
        get_selcrit(
            poly_deg = x,
            selcrit = selcrit,
            family = fname,
            spline = spline,
            method = method,
            data_name = data_name,
            age_name = age_name,
            score_name = score_name,
            env = env
          )
      })
      names(new_selcrits) <- 1:length(new_poly_degs)
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
  
  # --- final model ---

  best_model <- str_eval(
    x = str_create(
      poly_deg = i_poly_deg,
      selcrit  = if (selcrit == "CV") "AIC" else selcrit,
      family   = fname,
      spline   = spline,
      method   = method,
      data_name = data_name,
      age_name  = age_name,
      score_name = score_name
    ),
    env = env
  )
  
  best_model$selcrit <- i_selcrit
  best_model$call$data <- data
  
  return(best_model)
}



