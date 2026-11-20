#' Pattern identifying benign GAMLSS warnings (redundant or harmless)
#' @noRd
.benign_warning_regex <- "discrepancy|dBB|dBI|replacement"

#' Is a warning condition benign?
#' @noRd
.is_benign_warning <- function(w) {
  grepl(.benign_warning_regex, conditionMessage(w), ignore.case = TRUE)
}

#' Evaluate an expression with benign warnings muffled
#'
#' Wrap the body of any top-level function (e.g. `normtable_create()`) in this
#' helper so benign warnings are suppressed everywhere, including fits that
#' happen outside `get_selcrit()` (final refits, table creation, ...).
#' Non-benign warnings pass through untouched.
#'
#' @noRd
.muffle_benign_warnings <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (.is_benign_warning(w)) invokeRestart("muffleWarning")
    }
  )
}

#' Memoisation cache for selection-criterion values
#'
#' Closure-based cache keyed on the polynomial-degree vector. It remembers
#' insertion order so the search history is deterministic (ties in the criterion
#' keep the order in which models were evaluated). Only criterion values are
#' stored, never models, to avoid holding fitted models in memory.
#'
#' @return list of functions: `has`, `get`, `set`, `history`.
#' @noRd
.make_memo_cache <- function() {
  store     <- new.env(hash = TRUE, parent = emptyenv())
  keys_seen <- character()
  
  make_key <- function(poly_deg) paste(poly_deg, collapse = "-")
  
  list(
    has = function(poly_deg) {
      exists(make_key(poly_deg), envir = store, inherits = FALSE)
    },
    get = function(poly_deg) {
      get(make_key(poly_deg), envir = store, inherits = FALSE)
    },
    set = function(poly_deg, val) {
      k <- make_key(poly_deg)
      if (!exists(k, envir = store, inherits = FALSE)) {
        keys_seen <<- c(keys_seen, k)
      }
      assign(k, val, envir = store)
      invisible(val)
    },
    history = function() {
      vals <- vapply(keys_seen, function(k) get(k, envir = store, inherits = FALSE),
                     numeric(1), USE.NAMES = FALSE)
      df <- data.frame(poly_deg = keys_seen, selcrit = vals, stringsAsFactors = FALSE)
      df <- df[order(df$selcrit), , drop = FALSE]
      rownames(df) <- NULL
      df
    }
  )
}

#' Fit a GAMLSS model using substituted objects
#'
#' @description
#' Constructs and evaluates a GAMLSS model call. By using substitute, we pass the 
#' actual family object and data name directly into the expression, which is 
#' safer for parallel processing than string evaluation.
#'
#' @noRd
fit_gamlss <- function(poly_deg,
                       selcrit,
                       family_obj,
                       spline,
                       data_name,
                       age_name,
                       score_name,
                       method,
                       env,
                       start_from_mod = NULL) {
  age_sym <- as.name(age_name)
  
  form_rhs <- function(deg, is_mu = FALSE) {
    if (is_mu && isTRUE(spline)) return(substitute(pb(A), list(A = age_sym)))
    if (deg == 0) return(1)
    substitute(stats::poly(A, D), list(A = age_sym, D = deg))
  }
  
  one_sided <- function(i) {
    if (length(poly_deg) < i) return(NULL)
    stats::as.formula(substitute(~ FORM, list(FORM = form_rhs(poly_deg[i]))))
  }
  
  mu_f <- stats::as.formula(
    substitute(S ~ FORM, list(S = as.name(score_name), FORM = form_rhs(poly_deg[1], TRUE)))
  )
  
  is_cv          <- identical(selcrit, "CV")
  has_warm_start <- !is_cv && !is.null(start_from_mod) && inherits(start_from_mod, "gamlss")
  
  # Warm-start model is exposed in env under a fixed name for the duration of the fit
  if (has_warm_start) {
    assign(".warm_start_mod", start_from_mod, envir = env)
    on.exit(
      if (exists(".warm_start_mod", envir = env, inherits = FALSE)) rm(".warm_start_mod", envir = env),
      add = TRUE
    )
  }
  
  fun <- if (is_cv) quote(gamlss::gamlssCV) else quote(gamlss::gamlss)
  args <- list(
    mu_f,
    sigma.formula = one_sided(2),
    nu.formula    = one_sided(3),
    tau.formula   = one_sided(4),
    family        = family_obj,
    data          = as.name(data_name),
    method        = parse(text = method)[[1]]
  )
  if (is_cv)          args$rand       <- quote(folds)
  if (has_warm_start) args$start.from <- quote(.warm_start_mod)
  args$trace <- FALSE
  
  # Drop NULL formula arguments
  args <- args[!vapply(args, is.null, logical(1))]
  call_expr <- as.call(c(list(fun), args))
  
  eval(call_expr, envir = env)
}

#' Obtain candidate models (vectors of polynomial degrees) 
#' for forward-backward selection
#'
#' @inheritParams fb_select
#' @param poly_deg vector. current polynomial degrees.
#'
#' @return a list of candidate vectors of polynomial degrees.
#'
#' @noRd
get_fb_degs <- function(poly_deg,
                        spline,
                        max_poly = c(5, 5, 2, 2),
                        min_poly = c(1, 0, 0, 0)) {
  # Starting index depends on spline setting
  i_start <- if (spline) 2 else 1
  
  poly_list <- lapply(seq(from = i_start, to = length(poly_deg)), function(i) {
    if (poly_deg[i] > min_poly[i] && poly_deg[i] < max_poly[i]) {
      # Case: can decrease or increase
      list(
        replace(poly_deg, i, poly_deg[i] - 1),
        replace(poly_deg, i, poly_deg[i] + 1)
      )
    } else if (poly_deg[i] > min_poly[i] && poly_deg[i] == max_poly[i]) {
      # Case: at maximum, can only decrease
      list(replace(poly_deg, i, poly_deg[i] - 1))
    } else if (poly_deg[i] == min_poly[i] && poly_deg[i] < max_poly[i]) {
      # Case: at minimum, can only increase
      list(replace(poly_deg, i, poly_deg[i] + 1))
    } else {
      # Case: no candidates
      list()
    }
  })
  
  unlist(poly_list, recursive = FALSE)
}

#' Compute selection criterion and return fitted model
#'
#' @inheritParams fb_select
#' @param poly_deg Integer vector. Polynomial degrees for all parameters.
#' @param env Environment. Evaluation environment.
#' @param start_from_mod Optional GAMLSS object. Warm-start model.
#' @param warning_cache Optional environment. Cache to deduplicate repeated warnings.
#'
#' @return A list with \code{val} (numeric selection criterion) and \code{mod} (fitted GAMLSS model or NULL).
#' @noRd
get_selcrit <- function(poly_deg,
                        selcrit,
                        family_obj,
                        spline,
                        method,
                        data_name,
                        age_name,
                        score_name,
                        env,
                        start_from_mod = NULL,
                        warning_cache = NULL) {
  deg_label <- paste0("[", paste(poly_deg, collapse = ", "), "]")
  excluded  <- function(reason) {
    warning(sprintf("Model %s excluded: %s", deg_label, reason), call. = FALSE)
    list(val = Inf, mod = NULL)
  }
  
  if (is.null(warning_cache)) warning_cache <- new.env(parent = emptyenv())
  
  tryCatch({
    mod <- NULL
    
    withCallingHandlers(
      utils::capture.output({
        mod <- fit_gamlss(
          poly_deg       = poly_deg,
          selcrit        = selcrit,
          family_obj     = family_obj,
          spline         = spline,
          method         = method,
          data_name      = data_name,
          age_name       = age_name,
          score_name     = score_name,
          env            = env,
          start_from_mod = start_from_mod
        )
      }),
      warning = function(w) {
        if (!.is_benign_warning(w)) {
          call_txt <- if (is.null(conditionCall(w))) "<unknown call>" else
            paste(deparse(conditionCall(w)), collapse = " ")
          warning_id <- paste(conditionMessage(w), call_txt, sep = " | ")
          if (!exists(warning_id, envir = warning_cache, inherits = FALSE)) {
            assign(warning_id, TRUE, envir = warning_cache)
            message(sprintf("Model %s generated warning:\nCall: %s\nWarning: %s",
                            deg_label, call_txt, conditionMessage(w)))
          }
        }
        invokeRestart("muffleWarning")
      }
    )
    
    if (!identical(selcrit, "CV") && !is.null(mod) && !isTRUE(mod$converged)) {
      return(excluded("algorithm did not converge."))
    }
    
    val <- switch(
      selcrit,
      "AIC"     = stats::AIC(mod),
      "BIC"     = stats::BIC(mod),
      "GAIC(3)" = gamlss::GAIC(mod, k = 3),
      "CV"      = gamlss::CV(mod),
      stop("Unknown selection criterion: ", selcrit)
    )
    
    if (is.null(val) || !is.numeric(val) || is.na(val) || !is.finite(val)) {
      return(excluded("criterion undefined or non-finite."))
    }
    
    list(val = val, mod = if (identical(selcrit, "CV")) NULL else mod)
  },
  error = function(e) {
    warning(sprintf("Model %s excluded due to error: <%s>", deg_label, conditionMessage(e)),
            call. = FALSE)
    list(val = Inf, mod = NULL)
  })
}

#' @noRd
.strip_envs <- function(x, env = globalenv()) {
  if (is.environment(x) || is.name(x)) return(x)
  if (inherits(x, "formula") || inherits(x, "terms")) {
    attr(x, ".Environment") <- env
    return(x)
  }
  if (is.call(x)) return(as.call(lapply(as.list(x), .strip_envs, env = env)))
  if (is.list(x)) {
    attrs <- attributes(x)
    x <- lapply(x, .strip_envs, env = env)
    attributes(x) <- attrs
  }
  x
}
# ============================================================================
# Shared distribution / function-lookup helpers
# ============================================================================

#' Look up a function in a user environment, then in gamlss.dist
#' @return the function, or `NULL` if it cannot be found.
#' @noRd
.lookup_fun <- function(fname, envir = .GlobalEnv, exports = getNamespaceExports("gamlss.dist")) {
  if (exists(fname, envir = envir, mode = "function", inherits = FALSE)) {
    return(get(fname, envir = envir, mode = "function"))
  }
  if (fname %in% exports) {
    return(getExportedValue("gamlss.dist", fname))
  }
  NULL
}

#' Name of a GAMLSS family given as a name, constructor or family object
#' @noRd
.family_name <- function(family) {
  if (is.character(family)) return(family[1])
  if (is.function(family)) family <- family()
  if (inherits(family, "gamlss.family")) return(family$family[1])
  stop("'family' must be a family name, constructor function, or gamlss.family object.",
       call. = FALSE)
}

#' Is a family one of the binomial-type GAMLSS families (needs a denominator `bd`)?
#' @noRd
.is_bi_family <- function(fname) fname %in% gamlss::.gamlss.bi.list

#' Predict all distribution parameters of a GAMLSS model on new data
#'
#' The first argument names (`fitdata`) matter: `predictAll()` may look up the
#' fitting data by this name in calling frames.
#'
#' @param bd Optional binomial denominator; only passed for binomial-type families.
#' @noRd
.predict_distr <- function(model, fitdata, newdata, bd = NULL) {
  suppressWarnings({
    if (.is_bi_family(model$family[1]) && !is.null(bd)) {
      gamlss::predictAll(model, data = fitdata, newdata = newdata, type = "response", bd = bd)
    } else {
      gamlss::predictAll(model, data = fitdata, newdata = newdata, type = "response")
    }
  })
}

#' Build the argument list for a d/p/q/r function from predicted parameters
#'
#' @param pred output of `.predict_distr()`.
#' @param lpar number of distribution parameters.
#' @param first named list of leading arguments (e.g. `list(q = ...)`).
#' @param i optional row index; if `NULL` the full parameter vectors are used.
#' @noRd
.dist_args <- function(pred, lpar, first = list(), i = NULL, bd = NULL, is_bi = FALSE) {
  pick <- function(x) if (is.null(i)) x else x[i]
  args <- c(first, list(mu = pick(pred$mu)))
  if (lpar >= 2) args$sigma <- pick(pred$sigma)
  if (lpar >= 3) args$nu    <- pick(pred$nu)
  if (lpar >= 4) args$tau   <- pick(pred$tau)
  if (is_bi)     args$bd    <- bd
  args
}

#' Confidence bounds around a true score, truncated to the score range
#' @return list(lower, upper)
#' @noRd
.ci_bounds <- function(truescore, SEE, ci_level, score_min, score_max) {
  lower <- truescore + stats::qnorm((1 - ci_level) / 2) * SEE
  upper <- truescore + stats::qnorm(1 - (1 - ci_level) / 2) * SEE
  lower[lower < score_min] <- score_min
  upper[upper > score_max] <- score_max
  list(lower = lower, upper = upper)
}

#' Flat numeric score vector (first column if the score is stored as a matrix)
#' @noRd
.score_vector <- function(x) {
  if (is.matrix(x)) x[, 1] else as.numeric(x)
}


#' Estimate CDF and associated Z-scores
#'
#' @description
#' estimates the conditional cumulative distribution function (CDF) and
#' associated Z-scores based on a fitted \code{GAMLSS} model for given
#' ages and scores.
#'
#' @inheritParams normtable_create
#' @param object a fitted \code{gamlss} model.
#' @param scores data.frame. contains the score(s) for which the CDFs and Z-scores
#'   are estimated, conditional on age.
#' @param cont_cor logical. if \code{TRUE}, apply a continuity correction for
#'   discrete scores; otherwise (default), no correction is applied.
#'
#' @details
#' \code{scores} can be:
#' - a single row/column (then evaluated at all ages), or  
#' - the same number of rows as \code{ages} (then matched rowwise).
#'
#' @return
#' a numeric matrix (\code{CDF_matrix}) of estimated percentiles for each
#' age-score combination.
#'
#' @noRd
estimateCDF <- function(object,
                        ages,
                        scores,
                        data = NULL,
                        newdata = NULL,
                        lower.tail = TRUE) {
  
  # 1. Resolve fit data explicitly from argument or fallback to model
  #    (`fitdata` must exist under this name: the model call refers to it)
  fitdata <- if (!is.null(data)) as.data.frame(data) else get_model_data(object)
  object$call$data <- quote(fitdata)
  
  fname <- object$family[1]
  lpar  <- length(object$parameters)
  is_bi <- .is_bi_family(fname)
  
  # 2. Align evaluation rows
  eval_grid <- if (!is.null(newdata)) as.data.frame(newdata) else as.data.frame(ages)
  n_eval    <- nrow(eval_grid)
  
  if (is.vector(scores) || is.factor(scores)) {
    scores <- matrix(scores, ncol = 1)
  }
  
  if (nrow(scores) == n_eval) {
    scores_c <- as.matrix(scores)
  } else if (nrow(scores) == 1) {
    scores_c <- matrix(rep(as.matrix(scores), each = n_eval), nrow = n_eval)
  } else if (ncol(scores) == 1) {
    scores_c <- matrix(rep(as.vector(scores), each = n_eval), nrow = n_eval)
  } else {
    stop("Dimension mismatch: 'scores' must have 1 row, 1 column, or match evaluation rows.")
  }
  
  # 3. Predict distributional parameters
  pred_distr <- .predict_distr(object, fitdata, eval_grid)
  
  CDF_matrix <- matrix(NA_real_, nrow = nrow(scores_c), ncol = ncol(scores_c))
  pfun       <- find_fun(paste0("p", fname))
  
  for (i in seq_len(nrow(scores_c))) {
    args <- .dist_args(pred_distr, lpar, first = list(q = scores_c[i, ]), i = i,
                       bd = object$bd[1], is_bi = is_bi)
    args$lower.tail <- lower.tail
    CDF_matrix[i, ] <- do.call(pfun, args)
  }
  
  CDF_matrix
}


#' Linearly transform Z-scores to a scaled score
#'
#' @description
#' transforms Z-scores into normed scores of type \code{"Z"}, \code{"T"}, \code{"IQ"} or
#' \code{"SS"} (scaled scores) using a linear transformation.
#'
#' @param Zscores numeric vector of Z-scores.
#' @param normtype character. type of normed score:
#'   * \code{"Z"}  (mean = 0,   SD = 1, default)  
#'   * \code{"T"}  (mean = 50,  SD = 10)  
#'   * \code{"IQ"} (mean = 100, SD = 15)
#'   * \code{"SS"} (mean = 10, SD = 3)    
#'
#' @return numeric vector of scaled scores.
#'
#' @noRd
ZtoScale <- function(Zscores, normtype = "Z") {
  p <- .normtype_params(normtype)
  p[["mean"]] + p[["sd"]] * Zscores
}


#' Transform CDF to trimmed normalized Z-scores
#'
#' @description
#' converts CDF values to normalized Z-scores (via \code{qnorm}),
#' trimming them at \code{±trim} to avoid extreme values.
#'
#' @param CDF numeric vector of CDF values.
#' @param trim numeric. maximum absolute value allowed for Z-scores.
#'
#' @return numeric vector of trimmed Z-scores.
#'
#' @noRd
CDFtotrimZ <- function(CDF, trim) {
  # Clamp probabilities away from 0 and 1
  CDF <- pmin(pmax(CDF, 1e-4), 0.9999)
  
  # Convert to Z and trim
  Z <- stats::qnorm(CDF)
  trimZ <- pmin(pmax(Z, -trim), trim)
  
  return(trimZ)
}

#' Estimate mu and sigma by age from a GAMLSS model
#'
#' @description
#' estimates the conditional mean (\eqn{\mu}) and standard deviation (\eqn{\sigma})
#' for given ages, based on a fitted \code{gamlss} model.
#'
#' @inheritParams normtable_create
#' @param object a fitted \code{gamlss} model.
#'
#' @return
#' a list with two numeric vectors:
#' * \code{mean_dis} - estimated means by age  
#' * \code{sd_dis}   - estimated standard deviations by age  
#'
#' @noRd
estimate_musigma <- function(object,
                             normingdata,
                             score_max = NULL) {
  
  fitdata <- get_model_data(object, fallback_data = normingdata)
  fname   <- object$family[1]
  family  <- gamlss.dist::as.gamlss.family(fname)
  nperson <- nrow(normingdata)
  lpar    <- length(object$parameters)
  is_bi   <- .is_bi_family(fname)
  
  pred <- .predict_distr(object, fitdata, normingdata, bd = score_max)
  
  # --- Case 1: family provides mean() and variance() ---
  if (!(is.null(family$mean) && is.null(family$variance))) {
    args <- .dist_args(pred, lpar, bd = score_max, is_bi = is_bi)
    
    mean_dis <- do.call(family$mean, args)
    sd_dis   <- sqrt(do.call(family$variance, args))
    
    # --- Case 2: fall back on simulation ---
  } else {
    rfun     <- find_fun(paste0("r", fname))
    mean_dis <- numeric(nperson)
    sd_dis   <- numeric(nperson)
    
    for (i in seq_len(nperson)) {
      args <- .dist_args(pred, lpar, first = list(n = 10000), i = i, bd = score_max, is_bi = is_bi)
      sample_i <- do.call(rfun, args)
      mean_dis[i] <- mean(sample_i)
      sd_dis[i]   <- stats::sd(sample_i)
    }
  }
  
  list(mean_dis = mean_dis, sd_dis = sd_dis)
}

#' Confidence intervals of raw scores from a GAMLSS model
#'
#' @description
#' computes confidence intervals (CIs) for raw test scores, conditional on age,
#' using a fitted \code{gamlss} model and estimated test reliabilities.
#'
#' @inheritParams normtable_create
#' @param object a fitted \code{gamlss} model.
#' @param datarel data.frame with two columns:
#'   * \code{age} - age values  
#'   * \code{rel} - estimated reliability at those ages  
#' @param ci_level numeric. confidence level (default: 0.95).
#'
#' @return a list with some or all of the following elements:
#' * \code{lower_sample}, \code{upper_sample} - lower/upper CI bounds for sample scores  
#' * \code{lower_matrix}, \code{upper_matrix} - lower/upper CI bounds for norm table scores, conditional on age  
#'
#' returned elements depend on whether \code{normingdata}, \code{scores_c}, and \code{pop_age} are provided.
#'
#' @noRd
intervals <- function(object,
                      datarel,
                      age_name,
                      score_name,
                      score_min = NULL,
                      score_max = NULL,
                      normingdata = NULL,
                      scores_c = NULL,
                      pop_age = NULL,
                      ci_level = 0.95) {
  
  # Default min/max scores
  if (is.null(score_min) && !is.null(normingdata)) {
    score_min <- min(normingdata[[score_name]], na.rm = TRUE)
  }
  if (is.null(score_max) && !is.null(normingdata)) {
    score_max <- max(normingdata[[score_name]], na.rm = TRUE)
  }
  
  # Validate reliability data
  if (!all(c("age", "rel") %in% names(datarel))) {
    stop("`datarel` must have columns named 'age' and 'rel'.")
  }
  
  out <- list()
  
  # --- Case 1: Norm table-based CIs (scores_c + pop_age) ---
  if (!is.null(scores_c) && !is.null(pop_age)) {
    r <- stats::approx(x = datarel$age, y = datarel$rel, xout = unlist(pop_age))$y
    musigma <- estimate_musigma(object, normingdata = pop_age, score_max = score_max)
    
    # Expand mean, sd and reliability to match scores_c dimensions
    expand  <- function(x) matrix(x, nrow = nrow(scores_c), ncol = ncol(scores_c))
    mean_mat <- expand(musigma$mean_dis)
    r_mat    <- expand(r)
    sd_mat   <- expand(musigma$sd_dis)
    
    truescore <- mean_mat + r_mat * (scores_c - mean_mat)
    SEE <- sd_mat * sqrt(r_mat * (1 - r_mat))
    
    bounds <- .ci_bounds(truescore, SEE, ci_level, score_min, score_max)
    out$lower_matrix <- bounds$lower
    out$upper_matrix <- bounds$upper
  }
  
  # --- Case 2: Sample-based CIs (normingdata) ---
  if (!is.null(normingdata)) {
    r <- stats::approx(datarel$age, datarel$rel, xout = normingdata[[age_name]])$y
    musigma <- estimate_musigma(object, normingdata = normingdata, score_max = score_max)
    
    truescore <- musigma$mean_dis + r * (normingdata[[score_name]] - musigma$mean_dis)
    SEE <- musigma$sd_dis * sqrt(r * (1 - r))
    
    bounds <- .ci_bounds(truescore, SEE, ci_level, score_min, score_max)
    out$lower_sample <- bounds$lower
    out$upper_sample <- bounds$upper
  }
  
  out
}

#' Print fb_select iteration
#'
#' @inheritParams fb_select
#' @param iter integer. current iteration.
#' @param poly_deg vector. polynomial degrees on iteration.
#' @param selcrit_value numeric. value of selection criterion at iteration.
#'
#' @return invisibly returns `NULL`. called for side-effects (printing).
#'
#' @noRd
.print_iteration <- function(iter, poly_deg, spline, selcrit, selcrit_value) {
  # Replace first degree with "S" if spline is used
  display_deg <- poly_deg
  display_deg[1] <- if (isTRUE(spline)) "S" else poly_deg[1]
  
  # Label for criterion
  crit_label <- if (selcrit == "CV") {
    "Cross-validated Global Deviance (10 folds)"
  } else {
    selcrit
  }
  
  # Print formatted iteration info
  cat(
    "FB-select iteration ", iter,
    ": Polynomial degrees = ", paste0(display_deg, collapse = ","),
    " : ", crit_label, " = ", format(round(selcrit_value, 4)),
    "\n", sep = ""
  )
  
  invisible(NULL)
}


#' Find a function by name in a user environment or gamlss.dist
#'
#' This helper searches for a function (typically a distribution function
#' such as `q*`, `d*`, `p*`, or `r*`) in two places:
#'
#' 1. A user-supplied environment (default: the global environment).
#' 2. The \pkg{gamlss.dist} namespace (for built-in GAMLSS distributions).
#'
#' If the function is not found in either place, an error is raised.
#'
#' @param fname A character string giving the name of the function
#'   (e.g., `"qLOGNO"`, `"dGA"`, `"pPO"`, or a custom `"qMyDist"`).
#' @param envir Environment in which to first search for user-defined
#'   distribution functions. Defaults to `.GlobalEnv`.
#'
#' @return A function object corresponding to `fname`.
#'
#' @noRd
find_fun <- function(fname, envir = .GlobalEnv) {
  stopifnot(
    is.character(fname),
    length(fname) == 1,
    is.environment(envir)
  )
  
  fun <- .lookup_fun(fname, envir)
  if (is.null(fun)) {
    stop(
      sprintf("Function '%s' not found in the supplied environment or gamlss.dist.", fname),
      call. = FALSE
    )
  }
  fun
}

#' Pad (or truncate) a vector to a given length, filling missing values
#' @noRd
.pad_to_length <- function(x, len, fill = 0) {
  length(x) <- len
  x[is.na(x)] <- fill
  x
}

#' Capture a family of distribution functions and their closures
#'
#' Captures a user-defined family of distribution functions, such as those
#' defined in \pkg{gamlss.tr}, and stores their full function closures in a
#' new environment. The captured functions can subsequently be exported to
#' parallel workers, where their enclosing environments may otherwise not be
#' available.
#'
#' For a distribution named `fname`, the function looks for the corresponding
#' density, distribution, quantile, and random-generation functions
#' (`d*`, `p*`, `q*`, and `r*`). Functions are first searched for in
#' `lookup_envir`; if not found there, exported functions from
#' \pkg{gamlss.dist} are used.
#'
#' @param fname Name of the distribution family (a character string, or a
#'   family constructor / `gamlss.family` object from which the name is taken).
#' @param lookup_envir Environment in which to look for user-defined
#'   distribution functions. Defaults to `.GlobalEnv`.
#'
#' @return An environment containing the available distribution functions as
#'   complete function closures. The environment has `emptyenv()` as its
#'   parent.
#'
#' @examples
#' \dontrun{
#' family_env <- capture_family_env("BCCG")
#' }
#'
#' @seealso [gamlss.dist::gamlss.dist]
#' @noRd
capture_family_env <- function(fname, lookup_envir = .GlobalEnv) {
  fname   <- .family_name(fname)
  out     <- new.env(parent = emptyenv())
  exports <- getNamespaceExports("gamlss.dist")
  
  for (nm in c(fname, paste0(c("d", "p", "q", "r"), fname))) {
    fun <- .lookup_fun(nm, lookup_envir, exports)
    if (!is.null(fun)) assign(nm, fun, envir = out)
  }
  
  if (!exists(fname, envir = out, mode = "function", inherits = FALSE)) {
    stop(
      "Distribution '", fname, "' could not be found. ",
      "You seem to have used a user-specified distribution ",
      "(e.g., a truncated distribution). ",
      "Make sure that the distribution is loaded in your global environment.",
      call. = FALSE
    )
  }
  
  out
}

#' Extract or resolve original training data from a GAMLSS model
#'
#' @description
#' Robustly extracts the data frame used to fit a GAMLSS model by inspecting
#' `model$data`, evaluating `model$call[["data"]]` across relevant environments,
#' and falling back to a supplied data frame if needed.
#'
#' @param model a fitted GAMLSS model.
#' @param fallback_data optional data.frame to fall back to if model data cannot be retrieved.
#'
#' @return A data.frame.
#' @noRd
get_model_data <- function(model, fallback_data = NULL) {
  # 1. Direct attachment on the model object
  if (!is.null(model$data) && is.data.frame(model$data)) {
    return(model$data)
  }
  
  # 2. If data is stored in the call
  if (!is.null(model$call[["data"]])) {
    data_call <- model$call[["data"]]
    
    # If the call contains the data frame directly (not a symbol)
    if (is.data.frame(data_call)) {
      return(data_call)
    }
    
    # Try evaluating in formula environment
    form_env <- tryCatch(
      environment(stats::formula(model)),
      error = function(e) NULL
    )
    if (!is.null(form_env)) {
      d <- tryCatch(eval(data_call, envir = form_env), error = function(e) NULL)
      if (is.data.frame(d)) return(d)
    }
    
    # Try evaluating across parent frames
    for (i in seq_len(sys.nframe())) {
      d <- tryCatch(eval(data_call, envir = parent.frame(i)), error = function(e) NULL)
      if (is.data.frame(d)) return(d)
    }
    
    # Try GlobalEnv
    d <- tryCatch(eval(data_call, envir = .GlobalEnv), error = function(e) NULL)
    if (is.data.frame(d)) return(d)
  }
  
  # 3. Fall back to supplied data
  if (!is.null(fallback_data) && is.data.frame(fallback_data)) {
    return(fallback_data)
  }
  
  stop("Could not locate the data used to fit the model. Please provide the dataset to `data`.")
}


# ============================================================================
# Bootstrap, plotting and reference-SE helpers
# ============================================================================

# ============================================================================
# Statistical & Bootstrap Summary Helpers
# ============================================================================

#' Convert wide norm matrix to tidy long format
#'
#' @param x Matrix or data.frame with the independent continuous variable
#'   as the first column.
#' @param age_col Character. Column name for the independent variable.
#' @param score_col Character. Column name for the score variable.
#'
#' @return A tidy data.frame in long format.
#' @noRd
normref_table_to_long <- function(
    x,
    age_col = "age",
    score_col = "testscore"
) {
  df <- as.data.frame(x)
  
  covar_vals <- df[[1]]
  vals <- df[, -1, drop = FALSE]
  
  grid_list <- list(
    covar_vals,
    colnames(vals)
  )
  
  names(grid_list) <- c(age_col, score_col)
  
  out <- expand.grid(
    grid_list,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  
  out$original <- as.vector(as.matrix(vals))
  
  # Convert the score-grid labels, such as "10", "11", "12",
  # to numeric values while retaining the requested column name.
  out[[score_col]] <- as.numeric(out[[score_col]])
  
  out
}

#' Kurtosis (non-excess, m4 / s^4) for MCSE calculation
#' @noRd
calc_kurtosis <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 4) return(NA_real_)
  m4 <- mean((x - mean(x))^4)
  s2 <- stats::var(x)
  if (s2 == 0) return(NA_real_)
  m4 / (s2^2)
}

#' Single bootstrap distribution summary statistics
#' @noRd
bootstrap_summary_vec <- function(x, ci_level = 0.95, theta_hat = NULL) {
  x <- x[is.finite(x)]
  n <- length(x)
  
  if (n < 2) {
    return(c(boot_mean = NA_real_, boot_median = NA_real_, boot_se = NA_real_,
             boot_ci_lower = NA_real_, boot_ci_upper = NA_real_, boot_n = n))
  }
  
  # Default theta_hat to sample mean if not provided
  if (is.null(theta_hat)) {
    theta_hat <- mean(x)
  }
  
  # 1. Bias-correction parameter z0
  p0 <- mean(x < theta_hat)
  
  # Handle boundary cases where p0 is 0 or 1
  p0 <- min(max(p0, 1 / (2 * n)), 1 - 1 / (2 * n))
  z0 <- stats::qnorm(p0)
  
  # 2. Standard normal critical values
  alpha <- 1 - ci_level
  z_lower <- stats::qnorm(alpha / 2)
  z_upper <- stats::qnorm(1 - alpha / 2)
  
  # 3. Adjusted percentile levels (bounded between 0 and 1)
  a1 <- stats::pnorm(2 * z0 + z_lower)
  a2 <- stats::pnorm(2 * z0 + z_upper)
  
  # 4. Bias-corrected quantiles
  ci <- stats::quantile(x, probs = c(a1, a2), names = FALSE, type = 6)
  
  c(boot_mean = mean(x), 
    boot_median = stats::median(x), 
    boot_se = stats::sd(x),
    boot_ci_lower = ci[1], 
    boot_ci_upper = ci[2], 
    boot_n = n)
}
#' Summarize 2D matrix (sample) or 3D array (table) of bootstrap values
#' @noRd
summarise_boot_matrix_or_array <- function(boot_obj, original_df, ci_level = 0.95) {
  res <- t(apply(.flatten_boot(boot_obj), 1, bootstrap_summary_vec, ci_level = ci_level))
  cbind(original_df, as.data.frame(res))
}

#' Calculate MCSE of the bootstrap SE
#' @noRd
calc_mcse_df <- function(boot_obj) {
  na_res <- c(boot_mcse_se = NA_real_, boot_cv_se = NA_real_)
  
  mcse_res <- apply(.flatten_boot(boot_obj), 1, function(x) {
    x <- x[is.finite(x)]
    n <- length(x)
    if (n < 4) return(na_res)
    se <- stats::sd(x)
    k <- calc_kurtosis(x)
    if (is.na(k) || se == 0) return(na_res)
    mcse <- se * sqrt((k - 1) / (4 * n))
    c(boot_mcse_se = mcse, boot_cv_se = mcse / se)
  })
  as.data.frame(t(mcse_res))
}

#' Trim extreme norm scores in bootstrap summaries
#'
#' @param df Data frame containing bootstrap summaries (must contain 'original' or 'boot_mean').
#' @param trim Numeric. Truncation threshold in Z-score units (default is 3).
#' @param normtype string. Norm score type: `"Z"` (N(0,1); default), `"T"` (N(50,10)), `"IQ"` (N(100,15)), or `"SS"` (N(10,3)).
#'
#' @return The modified data frame with out-of-bound values set to \code{NA}.
#' @noRd
trim_bootstrap_results <- function(df, trim = 3, normtype = "Z") {
  lower <- ZtoScale(-trim, normtype)
  upper <- ZtoScale(trim, normtype)
  
  # Identify reference norm score column
  ref_col <- if ("original" %in% names(df)) "original" else if ("boot_mean" %in% names(df)) "boot_mean" else NULL
  
  if (!is.null(ref_col)) {
    # Flag values outside [-trim, +trim] SD or that are already NA
    out_of_bounds <- is.na(df[[ref_col]]) | df[[ref_col]] <= lower | df[[ref_col]] >= upper
    
    # Target all bootstrap and uncertainty statistic columns
    stat_cols <- intersect(
      c("boot_se", "boot_mean", "boot_median", "boot_ci_lower", "boot_ci_upper", 
        "boot_n", "boot_mcse_se", "boot_cv_se", "se"),
      names(df)
    )
    
    # Set out-of-bound rows to NA
    for (col in stat_cols) {
      df[[col]][out_of_bounds] <- NA_real_
    }
  }
  
  # Cap any remaining finite point estimates within bounds
  cap_cols <- intersect(c("boot_mean", "boot_median", "boot_ci_lower", "boot_ci_upper"), names(df))
  for (col in cap_cols) {
    df[[col]] <- pmin(pmax(df[[col]], lower), upper)
  }
  
  df
}
# ============================================================================
# GAMLSS Extraction & Model Refitting Helpers
# ============================================================================

#' Extract polynomial degrees and detect splines in fitted GAMLSS model
#' @noRd
extract_poly_degrees <- function(model) {
  lpar <- length(model$parameters)
  poly_degs <- stats::setNames(rep(NA_integer_, lpar), model$parameters)
  formula_slots <- c("mu.formula", "sigma.formula", "nu.formula", "tau.formula")[seq_len(lpar)]
  spline_found <- FALSE
  
  for (i in seq_len(lpar)) {
    f_obj <- model[[formula_slots[i]]]
    if (is.null(f_obj)) next
    
    if (length(attr(stats::terms(f_obj), "term.labels")) == 0L) {
      poly_degs[i] <- 0L
      next
    }
    
    f_str <- paste(as.character(f_obj), collapse = " ")
    if (grepl("\\b(pb|ps|pbm|pbz|cs|cc|tp|te)\\(", f_str, perl = TRUE)) {
      spline_found <- TRUE
      if (i == 1L) poly_degs[i] <- 0L
      next
    }
    
    matches <- regmatches(f_str, gregexpr("(?:stats::)?poly\\([^,]+,\\s*(\\d+)\\)", f_str, perl = TRUE))[[1]]
    if (length(matches) > 0L) {
      poly_degs[i] <- as.integer(gsub("(?:stats::)?poly\\([^,]+,\\s*(\\d+)\\)", "\\1", matches[1], perl = TRUE))
    }
  }
  list(spline = spline_found, poly_degrees = poly_degs)
}

#' Refit a bootstrap replicate by forward-backward model selection
#'
#' @param reselect Ignored; kept for backward compatibility (reselection is the
#'   only thing this function does; use `.fit_boot_replicate()` to refit with
#'   fixed polynomial degrees).
#' @return A selected gamlss model, or `NULL` if it did not converge.
#' @noRd
fit_boot_model <- function(
    model,
    data_boot,
    age_name,
    score_name,
    reselect = FALSE,
    poly_degrees = NULL,
    spline = NULL,
    fb_control = NULL
) {
  if (is.null(poly_degrees) || is.null(spline)) {
    meta <- extract_poly_degrees(model)
    poly_degrees <- meta$poly_degrees
    spline <- meta$spline
  }
  
  fb_args <- list(
    data = data_boot,
    age_name = age_name,
    score_name = score_name,
    family = model$family[1],
    spline = spline,
    start_poly = poly_degrees,
    selcrit = model$call$selcrit,   # NULL (-> fb_select default) for models not fitted by fb_select()
    trace = FALSE
  )
  fb_args <- fb_args[!vapply(fb_args, is.null, logical(1))]
  
  if (is.null(fb_control)) fb_control <- list()
  protected_args <- c("data", "age_name", "score_name", "family", "trace", "spline")
  fb_control[protected_args] <- NULL
  fb_args <- utils::modifyList(fb_args, fb_control)
  
  m_boot <- do.call(fb_select, fb_args)
  if (is.null(m_boot) || !isTRUE(m_boot$converged)) return(NULL)
  m_boot
}

#' Estimate model centiles across age
#' @noRd
estimate_centiles <- function(object, ages, centiles = c(5, 10, 25, 50, 75, 90, 95),
                              score_min, score_max, n_grid = 200) {
  
  probs <- centiles / 100
  score_grid <- seq(score_min, score_max, length.out = n_grid)
  scores_mat <- matrix(rep(score_grid, each = nrow(ages)), nrow = nrow(ages))
  
  # estimateCDF takes object, ages, scores, score_max
  cdf_mat <- estimateCDF(
    object = object,
    ages = ages,
    scores = scores_mat
  )
  
  result_list <- lapply(seq_along(probs), function(j) {
    p_target <- probs[j]
    cent_vals <- apply(cdf_mat, 1, function(cdf_row) {
      if (all(is.na(cdf_row))) return(NA_real_)
      cdf_row <- cummax(cdf_row)
      keep <- !duplicated(cdf_row)
      if (sum(keep) < 2) return(NA_real_)
      stats::approx(x = cdf_row[keep], y = score_grid[keep], xout = p_target, rule = 2)$y
    })
    data.frame(age = ages[[1]], score = cent_vals, centile = centiles[j])
  })
  
  do.call(rbind, result_list)
}

#' Helper to simulate random response scores from a fitted gamlss model
#' @noRd
simulate_gamlss_scores <- function(model, newdata, u = NULL, score_bounds = NULL, fallback_data = NULL) {
  fname <- model$family[1]
  lpar  <- length(model$parameters)
  is_bi <- .is_bi_family(fname)
  train_data <- get_model_data(model, fallback_data = fallback_data)
  
  bd_val <- if (is_bi) {
    if (!is.null(score_bounds)) score_bounds[2] else max(model$data[[all.vars(model$mu.formula)[1]]])
  }
  pred_distr <- .predict_distr(model, train_data, newdata, bd = bd_val)
  n_draws    <- nrow(newdata)
  
  call_dist <- function(fun, first) {
    do.call(fun, .dist_args(pred_distr, lpar, first = first, bd = score_bounds[2], is_bi = is_bi))
  }
  
  scores <- if (!is.null(u)) {
    # Inverse probability integral transform via quantile function
    call_dist(find_fun(paste0("q", fname)), list(p = u))
  } else {
    # Direct random generation via r-function, else inverse transform of uniforms
    rfun <- tryCatch(find_fun(paste0("r", fname)), error = function(e) NULL)
    if (!is.null(rfun)) {
      call_dist(rfun, list(n = n_draws))
    } else {
      call_dist(find_fun(paste0("q", fname)), list(p = stats::runif(n_draws)))
    }
  }
  
  # Respect optional score boundaries (e.g. truncated continuous models)
  if (!is.null(score_bounds)) {
    scores <- pmax(score_bounds[1], pmin(score_bounds[2], scores))
  }
  
  # Respect score shaping in binomial family scores
  if (is_bi) {
    scores <- cbind(scores, score_bounds[2] - scores)
  }
  unname(scores)
}

#' Helper to generate simulated age vectors from a sampling frame
#' @noRd
generate_age_frame <- function(age_frame, n, orig_ages) {
  if (is.null(age_frame)) {
    sample(orig_ages, size = n, replace = TRUE)
  } else if (is.function(age_frame)) {
    age_frame(n)
  } else if (is.numeric(age_frame)) {
    sample(age_frame, size = n, replace = TRUE)
  } else {
    stop("'age_sampling_frame' must be NULL, a numeric vector, or a function of n.")
  }
}


# ============================================================================
# Reference SE Formulas (Traditional Norming Benchmarks)
# ============================================================================

#' Single-test reference standard error for Z-scores
#' @noRd
get_single_se_z <- function(z_pop, n) {
  sqrt((1 + z_pop^2 / 2) / n)
}

#' Composite reference standard error for weighted sums
#' @noRd
get_composite_se_z <- function(z_pop, n, weights, P) {
  pairs   <- t(utils::combn(nrow(P), 2))
  n_pairs <- nrow(pairs)
  V_pop   <- as.numeric(t(weights) %*% P %*% weights)
  
  # Covariance term of the correlations of test pairs (j, k) and (h, m)
  psi <- function(j, k, h, m) {
    t1 <- (P[j, h] - P[j, k] * P[k, h]) * (P[k, m] - P[k, h] * P[h, m])
    t2 <- (P[j, m] - P[j, h] * P[h, m]) * (P[k, h] - P[k, j] * P[j, h])
    t3 <- (P[j, h] - P[j, m] * P[m, h]) * (P[k, m] - P[k, j] * P[j, m])
    t4 <- (P[j, m] - P[j, k] * P[k, m]) * (P[k, h] - P[k, m] * P[m, h])
    0.5 * (t1 + t2 + t3 + t4)
  }
  
  weighted_psi_sum <- 0
  for (u in seq_len(n_pairs)) {
    for (v in seq_len(n_pairs)) {
      j <- pairs[u, 1]; k <- pairs[u, 2]
      h <- pairs[v, 1]; m <- pairs[v, 2]
      weighted_psi_sum <- weighted_psi_sum +
        (weights[j] * weights[k] * weights[h] * weights[m]) * psi(j, k, h, m)
    }
  }
  (abs(z_pop) / V_pop) * sqrt(weighted_psi_sum / n)
}

#' Compute reference standard errors
#' @noRd
compute_reference_se <- function(z, n, type = c("single", "composite"), weights = NULL, P = NULL) {
  type <- match.arg(type)
  if (type == "single") return(get_single_se_z(z, n))
  if (is.null(weights) || is.null(P)) stop("'weights' and 'P' are required for composite reference SE.")
  get_composite_se_z(z, n, weights, P)
}


# ============================================================================
# Shared bootstrap-engine helpers (used by boot_subtest() and boot_composite())
# ============================================================================

#' Flatten a bootstrap result container to a (cells x replicates) matrix
#'
#' @param boot_obj 3D array (ages x scores x replicates) or 2D matrix
#'   (observations x replicates).
#' @noRd
.flatten_boot <- function(boot_obj) {
  d <- dim(boot_obj)
  if (length(d) == 3) matrix(boot_obj, nrow = d[1] * d[2], ncol = d[3]) else boot_obj
}

#' Set up the foreach backend (sequential or parallel)
#'
#' @param family_envs list of environments whose objects are exported to workers.
#' @return list with the infix operator `op` and a `cleanup()` function; call
#'   `on.exit(backend$cleanup(), add = TRUE)` in the caller.
#' @noRd
.boot_backend <- function(parallel, ncores, family_envs = list()) {
  op      <- foreach::`%do%`
  cleanup <- function() invisible(NULL)
  
  if (isTRUE(parallel)) {
    if (!requireNamespace("doParallel", quietly = TRUE)) {
      warning("Package 'doParallel' is not installed; running sequentially.",
              call. = FALSE)
    } else {
      ncores <- if (is.null(ncores)) max(1L, parallel::detectCores() - 1L) else ncores
      if (identical(Sys.getenv("_R_CHECK_LIMIT_CORES_"), "TRUE")) {
        ncores <- min(ncores, 2L)
      }
      
      cl <- parallel::makeCluster(ncores)
      doParallel::registerDoParallel(cl)
      for (env in family_envs) {
        parallel::clusterExport(cl, varlist = ls(env, all.names = TRUE), envir = env)
      }
      
      op      <- foreach::`%dopar%`
      cleanup <- function() {
        try(parallel::stopCluster(cl), silent = TRUE)
        foreach::registerDoSEQ()
        invisible(NULL)
      }
    }
  }
  
  list(op = op, cleanup = cleanup)
}

#' Fit one bootstrap replicate of a GAMLSS model
#'
#' Without reselection the original model call is re-evaluated on the bootstrap
#' data; if `boot_idx` is supplied, fitted parameter values of the resampled
#' rows are used as starting values. With reselection, `fb_select()` is run via
#' `fit_boot_model()`. Benign GAMLSS warnings are muffled.
#'
#' @noRd
.fit_boot_replicate <- function(model,
                                data_boot,
                                boot_idx = NULL,
                                age_name,
                                score_name,
                                reselect = FALSE,
                                poly_degrees = NULL,
                                spline = NULL,
                                fb_control = NULL) {
  call_env <- parent.frame()
  
  .muffle_benign_warnings({
    if (isFALSE(reselect)) {
      thecall <- as.call(model$call)
      thecall$control <- gamlss::gamlss.control(trace = FALSE)
      thecall$data <- data_boot
      if (!is.null(boot_idx)) {
        for (p in model$parameters) {
          thecall[[paste0(p, ".start")]] <- fitted(model, what = p)[boot_idx]
        }
      }
      eval(thecall, envir = call_env)
    } else {
      fit_boot_model(
        model = model, data_boot = data_boot, age_name = age_name, score_name = score_name,
        reselect = reselect, poly_degrees = poly_degrees, spline = spline,
        fb_control = fb_control
      )
    }
  })
}

#' Split foreach output into successful replicates
#' @return list(success, trueB)
#' @noRd
.collect_boot_results <- function(results) {
  is_err  <- vapply(results, function(x) is.character(x) || inherits(x, "error"), logical(1))
  success <- results[!is_err]
  if (length(success) == 0) stop("All bootstrap replicates failed.")
  list(success = success, trueB = length(success))
}

#' Stack replicate norm tables (3D array) and sample norm scores (matrix)
#' @return list(table_array, sample_matrix)
#' @noRd
.stack_boot_replicates <- function(results_success) {
  table_matrices <- lapply(results_success, function(x) as.matrix(x$table[, -1, drop = FALSE]))
  table_array <- array(
    unlist(table_matrices, use.names = FALSE),
    dim = c(dim(table_matrices[[1]]), length(table_matrices)),
    dimnames = list(rownames(table_matrices[[1]]), colnames(table_matrices[[1]]), NULL)
  )
  sample_matrix <- do.call(cbind, lapply(results_success, function(x) as.numeric(x$sample[, 2])))
  list(table_array = table_array, sample_matrix = sample_matrix)
}

# ============================================================================
# Norm-scale and plotting helpers
# ============================================================================

#' Mean and SD of a norm score type (single source of truth for all norm scales)
#' @noRd
.normtype_params <- function(normtype) {
  switch(
    normtype,
    "Z"  = c(mean = 0,   sd = 1),
    "T"  = c(mean = 50,  sd = 10),
    "IQ" = c(mean = 100, sd = 15),
    "SS" = c(mean = 10,  sd = 3),
    stop("Unsupported normtype: ", normtype)
  )
}

#' Convert norm scores of a given type to Z scores
#' @noRd
.norm_to_z <- function(x, normtype) {
  p <- .normtype_params(normtype)
  (x - p[["mean"]]) / p[["sd"]]
}


#' Resolve a column name in a bootstrap table
#'
#' Uses `preferred` (or `default`), falling back to the first available
#' candidate. Stops with `error_msg` if nothing is found and `error_msg` is given.
#' @noRd
.resolve_col <- function(df, preferred, default, candidates, error_msg = NULL) {
  nm <- if (!is.null(preferred)) preferred else default
  if (!nm %in% names(df)) {
    found <- intersect(candidates, names(df))
    if (length(found) > 0) {
      nm <- found[1]
    } else if (!is.null(error_msg)) {
      stop(error_msg, call. = FALSE)
    }
  }
  nm
}

#' Plot subtitle for bootstrap SE plots
#' @noRd
.boot_subtitle <- function(type, trueB, normtype, benchmark_n = NULL) {
  base <- paste0("Bootstrap SE (B = ", trueB, ", ", normtype, ")")
  switch(
    type,
    "ratio"      = paste0(base, " / Reference SE (n = ", benchmark_n, ")"),
    "difference" = paste0(base, " - Reference SE (n = ", benchmark_n, ")"),
    "se"         = base
  )
}

#' Print percentage of cells where bootstrap SE is below the reference SE
#' @return `FALSE` if there are no valid cells, otherwise `TRUE` (invisibly).
#' @noRd
.print_pct_below <- function(df, type, benchmark_n) {
  if (type == "difference") {
    below <- (df$boot_se - df$reference_se) < 0
    cutoff_label <- "difference < 0"
  } else {
    ratio <- df$boot_se / df$reference_se
    below <- ratio[is.finite(ratio)] < 1
    cutoff_label <- "ratio < 1"
  }
  
  total_cells <- length(below)
  if (total_cells == 0) {
    message("No valid norm table cells in the specified range.")
    return(FALSE)
  }
  
  cat(sprintf(
    "Contour threshold: %s (n = %d)\nPercentage of age-score range below contour: %.2f%% (%d / %d cells)\n",
    cutoff_label, benchmark_n, mean(below) * 100, sum(below), total_cells
  ))
  invisible(TRUE)
}
