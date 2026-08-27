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
                       env) {
  # 1. Helper to create formula expressions
  get_form_expr <- function(deg, is_mu = FALSE) {
    if (is_mu && isTRUE(spline)) return(substitute(pb(A), list(A = as.name(age_name))))
    if (deg == 0) return(1)
    substitute(stats::poly(A, D), list(A = as.name(age_name), D = deg))
  }
  
  # Construct the formulas as literal objects
  mu_f    <- stats::as.formula(substitute(S ~ FORM, list(S = as.name(score_name), FORM = get_form_expr(poly_deg[1], TRUE))))
  sig_f   <- if(length(poly_deg) >= 2) stats::as.formula(substitute(~ FORM, list(FORM = get_form_expr(poly_deg[2])))) else NULL
  nu_f    <- if(length(poly_deg) >= 3) stats::as.formula(substitute(~ FORM, list(FORM = get_form_expr(poly_deg[3])))) else NULL
  tau_f   <- if(length(poly_deg) >= 4) stats::as.formula(substitute(~ FORM, list(FORM = get_form_expr(poly_deg[4])))) else NULL
  
  # 2. Build the call by injecting the FORMULA OBJECTS directly
  if (identical(selcrit, "CV")) {
    call_expr <- substitute(
      gamlss::gamlssCV(MU, sigma.formula = SIG, nu.formula = NU, tau.formula = TAU,
                       family = FAM, data = DAT, method = MET, rand = folds, trace = FALSE),
      list(MU = mu_f, SIG = sig_f, NU = nu_f, TAU = tau_f,
           FAM = family_obj, DAT = as.name(data_name), MET = parse(text = method)[[1]])
    )
  } else {
    call_expr <- substitute(
      gamlss::gamlss(MU, sigma.formula = SIG, nu.formula = NU, tau.formula = TAU,
                     family = FAM, data = DAT, method = MET, trace = FALSE),
      list(MU = mu_f, SIG = sig_f, NU = nu_f, TAU = tau_f,
           FAM = family_obj, DAT = as.name(data_name), MET = parse(text = method)[[1]])
    )
  }
  
  # 3. Remove the NULL arguments (for families with < 4 parameters)
  call_list <- as.list(call_expr)
  call_list <- call_list[!vapply(call_list, is.null, logical(1))]
  call_expr <- as.call(call_list)
  
  # 4. Evaluate
  # Now it works because 'MU', 'SIG', etc. are replaced by the actual formula objects
  eval(call_expr, envir = env)
}



#' Obtain candidate models (vectors of polynomial degrees) 
#' for forward–backward selection
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

#' Compute selection criterion for a given model
#'
#' @inheritParams fb_select
#' @param poly_deg vector. polynomial degrees for all parameters.
#' @param env environment. evaluation environment.
#'
#' @return numeric value of the selected criterion for the fitted model,
#'   or \code{Inf} if the model fails or produces excluded warnings.
#'
#' @noRd
get_selcrit <- function(poly_deg,
                        selcrit,
                        family_obj,
                        spline,
                        method,
                        data_name,
                        age_name,
                        score_name,
                        env) {
  res <- tryCatch({
    withCallingHandlers({
      # Silence gamlss console output
      utils::capture.output({
        mod <- fit_gamlss(
          poly_deg   = poly_deg,
          selcrit    = selcrit,
          family_obj = family_obj,
          spline     = spline,
          method     = method,
          data_name  = data_name,
          age_name   = age_name,
          score_name = score_name,
          env        = env
        )
      })

      # Compute selection criterion
      switch(selcrit,
             "AIC"     = stats::AIC(mod),
             "BIC"     = stats::BIC(mod),
             "GAIC(3)" = gamlss::GAIC(mod, k = 3),
             "CV"      = gamlss::CV(mod),
             stop("Unknown selection criterion: ", selcrit)
      )
    },
    warning = function(w) {
      if (grepl("discrepancy", conditionMessage(w), fixed = TRUE) | grepl("dBB", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning") # ignore harmless warnings
      } else {
        message(
          "Model [", paste0(poly_deg, collapse = ","),
          "] excluded due to warning:\n<", conditionMessage(w), ">"
        )
        invokeRestart("muffleWarning")
        return(Inf)
      }
    })
  },
  error = function(e) {
    message(
      "Model [", paste0(poly_deg, collapse = ","),
      "] excluded due to error:\n<", conditionMessage(e), ">"
    )
    return(Inf)
  })

  # Ensure numeric return
  if (!is.numeric(res)) res <- Inf
  return(res)
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
#' age–score combination.
#'
#' @noRd
estimateCDF <- function(object,
                        ages,
                        scores,
                        score_max,
                        lower.tail = TRUE) {
  fname <- object$family[1]
  pfun  <- paste0("p", fname)
  lpar  <- length(object$parameters)
  
  # Align scores with ages
  if (nrow(ages) == nrow(scores)) {
    scores_c <- scores
  } else if (nrow(scores) == 1) {
    scores_c <- matrix(rep(scores, each = nrow(ages)), nrow = nrow(ages))
  } else if (ncol(scores) == 1) {
    scores_c <- matrix(rep(t(scores), each = nrow(ages)), nrow = nrow(ages))
  } else {
    stop("Dimension mismatch: 'scores' must have 1 row, 1 column, or match 'ages'.")
  }
  
  # Predict distributional parameters
  pred_distr <- suppressWarnings({
    if (fname %in% gamlss::.gamlss.bi.list) {
      gamlss::predictAll(object, data = object$data, newdata = ages, type = "response", bd = score_max)
    } else {
      gamlss::predictAll(object, data = object$data, newdata = ages, type = "response")
    }
  })
  
  # Empty results matrix
  CDF_matrix <- matrix(NA, nrow = nrow(scores_c), ncol = ncol(scores_c))
  
  # Helper: build call for p-distribution
  build_call <- function(i, q) {
    args <- list(q = q, mu = pred_distr$mu[i])
    if (lpar >= 2) args$sigma <- pred_distr$sigma[i]
    if (lpar >= 3) args$nu    <- pred_distr$nu[i]
    if (lpar == 4) args$tau   <- pred_distr$tau[i]
    if (fname %in% gamlss::.gamlss.bi.list) args$bd <- score_max
    
    pfun_fun <- find_fun(pfun)
    do.call(pfun_fun, args)
  }
  
  # Compute CDF values
  for (i in seq_len(nrow(scores_c))) {
    CDF_matrix[i, ] <- build_call(i, scores_c[i, ])
  }
  
  return(CDF_matrix)
}

#' Linearly transform Z-scores to a scaled score
#'
#' @description
#' transforms Z-scores into normed scores of type \code{"Z"}, \code{"T"}, or
#' \code{"IQ"} using a linear transformation.
#'
#' @param Zscores numeric vector of Z-scores.
#' @param normtype character. type of normed score:
#'   * \code{"Z"}  (mean = 0,   SD = 1, default)  
#'   * \code{"T"}  (mean = 50,  SD = 10)  
#'   * \code{"IQ"} (mean = 100, SD = 15)  
#'
#' @return numeric vector of scaled scores.
#'
#' @noRd
ZtoScale <- function(Zscores, normtype = "Z") {
  scaled_scores <- switch(normtype,
                          "Z"  = Zscores,
                          "T"  = Zscores * 10 + 50,
                          "IQ" = Zscores * 15 + 100,
                          stop("Unsupported normtype: ", normtype)
  )
  return(scaled_scores)
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
#' * \code{mean_dis} — estimated means by age  
#' * \code{sd_dis}   — estimated standard deviations by age  
#'
#' @noRd
estimate_musigma <- function(object,
                             normingdata,
                             score_max = NULL) {
  fname   <- object$family[1]
  family  <- gamlss.dist::as.gamlss.family(fname)
  rfun    <- paste0("r", fname)
  nperson <- nrow(normingdata)
  lpar    <- length(object$parameters)
  
  # Predict distribution parameters
  pred <- suppressWarnings({
    if (fname %in% gamlss::.gamlss.bi.list) {
      gamlss::predictAll(object, data = object$data, newdata = normingdata, type = "response", bd = score_max)
    } else {
      gamlss::predictAll(object,data = object$data, newdata = normingdata, type = "response")
    }
  })
  
  # --- Case 1: family provides mean() and variance() ---
  if (!(is.null(family$mean) && is.null(family$variance))) {
    args <- list(mu = pred$mu)
    if (lpar >= 2) args$sigma <- pred$sigma
    if (lpar >= 3) args$nu    <- pred$nu
    if (lpar == 4) args$tau   <- pred$tau
    if (fname %in% gamlss::.gamlss.bi.list) args$bd <- score_max
    
    mean_dis <- do.call(family$mean, args)
    sd_dis   <- sqrt(do.call(family$variance, args))
    
    # --- Case 2: fall back on simulation ---
  } else {
    mean_dis <- numeric(nperson)
    sd_dis   <- numeric(nperson)
    
    for (i in seq_len(nperson)) {
      args <- list(n = 10000, mu = pred$mu[i])
      if (lpar >= 2) args$sigma <- pred$sigma[i]
      if (lpar >= 3) args$nu    <- pred$nu[i]
      if (lpar == 4) args$tau   <- pred$tau[i]
      if (fname %in% gamlss::.gamlss.bi.list) args$bd <- score_max
      
      sample_i <- do.call(find_fun(rfun), args)
      mean_dis[i] <- mean(sample_i)
      sd_dis[i]   <- stats::sd(sample_i)
    }
  }
  
  return(list(mean_dis = mean_dis, sd_dis = sd_dis))
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
#'   * \code{age} — age values  
#'   * \code{rel} — estimated reliability at those ages  
#' @param ci_level numeric. confidence level (default: 0.95).
#'
#' @return a list with some or all of the following elements:
#' * \code{lower_sample}, \code{upper_sample} — lower/upper CI bounds for sample scores  
#' * \code{lower_matrix}, \code{upper_matrix} — lower/upper CI bounds for norm table scores, conditional on age  
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
    pop_age_vec <- unlist(pop_age)
    
    r <- stats::approx(x = datarel$age, y = datarel$rel, xout = pop_age_vec)$y
    musigma <- estimate_musigma(object, normingdata = pop_age, score_max = score_max)
    
    mean_dis <- musigma$mean_dis
    sd_dis   <- musigma$sd_dis
    
    # Expand mean_dis and r to match scores_c dimensions
    mean_mat <- matrix(mean_dis, nrow = nrow(scores_c), ncol = ncol(scores_c))
    r_mat    <- matrix(r,        nrow = nrow(scores_c), ncol = ncol(scores_c))
    sd_mat   <- matrix(sd_dis,   nrow = nrow(scores_c), ncol = ncol(scores_c))
    
    # Compute true score and SEE for all cells at once
    truescore <- mean_mat + r_mat * (scores_c - mean_mat)
    SEE <- sd_mat * sqrt(r_mat * (1 - r_mat))
    
    # Lower/upper bounds
    lower <- truescore + stats::qnorm((1 - ci_level) / 2) * SEE
    upper <- truescore + stats::qnorm(1 - (1 - ci_level) / 2) * SEE
    
    # Truncate bounds
    lower[lower < score_min] <- score_min
    upper[upper > score_max] <- score_max
    
    out$lower_matrix <- lower
    out$upper_matrix <- upper
  }
  
  # --- Case 2: Sample-based CIs (normingdata) ---
  if (!is.null(normingdata)) {
    r <- stats::approx(datarel$age, datarel$rel, xout = normingdata[[age_name]])$y
    musigma <- estimate_musigma(object, normingdata = normingdata, score_max = score_max)
    
    mean_dis <- musigma$mean_dis
    sd_dis   <- musigma$sd_dis
    
    truescore <- mean_dis + r * (normingdata[[score_name]] - mean_dis)
    SEE <- sd_dis * sqrt(r * (1 - r))
    
    lower <- truescore + stats::qnorm((1 - ci_level) / 2) * SEE
    upper <- truescore + stats::qnorm(1 - (1 - ci_level) / 2) * SEE
    
    lower[lower < score_min] <- score_min
    upper[upper > score_max] <- score_max
    
    out$lower_sample <- lower
    out$upper_sample <- upper
  }
  
  return(out)
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
  display_deg[1] <- ifelse(spline, "S", poly_deg[1])
  
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
  
  # 1. Check user environment (custom functions)
  if (exists(fname, envir = envir, mode = "function", inherits = FALSE)) {
    return(get(fname, envir = envir, mode = "function"))
  }
  
  # 2. Check gamlss.dist exported functions
  if (fname %in% getNamespaceExports("gamlss.dist")) {
    return(getExportedValue("gamlss.dist", fname))
  }
  
  # 3. Nothing found
  stop(
    sprintf(
      "Function '%s' not found in the supplied environment or gamlss.dist.",
      fname
    ),
    call. = FALSE
  )
}

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
#' @param fname Character string giving the name of the distribution family.
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
capture_family_env <- function(fname, lookup_envir = .GlobalEnv) {
  
  out <- new.env(parent = emptyenv())
  
  objs <- c(
    fname,
    paste0(c("d", "p", "q", "r"), fname)
  )
  
  for (nm in objs) {
    if (exists(nm, envir = lookup_envir, mode = "function", inherits = FALSE)) {
      
      assign(
        nm,
        get(nm, envir = lookup_envir, mode = "function"),
        envir = out
      )
      
    } else if (nm %in% getNamespaceExports("gamlss.dist")) {
      
      assign(
        nm,
        getExportedValue("gamlss.dist", nm),
        envir = out
      )
      
    }
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