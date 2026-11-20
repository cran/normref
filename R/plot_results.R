#' Plot centiles of a fitted GAMLSS model (binomial-type)
#'
#' @description
#' `centiles_bin()` plots centile curves and the sample data for
#' binomial-type distributions (see [gamlss::.gamlss.bi.list]) based on a
#' fitted \code{GAMLSS} object.
#'
#' @param model a GAMLSS fitted model, for example the result of [fb_select()].
#' @inheritParams gamlss::centiles
#'
#' @return No return value, only graphical output.
#'
#' @seealso [fb_select()]
#'
#' @examples
#' \donttest{
#' data("ids_data")
#'
#' mydata_BB_y14 <- shape_data(
#'   data       = ids_data,
#'   age_name   = "age",
#'   score_name = "y14",
#'   family     = "BB"
#' )
#'
#' mod_BB_y14 <- fb_select(
#'   data       = mydata_BB_y14,
#'   age_name   = "age",
#'   score_name = "shaped_score",
#'   family     = "BB",
#'   selcrit    = "BIC"
#' )
#'
#' centiles_bin(mod_BB_y14, xvar = age)
#' }
#'
#' @importFrom graphics lines
#' @importFrom stats fitted
#' 
#' @export

centiles_bin <- function(model,
                             xvar,
                             cent = c(0.4, 2, 10, 25, 50, 75, 90, 98, 99.6),
                             legend = TRUE,
                             ylab = "y",
                             xlab = "x",
                             main = NULL,
                             main.gsub = "@",
                             xleg = min(xvar),
                             yleg = max(model$y),
                             xlim = range(xvar),
                             ylim = range(model$y),
                             save = FALSE,
                             plot = TRUE,
                             points = TRUE,
                             pch = 15,
                             cex = 0.5,
                             col = "grey",
                             col.centiles = seq_along(cent) + 2,
                             lty.centiles = 1,
                             lwd.centiles = 1,
                             colors = "rainbow",
                             ...) {
  if (missing(xvar)) {
    xvar <- all.vars(model$call$formula)[[2]]
    if (any(grepl("data", names(model$call)))) {
      DaTa <- eval(model$call[["data"]])
      xvar <- get(xvar, envir = as.environment(DaTa))
    }
  }
  
  xvarO <- deparse(substitute(xvar))
  xvar <- try(xvar, silent = TRUE)
  
  if (any(class(xvar) %in% "try-error")) {
    DaTa <- eval(model$call[["data"]])
    xvar <- get(xvarO, envir = as.environment(DaTa))
  }
  
  fname <- model$family[1]
  if (!(fname %in% gamlss::.gamlss.bi.list)) {
    stop("Your gamlss object was not fitted with a binomial-type distribution. 
          Use the centiles function from gamlss.")
  }
  
  qfun <- paste0("q", fname)
  Title <- paste("Centile curves using", fname)
  main <- if (is.null(main)) Title else gsub(main.gsub, Title, main)
  
  oxvar <- xvar[order(xvar)]
  oyvar <- model$y[order(xvar)]
  
  if (is.matrix(model$y)) {
    oyvar <- model$y[, 1][order(xvar)]
    ylim <- range(model$y[, 1])
    yleg <- max(model$y[, 1])
  }
  
  if (plot) {
    lty.centiles <- rep(lty.centiles, length(cent))
    lwd.centiles <- rep(lwd.centiles, length(cent))
    col.centiles <- rep(col.centiles, length(cent))
    
    plot(
      oxvar, oyvar,
      type = if (points) "p" else "n",
      col = col, pch = pch, cex = cex,
      xlab = xlab, ylab = ylab,
      xlim = xlim, ylim = ylim,
      ...
    )
    graphics::title(main)
  }
  
  lpar <- length(model$parameters)
  per <- numeric(length(cent))
  
  for (i in seq_along(cent)) {
    var <- cent[i]
    fitted_mu <- fitted(model, "mu")[order(xvar)]
    fitted_sigma <- if (lpar >= 2) fitted(model, "sigma")[order(xvar)] else NULL
    fitted_nu <- if (lpar >= 3) fitted(model, "nu")[order(xvar)] else NULL
    fitted_tau <- if (lpar >= 4) fitted(model, "tau")[order(xvar)] else NULL
    bd <- model[["bd"]][order(xvar)]
    
    args <- list(var / 100, mu = fitted_mu, bd = bd)
    if (!is.null(fitted_sigma)) args$sigma <- fitted_sigma
    if (!is.null(fitted_nu)) args$nu <- fitted_nu
    if (!is.null(fitted_tau)) args$tau <- fitted_tau
    
    ll <- do.call(find_fun(qfun), args)
    
    if (plot) {
      lines(
        oxvar, ll,
        col = col.centiles[i],
        lty = lty.centiles[i],
        lwd = lwd.centiles[i],
        ...
      )
    }
    
    per[i] <- (1 - sum(oyvar > ll) / length(oyvar)) * 100
    if (!save) {
      message("% of cases below", var, "centile is ", round(per[i],2), "\n")
    }
  }
  
  if (plot && legend) {
    legend(
      list(x = xleg, y = yleg), legend = cent,
      col = col.centiles, lty = lty.centiles, lwd = lwd.centiles,
      ncol = 1, ...
    )
  }
  
  if (save) return(cbind(cent, per))
}

#' Plot norm curves from a NormTable object
#'
#' @description
#' `plot_normtable()` plots norm curves as a function of the predictor,
#' along with the sample data, based on a \code{NormTable} object.
#'
#' @param normtable a \code{NormTable} object (created by \code{normtable_create()} 
#'   with \code{new_data = FALSE}).
#' @param lty line type(s) for curves.
#' @param lwd line width(s) for curves.
#' @param pch symbol for sample points.
#' @param cex point size (default: \code{0.5}).
#' @param col point colour (default: \code{"aquamarine4"}).
#' @param xlab x-axis label (default: \code{"Age"}).
#' @param ylab y-axis label (default: \code{"Percentile"}).
#' @param ... additional graphical parameters passed to 
#'   [graphics::plot()], [graphics::lines()], or [graphics::points()].
#'
#' @return graphical output and the \code{ggplot} object used to create it.
#' 
#' @seealso [normtable_create()]
#' 
#' @examples
#' \donttest{
#' data("ids_data")
#'
#' mydata_BB_y14 <- shape_data(
#'   data       = ids_data,
#'   age_name   = "age",
#'   score_name = "y14",
#'   family     = "BB"
#' )
#'
#' mod_BB_y14 <- fb_select(
#'   data       = mydata_BB_y14,
#'   age_name   = "age",
#'   score_name = "shaped_score",
#'   family     = "BB",
#'   selcrit    = "BIC"
#' )
#'
#' norm_mod_BB_y14 <- normtable_create(
#'   model      = mod_BB_y14,
#'   data       = mydata_BB_y14,
#'   age_name   = "age",
#'   score_name = "shaped_score"
#' )
#'
#' # default plot
#' plot_normtable(norm_mod_BB_y14)
#' }
#' 
#' @export
plot_normtable <- function(normtable,
                           lty = 1,
                           lwd = 3,
                           pch = 1,
                           cex = 0.5,
                           col = "aquamarine4",
                           xlab = "Age",
                           ylab = "Percentile",
                           ...) {
  cdf_matrix <- normtable$cdf_matrix
  cdf_sample <- normtable$cdf_sample
  pop_age    <- normtable$pop_age
  mydata     <- normtable$data
  age_name   <- normtable$age_name
  
  if (all(is.na(pop_age))) {
    stop("Error: normtable must be created with normtable_create(newdata = NULL).")
  }
  
  # Plot first curve
  graphics::plot(
    x    = pop_age[[age_name]],
    y    = 100 * cdf_matrix[, 1],
    type = "l",
    lty  = lty,
    lwd  = lwd,
    pch  = pch,
    cex  = cex,
    col  = col,
    xlab = xlab,
    ylab = ylab,
    ylim = c(0, 100),
    ...
  )
  
  # Add remaining curves
  for (i in 2:ncol(cdf_matrix)) {
    graphics::lines(
      x = pop_age[[age_name]],
      y = 100 * cdf_matrix[, i],
      ...
    )
  }
  
  # Add sample observations
  graphics::points(
    x   = mydata[[age_name]],
    y   = 100 * cdf_sample,
    col = col,
    pch = pch,
    ...
  )
}

#' Plot reliability estimates over age
#'
#' @description
#' `plot_drel()` plots reliability estimates as a function of age,
#' based on different window widths, using a \code{Drel} object.
#'
#' @param drel a \code{Drel} object (created with [different_rel()]).
#' @param ncol number of plots per row (default: \code{3}).
#' @param nrow number of plots per column (default: \code{2}).
#' @param ... additional arguments passed to plotting functions.
#'
#' @return graphical output and the \code{ggplot} object used to create it.
#'
#' @seealso [different_rel()]
#'
#' @examples
#' \donttest{
#' data("ids_kn_data")
#'
#' rel_int <- different_rel(
#'   data           = ids_kn_data,
#'   item_variables = colnames(ids_kn_data),
#'   age_name       = "age_years",
#'   step_window    = c(0.5, 1, 2, 5, 10, 20),
#'   min_agegroup   = 5,
#'   max_agegroup   = 20,
#'   step_agegroup  = c(0.5, 1, 1.5, 2)
#' )
#'
#' plot_drel(rel_int, ncol = 2)
#' }
#'
#' @importFrom rlang .data
#' @export
plot_drel <- function(drel,
                      ncol = 3,
                      nrow = 2,
                      ...) {
  # Build a base data.frame
  df <- data.frame(
    rel             = drel$rel,
    age             = drel$age,
    window_width    = drel$window_width,
    version         = drel$version,
    age_group_width = paste0("step size ", drel$age_group_width),
    stringsAsFactors = FALSE
  )
  
  # Create the facet variable
  df$combi <- "evaluate each observed individual"
  df$combi[df$version == "step"] <- df$age_group_width[df$version == "step"]
  
  # Build ggplot with .data pronoun to satisfy R CMD check
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$age, y = .data$rel)) +
    ggplot2::geom_point(
      ggplot2::aes(shape = .data$window_width, color = .data$window_width)
    ) +
    ggplot2::geom_line(
      ggplot2::aes(color = .data$window_width, linetype = .data$window_width)
    ) +
    ggplot2::facet_wrap(~ combi, ncol = ncol, nrow = nrow) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text            = ggplot2::element_text(size = 13, family = "serif"),
      axis.line       = ggplot2::element_line(color = "black"),
      plot.background = ggplot2::element_blank(),
      panel.grid.minor= ggplot2::element_blank(),
      panel.grid.major= ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggplot2::xlab("Age") +
    ggplot2::ylab("Reliability estimate") +
    ggplot2::labs(
      color    = "Window width",
      shape    = "Window width",
      linetype = "Window width"
    )
  
  return(p)
}


# ============================================================================
# Plotting Functions
# ============================================================================

#' Plot bootstrap standard errors across continuous predictor with reference thresholds
#'
#' @description
#' Visualizes bootstrap-based standard errors (SE) across a continuous predictor
#' (e.g., age, grade) for selected centiles, and compares them to reference
#' standard errors derived under a traditional norming framework.
#'
#' @param boot Object of class \code{"normref_bootstrap"}.
#' @param centiles Numeric vector of centiles (0–100 scale) to evaluate (e.g., \code{c(5, 10, 50)}).
#' @param age_range Optional numeric vector of length 2 specifying the range of the independent variable to include.
#' @param benchmark_n Numeric vector of sample sizes used to compute benchmark standard errors.
#' @param interpolate Logical. If \code{TRUE} (default), a two-step interpolation is performed.
#'
#' @return A \code{ggplot2} object.
#' 
#' @seealso [boot_subtest()], [boot_heatmap()]
#' 
#' @examples
#' \donttest{
#' # 1. Prepare example data from normref
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
#'   parallel = FALSE # TRUE
#' )
#' 
#' # 4. Standard error along centile trajectories
#' boot_centiles(boot_res, centiles = c(5, 50, 95), benchmark_n = c(200, 400))
#' }
#' 
#' @export
boot_centiles <- function(
    boot,
    centiles = c(5, 25, 50, 75, 95),
    age_range = NULL,
    benchmark_n = c(200, 300, 400),
    interpolate = TRUE
) {
  if (!inherits(boot, "normref_bootstrap")) {
    stop("'boot' must be of class 'normref_bootstrap'")
  }
  
  meta <- boot$meta
  df <- boot$normtable
  
  # 1./2. Resolve continuous variable and score column names
  age_name <- .resolve_col(df, meta$age_name, "age", c("age", "covar"))
  score_name <- .resolve_col(
    df, meta$score_name, "testscore", c("testscore", "composite_testscore", "score"),
    error_msg = "Score column could not be identified in bootstrap table."
  )
  df$score <- df[[score_name]]
  
  # 3. Filter independent variable range
  if (!is.null(age_range)) {
    df <- df[df[[age_name]] >= age_range[1] & df[[age_name]] <= age_range[2], , drop = FALSE]
  }
  
  normtype <- if (!is.null(meta$normtype)) meta$normtype else "Z"
  
  # Target norm scale values for requested centiles
  z_vals <- stats::qnorm(centiles / 100)
  target_vals <- ZtoScale(z_vals, normtype)
  
  val_col <- if ("original" %in% names(df)) "original" else "boot_mean"
  
  covar_points <- sort(unique(df[[age_name]]))
  res_list <- list()
  
  for (a in covar_points) {
    sub_df <- df[df[[age_name]] == a & !is.na(df$boot_se) & !is.na(df[[val_col]]) & !is.na(df$score), , drop = FALSE]
    if (nrow(sub_df) < 2) next
    
    sub_df <- sub_df[order(sub_df$score), ]
    sub_agg <- stats::aggregate(
      cbind(val = sub_df[[val_col]], se = sub_df$boot_se) ~ score,
      data = sub_df,
      FUN = mean
    )
    sub_agg <- sub_agg[order(sub_agg$score), ]
    
    for (i in seq_along(centiles)) {
      t_val <- target_vals[i]
      p_val <- centiles[i]
      
      if (interpolate) {
        val_range <- range(sub_agg$val, na.rm = TRUE)
        if (t_val < val_range[1] || t_val > val_range[2]) {
          se_val <- NA_real_
        } else {
          ord_val <- order(sub_agg$val)
          clean_val <- sub_agg$val[ord_val]
          clean_score <- sub_agg$score[ord_val]
          keep_mono <- !duplicated(clean_val)
          
          raw_at_p <- stats::approx(
            x = clean_val[keep_mono],
            y = clean_score[keep_mono],
            xout = t_val,
            ties = "ordered"
          )$y
          
          if (is.na(raw_at_p)) {
            se_val <- NA_real_
          } else {
            se_val <- stats::approx(
              x = sub_agg$score,
              y = sub_agg$se,
              xout = raw_at_p,
              ties = "ordered"
            )$y
          }
        }
      } else {
        closest_idx <- which.min(abs(sub_agg$val - t_val))
        se_val <- sub_agg$se[closest_idx]
      }
      
      out_df <- data.frame(
        x_val = a,
        percentile = p_val,
        boot_se = se_val
      )
      names(out_df)[1] <- age_name
      res_list[[length(res_list) + 1]] <- out_df
    }
  }
  
  results <- do.call(rbind, res_list)
  if (is.null(results) || nrow(results) == 0) {
    stop("No valid standard errors could be computed for the requested centiles.")
  }
  
  scale_mult <- .normtype_params(normtype)[["sd"]]
  ref_grid <- expand.grid(
    percentile = centiles,
    n = benchmark_n
  )
  ref_grid$z <- stats::qnorm(ref_grid$percentile / 100)
  ref_grid$reference_se <- mapply(
    function(z, n) {
      compute_reference_se(
        z = z,
        n = n,
        type = meta$type,
        weights = meta$weights,
        P = meta$P
      ) * scale_mult
    },
    ref_grid$z,
    ref_grid$n
  )
  
  # --- Format Ordinal Labels for Facets ---
  format_ordinal_centile <- function(p) {
    vapply(p, function(x) {
      if (is.na(x)) return(NA_character_)
      mod100 <- x %% 100
      mod10 <- x %% 10
      suffix <- if (mod100 %in% 11:13) {
        "th"
      } else if (mod10 == 1) {
        "st"
      } else if (mod10 == 2) {
        "nd"
      } else if (mod10 == 3) {
        "rd"
      } else {
        "th"
      }
      paste0(x, suffix, " centile")
    }, character(1))
  }
  
  centile_labels <- format_ordinal_centile(centiles)
  results$percentile <- factor(results$percentile, levels = centiles, labels = centile_labels)
  ref_grid$percentile <- factor(ref_grid$percentile, levels = centiles, labels = centile_labels)
  
  # Ensure reference n factor levels match
  ref_grid$n_fac <- factor(ref_grid$n, levels = benchmark_n)
  
  # --- Non-Solid Linetypes for Reference Lines ---
  avail_linetypes <- c("dashed", "dotted", "dotdash", "longdash", "twodash")
  ref_linetypes <- stats::setNames(
    rep(avail_linetypes, length.out = length(benchmark_n)),
    as.character(benchmark_n)
  )
  
  ref_n_text <- paste(benchmark_n, collapse = ", ")
  
  p <- ggplot2::ggplot(results, ggplot2::aes(x = .data[[age_name]], y = .data$boot_se)) +
    ggplot2::geom_hline(
      data = ref_grid,
      ggplot2::aes(yintercept = .data$reference_se, linetype = .data$n_fac),
      color = "grey35",
      linewidth = 1,
      alpha = 1
    ) +
    ggplot2::geom_line(color = "#3B528B", linewidth = 1, na.rm = TRUE) +
    ggplot2::scale_linetype_manual(
      name = "Reference n",
      values = ref_linetypes
    ) +
    ggplot2::theme_bw() +
    ggplot2::ylim(0, NA) +
    ggplot2::labs(
      title = "Bootstrap SE vs Reference SE",
      subtitle = paste0("Bootstrap SE (B = ", boot$trueB, ", ", normtype, ") vs Reference SE (n = ", ref_n_text, ")"),
      x = age_name,
      y = "Standard Error (SE)"
    )
  
  # --- Facet when multiple centiles are requested ---
  if (length(centiles) > 1) {
    p <- p + ggplot2::facet_wrap(~ percentile, scales = "fixed")
  }
  
  p
}

#' Plot heatmap of bootstrap norm table uncertainty
#'
#' @description
#' Visualizes bootstrap-based uncertainty from normtables as a heatmap over
#' the independent variable and score. Regions trimmed or set to NA appear blank.
#'
#' @param boot Object of class \code{"normref_bootstrap"}.
#' @param type Character. Either \code{"se"}, \code{"difference"}, or \code{"ratio"}.
#'   \itemize{
#'     \item \code{"se"}: Displays raw bootstrap standard errors.
#'     \item \code{"difference"}: Displays the difference between the bootstrap SE and
#'       the reference SE (\eqn{SE_{\text{boot}} - SE_{\text{ref}}}).
#'     \item \code{"ratio"}: Displays the relative ratio of the bootstrap SE to the
#'       reference SE (\eqn{SE_{\text{boot}} / SE_{\text{ref}}}).
#'   }
#' @param benchmark_n Numeric. Reference sample size used to compute benchmark standard errors (required when \code{type = "difference"} or \code{type = "ratio"}).
#' @param centiles Optional numeric vector of centiles (0–100 scale) to overlay.
#' @param add_points Logical. If \code{TRUE}, overlay raw sample data points.
#' @param point_style Character. Style for points: \code{"hollow"} (default) or \code{"solid"}.
#' @param add_contour Logical. If \code{TRUE} and \code{type == "difference"} or \code{type == "ratio"},
#'   adds a reference contour line (break at 0 for difference, 1 for ratio).
#' @param age_range Numeric vector of length 2 specifying bounds for the continuous variable.
#' @param score_range Numeric vector of length 2 specifying score limits for plotting.
#' @param limits Optional numeric vector of length 2 for color scale limits.
#' @param centile_label_nudge Numeric. Horizontal offset for centile labels.
#' @param print_pct Logical. If \code{TRUE} and \code{type == "difference"} or \code{type == "ratio"},
#'   prints the percentage of table cells where the bootstrap SE exceeds the reference SE. Default is \code{FALSE}.
#'
#' @return A \code{ggplot2} object.
#' 
#' @seealso [boot_subtest()], [boot_centiles()]
#' 
#' @examples
#' \donttest{
#' # 1. Prepare example data from normref
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
#' # 3. Run nonparametric bootstrap on subtest norms
#' boot_res <- boot_subtest(
#'   model = mod,
#'   age_name = "age",
#'   normtype = "IQ",
#'   boot_type = "nonparametric",
#'   B = 10, #1000
#'   min_age = 5,
#'   max_age = 20,
#'   seed = 42,
#'   parallel = FALSE # TRUE
#' )
#' 
#' # 4. Standard error heatmap across age and score
#' boot_heatmap(
#'   boot_res, 
#'   type = "se",
#'   add_points = TRUE,
#'   centiles = c(1, 5, 25, 50, 75, 95, 99),
#'   centile_label_nudge = 2.0
#' )
#' 
#' # 5. Benchmark difference heatmap against discrete norming (N = 300 per group)
#' boot_heatmap(
#'   boot_res,
#'   type = "difference",
#'   benchmark_n = 300,
#'   add_points = TRUE,
#'   add_contour = TRUE,
#'   centiles = c(5,25,50,75,95)
#' )
#' }
#' 
#' 
#' @importFrom dplyr %>%
#' @importFrom rlang .data :=
#' @export
boot_heatmap <- function(
    boot,
    type = "se",
    benchmark_n = NULL,
    centiles = NULL,
    add_points = FALSE,
    point_style = "hollow",
    add_contour = FALSE,
    age_range = NULL,
    score_range = NULL,
    limits = NULL,
    centile_label_nudge = 0.5,
    print_pct = FALSE
) {
  if (!inherits(boot, "normref_bootstrap")) {
    stop("'boot' must be of class 'normref_bootstrap'")
  }
  
  if (!type %in% c("se", "difference", "ratio")) {
    stop("type must be 'se', 'difference', or 'ratio'")
  }
  
  meta <- boot$meta
  if (is.null(meta)) {
    meta <- list(type = "single", normtype = "Z", age_name = "age", score_name = "score")
  }
  
  df <- boot$normtable
  
  # 1./2. Resolve continuous variable and score variable names
  age_name <- .resolve_col(df, meta$age_name, "age", c("age", "covar"))
  score_name <- .resolve_col(df, meta$score_name, "testscore",
                             c("testscore", "composite_testscore", "score"))
  
  if (!"boot_se" %in% names(df) && "se" %in% names(df)) {
    df$boot_se <- df$se
  }
  df$score <- df[[score_name]]
  
  if (type %in% c("difference", "ratio") && is.null(benchmark_n)) {
    stop("benchmark_n must be provided when type = 'difference' or 'ratio'")
  }
  
  if (!is.null(centiles) && is.null(meta$model)) {
    stop("Centiles require model stored in boot$meta$model")
  }
  
  normtype <- if (!is.null(meta$normtype)) meta$normtype else "Z"
  val_col <- if ("original" %in% names(df)) "original" else "boot_mean"
  df$est_value <- df[[val_col]]
  
  # Z values for reference SE
  df$z <- .norm_to_z(df$est_value, normtype)
  
  scale_mult <- .normtype_params(normtype)[["sd"]]
  
  if (type %in% c("difference", "ratio") || add_contour) {
    df$reference_se <- compute_reference_se(
      z = df$z,
      n = benchmark_n,
      type = meta$type,
      weights = meta$weights,
      P = meta$P
    ) * scale_mult
    
    df$difference <- df$boot_se - df$reference_se
    df$ratio <- df$boot_se / df$reference_se
  }
  
  # Filter user range
  if (!is.null(age_range)) {
    df <- df[df[[age_name]] >= age_range[1] & df[[age_name]] <= age_range[2], , drop = FALSE]
  }
  if (!is.null(score_range)) {
    df <- df[df$score >= score_range[1] & df$score <= score_range[2], , drop = FALSE]
  }
  
  # Drop NA entries so trimmed boundary cells are left blank
  df <- df[!is.na(df$boot_se), , drop = FALSE]
  if (type == "difference") {
    df <- df[!is.na(df$difference), , drop = FALSE]
  } else if (type == "ratio") {
    df <- df[!is.na(df$ratio) & is.finite(df$ratio), , drop = FALSE]
  }
  
  if (type == "difference" && is.null(limits)) {
    max_abs <- max(abs(df$difference), na.rm = TRUE)
    limits <- c(-max_abs, max_abs)
  } else if (type == "ratio" && is.null(limits)) {
    max_dev <- max(abs(df$ratio - 1), na.rm = TRUE)
    limits <- c(max(0, 1 - max_dev), 1 + max_dev)
  }
  
  df$fill <- if (type == "se") {
    df$boot_se
  } else if (type == "difference") {
    df$difference
  } else {
    df$ratio
  }
  
  # Base Heatmap
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[age_name]], y = .data$score)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$fill)) +
    ggplot2::theme_bw()
  
  if (type == "se") {
    p <- p + ggplot2::scale_fill_viridis_c(
      option = "B",
      begin = 0.05,
      end = 0.9,
      direction = -1,
      na.value = "transparent",
      limits = limits,
      oob = scales::squish
    )
  } else if (type == "difference") {
    p <- p + ggplot2::scale_fill_gradient2(
      low = "#5DC863",
      mid = "white",
      high = "#3B528B",
      midpoint = 0,
      na.value = "transparent",
      limits = limits,
      oob = scales::squish
    )
  } else if (type == "ratio") {
    p <- p + ggplot2::scale_fill_gradient2(
      low = "#5DC863",
      mid = "white",
      high = "#3B528B",
      midpoint = 1,
      na.value = "transparent",
      limits = limits,
      oob = scales::squish
    )
  }
  
  if (add_contour && type %in% c("difference", "ratio")) {
    contour_break <- if (type == "difference") 0 else 1
    contour_var <- if (type == "difference") "difference" else "ratio"
    p <- p + ggplot2::geom_contour(
      data = df,
      ggplot2::aes(x = .data[[age_name]], y = .data$score, z = .data[[contour_var]]),
      breaks = contour_break,
      color = "black",
      linewidth = 1,
      linetype = "dashed"
    )
  }
  
  # Centiles overlay
  if (!is.null(centiles)) {
    age_eval_df <- stats::setNames(data.frame(sort(unique(df[[age_name]]))), age_name)
    cent_df <- estimate_centiles(
      object = meta$model,
      ages = age_eval_df,
      centiles = centiles,
      score_min = min(df$score),
      score_max = max(df$score)
    )
    
    if (!age_name %in% names(cent_df) && "age" %in% names(cent_df)) {
      cent_df[[age_name]] <- cent_df$age
    }
    
    cent_labels <- cent_df %>%
      dplyr::group_by(.data$centile) %>%
      dplyr::slice_max(.data[[age_name]], n = 1) %>%
      dplyr::mutate(!!age_name := .data[[age_name]] + centile_label_nudge)
    
    p <- p +
      ggplot2::geom_line(
        data = cent_df,
        ggplot2::aes(x = .data[[age_name]], y = .data$score, group = .data$centile),
        inherit.aes = FALSE, color = "black"
      ) +
      ggplot2::geom_text(
        data = cent_labels,
        ggplot2::aes(x = .data[[age_name]], y = .data$score,
                     label = paste0("P", .data$centile)),
        inherit.aes = FALSE, hjust = 0, size = 3
      )
  }
  
  # Raw data points overlay
  if (add_points && !is.null(meta$data)) {
    pt_data <- meta$data
    
    # Resolve score column in data points dataframe
    pt_score_col <- if (score_name %in% names(pt_data)) {
      score_name
    } else if ("z_sum" %in% names(pt_data)) {
      "z_sum"
    } else {
      # Fallback to model's formula response name
      all.vars(meta$model$mu.formula)[1]
    }
    
    # Extract scores (first column if stored as matrix)
    pt_data$.pt_score <- .score_vector(pt_data[[pt_score_col]])
    
    if (point_style == "hollow") {
      p <- p + ggplot2::geom_point(
        data = pt_data,
        ggplot2::aes(
          x = .data[[age_name]],
          y = .data[[".pt_score"]]
        ),
        shape = 21,
        fill = "white",
        color = "black",
        alpha = 0.7
      )
    } else {
      p <- p + ggplot2::geom_point(
        data = pt_data,
        ggplot2::aes(
          x = .data[[age_name]],
          y = .data[[".pt_score"]]
        ),
        color = "black",
        alpha = 0.5
      )
    }
  }
  
  fill_label <- if (type == "se") {
    paste0("SE (", normtype, ")")
  } else if (type == "difference") {
    paste0("SE Difference")
  } else {
    paste0("SE Ratio")
  }
  
  subtitle <- .boot_subtitle(type, boot$trueB, normtype, benchmark_n)
  
  title <- if (type == "se") "Bootstrap SE" else "Bootstrap SE vs Reference SE"
  
  # Report share of cells below the benchmark contour
  if (type %in% c("difference", "ratio") || isTRUE(print_pct)) {
    if (!.print_pct_below(df, type, benchmark_n)) return(invisible(NA_real_))
  }
  
  
  p <- p + ggplot2::labs(title = title, fill = fill_label, subtitle = subtitle, x = age_name, y = score_name)
  return(p)
}

#' Plot sample-level bootstrap precision across age, faceted by score bins
#'
#' @description
#' Visualizes person-level bootstrap standard errors, differences, or relative ratios
#' against a continuous predictor (such as age), faceted into standard-score bins
#' (e.g., z-score bins from -3 to +3). Individual points represent observed sample cases.
#'
#' @param boot Object of class \code{"normref_bootstrap"}.
#' @param benchmark_n Numeric. Reference sample size used to compute benchmark standard errors (default = 300).
#' @param type Character. One of \code{"ratio"} (default), \code{"difference"}, or \code{"se"}.
#'   \itemize{
#'     \item \code{"ratio"}: Relative ratio \eqn{\widehat{SE}_{\text{boot}} / SE_{\text{ref}}}.
#'     \item \code{"difference"}: Difference \eqn{\widehat{SE}_{\text{boot}} - SE_{\text{ref}}}.
#'     \item \code{"se"}: Raw bootstrap standard error \eqn{\widehat{SE}_{\text{boot}}}.
#'   }
#' @param z_bins Numeric vector specifying bin edges on the \eqn{z}-score scale (default: \code{seq(-3, 3, by = 1)}).
#' @param age_range Optional numeric vector of length 2 specifying bounds for the continuous age variable.
#' @param smooth Logical. If \code{TRUE}, adds a LOESS smoother per facet (default = \code{FALSE}).
#' @param point_alpha Numeric. Alpha transparency for individual points (default = 0.5).
#'
#' @return A \code{ggplot2} object.
#'
#' @seealso [boot_centiles()], [boot_heatmap()], [boot_composite()]
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
#' # 3. Construct composite score
#' normtable1 <- normtable_create(mod1, mydata1, age_name = "age", score_name = "shaped_score", 
#'                 trim = Inf)
#' normtable2 <- normtable_create(mod2, mydata2, age_name = "age", score_name = "shaped_score", 
#'                 trim = Inf)
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
#' # 5. Observed sample-level standard error ratios by score bin
#' boot_sample(
#'   boot_comp,
#'   type = "ratio",
#'   benchmark_n = 400,
#'   z_bins = seq(-3, 3, by = 1)
#'   )
#' }
#' 
#' @export
boot_sample <- function(
    boot,
    benchmark_n = 300,
    type = c("ratio", "difference", "se"),
    z_bins = seq(-3, 3, by = 1),
    age_range = NULL,
    smooth = FALSE,
    point_alpha = 0.5
) {
  if (!inherits(boot, "normref_bootstrap")) {
    stop("'boot' must be of class 'normref_bootstrap'")
  }
  
  type <- match.arg(type)
  
  if (is.null(boot$sample)) {
    stop("No '$sample' slot found in the bootstrap object. Ensure sample-level evaluation is enabled.")
  }
  
  meta <- boot$meta
  if (is.null(meta)) {
    meta <- list(type = "single", normtype = "Z", age_name = "age")
  }
  
  df <- boot$sample
  
  # 1. Resolve continuous variable name (age)
  age_name <- .resolve_col(df, meta$age_name, "age", c("age", "covar"),
                           error_msg = "Age column not found in $sample.")
  
  # 3. Filter age range if specified
  if (!is.null(age_range)) {
    df <- df[df[[age_name]] >= age_range[1] & df[[age_name]] <= age_range[2], , drop = FALSE]
  }
  
  # 4. Standardize score to z-scale
  normtype <- if (!is.null(meta$normtype)) meta$normtype else "Z"
  df$z <- .norm_to_z(df$original, normtype)
  
  scale_mult <- .normtype_params(normtype)[["sd"]]
  
  # 5. Compute Reference SE
  df$reference_se <- compute_reference_se(
    z = df$z,
    n = benchmark_n,
    type = "single",
    weights = meta$weights,
    P = meta$P
  ) * scale_mult
  
  df$difference <- df$boot_se - df$reference_se
  df$ratio      <- df$boot_se / df$reference_se
  
  # 6. Binning by z-score
  bin_labels <- character(length(z_bins) - 1)
  for (i in seq_len(length(z_bins) - 1)) {
    z_low  <- z_bins[i]
    z_high <- z_bins[i + 1]
    
    if (normtype == "Z") {
      bin_labels[i] <- sprintf("z: [%.1f, %.1f)", z_low, z_high)
    } else {
      s_low  <- switch(normtype, "T" = 50 + 10 * z_low, "IQ" = 100 + 15 * z_low, "SS" = 10 + 3 * z_low)
      s_high <- switch(normtype, "T" = 50 + 10 * z_high, "IQ" = 100 + 15 * z_high, "SS" = 10 + 3 * z_high)
      bin_labels[i] <- sprintf("%s: [%.0f, %.0f)\n(z: %.1f to %.1f)", normtype, s_low, s_high, z_low, z_high)
    }
  }
  
  df$score_bin <- cut(
    df$z,
    breaks = z_bins,
    labels = bin_labels,
    include.lowest = TRUE,
    right = FALSE
  )
  
  # Remove points outside specified bins
  df_plot <- df[!is.na(df$score_bin) & !is.na(df$boot_se), , drop = FALSE]
  
  # 7. Setup plotting aesthetics based on 'type'
  y_var <- switch(type, "ratio" = "ratio", "difference" = "difference", "se" = "boot_se")
  
  y_label <- switch(
    type,
    "ratio"      = paste0("SE Ratio"),
    "difference" = paste0("SE Difference"),
    "se"         = paste0("Bootstrap SE")
  )
  
  title <- switch(
    type,
    "ratio"      = "Person-Level Bootstrap SE vs Reference SE",
    "difference" = "Person-Level Bootstrap SE vs Reference SE",
    "se"         = "Person-Level Bootstrap SE"
  )
  
  subtitle <- .boot_subtitle(type, boot$trueB, normtype, benchmark_n)
  
  # 8. Build ggplot
  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data[[age_name]], y = .data[[y_var]])) +
    ggplot2::geom_point(
      color = "grey35",
      alpha = point_alpha,
      size = 1.4
    ) +
    ggplot2::facet_wrap(~ score_bin, scales = "fixed") +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = age_name,
      y = y_label
    )
  
  # Reference benchmark line
  if (type == "ratio") {
    p <- p +
      ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 0.8)
  } else if (type == "difference") {
    p <- p +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8)
  }
  
  # Optional LOESS smoother
  if (smooth) {
    p <- p + ggplot2::geom_smooth(
      method = "loess",
      se = TRUE,
      color = "#3B528B",
      fill = "grey70",
      alpha = 0.25,
      linewidth = 0.75
    )
  }
  
  return(p)
}

