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
  
  if (all(is.na(pop_age))) {
    stop("Error: normtable must be created with normtable_create(new_data = FALSE).")
  }
  
  # Plot first curve
  graphics::plot(
    x    = pop_age[, "age"],
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
      x = pop_age[, "age"],
      y = 100 * cdf_matrix[, i],
      ...
    )
  }
  
  # Add sample observations
  graphics::points(
    x   = mydata$age,
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
