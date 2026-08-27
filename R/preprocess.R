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
#' for the data. It only ensures compatibility.  
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
#' `composite_shape()` creates a data.frame with age values and a weighted sum of normalized 
#' z-scores from multiple NormTable objects, suitable for use as input to [fb_select()].
#'
#' @details
#' The function combines normalized z-scores across multiple norm tables into a composite score.
#' By default, scores are equally weighted. Optional weights allow differential contribution of
#' subtests to the composite.
#'
#' @param normtables list of NormTable objects created by [normtable_create()]. 
#'   Each must contain `znorm_sample` and `norm_sample`.
#' @param weights Optional numeric vector of weights for a weighted composite score.
#'   If `NULL` (default), the sum of subtest z-scores is used.
#'
#' @return A data.frame with:
#'   \item{age}{Age values from the first NormTable}
#'   \item{z_sum}{Weighted sum of normalized z-scores across all objects}
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
composite_shape <- function(normtables, weights = NULL) {
  
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
  
  k <- length(normtables)
  
  # ----------------------------
  # Handle weights
  # ----------------------------
  if (is.null(weights)) {
    weights <- rep(1, k)
  } else {
    if (!is.numeric(weights) || length(weights) != k) {
      stop("'weights' must be a numeric vector of length equal to normtables")
    }
  }
  
  # ----------------------------
  # Extract z-scores
  # ----------------------------
  z_scores <- lapply(normtables, function(x) x$znorm_sample[, 2])
  
  # Weighted sum
  z_sum <- Reduce(`+`, Map(`*`, z_scores, weights))
  
  data.frame(
    age = normtables[[1]]$norm_sample[, 1],
    z_sum = z_sum
  )
}
#' Sample size planning for continuous norming using polynomial regression
#'
#' Computes optimal sample sizes per group under a distribution free polynomial regression model
#' for group means, following \insertCite{hessen2026richtlijnen;textual}{normref}.
#'
#' In the assumed continuous norming model, group means are first estimated and then smoothed
#' using a polynomial model that regresses group means on age. This function
#' determines group sample sizes such that the precision of the estimated means
#' (not higher-order moments) is at least as high as under traditional norming.
#' 
#'
#' @param n_groups Integer. Number of groups (e.g., age groups).
#' @param poly_degree Integer scalar or vector. Polynomial degree(s) of polynomial regression model.
#' @param n0 Integer scalar or vector. Reference sample size per group under traditional norming.
#' Typical values are 200, 300, or 400 (see e.g., \insertCite{egberink2026COTAN;textual}{normref}).
#' @param variances Optional numeric vector of length \code{n_groups}. Estimated variances of the means in each group.
#' If not provided, homoscedastic variances are assumed (\code{rep(1, n_groups)}).
#' @param solution_type Character string indicating which solution(s) to return.
#' Must be one of:
#' \describe{
#'   \item{"balanced"}{(default) Returns a single optimal solution with the most balanced allocation across groups.}
#'   \item{"range"}{Returns, for each group, the range of sample sizes across all optimal solutions.}
#'   \item{"all"}{Returns all optimal solutions.}
#' }
#'
#' @details
#' This function implements the linear programming (LP) approach described in
#' \insertCite{hessen2026richtlijnen;textual}{normref}. Multiple optimal solutions may exist:
#' different combinations of \eqn{n_{min}} and \eqn{n_{max}} can yield the same minimal
#' total sample size while satisfying the precision constraints.
#'
#' By default (\code{solution_type = "balanced"}), a single solution is returned,
#' defined as the solution minimizing the difference between the largest and smallest group sample sizes. This yields
#' a practically balanced design. Alternatively, users can inspect all optimal
#' solutions (\code{solution_type = "all"}) or examine the range of optimal sample
#' sizes per group (\code{solution_type = "range"}).
#' 
#' \strong{How to use:}
#' \itemize{
#'   \item Before data collection: do not provide \code{variances} (assumes homoscedasticity).
#'   \item During/after data collection: provide estimated \code{variances} per group.
#' }
#'
#' @return A data.frame. The structure depends on \code{solution_type}:
#'
#' \describe{
#'   \item{solution_type = "balanced"}{Returns one optimal solution with:
#'     \describe{
#'       \item{n0}{Reference sample size per group}
#'       \item{group}{Group index}
#'       \item{poly_degree}{Polynomial degree}
#'       \item{n}{Sample size per group}
#'       \item{variance}{Expected variance of estimated mean}
#'       \item{variance_traditional}{Variance under traditional norming (\eqn{\sigma_j^2 / n0})}
#'       \item{total_n}{Total sample size across all groups}
#'       \item{traditional_total_n}{Total sample size under traditional norming}
#'       \item{n_min}{Lower bound used in LP}
#'       \item{n_max}{Upper bound used in LP}
#'     }
#'   }
#'
#'   \item{solution_type = "all"}{Returns all optimal solutions with the same
#'   columns as above, plus:
#'     \describe{
#'       \item{solution_id}{Identifier for solution number}
#'     }
#'   }
#'
#'   \item{solution_type = "range"}{Returns one row per group with:
#'     \describe{
#'       \item{n0}{Reference sample size per group}
#'       \item{group}{Group index}
#'       \item{degree}{Polynomial degree}
#'       \item{n_range}{Range of sample sizes over optimal solutions}
#'     }
#'   }
#' }
#' 
#' @references
#'   \insertRef{egberink2026COTAN}{normref}
#'   \insertRef{hessen2026richtlijnen}{normref}
#'   
#' 
#' @examples
#' # Example 1: Planning before data collection (homoscedastic)
#' \dontrun{
#' res1 <- sample_size_poly(
#'   n_groups = 5,
#'   poly_degree = 2,
#'   n0 = 400
#' )
#'
#' # Example 2: Midway planning with variance estimates (heteroscedastic)
#'ids_data$age_group <- cut(ids_data$age, breaks = seq(6, 18, by = 1))
#'v <- tapply(ids_data$y7, ids_data$age_group, var, na.rm = TRUE)
#'
#' res2 <- sample_size_poly(
#'  n_groups = length(v),
#'  poly_degree = c(1,2,3),
#'  n0 = c(300,400),
#'  variances = v
#'  )
#'}
#' @export
sample_size_poly <- function(
    n_groups,
    poly_degree,
    n0,
    variances = NULL,
    solution_type = c("balanced", "all", "range")
) {
  solution_type <- match.arg(solution_type)
  
  if (!is.numeric(n_groups) || length(n_groups) != 1 || n_groups < 1) {
    stop("'n_groups' must be a positive integer.")
  }
  
  if (missing(n0)) stop("'n0' must be provided (e.g., 200, 300, or 400).")
  
  # Variances
  if (is.null(variances)) {
    variances <- rep(1, n_groups)
  } else if (length(variances) != n_groups) {
    stop("Length of 'variances' must equal 'n_groups'.")
  }
  
  results_list <- list()
  
  for (n0_val in n0) {
    
    lb_vals <- 1:(n0_val - 1)
    ub_vals <- 2:n0_val
    pairs <- expand.grid(lb = lb_vals, ub = ub_vals)
    pairs <- pairs[pairs$lb < pairs$ub, ]
    
    for (deg in poly_degree) {
      
      # Design matrix
      X <- matrix(1, nrow = n_groups, ncol = deg + 1)
      if (deg > 0) {
        for (i in seq_len(deg)) {
          X[, i + 1] <- (seq_len(n_groups))^i
        }
      }
      
      XtX_inv <- solve(t(X) %*% X)
      A <- XtX_inv %*% t(X)
      
      # B matrix
      B <- matrix(0, n_groups, n_groups)
      for (i in seq_len(n_groups)) {
        for (j in seq_len(n_groups)) {
          B[i, j] <- t(X[i, ]) %*% A[, j] %*% t(A[, j]) %*% X[i, ]
        }
      }
      
      C <- B %*% diag(variances)
      s <- variances / n0_val
      
      # LP solve
      lp_results <- lapply(seq_len(nrow(pairs)), function(i) {
        lb_try <- pairs$lb[i]
        ub_try <- pairs$ub[i]
        
        D <- rbind(C, -diag(n_groups), diag(n_groups))
        rhs <- c(s, rep(-1 / ub_try, n_groups), rep(1 / lb_try, n_groups))
        
        result <- lpSolve::lp(
          direction = "max",
          objective.in = rep(1, n_groups),
          const.mat = D,
          const.dir = rep("<=", 3 * n_groups),
          const.rhs = rhs
        )
        
        if (result$status == 0) {
          n_vec <- ceiling(1 / result$solution)
          list(
            total_n = sum(n_vec),
            lb = lb_try,
            ub = ub_try,
            n_vec = n_vec
          )
        } else NULL
      })
      
      lp_results <- lp_results[!sapply(lp_results, is.null)]
      if (length(lp_results) == 0) next
      
      # Minimal solutions
      total_n_vals <- sapply(lp_results, function(x) x$total_n)
      min_total <- min(total_n_vals)
      best_idx <- which(total_n_vals == min_total)
      best_list <- lp_results[best_idx]
      
      # Build full df
      df_list <- lapply(seq_along(best_list), function(k) {
        best <- best_list[[k]]
        
        best_n <- best$n_vec
        precision <- as.vector(C %*% (1 / best_n))
        
        data.frame(
          n0 = n0_val,
          group = seq_len(n_groups),
          poly_degree = deg,
          solution_id = paste0("deg", deg, "_n0", n0_val, "_s", k),
          n = best_n,
          variance = precision,
          variance_traditional = variances / n0_val,
          total_n = best$total_n,
          traditional_total_n = n_groups * n0_val,
          n_min = best$lb,
          n_max = best$ub
        )
      })
      
      df_all <- do.call(rbind, df_list)
      
      # ---- OUTPUT TYPES ----
      
      if (solution_type == "all") {
        df <- df_all
        
      } else if (solution_type == "balanced") {
        # choose solution with smallest spread in n
        spread_df <- stats::aggregate(n ~ solution_id, df_all, function(x) max(x) - min(x))
        best_sol <- spread_df$solution_id[which.min(spread_df$n)]
        df <- df_all[df_all$solution_id == best_sol, ]
        df$solution_id <- NULL
        
      } else if (solution_type == "range") {
        agg <- stats::aggregate(n ~ group, df_all,
                                function(x) paste0(min(x), "-", max(x)))
        
        df <- data.frame(
          n0 = n0_val,
          group = agg$group,
          poly_degree = deg,
          n_range = agg$n
        )
      }
      
      results_list[[paste0("p_", deg, "_n0_", n0_val)]] <- df
    }
  }
  
  do.call(rbind, results_list)
}