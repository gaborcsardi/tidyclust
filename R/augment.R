#' Augment data with predictions
#'
#' `augment()` will add column(s) for predictions to the given data.
#'
#' For partition models, a `.pred_cluster` column is added.
#'
#' @param x A `cluster_fit` object produced by [fit.cluster_spec()] or
#' [fit_xy.cluster_spec()] .
#' @param new_data A data frame or matrix.
#' @param ... Not currently used.
#' @rdname augment
#' @export
#' @examples
#' kmeans_spec <- k_means(k = 5) %>%
#'   set_engine_tidyclust("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit %>%
#'   augment(new_data = mtcars)
augment.cluster_fit <- function(x, new_data, ...) {
  ret <- new_data
  if (x$spec$mode == "partition") {
    check_spec_pred_type(x, "cluster")
    ret <- dplyr::bind_cols(
      ret,
      stats::predict(x, new_data = new_data)
    )
  } else {
    rlang::abort(paste("Unknown mode:", x$spec$mode))
  }
  as_tibble(ret)
}
