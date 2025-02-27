
#' S3 method to get fitted model summary info depending on engine
#'
#' @param object a fitted cluster_spec object
#' @param ... other arguments passed to methods
#'
#' @return A list with various summary elements
#'
#' @examples
#' kmeans_spec <- k_means(k = 5) %>%
#'   set_engine_tidyclust("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit %>%
#'   extract_fit_summary()
#' @export
extract_fit_summary <- function(object, ...) {
  UseMethod("extract_fit_summary")
}

#' @export
extract_fit_summary.cluster_fit <- function(object, ...) {
  extract_fit_summary(object$fit)
}

#' @export
extract_fit_summary.workflow <- function(object, ...) {
  extract_fit_summary(object$fit$fit$fit)
}

#' @export
extract_fit_summary.kmeans <- function(object, ...) {
  reorder_clusts <- order(unique(object$cluster))
  names <- paste0("Cluster_", seq_len(nrow(object$centers)))

  list(
    cluster_names = names,
    centroids = tibble::as_tibble(object$centers[reorder_clusts, , drop = FALSE]),
    n_members = object$size[reorder_clusts],
    within_sse = object$withinss[reorder_clusts],
    tot_sse = object$totss,
    orig_labels = unname(object$cluster),
    cluster_assignments = names[reorder_clusts][object$cluster]
  )
}

#' @export
extract_fit_summary.KMeansCluster <- function(object, ...) {
  reorder_clusts <- order(unique(object$cluster))
  names <- paste0("Cluster_", seq_len(nrow(object$centroids)))

  list(
    cluster_names = names,
    centroids = tibble::as_tibble(object$centroids[reorder_clusts, , drop = FALSE]),
    n_members = object$obs_per_cluster[reorder_clusts],
    within_sse = object$WCSS_per_cluster[reorder_clusts],
    tot_sse = object$total_SSE,
    orig_labels = object$clusters,
    cluster_assignments = names[reorder_clusts][object$clusters]
  )
}
