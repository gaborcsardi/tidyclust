#' Splice final parameters into objects
#'
#' The `finalize_*` functions take a list or tibble of tuning parameter values and
#' update objects with those values.
#'
#' @param x A recipe, `parsnip` model specification, or workflow.
#' @param parameters A list or 1-row tibble of parameter values. Note that the
#'  column names of the tibble should be the `id` fields attached to `tune()`.
#'  For example, in the `Examples` section below, the model has `tune("K")`. In
#'  this case, the parameter tibble should be "K" and not "neighbors".
#' @return An updated version of `x`.
#' @export
#' @examples
#' kmeans_spec <- k_means(k = tune())
#'
#' best_params <- data.frame(k = 5)
#' best_params
#'
#' kmeans_spec
#' finalize_model_tidyclust(kmeans_spec, best_params)
finalize_model_tidyclust <- function(x, parameters) {
  if (!inherits(x, "cluster_spec")) {
    rlang::abort("`x` should be a tidyclust model specification.")
  }
  parsnip::check_final_param(parameters)
  pset <- hardhat::extract_parameter_set_dials(x)
  if (tibble::is_tibble(parameters)) {
    parameters <- as.list(parameters)
  }

  parameters <- parameters[names(parameters) %in% pset$id]

  discordant <- dplyr::filter(pset, id != name & id %in% names(parameters))
  if (nrow(discordant) > 0) {
    for (i in 1:nrow(discordant)) {
      names(parameters)[names(parameters) == discordant$id[i]] <-
        discordant$name[i]
    }
  }
  rlang::exec(stats::update, object = x, !!!parameters)
}

#' @rdname finalize_model_tidyclust
#' @export
finalize_workflow_tidyclust <- function(x, parameters) {
  if (!inherits(x, "workflow")) {
    rlang::abort("`x` should be a workflow")
  }
  parsnip::check_final_param(parameters)

  mod <- extract_spec_parsnip(x)
  mod <- finalize_model_tidyclust(mod, parameters)
  x <- set_workflow_spec(x, mod)

  if (has_preprocessor_recipe(x)) {
    rec <- extract_preprocessor(x)
    rec <- tune::finalize_recipe(rec, parameters)
    x <- set_workflow_recipe(x, rec)
  }

  x
}
