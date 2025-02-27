#' Model predictions
#'
#' Apply a model to create different types of predictions.
#'  `predict()` can be used for all types of models and uses the
#'  "type" argument for more specificity.
#'
#' @param object An object of class `cluster_fit`
#' @param new_data A rectangular data object, such as a data frame.
#' @param type A single character value or `NULL`. Possible values
#'   are "cluster", or "raw". When `NULL`, `predict()` will choose an
#'  appropriate value based on the model's mode.
#' @param opts A list of optional arguments to the underlying
#'  predict function that will be used when `type = "raw"`. The
#'  list should not include options for the model object or the
#'  new data being predicted.
#' @param ... Arguments to the underlying model's prediction
#'  function cannot be passed here (see `opts`).
#' @details If "type" is not supplied to `predict()`, then a choice
#'  is made:
#'
#'   * `type = "cluster"` for clustering models
#'
#' `predict()` is designed to provide a tidy result (see "Value"
#'  section below) in a tibble output format.
#'
#' @return With the exception of `type = "raw"`, the results of
#'  `predict.cluster_fit()` will be a tibble as many rows in the output
#'  as there are rows in `new_data` and the column names will be
#'  predictable.
#'
#' For clustering results the tibble will have a `.pred_cluster` column.
#'
#' Using `type = "raw"` with `predict.cluster_fit()` will return
#'  the unadulterated results of the prediction function.
#'
#' When the model fit failed and the error was captured, the
#'  `predict()` function will return the same structure as above but
#'  filled with missing values. This does not currently work for
#'  multivariate models.
#'
#' @examples
#' kmeans_spec <- k_means(k = 5) %>%
#'   set_engine_tidyclust("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit %>%
#'   predict(new_data = mtcars)
#' @method predict cluster_fit
#' @export predict.cluster_fit
#' @export
predict.cluster_fit <- function(object, new_data, type = NULL, opts = list(), ...) {
  if (inherits(object$fit, "try-error")) {
    rlang::warn("Model fit failed; cannot make predictions.")
    return(NULL)
  }

  check_installs(object$spec)
  load_libs(object$spec, quiet = TRUE)

  type <- check_pred_type(object, type)

  res <- switch(type,
    cluster = predict_cluster(object = object, new_data = new_data, ...),
    raw     = predict_raw(object = object, new_data = new_data, opts = opts, ...),

    rlang::abort(glue::glue("I don't know about type = '{type}'"))
  )

  res <- switch(type,
    cluster = format_cluster(res),
    res
  )
  res
}


check_pred_type <- function(object, type, ...) {
  if (is.null(type)) {
    type <-
      switch(object$spec$mode,
        partition = "cluster",
        rlang::abort("`type` should be 'cluster'.")
      )
  }
  if (!(type %in% pred_types)) {
    rlang::abort(
      glue::glue(
        "`type` should be one of: ",
        glue::glue_collapse(pred_types, sep = ", ", last = " and ")
      )
    )
  }
  type
}

format_cluster <- function(x) {
  tibble::tibble(.pred_cluster = unname(x))
}

#' Prepare data based on parsnip encoding information
#' @param object A parsnip model object
#' @param new_data A data frame
#' @return A data frame or matrix
#' @keywords internal
#' @export
prepare_data <- function(object, new_data) {
  fit_interface <- object$spec$method$fit$interface

  pp_names <- names(object$preproc)
  if (any(pp_names == "terms") | any(pp_names == "x_var")) {
    # Translation code
    if (fit_interface == "formula") {
      new_data <- .convert_x_to_form_new(object$preproc, new_data)
    } else {
      new_data <- .convert_form_to_x_new(object$preproc, new_data)$x
    }
  }

  remove_intercept <-
    get_encoding_tidyclust(class(object$spec)[1]) %>%
    dplyr::filter(mode == object$spec$mode, engine == object$spec$engine) %>%
    dplyr::pull(remove_intercept)
  if (remove_intercept & any(grepl("Intercept", names(new_data)))) {
    new_data <- new_data %>% dplyr::select(-dplyr::one_of("(Intercept)"))
  }

  switch(fit_interface,
    none = new_data,
    data.frame = as.data.frame(new_data),
    matrix = as.matrix(new_data),
    new_data
  )
}

make_pred_call <- function(x) {
  if ("pkg" %in% names(x$func)) {
    cl <- rlang::call2(x$func["fun"], !!!x$args, .ns = x$func["pkg"])
  } else {
    cl <- rlang::call2(x$func["fun"], !!!x$args)
  }

  cl
}
