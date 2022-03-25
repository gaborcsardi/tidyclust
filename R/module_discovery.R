correlation_mining <- function(.data,
                               null = 0.5,
                               max_tries = 20) {

  ## decorrelate data
  .data <- .data %>%
    as.matrix() %>%
    scale()

  final_A <- c()
  tries <- 0

  while(length(final_A) == 0 & tries < max_tries) {

    old_A <- -1
    A <- sample(1:nrow(.data), 1)

    while(length(setdiff(A, old_A)) > 0) {

      avg_A <- colMeans(.data[A,, drop = FALSE])
      min_sigs <- sapply(1:nrow(.data),
                         function(x)
                           cor.test(avg_A, colMeans(.data[x,,drop = FALSE]),
                                    alternative = "greater") %>%
                           tidy() %>%
                           pull(conf.low))

      old_A <- A
      A <- which(min_sigs > null)

    }

    final_A <- A
    tries <- tries + 1
  }

  res <- rep(0, nrow(.data))
  res[final_A] <- 1
  return(res)
}

# set.seed(1234)
# module <- penguins %>%
#   select_if(is.numeric) %>%
#   correlation_mining(null = 0)
#
# penguins %>%
#   mutate(
#     module = module
#   ) %>%
#   count(module, species)
