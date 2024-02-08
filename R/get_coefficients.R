#' Create a tidied tibble of regression results
#'
#' This function was created to produce results very similar to what you'll find
#' at broom.helpers, with a few changes. Most notably, and the main reason for
#' creating this function, you can standardize the regression coefficients by
#' scaling and mean-centering the input data.
#'
#' This function also takes advantage of
#' \code{\link[broom.helpers]{tidy_add_reference_rows}}/,
#' \code{\link[broom.helpers]{tidy_add_term_labels}}/, and
#' \code{\link[broom.helpers]{tidy_add_n}}/ to allow you to include the
#' reference row for each variable, the underlying variable and value labels,
#' and the number of observations.
#'
#' @param model A model object created using either `lm` or `glm`. Can also be
#'   piped into the function.
#' @param conf.level A number between 0 and 1 that signifies the width of the
#'   desired confidence interval. Default is 0.95, which corresponds to a 95%
#'   confidence interval.
#' @param standardize If TRUE, reports standardized regression coefficients by
#'   scaling and mean-centering input data. Default is FALSE.
#' @param n.sd If `standardize` is TRUE, determines the number of standard
#'   deviations used to scale the data. Default is 2.
#' @param exponentiate Logical. If TRUE, reports exponentiated coefficients with
#'   confidence intervals for exponential models like logit and Poisson models.
#'   This quantity is known as an odds ratio for binary outcomes and incidence
#'   rate ratio for count models. Default is FALSE.
#' @param add_ss Logical. If TRUE, the default, a new column is created called
#'   `ss` that gives a "Yes" if the term is statistically significant and a
#'   "No" if the term is not statistically significant.
#' @param add_labels Logical. If TRUE adds variable and value labels
#' @param add_n Logical. If true adds the number of observations per variable
#'
#' @export



get_coefficients <- function(
    model,
    conf.level = 0.95,
    standardize = FALSE,
    n.sd = 2,
    exponentiate = FALSE,
    add_ss = TRUE,
    add_labels = TRUE,
    add_n = FALSE
) {

  # get the model summary
  if (standardize == TRUE) {
    # use jtools::scale_mod to get standardize the data
    model_results <- jtools::scale_mod(model, n.sd = n.sd) %>% summary()
  } else {
    model_results <- model %>% summary()
  }

  # get the model coefficients and make it a tibble
  model_results <- model_results$coefficients %>%
    # make the rownames a column called "term"
    tibble::as_tibble(rownames = "term")
  # rename the columns
  colnames(model_results) <- c("term", "estimate", "std.error", "statistic", "p.value")

  # add confidence intervals
  model_results <- model_results %>%
    mutate(
      # manually calculate the confidence intervals
      conf.high = estimate - (std.error * qt(p = (1 - conf.level) / 2,
                                             df = df.residual(model))),
      conf.low = estimate + (std.error * qt(p = (1 - conf.level) / 2,
                                            df = df.residual(model))),
      # round the confidence intervals to the nearest thousandth
      across(where(is.numeric), ~round(.x, 3))
    )

  # attach the model to the tidy tibble
  # this enables the use of other broom.helpers functions
  model_results <- model_results %>%
    broom.helpers::tidy_attach_model(
      model,
      .attributes = list(
        exponentiate = exponentiate,
        conf.level = conf.level
      )
    )

  # add reference rows
  if (add_labels == FALSE) {
    # remove the unnecessary columns if we are not adding the labels
    model_results <- model_results %>%
      broom.helpers::tidy_add_reference_rows() %>%
      select(-c(var_class:contrasts_type))
  } else if (add_labels == TRUE) {
    model_results <- model_results %>%
      broom.helpers::tidy_add_reference_rows()
  }

  # determine if the values should be exponentiated
  if (exponentiate == TRUE) {

    # exponentiate the estimate and confidence intervals
    model_results <- model_results %>%
      dplyr::mutate(across(c(estimate, conf.high, conf.low), exp))

  } else if (exponentiate == FALSE) {

    # don't exponentiate
    model_results

  }

  # should we add statistical signficance
  if (add_ss == TRUE) {
    model_results <- model_results %>%
      dplyr::mutate(
        ss = dplyr::case_when(
          p.value < 0.05 ~ "Yes",
          .default = "No"
        )
      ) %>%
      # move it after p.value
      dplyr::relocate(ss, .after = p.value)
  } else if (add_ss == FALSE) {
    model_results <- model_results
  }

  # determine if we should add labels
  if (add_labels == TRUE) {

    model_results <- model_results %>%
      # add value labels
      broom.helpers::tidy_add_variable_labels() %>%
      broom.helpers::tidy_add_term_labels() %>%
      select(-c(var_class:contrasts_type)) %>%
      rename(value_label = label)

  } else if (add_labels == FALSE) {

    # don't add any labels
    model_results <- model_results

  }

  # Determine if number of observations should be added
  if (add_n == TRUE) {

    # add the number of observations for each term
    model_results <- model_results %>%
      broom.helpers::tidy_add_n()
  } else if (add_n == FALSE) {

    # don't add number of observations
    model_results <- model_results

  }

  return(model_results)

}










