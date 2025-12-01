# Create a tidied tibble of regression results

This function was created to produce results very similar to what you'll
find at broom.helpers, with a few changes. Most notably, and the main
reason for creating this function, you can standardize the regression
coefficients by scaling and mean-centering the input data.

## Usage

``` r
get_coefficients(
  model,
  conf.level = 0.95,
  standardize = FALSE,
  n.sd = 2,
  exponentiate = FALSE,
  add_ss = TRUE,
  add_labels = TRUE,
  add_n = FALSE,
  model_name = NULL
)
```

## Arguments

- model:

  A model object created using either `lm` or `glm`. Can also be piped
  into the function.

- conf.level:

  A number between 0 and 1 that signifies the width of the desired
  confidence interval. Default is 0.95, which corresponds to a 95%
  confidence interval.

- standardize:

  Logical. If TRUE, reports standardized regression coefficients by
  scaling and mean-centering input data. Default is FALSE.

- n.sd:

  Logical. If `standardize` is TRUE, determines the number of standard
  deviations used to scale the data. Default is 2.

- exponentiate:

  Logical. If TRUE, reports exponentiated coefficients with confidence
  intervals for exponential models like logit and Poisson models. This
  quantity is known as an odds ratio for binary outcomes and incidence
  rate ratio for count models. Default is FALSE.

- add_ss:

  Logical. If TRUE, the default, a new column is created called `ss`
  that gives a "Yes" if the term is statistically significant and a "No"
  if the term is not statistically significant.

- add_labels:

  Logical. If TRUE adds variable and value labels

- add_n:

  Logical. If true adds the number of observations per variable

- model_name:

  A character string that adds a new column titled `model` with the
  supplied character string as the rows. If `NULL`, the default, no
  column is created.

  This is useful if you are comparing multiple models with similar
  variable and need to clarify which estimates are associated with which
  model.

## Value

A data.frame summarizing the results of an
[`lm()`](https://rdrr.io/r/stats/lm.html) or
[`glm()`](https://rdrr.io/r/stats/glm.html) object.

## Details

This function also takes advantage of
[`tidy_add_reference_rows`](https://larmarange.github.io/broom.helpers/reference/tidy_add_reference_rows.html)/,
[`tidy_add_term_labels`](https://larmarange.github.io/broom.helpers/reference/tidy_add_term_labels.html)/,
and
[`tidy_add_n`](https://larmarange.github.io/broom.helpers/reference/tidy_add_n.html)/
to allow you to include the reference row for each variable, the
underlying variable and value labels, and the number of observations.
