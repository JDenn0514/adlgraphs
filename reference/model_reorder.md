# Reorder model terms to ensure reference levels appear first

Orders the rows of a coefficient table so that reference levels for
categorical predictors appear before other levels within each term.

## Usage

``` r
model_reorder(model, coefs)
```

## Arguments

- model:

  A fitted model object.

- coefs:

  A tibble or data frame of model coefficients.

## Value

A reordered version of `coefs`.
