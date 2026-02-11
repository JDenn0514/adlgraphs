# Extract reference level terms and counts

Identifies reference levels for factor predictors in a model and returns
their names and observation counts.

## Usage

``` r
get_reference_levels(model, model_data)
```

## Arguments

- model:

  A fitted model object.

- model_data:

  The data used to fit the model.

## Value

A list with elements `terms` (character vector of reference terms) and
`n_values` (named integer vector of counts).
