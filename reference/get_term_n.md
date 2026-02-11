# Get the number of observations for each model term

Returns the number of observations contributing to each coefficient,
including optional reference levels.

## Usage

``` r
get_term_n(model, include_reference = FALSE)
```

## Arguments

- model:

  A fitted model object (e.g., `lm`, `glm`, or `svyglm`).

- include_reference:

  Logical; whether to include reference levels.

## Value

A named integer vector with term names as names.
