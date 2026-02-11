# Clean and augment model output

A simplified alternative to
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) that
adds confidence intervals, optional reference levels, and sample sizes
for each term.

## Usage

``` r
clean(
  model,
  conf_level = 0.95,
  include_reference = FALSE,
  conf_method = c("wald", "profile")
)
```

## Arguments

- model:

  A fitted model object (e.g., `lm`, `glm`, or `svyglm`).

- conf_level:

  Confidence level for intervals (default = 0.95).

- include_reference:

  Logical; whether to include reference levels for categorical
  predictors.

- conf_method:

  Determines whether the confidence intervals are calculated using the
  profile likelihood or the Wald method. Obviously has two options,
  "profile" and "wald". Wald is between 3 to 25 times as fast but not as
  reliable for small sample sizes (n \< 50). For larger sample sizes, (n
  \> 100), they will be very similar. **The default is Wald.**

## Value

A tibble of coefficients with confidence intervals and counts.
