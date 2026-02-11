# Calculate confidence intervals for lm, glm, and svyglm

This function calculates confidence intervals for `lm`, `glm`, `svyglm`,
and `svystat` objects. The `svyglm`and `svystat` methods are based on
code from the`survey` package. In addition, there are two methods: wald
and profile likelihood.

## Usage

``` r
calc_ci(model, conf_level = 0.95, method = c("wald", "profile"), df = Inf)
```

## Arguments

- model:

  The model/data object

- conf_level:

  The confidence interval, default is 0.95

- method:

  The method for calculating CIs

- df:

  Degrees of freedom
