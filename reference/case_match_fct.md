# `case_match` with factor levels

Recode a variable using the
[`dplyr::case_match()`](https://dplyr.tidyverse.org/reference/case_match.html)
syntax

## Usage

``` r
case_match_fct(x, ..., .default = NULL)
```

## Arguments

- x:

  A vector to match against.

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  A sequence of two-sided formulas: `old_values ~ new_value`. The right
  hand side (RHS) determines the output value for all values of `.x`
  that match the left hand side (LHS).

  The LHS must evaluate to the same type of vector as `.x`. It can be
  any length, allowing you to map multiple `.x` values to the same RHS
  value. If a value is repeated in the LHS, i.e. a value in `.x` matches
  to multiple cases, the first match is used.

  The RHS inputs will be coerced to their common type. Each RHS input
  will be
  [recycled](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
  to the size of `.x`.

- .default:

  The value used when values in `.x` aren't matched by any of the LHS
  inputs. If `NULL`, the default, a `NA` will be used.

## Value

A factor vector with the same size as `.x` and the same type as the
common type of the RHS inputs and `.default` and levels as defined by
the order of the RHS inputs.

## See also

[`case_when_fct()`](https://jdenn0514.github.io/adlgraphs/reference/case_when_fct.md)

## Examples

``` r
# import dplyr so we can use their starwars dataset
library(dplyr)
# create a vector with a variable label
species <- starwars$species %>% structure(label = "This is a variable label")

new_species <- case_match_fct(
  species,
  "Human" ~ "Humanoid",
  "Droid" ~ "Robot",
  c("Wookiee", "Ewok") ~ "Hairy",
  .default = "Other"
)

# now let's check to see that it added the transformation metadata and the
#variable label
str(new_species)
#>  Factor w/ 4 levels "Humanoid","Robot",..: 1 2 2 1 1 1 1 2 1 1 ...
#>  - attr(*, "label")= chr "This is a variable label"
#>  - attr(*, "transformation")= chr "Recoded 'species' as a factor and set the levels based on their order.\nThe data transformation is as follows:\"| __truncated__

# now let's create a variable "new_species" and get the frequencies for it
# we can see the frequencies are in the same order we applied.
starwars %>%
  dplyr::mutate(
    new_species = case_match_fct(
      species,
      "Human" ~ "Humanoid",
      "Droid" ~ "Robot",
      c("Wookiee", "Ewok") ~ "Hairy",
      .default = "Other"
    )
  ) %>%
  dplyr::count(new_species)
#> # A tibble: 4 × 2
#>   new_species     n
#>   <fct>       <int>
#> 1 Humanoid       35
#> 2 Robot           6
#> 3 Hairy           3
#> 4 Other          43

# now let's do the same but with dplyr::case_match()
# we can see that the frequencies are in alphabetical order
starwars %>%
  dplyr::mutate(
    new_species = dplyr::case_match(
      species,
      "Human" ~ "Humanoid",
      "Droid" ~ "Robot",
      c("Wookiee", "Ewok") ~ "Hairy",
      .default = "Other"
    )
  ) %>%
  dplyr::count(new_species)
#> # A tibble: 4 × 2
#>   new_species     n
#>   <chr>       <int>
#> 1 Hairy           3
#> 2 Humanoid       35
#> 3 Other          43
#> 4 Robot           6
```
