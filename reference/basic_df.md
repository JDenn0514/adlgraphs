# Example Survey Data

A small example dataset containing simulated survey responses with
survey design variables, demographics, and various question types.
Variables include labels and value labels as attributes, mimicking data
imported from SPSS or other statistical software.

## Usage

``` r
basic_df
```

## Format

A tibble with 12 rows and 31 variables:

- id, id2:

  Respondent identifiers

- grp:

  Grouping variable (A, B, C)

- strata, strata2:

  Stratification variables

- psu:

  Primary sampling unit (1-6)

- ssu:

  Secondary sampling unit (1-12)

- fpc_psu, fpc_ssu:

  Finite population correction values for each stage

- wts, w2:

  Survey weights

- age:

  Respondent age in years

- income:

  Annual income in dollars

- satisfaction_service, satisfaction_price, satisfaction_quality,
  satisfaction_support:

  Satisfaction ratings (1-5 Likert scale)

- agree_recommend, agree_repurchase, agree_trust:

  Agreement ratings (1-7 scale)

- freq_use_product, freq_visit_store, freq_contact_support:

  Frequency counts (times per month)

- x1, x2:

  Binary yes/no questions

- x3:

  Categorical variable (1 or 2)

- x4:

  Logical variable

- rating_overall, rating_value, rating_experience:

  Rating scales (0-10)

## Details

Each survey question variable has the following attributes:

- `label`: A descriptive label for the variable

- `labels`: Named vector of value labels

- `question_preface`: The question stem shown to respondents

## Examples

``` r
# View the data
basic_df
#> # A tibble: 12 × 30
#>       id   id2 grp   strata strata2   psu   ssu fpc_psu fpc_ssu   wts    w2
#>    <int> <int> <chr>  <int> <chr>   <int> <int>   <int>   <int> <dbl> <dbl>
#>  1     1     1 A          1 X           1     1      15       8     1   1.1
#>  2     2     2 A          1 Y           1     2      15       8     2   1.1
#>  3     3     3 A          1 Z           2     3      15       8     1   1.1
#>  4     4     4 A          1 X           2     4      15       8     1   1.1
#>  5     5     5 B          2 Y           3     5      15       8     1   0.9
#>  6     6     6 B          2 Z           3     6      15       8     3   0.9
#>  7     7     1 B          2 X           4     7      15       8     1   0.9
#>  8     8     2 B          2 Y           4     8      15       8     2   0.9
#>  9     9     3 C          3 Z           5     9      15       8     2   1.2
#> 10    10     4 C          3 X           5    10      15       8     1   1.2
#> 11    11     5 C          3 Y           6    11      15       8     3   1.2
#> 12    12     6 C          3 Z           6    12      15       8     1   1.2
#> # ℹ 19 more variables: age <dbl>, income <dbl>, satisfaction_service <dbl>,
#> #   satisfaction_price <dbl>, satisfaction_quality <dbl>,
#> #   satisfaction_support <dbl>, agree_recommend <dbl>, agree_repurchase <dbl>,
#> #   agree_trust <dbl>, freq_use_product <dbl>, freq_visit_store <dbl>,
#> #   freq_contact_support <dbl>, x1 <chr>, x2 <chr>, x3 <int>, x4 <lgl>,
#> #   rating_overall <dbl>, rating_value <dbl>, rating_experience <dbl>

# Access variable label
attr_var_label(basic_df$satisfaction_service)
#> [1] "Satisfaction with Service"

# Access value labels
attr_val_labels(basic_df$satisfaction_service)
#> Very Dissatisfied      Dissatisfied           Neutral         Satisfied 
#>                 1                 2                 3                 4 
#>    Very Satisfied 
#>                 5 
```
