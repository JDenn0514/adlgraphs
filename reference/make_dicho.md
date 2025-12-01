# Make dichotomous factors

Convert a vector of class `factor` or `numeric` with value labels (e.g.,
[`haven_labelled`](https://haven.tidyverse.org/reference/labelled.html))
to a dichotomous factor vector. A dichotomous factor vector is a vector
of class `factor` with only two bipolar levels (e.g., "Agree" and
"Disagree")

## Usage

``` r
make_dicho(x, flip_levels = FALSE)
```

## Arguments

- x:

  A labelled vector or `factor`.

- flip_levels:

  Logical. If `FALSE`, the default, the factor levels are kept the same.
  If `TRUE`, the factor levels are flipped. Only specify this if you
  want to change the order of the factor level.

## Value

A factor vector with two levels of same length as `x`.

## Details

`make_dicho` was designed to work on any vector that is of class
`factor`,
[`haven_labelled`](https://haven.tidyverse.org/reference/labelled.html),
or `numeric` with value labels. If the vector is numeric with no value
labels, the function will return an error. This is because the function
first converts the vector to a factor using
[`make_factor()`](https://jdenn0514.github.io/adlgraphs/reference/make_factor.md).
Then, it removes the first word if there are multiple words in the
factor level.

The resulting factor levels default to alphabetical but if you want to
reverse them, just set `flip_levels = TRUE` in the function.

In addition, this function adds two new attributes. The first attribute,
`transformation`, indicates the data transformation that the original
vector underwent to create this new vector. The second attribute,
`label`, contains the variable label that was found in the original
variable. However, if the original vector did not have a variable label,
then this attribute will not show up.

## Examples

``` r
# load the libraries
library(dplyr)
library(adlgraphs)

# Check a labelled variable -------------------------------------------------

# convert "top" into a dichotomous factor
new_df <- test_data %>%
  mutate(top_f2 = make_dicho(top)) %>%
  # keep only two relevant variables
  select(top, top_f2)

# compare "top" to "top_f2"
# we can see all response with "agree" in the name are now "agree" and
# all those with "disagree" are now "disagree"
new_df
#> # A tibble: 250 × 2
#>    top                   top_f2  
#>    <dbl+lbl>             <fct>   
#>  1 1 [Strongly agree]    Agree   
#>  2 2 [Somewhat agree]    Agree   
#>  3 2 [Somewhat agree]    Agree   
#>  4 3 [Somewhat disagree] Disagree
#>  5 2 [Somewhat agree]    Agree   
#>  6 4 [Strongly disagree] Disagree
#>  7 2 [Somewhat agree]    Agree   
#>  8 2 [Somewhat agree]    Agree   
#>  9 2 [Somewhat agree]    Agree   
#> 10 4 [Strongly disagree] Disagree
#> # ℹ 240 more rows

# show the attributes
attributes(new_df$top_f2)
#> $levels
#> [1] "Agree"    "Disagree"
#> 
#> $class
#> [1] "factor"
#> 
#> $transformation
#> [1] "Converting 'top' to a dichotomous factor with 'Agree' as the reference level"
#> 
#> $label
#> [1] "An ideal society requires some groups to be on top and others to be on the bottom"
#> 

# Check a factor variable ---------------------------------------------------
new_df <- test_data %>%
  mutate(
    # convert it to a factor
    top_f = make_factor(top),
    # convert the dichotomous factor
    top_f2 = make_dicho(top_f)
  ) %>%
  # select the three variables to compare them
  select(top, top_f, top_f2)

# compare the three variables
new_df
#> # A tibble: 250 × 3
#>    top                   top_f             top_f2  
#>    <dbl+lbl>             <fct>             <fct>   
#>  1 1 [Strongly agree]    Strongly agree    Agree   
#>  2 2 [Somewhat agree]    Somewhat agree    Agree   
#>  3 2 [Somewhat agree]    Somewhat agree    Agree   
#>  4 3 [Somewhat disagree] Somewhat disagree Disagree
#>  5 2 [Somewhat agree]    Somewhat agree    Agree   
#>  6 4 [Strongly disagree] Strongly disagree Disagree
#>  7 2 [Somewhat agree]    Somewhat agree    Agree   
#>  8 2 [Somewhat agree]    Somewhat agree    Agree   
#>  9 2 [Somewhat agree]    Somewhat agree    Agree   
#> 10 4 [Strongly disagree] Strongly disagree Disagree
#> # ℹ 240 more rows

# show the attributes for the new variable
attributes(new_df$top_f2)
#> $levels
#> [1] "Agree"    "Disagree"
#> 
#> $class
#> [1] "factor"
#> 
#> $transformation
#> [1] "Converting 'top_f' to a dichotomous factor with 'Agree' as the reference level"
#> 
#> $label
#> [1] "An ideal society requires some groups to be on top and others to be on the bottom"
#> 

# Show it with flipped levels -----------------------------------------------

# let's do the same thing but let's flip the levels now
new_df <- test_data %>%
  mutate(
    # show it without flipping the levels
    top_f2 = make_dicho(top),
    # show it with the levels being flipped
    top_f2_flip = make_dicho(top, flip_levels = TRUE)
  ) %>%
  # keep only relevant variables
  select(top, top_f2, top_f2_flip)

# compare them
new_df
#> # A tibble: 250 × 3
#>    top                   top_f2   top_f2_flip
#>    <dbl+lbl>             <fct>    <fct>      
#>  1 1 [Strongly agree]    Agree    Agree      
#>  2 2 [Somewhat agree]    Agree    Agree      
#>  3 2 [Somewhat agree]    Agree    Agree      
#>  4 3 [Somewhat disagree] Disagree Disagree   
#>  5 2 [Somewhat agree]    Agree    Agree      
#>  6 4 [Strongly disagree] Disagree Disagree   
#>  7 2 [Somewhat agree]    Agree    Agree      
#>  8 2 [Somewhat agree]    Agree    Agree      
#>  9 2 [Somewhat agree]    Agree    Agree      
#> 10 4 [Strongly disagree] Disagree Disagree   
#> # ℹ 240 more rows

# They look the same but if we check the levels of the factor we can see
# that they are in different orders
attributes(new_df$top_f2)
#> $levels
#> [1] "Agree"    "Disagree"
#> 
#> $class
#> [1] "factor"
#> 
#> $transformation
#> [1] "Converting 'top' to a dichotomous factor with 'Agree' as the reference level"
#> 
#> $label
#> [1] "An ideal society requires some groups to be on top and others to be on the bottom"
#> 
attributes(new_df$top_f2_flip)
#> $levels
#> [1] "Disagree" "Agree"   
#> 
#> $class
#> [1] "factor"
#> 
#> $transformation
#> [1] "Converting 'top' to a dichotomous factor and reordering the factor levels so that 'Disagree' is the reference level"
#> 
#> $label
#> [1] "An ideal society requires some groups to be on top and others to be on the bottom"
#> 

# ----------------------------------------------------------------------------
# function also works inside dplyr::across()

# Create new columns using `across()`
new_df <- test_data %>%
  dplyr::mutate(
    # use this example if you don't want to flip the factor levels
    dplyr::across(
      c(top:deserving),
      make_dicho,
      .names = "{col}_f2"
    ),
    # if you want to flip the factor levels, follow this example
    dplyr::across(
      c(top:deserving),
      ~make_dicho(., flip_levels = TRUE),
      .names = "{col}_f2_flip"
    )
  ) %>%
  # select the variables with "f2" in the name
  select(contains("f2"))

# show that the function worked properly by creating two new sets of variables
new_df
#> # A tibble: 250 × 9
#>    edu_f2                top_f2 inferior_f2 dominate_f2 deserving_f2 top_f2_flip
#>    <fct>                 <fct>  <fct>       <fct>       <fct>        <fct>      
#>  1 No College Degree     Agree  Disagree    Agree       Agree        Agree      
#>  2 No College Degree     Agree  Disagree    Agree       Agree        Agree      
#>  3 At Least a Bachelor'… Agree  Agree       Agree       Agree        Agree      
#>  4 No College Degree     Disag… Disagree    Agree       Agree        Disagree   
#>  5 No College Degree     Agree  Disagree    Agree       Disagree     Agree      
#>  6 At Least a Bachelor'… Disag… Disagree    Agree       Agree        Disagree   
#>  7 At Least a Bachelor'… Agree  Disagree    Agree       Agree        Agree      
#>  8 At Least a Bachelor'… Agree  Disagree    Agree       Agree        Agree      
#>  9 No College Degree     Agree  Disagree    Disagree    Agree        Agree      
#> 10 No College Degree     Disag… Disagree    Agree       Agree        Disagree   
#> # ℹ 240 more rows
#> # ℹ 3 more variables: inferior_f2_flip <fct>, dominate_f2_flip <fct>,
#> #   deserving_f2_flip <fct>

# show the underlying structure of the entire df
str(new_df)
#> tibble [250 × 9] (S3: tbl_df/tbl/data.frame)
#>  $ edu_f2           : Factor w/ 2 levels "No College Degree",..: 1 1 2 1 1 2 2 2 1 1 ...
#>   ..- attr(*, "label")= chr "What is the highest level of school you have completed or the highest degree you have received?"
#>   ..- attr(*, "transformation")= 'glue' chr "Recoded 'edu' as a factor and set the levels based on their order.\nThe data transformation is as follows:\nWha"| __truncated__
#>  $ top_f2           : Factor w/ 2 levels "Agree","Disagree": 1 1 1 2 1 2 1 1 1 2 ...
#>   ..- attr(*, "transformation")= chr "Converting 'top' to a dichotomous factor with 'Agree' as the reference level"
#>   ..- attr(*, "label")= chr "An ideal society requires some groups to be on top and others to be on the bottom"
#>  $ inferior_f2      : Factor w/ 2 levels "Agree","Disagree": 2 2 1 2 2 2 2 2 2 2 ...
#>   ..- attr(*, "transformation")= chr "Converting 'inferior' to a dichotomous factor with 'Agree' as the reference level"
#>   ..- attr(*, "label")= chr "Some groups of people are simply inferior to other groups"
#>  $ dominate_f2      : Factor w/ 2 levels "Agree","Disagree": 1 1 1 1 1 1 1 1 2 1 ...
#>   ..- attr(*, "transformation")= chr "Converting 'dominate' to a dichotomous factor with 'Agree' as the reference level"
#>   ..- attr(*, "label")= chr "No one group should dominate in society"
#>  $ deserving_f2     : Factor w/ 2 levels "Agree","Disagree": 1 1 1 1 2 1 1 1 1 1 ...
#>   ..- attr(*, "transformation")= chr "Converting 'deserving' to a dichotomous factor with 'Agree' as the reference level"
#>   ..- attr(*, "label")= chr "Groups at the bottom are just as deserving as groups at the top"
#>  $ top_f2_flip      : Factor w/ 2 levels "Disagree","Agree": 2 2 2 1 2 1 2 2 2 1 ...
#>   ..- attr(*, "transformation")= chr "Converting 'top' to a dichotomous factor and reordering the factor levels so that 'Disagree' is the reference level"
#>   ..- attr(*, "label")= chr "An ideal society requires some groups to be on top and others to be on the bottom"
#>  $ inferior_f2_flip : Factor w/ 2 levels "Disagree","Agree": 1 1 2 1 1 1 1 1 1 1 ...
#>   ..- attr(*, "transformation")= chr "Converting 'inferior' to a dichotomous factor and reordering the factor levels so that 'Disagree' is the reference level"
#>   ..- attr(*, "label")= chr "Some groups of people are simply inferior to other groups"
#>  $ dominate_f2_flip : Factor w/ 2 levels "Disagree","Agree": 2 2 2 2 2 2 2 2 1 2 ...
#>   ..- attr(*, "transformation")= chr "Converting 'dominate' to a dichotomous factor and reordering the factor levels so that 'Disagree' is the reference level"
#>   ..- attr(*, "label")= chr "No one group should dominate in society"
#>  $ deserving_f2_flip: Factor w/ 2 levels "Disagree","Agree": 2 2 2 2 1 2 2 2 2 2 ...
#>   ..- attr(*, "transformation")= chr "Converting 'deserving' to a dichotomous factor and reordering the factor levels so that 'Disagree' is the reference level"
#>   ..- attr(*, "label")= chr "Groups at the bottom are just as deserving as groups at the top"

# show how the levels are flipped when "flip_levels = TRUE"
levels(new_df$top_f2)
#> [1] "Agree"    "Disagree"
levels(new_df$top_f2_flip)
#> [1] "Disagree" "Agree"   
```
