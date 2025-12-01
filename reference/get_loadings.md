# Calculate the loadings in factor analysis

This function creates a data frame of factor loadings from a factor
analysis. It can be on an object that was created with
[`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html) or it can be used
on a data frame. If used on a data frame, it will run it on all columns
in the data frame. Also works on grouped data frame if you want to check
how the factor loadings may change along the different levels of a
group.

## Usage

``` r
get_loadings(
  data,
  cols,
  group,
  labels = NULL,
  threshold = 0.4,
  print = "short",
  nfactors = 1,
  fm = "pa",
  rotate = "oblimin",
  sort = TRUE,
  decimals = 3,
  na.rm = FALSE
)
```

## Arguments

- data:

  Either a data created using
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html) or a data.frame

- cols:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  The variables you want to get the correlations for.

- group:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  A selection of columns to group the data by in addition to `treats`.
  This operates very similarly to `.by` from dplyr (for more info on
  that see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html)). See
  examples to see how it operates.

- labels:

  Either a character vector or data frame. Creates a new column called
  "labels" for each variable in the factor analysis.

- threshold:

  The threshold with which to not show the factor loadings. Default is
  0.4.

- print:

  The printing method. Default is "short" which only prints a dataframe
  of the factor loadings. The alternative is "long" but that has not
  been created yet.

- nfactors:

  Number of factors to extract, default is 1.

- fm:

  Factoring method used in the factor analysis. Default is "pa". For
  more information on the various methods see the `fm` argument in the
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html) function.

- rotate:

  The type of factor rotation to perform when conducting the factor
  analysis. Default is "oblimin". For more information on the different
  rotation methods available, check the documentation for the `rotate`
  argument in [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html).

- sort:

  Logical. If `TRUE`, the default, the loadings are sorted largest to
  smallest. If `FALSE`, no sorting occurs.

- decimals:

  Number of decimals each number should be rounded to. Default is 3.

- na.rm:

  Logical. Default is `TRUE` which removes NAs prior to calculation.

## Value

A data.frame showing factor loadings.

## Examples

``` r
library(dplyr)
library(psych)

# first lets get our data set
data <- test_data %>% 
  select(top:run)

# now create the fa object
model <- fa(data, nfactors = 1, fm = "pa", rotate = "oblimin")
# get just the loadings
get_loadings(model)
#> # A tibble: 9 × 4
#>   variables     PA1 communality uniqueness
#> * <chr>       <dbl>       <dbl>      <dbl>
#> 1 controlled  0.685       0.469      0.531
#> 2 small       0.654       0.428      0.572
#> 3 run         0.61        0.373      0.627
#> 4 top        NA           0.086      0.914
#> 5 inferior   NA           0.122      0.878
#> 6 dominate   NA           0.021      0.979
#> 7 deserving  NA           0.001      0.999
#> 8 special    NA           0.088      0.912
#> 9 harder     NA           0.069      0.931
# get the loadings with the variable labels based on data object
get_loadings(model, data)
#> `cols` is only valid when supplying an object of class <data.frame>
#> You've supplied an object with class <psych/fa>
#> # A tibble: 9 × 4
#>   variables     PA1 communality uniqueness
#> * <chr>       <dbl>       <dbl>      <dbl>
#> 1 controlled  0.685       0.469      0.531
#> 2 small       0.654       0.428      0.572
#> 3 run         0.61        0.373      0.627
#> 4 top        NA           0.086      0.914
#> 5 inferior   NA           0.122      0.878
#> 6 dominate   NA           0.021      0.979
#> 7 deserving  NA           0.001      0.999
#> 8 special    NA           0.088      0.912
#> 9 harder     NA           0.069      0.931

# we can do all of this in one step with pipes
test_data %>% 
  # select only the variables we want in the factor analysis
  select(top:run) %>% 
  # run the factor analysis
  fa(., nfactors = 1, fm = "pa", rotate = "oblimin") %>% 
  # get the loadings
  get_loadings()
#> # A tibble: 9 × 4
#>   variables     PA1 communality uniqueness
#> * <chr>       <dbl>       <dbl>      <dbl>
#> 1 controlled  0.685       0.469      0.531
#> 2 small       0.654       0.428      0.572
#> 3 run         0.61        0.373      0.627
#> 4 top        NA           0.086      0.914
#> 5 inferior   NA           0.122      0.878
#> 6 dominate   NA           0.021      0.979
#> 7 deserving  NA           0.001      0.999
#> 8 special    NA           0.088      0.912
#> 9 harder     NA           0.069      0.931

# Now let's remove the threshold for the loadings and include labels
test_data %>% 
  # select only the variables we want in the factor analysis
  select(top:run) %>% 
  # run the factor analysis
  fa(., nfactors = 1, fm = "pa", rotate = "oblimin") %>% 
  # specify threshold is 0
  get_loadings(threshold = 0, labels = data)
#> # A tibble: 9 × 5
#>   variables  labels                                   PA1 communality uniqueness
#> * <chr>      <chr>                                  <dbl>       <dbl>      <dbl>
#> 1 controlled "Much of our lives are being control…  0.685       0.469      0.531
#> 2 small      "Even though we live in a democracy,…  0.654       0.428      0.572
#> 3 run        "The people who really \"run\" the c…  0.61        0.373      0.627
#> 4 inferior   "Some groups of people are simply in…  0.35        0.122      0.878
#> 5 special    "It is unfair for some groups in soc…  0.297       0.088      0.912
#> 6 top        "An ideal society requires some grou…  0.294       0.086      0.914
#> 7 harder     "I have a harder time succeeding tha…  0.262       0.069      0.931
#> 8 dominate   "No one group should dominate in soc…  0.146       0.021      0.979
#> 9 deserving  "Groups at the bottom are just as de… -0.033       0.001      0.999

# alternatively, we could skip the fa step entirely like so
test_data %>% 
  # select only the variables we want in the factor analysis
  select(top:run) %>% 
  # specify number of factors, rotation, and factor method
  get_loadings()
#> # A tibble: 9 × 5
#>   variables  labels                                   PA1 communality uniqueness
#>   <chr>      <chr>                                  <dbl>       <dbl>      <dbl>
#> 1 controlled "Much of our lives are being control…  0.685       0.469      0.531
#> 2 small      "Even though we live in a democracy,…  0.654       0.428      0.572
#> 3 run        "The people who really \"run\" the c…  0.61        0.373      0.627
#> 4 top        "An ideal society requires some grou… NA           0.086      0.914
#> 5 inferior   "Some groups of people are simply in… NA           0.122      0.878
#> 6 dominate   "No one group should dominate in soc… NA           0.021      0.979
#> 7 deserving  "Groups at the bottom are just as de… NA           0.001      0.999
#> 8 special    "It is unfair for some groups in soc… NA           0.088      0.912
#> 9 harder     "I have a harder time succeeding tha… NA           0.069      0.931

# we can also specify the number of factors, rotation, and factoring method
test_data %>% 
  # select only the variables we want in the factor analysis
  select(top:run) %>% 
  # specify number of factors, rotation, factor method, and threshold
  get_loadings(nfactors = 2, rotate = "varimax", fm = "minres", threshold = 0.2) 
#> # A tibble: 9 × 6
#>   variables  labels                            MR1    MR2 communality uniqueness
#>   <chr>      <chr>                           <dbl>  <dbl>       <dbl>      <dbl>
#> 1 run        "The people who really \"run\…  0.692 NA           0.503      0.497
#> 2 small      "Even though we live in a dem…  0.656 NA           0.43       0.57 
#> 3 controlled "Much of our lives are being …  0.642 NA           0.425      0.575
#> 4 special    "It is unfair for some groups…  0.294 NA           0.089      0.911
#> 5 inferior   "Some groups of people are si…  0.292  0.646       0.503      0.497
#> 6 dominate   "No one group should dominate…  0.263 -0.415       0.241      0.759
#> 7 harder     "I have a harder time succeed…  0.254 NA           0.065      0.935
#> 8 top        "An ideal society requires so…  0.225  0.587       0.395      0.605
#> 9 deserving  "Groups at the bottom are jus… NA     -0.429       0.187      0.813

# we can also calculate the factor loadings by a grouping variable
test_data %>% 
  # select the grouping variable and the variables to be used in factor analysis
  select(edu_f2, top:run) %>% 
  # group the data
  group_by(edu_f2) %>% 
  # specify number of factors, and the threshold
  get_loadings(nfactors = 2, threshold = 0.2) 
#> Loading required namespace: GPArotation
#> # A tibble: 18 × 7
#>    edu_f2                  variables labels    PA1    PA2 communality uniqueness
#>    <chr>                   <chr>     <chr>   <dbl>  <dbl>       <dbl>      <dbl>
#>  1 No College Degree       run       "The …  0.73  NA           0.526      0.474
#>  2 No College Degree       small     "Even…  0.661 NA           0.454      0.546
#>  3 No College Degree       controll… "Much…  0.569  0.22        0.402      0.598
#>  4 No College Degree       dominate  "No o…  0.484 -0.329       0.304      0.696
#>  5 No College Degree       harder    "I ha…  0.287 NA           0.106      0.894
#>  6 No College Degree       special   "It i…  0.218 NA           0.096      0.904
#>  7 No College Degree       inferior  "Some… NA      0.687       0.48       0.52 
#>  8 No College Degree       top       "An i… NA      0.639       0.415      0.585
#>  9 No College Degree       deserving "Grou… NA     -0.489       0.236      0.764
#> 10 At Least a Bachelor's … run       "The …  0.654 NA           0.413      0.587
#> 11 At Least a Bachelor's … small     "Even…  0.637 NA           0.429      0.571
#> 12 At Least a Bachelor's … controll… "Much…  0.633 NA           0.457      0.543
#> 13 At Least a Bachelor's … special   "It i…  0.336 NA           0.112      0.888
#> 14 At Least a Bachelor's … deserving "Grou…  0.298 -0.433       0.229      0.771
#> 15 At Least a Bachelor's … inferior  "Some…  0.252  0.561       0.431      0.569
#> 16 At Least a Bachelor's … dominate  "No o…  0.228 -0.53        0.289      0.711
#> 17 At Least a Bachelor's … top       "An i… NA      0.557       0.367      0.633
#> 18 At Least a Bachelor's … harder    "I ha… NA     NA           0.022      0.978

# let's repeat the previous analysis, but use internal arguments to select the 
# columns to include in the factor analysis and the grouping variables
test_data %>% 
  # group indicates we want to run the factor analysis across each level in edu_f2
  # nfactors specifies we want two factors
  # threshold sets the cut off for the loadings
  get_loadings(
    # specify the variables to include in the factor analysis
    cols = c(top:run), 
    # specify the group variable, run separate factor analyses for each level
    group = edu_f2, 
    # specify two factors
    nfactors = 2, 
    # specify the loadings are cut off at |0.2|
    threshold = 0.2
  ) 
#> # A tibble: 18 × 7
#>    edu_f2                  variables labels    PA1    PA2 communality uniqueness
#>    <chr>                   <chr>     <chr>   <dbl>  <dbl>       <dbl>      <dbl>
#>  1 No College Degree       run       "The …  0.73  NA           0.526      0.474
#>  2 No College Degree       small     "Even…  0.661 NA           0.454      0.546
#>  3 No College Degree       controll… "Much…  0.569  0.22        0.402      0.598
#>  4 No College Degree       dominate  "No o…  0.484 -0.329       0.304      0.696
#>  5 No College Degree       harder    "I ha…  0.287 NA           0.106      0.894
#>  6 No College Degree       special   "It i…  0.218 NA           0.096      0.904
#>  7 No College Degree       inferior  "Some… NA      0.687       0.48       0.52 
#>  8 No College Degree       top       "An i… NA      0.639       0.415      0.585
#>  9 No College Degree       deserving "Grou… NA     -0.489       0.236      0.764
#> 10 At Least a Bachelor's … run       "The …  0.654 NA           0.413      0.587
#> 11 At Least a Bachelor's … small     "Even…  0.637 NA           0.429      0.571
#> 12 At Least a Bachelor's … controll… "Much…  0.633 NA           0.457      0.543
#> 13 At Least a Bachelor's … special   "It i…  0.336 NA           0.112      0.888
#> 14 At Least a Bachelor's … deserving "Grou…  0.298 -0.433       0.229      0.771
#> 15 At Least a Bachelor's … inferior  "Some…  0.252  0.561       0.431      0.569
#> 16 At Least a Bachelor's … dominate  "No o…  0.228 -0.53        0.289      0.711
#> 17 At Least a Bachelor's … top       "An i… NA      0.557       0.367      0.633
#> 18 At Least a Bachelor's … harder    "I ha… NA     NA           0.022      0.978


```
