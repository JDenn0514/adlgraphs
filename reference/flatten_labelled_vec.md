# Flatten a labelled vector

This function "flattens" a labelled vector into a string, where each
label is equal to the value. For example if a vector has values 1 and 0
where 1 is "Yes" and 0 is "No", it will output a string that reads:
`1 = "Yes", 0 = "No"`. This can be done on an individual vector or on an
entire data frame. If the vector is not labelled then it will simply
return `NULL`.

## Usage

``` r
flatten_labelled_vec(x, data)
```

## Arguments

- x:

  A vector object, the name of a column in a `data.frame`, or an an
  actual `data.frame` object.

- data:

  A `data.frame` or `tibble` object. This should only be specified when
  `x` is only the name of a column in a `data.frame`.

## Value

A string if `x` is a vector or column, a named vector if `x` is a
data.frame.

## Examples

``` r
library(adlgraphs)
# run on a single vector
flatten_labelled_vec(test_data$inferior)
#> [1] "1 = ''Strongly agree', 2 = ''Somewhat agree', 3 = ''Somewhat disagree', 4 = ''Strongly disagree'"
# run on a data frame
flatten_labelled_vec(test_data)
#> $resp_id
#> [1] ""
#> 
#> $wts
#> [1] ""
#> 
#> $edu
#> [1] "1 = ''Less than High School', 2 = ''High School or GED', 3 = ''Some College', 4 = ''Bachelor's Degree', 5 = ''Graduate Degree'"
#> 
#> $pid_f3
#> [1] ""
#> 
#> $pol_part_rally
#> [1] ""
#> 
#> $pol_part_worked
#> [1] ""
#> 
#> $pol_part_contact
#> [1] ""
#> 
#> $pol_part_money
#> [1] ""
#> 
#> $pol_part_social
#> [1] ""
#> 
#> $pol_part_attended
#> [1] ""
#> 
#> $pol_part_none
#> [1] ""
#> 
#> $top
#> [1] "1 = ''Strongly agree', 2 = ''Somewhat agree', 3 = ''Somewhat disagree', 4 = ''Strongly disagree'"
#> 
#> $inferior
#> [1] "1 = ''Strongly agree', 2 = ''Somewhat agree', 3 = ''Somewhat disagree', 4 = ''Strongly disagree'"
#> 
#> $dominate
#> [1] "1 = ''Strongly agree', 2 = ''Somewhat agree', 3 = ''Somewhat disagree', 4 = ''Strongly disagree'"
#> 
#> $deserving
#> [1] "1 = ''Strongly agree', 2 = ''Somewhat agree', 3 = ''Somewhat disagree', 4 = ''Strongly disagree'"
#> 
#> $special
#> [1] "1 = ''Strongly agree', 2 = ''Somewhat agree', 3 = ''Somewhat disagree', 4 = ''Strongly disagree'"
#> 
#> $harder
#> [1] "1 = ''Strongly agree', 2 = ''Somewhat agree', 3 = ''Somewhat disagree', 4 = ''Strongly disagree'"
#> 
#> $controlled
#> [1] "1 = ''Strongly agree', 2 = ''Somewhat agree', 3 = ''Somewhat disagree', 4 = ''Strongly disagree'"
#> 
#> $small
#> [1] "1 = ''Strongly agree', 2 = ''Somewhat agree', 3 = ''Somewhat disagree', 4 = ''Strongly disagree'"
#> 
#> $run
#> [1] "1 = ''Strongly agree', 2 = ''Somewhat agree', 3 = ''Somewhat disagree', 4 = ''Strongly disagree'"
#> 
#> $big_events
#> [1] "1 = ''Strongly agree', 2 = ''Somewhat agree', 3 = ''Somewhat disagree', 4 = ''Strongly disagree'"
#> 
#> $accept_hamas
#> [1] "1 = ''Totally acceptable', 2 = ''Somewhat acceptable', 3 = ''Somewhat unacceptable', 4 = ''Totally unacceptable'"
#> 
#> $accept_isr
#> [1] "1 = ''Totally acceptable', 2 = ''Somewhat acceptable', 3 = ''Somewhat unacceptable', 4 = ''Totally unacceptable'"
#> 
#> $dislike_jews
#> [1] "1 = ''None', 2 = ''A few', 3 = ''Some', 4 = ''Many', 5 = ''All'"
#> 
#> $stick_together
#> [1] "1 = ''Mostly true', 2 = ''Somewhat true', 3 = ''Somewhat false', 4 = ''Mostly false'"
#> 
#> $values
#> [1] "1 = ''Mostly true', 2 = ''Somewhat true', 3 = ''Somewhat false', 4 = ''Mostly false'"
#> 
#> $head
#> [1] "1 = ''Mostly true', 2 = ''Somewhat true', 3 = ''Somewhat false', 4 = ''Mostly false'"
#> 
#> $loyal
#> [1] "1 = ''Mostly true', 2 = ''Somewhat true', 3 = ''Somewhat false', 4 = ''Mostly false'"
#> 
#> $business_power
#> [1] "1 = ''Mostly true', 2 = ''Somewhat true', 3 = ''Somewhat false', 4 = ''Mostly false'"
#> 
#> $wall_street
#> [1] "1 = ''Mostly true', 2 = ''Somewhat true', 3 = ''Somewhat false', 4 = ''Mostly false'"
#> 
#> $trad_n
#> [1] ""
#> 
#> $edu_f
#> [1] ""
#> 
#> $edu_f2
#> [1] ""
#> 
#> $top_rev
#> [1] "1 = ''Strongly disagree', 2 = ''Somewhat disagree', 3 = ''Somewhat agree', 4 = ''Strongly agree'"
#> 
#> $inferior_rev
#> [1] "1 = ''Strongly disagree', 2 = ''Somewhat disagree', 3 = ''Somewhat agree', 4 = ''Strongly agree'"
#> 
#> $dominate_flip
#> [1] "1 = ''Strongly disagree', 2 = ''Somewhat disagree', 3 = ''Somewhat agree', 4 = ''Strongly agree'"
#> 
#> $deserving_flip
#> [1] "1 = ''Strongly disagree', 2 = ''Somewhat disagree', 3 = ''Somewhat agree', 4 = ''Strongly agree'"
#> 
#> $sdo_sum
#> [1] ""
#> 
#> $sdo_avg
#> [1] ""
#> 
#> $pol_part_sum
#> [1] ""
#> 
#> $controlled_rev
#> [1] "1 = ''Strongly disagree', 2 = ''Somewhat disagree', 3 = ''Somewhat agree', 4 = ''Strongly agree'"
#> 
#> $small_rev
#> [1] "1 = ''Strongly disagree', 2 = ''Somewhat disagree', 3 = ''Somewhat agree', 4 = ''Strongly agree'"
#> 
#> $run_rev
#> [1] "1 = ''Strongly disagree', 2 = ''Somewhat disagree', 3 = ''Somewhat agree', 4 = ''Strongly agree'"
#> 
#> $big_events_rev
#> [1] "1 = ''Strongly disagree', 2 = ''Somewhat disagree', 3 = ''Somewhat agree', 4 = ''Strongly agree'"
#> 
#> $acts_sum
#> [1] ""
#> 
#> $acts_avg
#> [1] ""
#> 

```
