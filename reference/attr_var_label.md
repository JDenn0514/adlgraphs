# Get variable label

This function makes it easy to get the variable labels from either an
individual vector or a data frame. NOTE: it is not possible to set or
modify the variable labels with this function.

## Usage

``` r
attr_var_label(data, x, unlist = TRUE, if_null = NULL)
```

## Arguments

- data:

  A `data.frame` or `tibble` object. This should only be specified when
  `x` is only the name of a column in a `data.frame`.

- x:

  A vector object, the name of a column in a `data.frame`, or an an
  actual `data.frame` object.

- unlist:

  Logical. If `TRUE`, the default, returns a named vector. If `FALSE`,
  returns a list. This only works when `x` is a `data.frame`

- if_null:

  String. This determines what to return should there be no variable
  label. There are three options:

      - `NULL` - This is the default. Will return `NULL`

      - "name" - This returns the variable name

      - "NA" - This returns an `NA` value

## Value

If `x` is a variable or vector, a string containing the "label"
attribute, if one is present, is returned. If `x` is a `data.frame` then
a named vector or list with the "label" attribute from each variable is
returned.

## Examples

``` r
# load dplyr so we can see how it might work in a typical workflow
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
# check for an individual vector
attr_var_label(test_data$top)
#> [1] "An ideal society requires some groups to be on top and others to be on the bottom"
# get the variable label for the entire data set
attr_var_label(test_data)
#>                                                                                                                                                     resp_id 
#>                                                                                                                             "Unique ID for each Respondent" 
#>                                                                                                                                                         wts 
#>                                                                                                                                 "Survey weighting variable" 
#>                                                                                                                                                         edu 
#>                                                           "What is the highest level of school you have completed or the highest degree you have received?" 
#>                                                                                                                                                      pid_f3 
#>                                                                                                                                    "Political Partisanship" 
#>                                                                                                                                              pol_part_rally 
#>                                                                                                      "Attended a political rally, speech or campaign event" 
#>                                                                                                                                             pol_part_worked 
#>                                                                                        "Worked or volunteered for a political party, candidate or campaign" 
#>                                                                                                                                            pol_part_contact 
#>                                                                                                                             "Contacted an elected official" 
#>                                                                                                                                              pol_part_money 
#>                                                     "Contributed money to a candidate running for public office or to a group working to elect a candidate" 
#>                                                                                                                                             pol_part_social 
#>                                        "Publicly expressed your support for a political candidate on Facebook, Twitter/X, Instagram, or other social media" 
#>                                                                                                                                           pol_part_attended 
#>                                                                     "Attended government meetings in your community, such as city or town council meetings" 
#>                                                                                                                                               pol_part_none 
#>                                                                                                                                         "None of the above" 
#>                                                                                                                                                         top 
#>                                                                         "An ideal society requires some groups to be on top and others to be on the bottom" 
#>                                                                                                                                                    inferior 
#>                                                                                                 "Some groups of people are simply inferior to other groups" 
#>                                                                                                                                                    dominate 
#>                                                                                                                   "No one group should dominate in society" 
#>                                                                                                                                                   deserving 
#>                                                                                           "Groups at the bottom are just as deserving as groups at the top" 
#>                                                                                                                                                     special 
#>                                                                  "It is unfair for some groups in society to receive special treatment from the government" 
#>                                                                                                                                                      harder 
#>                                                                                                       "I have a harder time succeeding than my parents did" 
#>                                                                                                                                                  controlled 
#>                                                                                        "Much of our lives are being controlled by plots hatched in secrecy" 
#>                                                                                                                                                       small 
#>                                                                            "Even though we live in a democracy, a few people will always run things anyway" 
#>                                                                                                                                                         run 
#>                                                                                     "The people who really \"run\" the country are not known to the voters" 
#>                                                                                                                                                  big_events 
#> "Big events like wars, recessions, and the outcomes of elections are controlled by small groups of people who are working in secret against the rest of us" 
#>                                                                                                                                                accept_hamas 
#>                                                              "If you had a close family member who supported Hamas, would your family and friends find it…" 
#>                                                                                                                                                  accept_isr 
#>                                                             "If you had a close family member who supported Israel, would your family and friends find it…" 
#>                                                                                                                                                dislike_jews 
#>                                                                                                        "How many of your close friends/family dislike Jews" 
#>                                                                                                                                              stick_together 
#>                                                                                                             "Jews stick together more than other Americans" 
#>                                                                                                                                                      values 
#>                                                                                                                                     "Jews share my values." 
#>                                                                                                                                                        head 
#>                                                                                                             "Jews always like to be at the head of things." 
#>                                                                                                                                                       loyal 
#>                                                                                                            "Jews are more loyal to Israel than to America." 
#>                                                                                                                                              business_power 
#>                                                                                                           "Jews have too much power in the business world." 
#>                                                                                                                                                 wall_street 
#>                                                                                                  "Jews have too much control and influence on Wall Street." 
#>                                                                                                                                                      trad_n 
#>                                                                                                                                                 "ADL Index" 
#>                                                                                                                                                       edu_f 
#>                                                           "What is the highest level of school you have completed or the highest degree you have received?" 
#>                                                                                                                                                      edu_f2 
#>                                                           "What is the highest level of school you have completed or the highest degree you have received?" 
#>                                                                                                                                                     top_rev 
#>                                                                         "An ideal society requires some groups to be on top and others to be on the bottom" 
#>                                                                                                                                                inferior_rev 
#>                                                                                                 "Some groups of people are simply inferior to other groups" 
#>                                                                                                                                               dominate_flip 
#>                                                                                                                      "One group should dominate in society" 
#>                                                                                                                                              deserving_flip 
#>                                                                                       "Groups at the bottom are not just as deserving as groups at the top" 
#>                                                                                                                                                     sdo_sum 
#>                                                                                                                              "Social Dominance Orientation" 
#>                                                                                                                                                     sdo_avg 
#>                                                                                                                              "Social Dominance Orientation" 
#>                                                                                                                                                pol_part_sum 
#>                                                                                                                                   "Political Participation" 
#>                                                                                                                                              controlled_rev 
#>                                                                                        "Much of our lives are being controlled by plots hatched in secrecy" 
#>                                                                                                                                                   small_rev 
#>                                                                            "Even though we live in a democracy, a few people will always run things anyway" 
#>                                                                                                                                                     run_rev 
#>                                                                                     "The people who really \"run\" the country are not known to the voters" 
#>                                                                                                                                              big_events_rev 
#> "Big events like wars, recessions, and the outcomes of elections are controlled by small groups of people who are working in secret against the rest of us" 
#>                                                                                                                                                    acts_sum 
#>                                                                                                                                  "Conspiracy Theory Belief" 
#>                                                                                                                                                    acts_avg 
#>                                                                                                                                  "Conspiracy Theory Belief" 
# same, but as a list
attr_var_label(test_data, unlist = FALSE)
#> $resp_id
#> [1] "Unique ID for each Respondent"
#> 
#> $wts
#> [1] "Survey weighting variable"
#> 
#> $edu
#> [1] "What is the highest level of school you have completed or the highest degree you have received?"
#> 
#> $pid_f3
#> [1] "Political Partisanship"
#> 
#> $pol_part_rally
#> [1] "Attended a political rally, speech or campaign event"
#> 
#> $pol_part_worked
#> [1] "Worked or volunteered for a political party, candidate or campaign"
#> 
#> $pol_part_contact
#> [1] "Contacted an elected official"
#> 
#> $pol_part_money
#> [1] "Contributed money to a candidate running for public office or to a group working to elect a candidate"
#> 
#> $pol_part_social
#> [1] "Publicly expressed your support for a political candidate on Facebook, Twitter/X, Instagram, or other social media"
#> 
#> $pol_part_attended
#> [1] "Attended government meetings in your community, such as city or town council meetings"
#> 
#> $pol_part_none
#> [1] "None of the above"
#> 
#> $top
#> [1] "An ideal society requires some groups to be on top and others to be on the bottom"
#> 
#> $inferior
#> [1] "Some groups of people are simply inferior to other groups"
#> 
#> $dominate
#> [1] "No one group should dominate in society"
#> 
#> $deserving
#> [1] "Groups at the bottom are just as deserving as groups at the top"
#> 
#> $special
#> [1] "It is unfair for some groups in society to receive special treatment from the government"
#> 
#> $harder
#> [1] "I have a harder time succeeding than my parents did"
#> 
#> $controlled
#> [1] "Much of our lives are being controlled by plots hatched in secrecy"
#> 
#> $small
#> [1] "Even though we live in a democracy, a few people will always run things anyway"
#> 
#> $run
#> [1] "The people who really \"run\" the country are not known to the voters"
#> 
#> $big_events
#> [1] "Big events like wars, recessions, and the outcomes of elections are controlled by small groups of people who are working in secret against the rest of us"
#> 
#> $accept_hamas
#> [1] "If you had a close family member who supported Hamas, would your family and friends find it…"
#> 
#> $accept_isr
#> [1] "If you had a close family member who supported Israel, would your family and friends find it…"
#> 
#> $dislike_jews
#> [1] "How many of your close friends/family dislike Jews"
#> 
#> $stick_together
#> [1] "Jews stick together more than other Americans"
#> 
#> $values
#> [1] "Jews share my values."
#> 
#> $head
#> [1] "Jews always like to be at the head of things."
#> 
#> $loyal
#> [1] "Jews are more loyal to Israel than to America."
#> 
#> $business_power
#> [1] "Jews have too much power in the business world."
#> 
#> $wall_street
#> [1] "Jews have too much control and influence on Wall Street."
#> 
#> $trad_n
#> [1] "ADL Index"
#> 
#> $edu_f
#> [1] "What is the highest level of school you have completed or the highest degree you have received?"
#> 
#> $edu_f2
#> [1] "What is the highest level of school you have completed or the highest degree you have received?"
#> 
#> $top_rev
#> [1] "An ideal society requires some groups to be on top and others to be on the bottom"
#> 
#> $inferior_rev
#> [1] "Some groups of people are simply inferior to other groups"
#> 
#> $dominate_flip
#> [1] "One group should dominate in society"
#> 
#> $deserving_flip
#> [1] "Groups at the bottom are not just as deserving as groups at the top"
#> 
#> $sdo_sum
#> [1] "Social Dominance Orientation"
#> 
#> $sdo_avg
#> [1] "Social Dominance Orientation"
#> 
#> $pol_part_sum
#> [1] "Political Participation"
#> 
#> $controlled_rev
#> [1] "Much of our lives are being controlled by plots hatched in secrecy"
#> 
#> $small_rev
#> [1] "Even though we live in a democracy, a few people will always run things anyway"
#> 
#> $run_rev
#> [1] "The people who really \"run\" the country are not known to the voters"
#> 
#> $big_events_rev
#> [1] "Big events like wars, recessions, and the outcomes of elections are controlled by small groups of people who are working in secret against the rest of us"
#> 
#> $acts_sum
#> [1] "Conspiracy Theory Belief"
#> 
#> $acts_avg
#> [1] "Conspiracy Theory Belief"
#> 

# now let's do it on a variable without a label
top <- sample(c(1:3), 10, replace = TRUE)
# if no label is present and if_null = "name", it will use the variable name
attr_var_label(top, if_null = "name")
#>  [1] 3 2 1 1 3 2 1 1 3 3
# if it's se to "NA" it will give NA
attr_var_label(top, if_null = "NA")
#> [1] NA

```
