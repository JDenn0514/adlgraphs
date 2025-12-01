# Create a data codebook

This function takes a `data.frame` and creates a codebook. The new
object is a `data.frame` where each row contains different metadata info
found within each column from the original data set. Each column in the
new data set represents a different element of the underlying metadata.
This is similar to
[`labelled::look_for()`](https://larmarange.github.io/labelled/reference/look_for.html)
but shows more of the underlying metadata.

## Usage

``` r
codebook(data)
```

## Arguments

- data:

  An object of class `data.frame` or `tibble`

## Value

A tibble with with the number of rows as columns in `data`

## Details

Currently, the new data set provided by this function has the following
columns:

- pos - The position of the variable.

- variable - The name of the variable.

- label - The variable label.

- levels - If the variable is a factor, the factor levels are listed
  here.

- value_labels - If the variable is a labelled vector, the value labels
  are listed here.

- transformation - An explanation of any potential data transformations
  the variable underwent is listed here. This useful if you want to
  remember how a variable was created without going back to the cleaning
  script.

- question_preface - This contains the question preface. To elaborate,
  some questions in surveys enable respondents to select multiple
  responses. Each response gets it's own variable in the data. The value
  listed here is supposed to contain the text that prefaced the response
  options. The actual response option is listed under 'label'.

- survey flow - This is used to indicate if there was an experiment or
  some sort of branching involved in the survey flow.

- note - A miscellaneous attribute in which you can add random
  information about the variable that doesn't fit in the other
  attributes.

- class - The class attribute of the variable.

- type - The type of the variable.

- missing - Indicates how many missing values there are.

- range - If a numeric variable, shows the range of the values.

## Examples

``` r
# create the codebook
test_data_codebook <- codebook(test_data)
#> Converted 'pid_f3' into a factor based on its value labelsRecoded 'edu' as a factor and set the levels based on their order.
#> The data transformation is as follows:
#> What was 'c(1:2)' has become 'High School or Less'
#> What was '3' has become 'Some College'
#> What was '4' has become 'Bachelor's Degree'
#> What was '5' has become 'Graduate Degree'Recoded 'edu' as a factor and set the levels based on their order.
#> The data transformation is as follows:
#> What was 'c(1, 2, 3)' has become 'No College Degree'
#> What was 'c(4, 5)' has become 'At Least a Bachelor's Degree'Reversing 'top' while maintaining correct value labelsReversing 'inferior' while maintaining correct value labelsFlipped the valance of `dominate` so it is now negative by updating the label and value labels accordingly.Flipped the valance of `deserving` so it is now negative by updating the label and value labels accordingly.Simple sum of `top_rev`, `inferior_rev`, `dominate_flip`, `deserving_flip`.Simple sum of `top_rev`, `inferior_rev`, `dominate_flip`, `deserving_flip`.Simple sum of `pol_part_rally`, `pol_part_worked`, `pol_part_contact`, `pol_part_money`, `pol_part_social`, `pol_part_attended`Reversing 'controlled' while maintaining correct value labelsReversing 'small' while maintaining correct value labelsReversing 'run' while maintaining correct value labelsReversing 'big_events' while maintaining correct value labelsSimple sum of `controlled_rev`, `small_rev`, `run_rev`, `big_events_rev`.Simple sum of `controlled_rev`, `small_rev`, `run_rev`, `big_events_rev`.
# view the codebook
test_data_codebook
#> # A tibble: 46 × 15
#>      pos variable      label levels value_labels transformation question_preface
#>    <int> <chr>         <chr> <name> <chr>        <named list>   <chr>           
#>  1     1 resp_id       Uniq… <NULL> ""           <NULL>          NA             
#>  2     2 wts           Surv… <NULL> ""           <NULL>          NA             
#>  3     3 edu           What… <NULL> "1 = ''Less… <NULL>          NA             
#>  4     4 pid_f3        Poli… <chr>  ""           <NULL>          NA             
#>  5     5 pol_part_ral… Atte… <NULL> ""           <NULL>         "In the last tw…
#>  6     6 pol_part_wor… Work… <NULL> ""           <NULL>         "In the last tw…
#>  7     7 pol_part_con… Cont… <NULL> ""           <NULL>         "In the last tw…
#>  8     8 pol_part_mon… Cont… <NULL> ""           <NULL>         "In the last tw…
#>  9     9 pol_part_soc… Publ… <NULL> ""           <NULL>         "In the last tw…
#> 10    10 pol_part_att… Atte… <NULL> ""           <NULL>         "In the last tw…
#> # ℹ 36 more rows
#> # ℹ 8 more variables: survey_flow <named list>, note <named list>,
#> #   col_type <chr>, class <named list>, type <chr>, missing <int>,
#> #   unique_values <int>, range <named list>

```
