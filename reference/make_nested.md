# Nest a data frame or survey object by one or more grouping variables

`make_nested()` creates a nested tibble where each row corresponds to a
unique combination of one or more grouping variables. The resulting
tibble contains a list-column `data` with the subset of the original
data for that group.

This function has S3 methods for `data.frame`, `survey.design`, and
`svyrep.design` objects. For more information on how this function
differs from
[`tidyr::nest()`](https://tidyr.tidyverse.org/reference/nest.html) and
how it works for `survey.design` and `svyrep.design` objects, check the
details section below.

## Usage

``` r
make_nested(data, group, na.rm = TRUE, sep = "_")
```

## Arguments

- data:

  A data frame or survey design object (`survey.design`).

- group:

  One or more grouping variables. Can be supplied as bare names
  (unquoted) or a character vector of column names.

- na.rm:

  Logical, whether to remove rows with missing values in the grouping
  variables. Defaults to `TRUE`.

- sep:

  Character string used to separate levels when composing the `name`
  column for each group. Defaults to `"_"`.

## Value

A tibble with one row per unique combination of the grouping variables.
The tibble contains the following columns:

- The grouping variables

- `data`: a list-column with the subset of data or survey design for
  each group

- `name`: a string combining the levels of the grouping variables

## Details

While `make_nested()` operates similarly to
[`tidyr::nest()`](https://tidyr.tidyverse.org/reference/nest.html),
there are some important differences:

- It is optimized for survey data and preserves weights, strata, and PSU
  information when nesting `survey.design` objects and replication
  weights when nesting `svyrep.design` objects.

- It uses
  [`vctrs::vec_split()`](https://vctrs.r-lib.org/reference/vec_split.html)
  for faster grouping.

- The output always contains a `name` column that concatenates the
  levels of the grouping variables, which is useful for labeling and
  further analysis.

- Unlike
  [`tidyr::nest()`](https://tidyr.tidyverse.org/reference/nest.html),
  you cannot control which columns go into the inner list-column; the
  inner data frames always include all columns not used for grouping.

- Unlike
  [`tidyr::nest()`](https://tidyr.tidyverse.org/reference/nest.html)
  where `.by` supersedes any
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  grouping, `make_nested()` combines any existing
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  variables with the supplied `group` argument when nesting.

When nesting a `survey.design` object, each nested group is subsetted
and converted into a new `survey.design` object using
[`srvyr::as_survey_rep`](http://gdfe.co/srvyr/reference/as_survey_rep.md).
The `.data`, `ids`, `strata`, `weights`, `fpc`, and `nest` arguments are
specified.

**Note:** If a nested group contains only one primary sampling unit
(PSU), [`svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)
cannot compute variance estimates and the function will fail for that
group. This is most likely to occur with small datasets, highly granular
grouping variables, or a small number of PSUs per stratum. For typical
survey datasets with sufficient PSUs, this is unlikely to occur.

When nesting a `svyrep.design` object, each nested group is subsetted
and converted into a new `svyrep.design` object using
[`srvyr::as_survey_rep()`](http://gdfe.co/srvyr/reference/as_survey_rep.md).

**Note:** The finite population corrections (`fpc`) are not carried over
when

## Examples

``` r
# Example with a data frame
df <- basic_df
nested_df <- make_nested(df, grp)

# Nested by multiple variables
nested_df2 <- make_nested(df, c(grp, x2))

# Example with a survey.design object
library(survey)
#> Loading required package: grid
#> Loading required package: Matrix
#> Loading required package: survival
#> 
#> Attaching package: ‘survival’
#> The following object is masked from ‘package:future’:
#> 
#>     cluster
#> 
#> Attaching package: ‘survey’
#> The following object is masked from ‘package:graphics’:
#> 
#>     dotchart
design <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~wts,
  data = df,
  fpc = ~fpc_psu
)
nested_svy <- make_nested(design, grp)
```
