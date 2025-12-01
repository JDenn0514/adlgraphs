# Get bogus data from a data frame

This function is the opposite of
[`remove_bogus()`](https://jdenn0514.github.io/adlgraphs/reference/remove_bogus.md).
Instead of removing bots, duplicates, and speedster from a data frame,
it keeps them. This was designed to make it easy to send to survey panel
providers so they can remove them from your data set.

## Usage

``` r
get_bogus(data, duration, cut_off = 0.3)
```

## Arguments

- data:

  A data.frame object you want to operate on

- duration:

  The name of the time duration variable. Must be a string.

- cut_off:

  Specify what percentage of the median time should be used to remove
  speedsters. Default is 0.3, which means people who's time to complete
  is 0.3 that of the median completion time are removed.
