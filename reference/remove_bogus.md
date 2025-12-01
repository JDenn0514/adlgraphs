# Remove bots, duplicates, and/or speedsters from a data frame

This function removes respondents suspected of being bots or duplicate
survey takers, as well as previews, people under 18, and speedsters from
a data frame. It is made specifically for surveys programmed with
Qualtrics.

## Usage

``` r
remove_bogus(data, duration, cut_off = 0.3, only_finished = TRUE)
```

## Arguments

- data:

  A data frame.

- duration:

  The name of the time duration variable. Must be a string.

- cut_off:

  Specify what percentage of the median time should be used to remove
  speedsters. Default is 0.3, which means people who's time to complete
  is 0.3 that of the median completion time are removed.

- only_finished:

  Logical. If `TRUE`, the default, removes all respondents who did not
  finish according to the `finished`/`Finished` variable. If `FALSE`,
  keeps those who did not finish.
