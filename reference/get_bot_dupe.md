# Get the bots or duplicates from a data frame

**\[deprecated\]**

## Usage

``` r
get_bot_dupe(data)
```

## Arguments

- data:

  A data.frame object you want to operate on

## Details

`get_bot_dupe()` is deprecated because the function
[`get_bogus()`](https://jdenn0514.github.io/adlgraphs/reference/get_bogus.md)
does the same thing but also removes speedsters.

This function is the opposite of `remove_bot_dupes()`. Instead of
removing bots and duplicates from a data frame, it keeps them. This was
designed to make it easy to send to survey panel providers so they can
remove them from your data set.
