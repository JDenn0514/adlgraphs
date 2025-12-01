# Remove bots and/or duplicates from a data frame

**\[deprecated\]**

## Usage

``` r
remove_bot_dupe(data)
```

## Arguments

- data:

  A data.frame object you want to operate on

## Details

`remove_bot_dupe()` is deprecated because the function
[`remove_bogus()`](https://jdenn0514.github.io/adlgraphs/reference/remove_bogus.md)
does the same thing but also removes speedsters.

This funciton removes respondents suspected of being bots or duplicate
survey takers, as well as previews and people under 18 from a data
frame. It is made specifically for surveys programmed with Qualtrics.
