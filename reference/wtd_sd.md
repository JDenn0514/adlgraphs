# Calculate weighted standard deviation

While this is mainly an internal function, it is available for everyone
since I could not find a function that calculates a weighted standard
deviation.

## Usage

``` r
wtd_sd(x, wt = NULL, na.rm = TRUE)
```

## Arguments

- x:

  A numeric vector that you want to calculate the SD

- wt:

  A numeric vector indicating the weights used in the calculation

- na.rm:

  Logical. Indicates if NAs should be removed or not

## Value

A number indicating the weighted SD of `x`

## Details

Thank you to the team at Hmisc for the creation of `Hmisc::wtd.var()`.
This function could not have been created without it. I simplified their
source code and then took the square root.
