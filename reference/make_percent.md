# Convert a numeric vector to a percent

Convert a numeric vector to a percent

## Usage

``` r
make_percent(x, decimals = 2, scale = 100)
```

## Arguments

- x:

  A numeric vector you want to convert to percentages

- decimals:

  How many decimals should the value be rounded to. Default is 2 which
  means it will show two decimal places, or the hundredth decimal.

- scale:

  A scaling factor: `x` will be multiplied by `scale` before formatting.
  This is useful if the underlying data is very small or very large.
  Default is 100.

## Value

A character vector of the same length as `x`.

## Examples

``` r
# here's the default scale of 100
x <- c(0.0163, 0.95, 0.0008, 0.002)
make_percent(x)
#> [1] "1.63%" "95%"   "0.08%" "0.2%" 
#> attr(,"transformation")
#> [1] "Added a `%` symbol to `x`"

# if the values have already been multiplied by 100 and you don't need to
# transform them, then make the scale 1
x <- c(1.63, 95, 0.08, 0.2)
make_percent(x, scale = 1)
#> [1] "1.63%" "95%"   "0.08%" "0.2%" 
#> attr(,"transformation")
#> [1] "Added a `%` symbol to `x`"

# And if we want to round to the closest whole number set the digits to 0
x <- c(0.0163, 0.95, 0.0008, 0.002)
make_percent(x, decimals = 0)
#> [1] "2%"  "95%" "0%"  "0%" 
#> attr(,"transformation")
#> [1] "Added a `%` symbol to `x`"

```
