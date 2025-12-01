# Complete list of available adl official color palettes

A list of all color palettes to choose from, and preview directly in the
console.

## Usage

``` r
adl_palettes
```

## Format

A list of 9 `color` objects elements.

- primary:

  Primary categorical color

- secondary:

  Secondary categorical color

- tertiary:

  Tertiary categorical color

- binary:

  Palette comprised of the primary and secondary colors (categorical)

- pid3:

  Palette for partisanship/ideology (categorical)

- categorical:

  Palette for categorical data for up to 7 groups (categorical)

- likert_6:

  Palette for likert scales with 6 response options (diverging)

- likert_4:

  Palette for likert scales with 4 response options (diverging)

- bluescale:

  Blue scale color palette (sequential)

- grayscale:

  Grayscale color palette (sequential)

## Details

Pretty print and plot methods are powered by the
[prismatic](https://CRAN.R-project.org/package=prismatic) package. Each
palette is stored as a `color` object to enact this behaviour.

## Examples

``` r
adl_palettes$primary
#> <colors>
#> #14A2FCFF 
adl_palettes$secondary
#> <colors>
#> #B0B1B3FF 
adl_palettes$tertiary
#> <colors>
#> #E84C4CFF 
adl_palettes$pid3
#> <colors>
#> #14A2FCFF #60269EFF #E84C4CFF 
adl_palettes$categorical
#> <colors>
#> #14A2FCFF #B0B1B3FF #E84C4CFF #0A1A50FF #FFE500FF #69DA78FF #60269EFF #FFA828FF 
adl_palettes$likert_6
#> <colors>
#> #0A1A50FF #005B98FF #14A2FCFF #DBDCDDFF #B0B1B3FF #595B60FF 
adl_palettes$likert_4
#> <colors>
#> #0A1A50FF #14A2FCFF #DBDCDDFF #595B60FF 
adl_palettes$bluescale
#> <colors>
#> #0A1A50FF #E7E8EEFF 
adl_palettes$grayscale
#> NULL
```
