# Flip the valence of a vector

Reverse the valence of a vector by keeping its values consistent, but
reversing its value labels and providing a new variable label. By
reversing the valence, I mean changing a negative statement to a
positive one and vice versa. Please check the vignette to have a better
understanding of exactly what this function does (still needs to be
updated).

## Usage

``` r
flip_val(x, label = NULL)
```

## Arguments

- x:

  A vector of class `haven_labelled` or `numeric`

- label:

  A new variable label given to `x`.

## Value

A numeric vector of the same length as `x`

## Examples

``` r
# create a vector
blue <- sample(c(1:4), size = 20, replace = TRUE)
# add value labels
attr(blue, "labels") <- c(
  "Strongly agree" = 1, 
  "Somewhat agree" = 2, 
  "Somewhat disagree" = 3, 
  "Strongly disagree" = 4
)
# add a variable label
attr(blue, "label") <- "I like the color blue"

# make it a haven-labelled object (this is just for printing purposes)
class(blue) <- c("haven_labelled", "vctrs_vctr", "double")

# flip the valence
blue_flip <- flip_val(blue, "I dislike the color blue")
# show the output comapring the two
str(list(blue = blue, blue_flip = blue_flip))
#> List of 2
#>  $ blue     : int+lbl [1:20] 3, 3, 3, 3, 2, 4, 4, 3, 3, 4, 4, 4, 3, 4, 4, 4, 4, 2, 3, 2
#>    ..@ labels: Named num [1:4] 1 2 3 4
#>    .. ..- attr(*, "names")= chr [1:4] "Strongly agree" "Somewhat agree" "Somewhat disagree" "Strongly disagree"
#>    ..@ label : chr "I like the color blue"
#>  $ blue_flip: int+lbl [1:20] 3, 3, 3, 3, 2, 4, 4, 3, 3, 4, 4, 4, 3, 4, 4, 4, 4, 2, 3, 2
#>    ..@ labels        : Named num [1:4] 1 2 3 4
#>    .. ..- attr(*, "names")= chr [1:4] "Strongly disagree" "Somewhat disagree" "Somewhat agree" "Strongly agree"
#>    ..@ label         : chr "I dislike the color blue"
#>    ..@ transformation: chr "Flipped the valance of `blue` so it is now negative by updating the label and value labels accordingly."

# can also be used inside of dplyr::mutate()
library(dplyr)
  
new_df <- test_data %>% 
  dplyr::mutate(
    # flip the valence for deserving
    deserving_flip = flip_val(
      deserving, 
      "Groups at the bottom are not just as deserving as groups at the top"
    )
  ) %>% 
  # keep only the relevant columns
  dplyr::select(deserving, deserving_flip)

# show the output
str(new_df)
#> tibble [250 × 2] (S3: tbl_df/tbl/data.frame)
#>  $ deserving     : dbl+lbl [1:250] 1, 2, 2, 2, 3, 1, 1, 2, 1, 1, 1, 1, 1, 2, 3, 2, 3, 1, ...
#>    ..@ label        : chr "Groups at the bottom are just as deserving as groups at the top"
#>    ..@ format.spss  : chr "F40.0"
#>    ..@ display_width: int 5
#>    ..@ labels       : Named num [1:4] 1 2 3 4
#>    .. ..- attr(*, "names")= chr [1:4] "Strongly agree" "Somewhat agree" "Somewhat disagree" "Strongly disagree"
#>  $ deserving_flip: dbl+lbl [1:250] 1, 2, 2, 2, 3, 1, 1, 2, 1, 1, 1, 1, 1, 2, 3, 2, 3, 1, ...
#>    ..@ label         : chr "Groups at the bottom are not just as deserving as groups at the top"
#>    ..@ format.spss   : chr "F40.0"
#>    ..@ display_width : int 5
#>    ..@ labels        : Named num [1:4] 1 2 3 4
#>    .. ..- attr(*, "names")= chr [1:4] "Strongly disagree" "Somewhat disagree" "Somewhat agree" "Strongly agree"
#>    ..@ transformation: chr "Flipped the valance of `deserving` so it is now negative by updating the label and value labels accordingly."


```
