# Data Transformations

``` r
# load in the libraries

# for num_rev and test_data
library(adlgraphs)
# this is for some other basic data transformations
library(dplyr)
```

`adlgraphs` provides four main functions to reduce the amount of time it
takes to perform a few very common data transformations:

- \`flip_val() - flips the valence of a vector

- [`num_rev()`](https://jdenn0514.github.io/adlgraphs/reference/num_rev.md) -
  reverses the values of a vector

- [`make_dicho()`](https://jdenn0514.github.io/adlgraphs/reference/make_dicho.md) -
  converts a vector to a dichotomous factor

- [`make_binary()`](https://jdenn0514.github.io/adlgraphs/reference/make_binary.md) -
  converts a vector to a binary (0 and 1)

## `num_rev()` and `flip_val()`

Let’s create a vector called `blue` that measures people’s agreement
with the statement “I like the color blue” and set it so that a higher
number means more disagreement.

``` r

# create the vector
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

# show the output and attributes
str(blue)
#>  int+lbl [1:20] 1, 3, 4, 3, 1, 2, 3, 3, 3, 4, 2, 1, 1, 1, 3, 4, 3, 1, 2, 2
#>  @ labels: Named num [1:4] 1 2 3 4
#>   ..- attr(*, "names")= chr [1:4] "Strongly agree" "Somewhat agree" "Somewhat disagree" "Strongly disagree"
#>  @ label : chr "I like the color blue"
```

Now, given the way the statement is written, we would naturally assume a
higher number means more agreement and therefore the person likes blue
more. However, that’s not the case. This is particularly relevant when
comparing means or running regressions, etc. To fix this, we just need
to reverse the values and their associated labels to show that a higher
number means more agreement. This is what
[`num_rev()`](https://jdenn0514.github.io/adlgraphs/reference/num_rev.md)
does.

Looking at the example below, we can see that the labels for the two
vectors are reversed and the values are different as well. However, the
variable labels for both are the same. Doing it this way ensures that
the overall output of the question stays the same. We still get the same
percentage of people who agree and disagree, but the actual numbers that
underlie the variable have reversed.

``` r
# reverse the direction of the scale
blue_rev <- num_rev(blue)
# let's compare the two
str(list(blue = blue, blue_rev = blue_rev))
#> List of 2
#>  $ blue    : int+lbl [1:20] 1, 3, 4, 3, 1, 2, 3, 3, 3, 4, 2, 1, 1, 1, 3, 4, 3, 1, 2, 2
#>    ..@ labels: Named num [1:4] 1 2 3 4
#>    .. ..- attr(*, "names")= chr [1:4] "Strongly agree" "Somewhat agree" "Somewhat disagree" "Strongly disagree"
#>    ..@ label : chr "I like the color blue"
#>  $ blue_rev: num [1:20] 4 2 1 2 4 3 2 2 2 1 ...
#>   ..- attr(*, "labels")= Named num [1:4] 1 2 3 4
#>   .. ..- attr(*, "names")= chr [1:4] "Strongly disagree" "Somewhat disagree" "Somewhat agree" "Strongly agree"
#>   ..- attr(*, "label")= chr "I like the color blue"
#>   ..- attr(*, "transformation")= chr "Reversing 'blue' while maintaining correct value labels"
```

Conversely, if we wanted to show how much people dislike the color blue,
we could show how many people disagree with the statement, however this
is not as intuitive as simply showing agreement with a negative
statement. To fix this proble, we could flip the valence of the question
to something like, “I dislike the color blue” and change the value
labels, while keep the values the same. This is what
[`flip_val()`](https://jdenn0514.github.io/adlgraphs/reference/flip_val.md)
does.

Looking below, we can see that the values between `blue` and `blue_flip`
are the same, but the value labels and variable labels are different.
Similarly, we can see that the values and variable labels between
`blue_flip` and `blue_rev` are different, but their value labels are the
same.

``` r
blue_flip <- flip_val(blue, "I dislike the color blue")
str(list(blue = blue, blue_rev = blue_rev, blue_flip = blue_flip))
#> List of 3
#>  $ blue     : int+lbl [1:20] 1, 3, 4, 3, 1, 2, 3, 3, 3, 4, 2, 1, 1, 1, 3, 4, 3, 1, 2, 2
#>    ..@ labels: Named num [1:4] 1 2 3 4
#>    .. ..- attr(*, "names")= chr [1:4] "Strongly agree" "Somewhat agree" "Somewhat disagree" "Strongly disagree"
#>    ..@ label : chr "I like the color blue"
#>  $ blue_rev : num [1:20] 4 2 1 2 4 3 2 2 2 1 ...
#>   ..- attr(*, "labels")= Named num [1:4] 1 2 3 4
#>   .. ..- attr(*, "names")= chr [1:4] "Strongly disagree" "Somewhat disagree" "Somewhat agree" "Strongly agree"
#>   ..- attr(*, "label")= chr "I like the color blue"
#>   ..- attr(*, "transformation")= chr "Reversing 'blue' while maintaining correct value labels"
#>  $ blue_flip: int+lbl [1:20] 1, 3, 4, 3, 1, 2, 3, 3, 3, 4, 2, 1, 1, 1, 3, 4, 3, 1, 2, 2
#>    ..@ labels        : Named num [1:4] 1 2 3 4
#>    .. ..- attr(*, "names")= chr [1:4] "Strongly disagree" "Somewhat disagree" "Somewhat agree" "Strongly agree"
#>    ..@ label         : chr "I dislike the color blue"
#>    ..@ transformation: chr "Flipped the valance of `blue` so it is now negative by updating the label and value labels accordingly."
```
