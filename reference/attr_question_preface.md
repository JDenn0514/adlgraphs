# Get the question_preface attribute

This function makes it easy to get the question_preface from either an
individual vector or a data frame. NOTE: it is not possible to set or
modify the question preface attribute with this function.

## Usage

``` r
attr_question_preface(x, data)
```

## Arguments

- x:

  A vector object, the name of a column in a `data.frame`, or an an
  actual `data.frame` object.

- data:

  A `data.frame` or `tibble` object. This should only be specified when
  `x` is only the name of a column in a `data.frame`.

## Value

If `x` is a variable or vector, a string containing the
"question_preface" attribute, if one is present, is returned. If `x` is
a `data.frame` then a named vector with the "question_preface" attribute
from each variable is returned.

## Examples

``` r
# create a random vector
x <- sample(c(0, 1), replace = TRUE, size = 10)
# add a question preface attribute
attr(x, "question_preface") <- "This is a question preface"
# check to see that there is a new attribute called `question_preface`
attributes(x)
#> $question_preface
#> [1] "This is a question preface"
#> 
# now get the question_preface
attr_question_preface(x)
#> [1] "This is a question preface"

# now let's create a realistic workflow with a data.frame --------
# create a fake dataset
df <- data.frame(
  x_1 = sample(c(0, 1), replace = TRUE, size = 10),
  x_2 = sample(c(0, 1), replace = TRUE, size = 10),
  x_3 = sample(c(0, 1), replace = TRUE, size = 10),
  x_4 = sample(c(0, 1), replace = TRUE, size = 10)
) 

# set the variable labels
attr(df$x_1, "label") <- "Which of the following colors do you like? Blue"
attr(df$x_2, "label") <- "Which of the following colors do you like? Red"
attr(df$x_3, "label") <- "Which of the following colors do you like? Yellow"
attr(df$x_4, "label") <- "Which of the following colors do you like? Purple" 

# set the value labels
attr(df$x_1, "labels") <- c("Blue" = 1)
attr(df$x_2, "labels") <- c("Red" = 1)
attr(df$x_3, "labels") <- c("Yellow" = 1)
attr(df$x_4, "labels") <- c("Purple" = 1)

# check the attributes


# add the question prefaces and update the variable labels for each column in df
for(x in names(df)) {
  df[[x]] <- set_question_preface(x, df)
}

# now if I'm curious what the question prefaces are for the df, I can easily
# see all of them using `attr_question_preface`
attr_question_preface(df)
#> $x_1
#> [1] "Which of the following colors do you like?"
#> 
#> $x_2
#> [1] "Which of the following colors do you like?"
#> 
#> $x_3
#> [1] "Which of the following colors do you like?"
#> 
#> $x_4
#> [1] "Which of the following colors do you like?"
#> 


```
