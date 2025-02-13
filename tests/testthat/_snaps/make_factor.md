# should force s1 to a factor

    Code
      make_factor(s1, force = TRUE)
    Condition
      Warning in `make_factor()`:
      `x` has no value labels so forcing to a factor with `as.factor()`
    Output
      [1] 0 1 2
      attr(,"transformation")
      [1] Converted 's1' from a numeric vector to a factor
      attr(,"label")
      [1] s1
      Levels: 0 1 2

