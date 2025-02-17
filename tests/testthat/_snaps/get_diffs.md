# error when x is not numeric

    Code
      test_data %>% get_diffs(edu_f2, pid_f3)
    Condition
      Error in `get_diffs()`:
      ! `x` must be of class `numeric`
      i `edu_f2` is of class factor

# error when wt is not numeric

    Code
      test_data %>% get_diffs(trad_n, pid_f3, wt = edu_f2)
    Condition
      Error in `get_diffs()`:
      ! `edu_f2` must be a numeric variable.
      x Supplied variable is factor.

