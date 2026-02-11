# error when x is not numeric

    Code
      get_means(test_data, pid_f3)
    Condition
      Error in `prep_means_data()`:
      ! `x` must be of class `numeric`
      i `pid_f3` is of class factor

# error when wt is not numeric

    Code
      get_means(test_data, trad_n, wt = pid_f3)
    Condition
      Error in `ensure_weight()`:
      ! `pid_f3` must be a numeric variable.
      x Supplied variable is factor.

