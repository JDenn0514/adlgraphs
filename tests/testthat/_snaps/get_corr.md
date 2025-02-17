# error when x is not numeric

    Code
      get_corr(test_data, edu_f2, trad_n)
    Condition
      Error in `wtd_corr()`:
      ! `edu_f2` must be a numeric variable.
      x Supplied variable is factor.

# error when y is not numeric

    Code
      get_corr(test_data, trad_n, edu_f2)
    Condition
      Error in `wtd_corr()`:
      ! `edu_f2` must be a numeric variable.
      x Supplied variable is factor.

---

    Code
      get_corr(test_data, trad_n, sdo_sum, wt = pid_f3)
    Condition
      Warning in `[<-.factor`:
      invalid factor level, NA generated
      Error in `wtd_corr()`:
      ! `pid_f3` must be a numeric variable.
      x Supplied variable is factor.

