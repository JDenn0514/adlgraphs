# requires at least one condition

    Code
      case_match_fct(1)
    Condition
      Warning:
      `case_match()` was deprecated in dplyr 1.2.0.
      i Please use `recode_values()` instead.
      Error in `dplyr::case_match()`:
      ! `...` can't be empty.

---

    Code
      case_match_fct(1, NULL)
    Condition
      Error in `purrr::map()`:
      i In index: 1.
      Caused by error in `.f()`:
      ! `x` must be a formula

