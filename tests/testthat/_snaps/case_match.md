# requires at least one condition

    Code
      case_match_fct(1)
    Condition
      Error in `dplyr::case_match()`:
      ! At least one condition must be supplied.

---

    Code
      case_match_fct(1, NULL)
    Condition
      Error in `purrr::map()`:
      i In index: 1.
      Caused by error in `.f()`:
      ! `x` must be a formula

# `.default` is part of common type computation

    Code
      case_match(1, 1 ~ 1L, .default = "x")
    Condition
      Error in `case_match()`:
      ! Can't combine `..1 (right)` <integer> and `.default` <character>.

