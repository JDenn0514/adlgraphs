#' #' this is just an internal function
#'
#'  get_mean_table <- function(df, var, group, wt) {
#'
#'   if (missing(wt)) {
#'
#'       if (missing(group)) {
#'
#'       # "Returns a naked expression of the variable"
#'       var <- rlang::enexpr(var)
#'       if (!is.character(var)) {
#'         # convert to a sym() object and then use as_name to make it a string
#'         var <- rlang::as_name(rlang::ensym(var))
#'       }
#'
#'       df %>%
#'         dplyr::summarise(
#'           # calculate the mean
#'           mean = mean(.data[[var]],  na.rm = TRUE),
#'           # calculate the standard deviation
#'           sd = sd(.data[[var]], na.rm = TRUE),
#'           # get the number of respondents
#'           n = n()
#'         ) %>%
#'         dplyr::mutate(
#'           # calculate the standard error
#'           std.error = sd/sqrt(n),
#'           # calculate the lower CI
#'           conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#'           # calculate the higher CI
#'           conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#'           # round all of the numbers to the second decimal
#'           across(where(is.numeric), ~round(.x, 2))
#'         ) %>%
#'         gt() %>%
#'         # rename the columns
#'         gt::cols_label(
#'           mean = "Mean",
#'           n = "N",
#'           std.error = "Standard Error",
#'           conf.low = "Lower CI",
#'           conf.high = "Higher CI"
#'         ) %>%
#'         # add a title to the table of the variable label
#'         gt::tab_header(title = var_label(df[[var]]))
#'
#'     } else {
#'
#'       ## if var is a symbol convert it to a string
#'       # "Returns a naked expression of the variable"
#'       var <- rlang::enexpr(var)
#'       if (!is.character(var)) {
#'         # convert to a sym() object and then use as_name to make it a string
#'         var <- rlang::as_name(rlang::ensym(var))
#'       }
#'
#'       ## if group is a symbol convert it to a string
#'       # "Returns a naked expression of the variable"
#'       group <- rlang::enexpr(group)
#'       if (!is.character(group)) {
#'         # convert to a sym() object and then use as_name to make it a string
#'         group <- rlang::as_name(rlang::ensym(group))
#'       }
#'
#'       if (class(df[[group]])[1] == "haven_labelled" || class(df[[group]]) == "numeric") {
#'         df <- df %>%
#'           dplyr::mutate(.data[[group]] = haven::as_factor(.data[[group]])) %>%
#'           dplyr::count(.data[[group]])
#'       } #else if (class(df[[group]] == "numeric")) {
#'         # df <- df %>%
#'         #   dplyr::mutate(.data[[group]] := haven::as_factor(.data[[group]])) %>%
#'         #   dplyr::count(.data[[group]])
#'       #}
#'
#'       return(df)
#'
#'       # get the length of the of the number of values in the group variable
#'       # and add a 1 to it. use this with gt::tab_row_group()
#'       group_length <- length(unique(df[[group]])) + 1
#'
#'       # calculate the mean for the general population
#'       genpop <- df %>%
#'         dplyr::summarise(
#'           # calculate the mean
#'           mean = mean(.data[[var]],  na.rm = TRUE),
#'           # calculate the standard deviation
#'           sd = sd(.data[[var]], na.rm = TRUE),
#'           # get the number of respondents
#'           n = n()
#'         ) %>%
#'         dplyr::mutate(
#'           # calculate the standard error
#'           std.error = sd/sqrt(n),
#'           # calculate the lower CI
#'           conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#'           # calculate the higher CI
#'           conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#'           # round all of the numbers to the second decimal
#'           across(where(is.numeric), ~round(.x, 2)),
#'           # add a new variable using the group as the name
#'           {{group}} := "General Population"
#'         )
#'
#'       # calculate the mean for the group variable
#'       group <- df %>%
#'         # group the data with the group variable
#'         dplyr::group_by(.data[[group]]) %>%
#'         dplyr::summarise(
#'           # calculate the mean
#'           mean = mean(.data[[var]],  na.rm = TRUE),
#'           # calculate the standard deviation
#'           sd = sd(.data[[var]], na.rm = TRUE),
#'           # get the number of respondents
#'           n = n()
#'         ) %>%
#'         dplyr::mutate(
#'           # calculate the standard error
#'           std.error = sd/sqrt(n),
#'           # calculate the lower CI
#'           conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#'           # calculate the higher CI
#'           conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#'           # round all of the numbers to the second decimal
#'           across(where(is.numeric), ~round(.x, 2)),
#'         )
#'
#'       # combine the two tables and make it a
#'       bind_rows(genpop, group) %>%
#'         # remove the standard deviation
#'         select(-sd) %>%
#'         # move n to the end of the data
#'         relocate(n, .after = conf.high) %>%
#'         # moev std.error after conf.high
#'         relocate(std.error, .after = conf.high) %>%
#'         # make a gt object and set the group variable to be used as row labels
#'         # placed in the table tub
#'         gt::gt(rowname_col = group) %>%
#'         # create a row group from a collection of rows
#'         gt::tab_row_group(
#'           # specify the row group label
#'           label = var_label(df[[group]]),
#'           # specify which rows, since genpop will always be on top this should
#'           # rows 2 through group_length
#'           rows = c(2:group_length)
#'         ) %>%
#'         # reorder the group order
#'         gt::row_group_order(c(NA, var_label(df[[group]]))) %>%
#'         # rename the columns
#'         gt::cols_label(
#'           mean = "Mean",
#'           n = "N",
#'           std.error = "Standard Error",
#'           conf.low = "Lower CI",
#'           conf.high = "Higher CI"
#'         ) %>%
#'         # add a title to the table of the variable label
#'         gt::tab_header(title = var_label(df[[var]]))
#'     }
#'
#'   } else {
#'     if (missing(group)) {
#'
#'       # "Returns a naked expression of the variable"
#'       var <- rlang::enexpr(var)
#'       if (!is.character(var)) {
#'         # convert to a sym() object and then use as_name to make it a string
#'         var <- rlang::as_name(rlang::ensym(var))
#'       }
#'
#'       ## if wt is a symbol convert it to a string
#'       # "Returns a naked expression of the variable"
#'       wt <- rlang::enexpr(wt)
#'       if (!is.character(wt)) {
#'         # convert to a sym() object and then use as_name to make it a string
#'         wt <- rlang::as_name(rlang::ensym(wt))
#'       }
#'
#'       df %>%
#'         dplyr::summarise(
#'           # calculate the mean
#'           mean = weighted.mean(.data[[var]], .data[[wt]],  na.rm = TRUE),
#'           # calculate the standard deviation
#'           sd = sd(.data[[var]], na.rm = TRUE),
#'           # get the number of respondents
#'           n = n()
#'         ) %>%
#'         dplyr::mutate(
#'           # calculate the standard error
#'           std.error = sd/sqrt(n),
#'           # calculate the lower CI
#'           conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#'           # calculate the higher CI
#'           conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#'           # round all of the numbers to the second decimal
#'           across(where(is.numeric), ~round(.x, 2))
#'         ) %>%
#'         gt() %>%
#'         # rename the columns
#'         gt::cols_label(
#'           mean = "Mean",
#'           n = "N",
#'           std.error = "Standard Error",
#'           conf.low = "Lower CI",
#'           conf.high = "Higher CI"
#'         ) %>%
#'         # add a title to the table of the variable label
#'         gt::tab_header(title = var_label(df[[var]]))
#'
#'
#'     } else {
#'
#'       ## if var is a symbol convert it to a string
#'       # "Returns a naked expression of the variable"
#'       var <- rlang::enexpr(var)
#'       if (!is.character(var)) {
#'         # convert to a sym() object and then use as_name to make it a string
#'         var <- rlang::as_name(rlang::ensym(var))
#'       }
#'
#'       ## if group is a symbol convert it to a string
#'       # "Returns a naked expression of the variable"
#'       group <- rlang::enexpr(group)
#'       if (!is.character(group)) {
#'         # convert to a sym() object and then use as_name to make it a string
#'         group <- rlang::as_name(rlang::ensym(group))
#'       }
#'
#'       ## if wt is a symbol convert it to a string
#'       # "Returns a naked expression of the variable"
#'       wt <- rlang::enexpr(wt)
#'       if (!is.character(wt)) {
#'         # convert to a sym() object and then use as_name to make it a string
#'         wt <- rlang::as_name(rlang::ensym(wt))
#'       }
#'
#'       #
#'       if (is.null(var_label(df[[group]]))) {
#'         group_label <- group
#'       } else {
#'         group_label <- var_label(df[[group]])
#'       }
#'
#'       # get the length of the of the number of values in the group variable
#'       # and add a 1 to it. use this with gt::tab_row_group()
#'
#'       group_length <- length(unique(df[[group]])) + 1
#'
#'       # calculate the mean for the general population
#'       genpop <- df %>%
#'         dplyr::summarise(
#'           # calculate the mean
#'           mean = weighted.mean(.data[[var]], .data[[wt]],  na.rm = TRUE),
#'           # calculate the standard deviation
#'           sd = sd(.data[[var]], na.rm = TRUE),
#'           # get the number of respondents
#'           n = n()
#'         ) %>%
#'         dplyr::mutate(
#'           # calculate the standard error
#'           std.error = sd/sqrt(n),
#'           # calculate the lower CI
#'           conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#'           # calculate the higher CI
#'           conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#'           # round all of the numbers to the second decimal
#'           across(where(is.numeric), ~round(.x, 2)),
#'           # add a new variable using the group as the name
#'           {{group}} := "General Population"
#'         )
#'
#'       # calculate the mean for the group variable
#'       group <- df %>%
#'         # group the data with the group variable
#'         dplyr::group_by(.data[[group]]) %>%
#'         dplyr::summarise(
#'           # calculate the mean
#'           mean = weighted.mean(.data[[var]], .data[[wt]],  na.rm = TRUE),
#'           # calculate the standard deviation
#'           sd = sd(.data[[var]], na.rm = TRUE),
#'           # get the number of respondents
#'           n = n()
#'         ) %>%
#'         dplyr::mutate(
#'           # calculate the standard error
#'           std.error = sd/sqrt(n),
#'           # calculate the lower CI
#'           conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#'           # calculate the higher CI
#'           conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#'           # round all of the numbers to the second decimal
#'           across(where(is.numeric), ~round(.x, 2)),
#'         )
#'
#'       # combine the two tables and make it a
#'       bind_rows(genpop, group) %>%
#'         # remove the standard deviation
#'         select(-sd) %>%
#'         # move n to the end of the data
#'         relocate(n, .after = conf.high) %>%
#'         # moev std.error after conf.high
#'         relocate(std.error, .after = conf.high) %>%
#'         # make a gt object and set the group variable to be used as row labels
#'         # placed in the table tub
#'         gt::gt(rowname_col = group) %>%
#'         # create a row group from a collection of rows
#'         gt::tab_row_group(
#'           # specify the row group label
#'           label = group_label,
#'           # specify which rows, since genpop will always be on top this should
#'           # rows 2 through group_length
#'           rows = c(2:group_length)
#'         ) %>%
#'         # reorder the group order
#'         gt::row_group_order(c(NA, group_label)) %>%
#'         # rename the columns
#'         gt::cols_label(
#'           mean = "Mean",
#'           n = "N",
#'           std.error = "Standard Error",
#'           conf.low = "Lower CI",
#'           conf.high = "Higher CI"
#'         ) %>%
#'         # add a title to the table of the variable label
#'         gt::tab_header(title = var_label(df[[var]]))
#'     }
#'
#'   }
#'
#'
#' }
