


# get_mean_table <- function(df, x, group, wt) {
#
#   df %>%
#     get_means({{ x }}, {{ group }}, {{ wt }}) %>%
#     gt::gt()
#
# }
#
#
#
#
#
#
#
# get_mean_table <- function(df, x, group, wt) {
#
#
#
#   # use enexpr() to capture the expressions supplied in "x"
#   # enexpr returns a naked expression of the argument supplied in "x"
#   # this is what allows the input to be either a string or a symbol
#   x <- rlang::enexpr(x)
#   # if the object supplied in "x" is not a character...
#   if (!is.character(x)) {
#     # capture x and convert to a symbol object with ensym()
#     #then use as_name() to make it a string
#     x <- rlang::as_name(rlang::ensym(x))
#   }
#
#   # determine the title of the table
#   if (!is.null(labelled::var_label(df[[x]]))) {
#     # if x has a variable label
#
#     x_variable_label <- labelled::var_label(df[[x]])
#
#   } else {
#     # if there is no variable label
#
#     x_variable_label <- x
#
#   }
#
#   if (haven::is.labelled(df[[x]])) {
#     # if x is class haven_labelled
#
#     # get the number values for the variables
#     values <- labelled::val_labels(df[[x]])
#     # get the minimum number
#     min_values <- min(values)
#     # get the maximum number
#     max_values <- max(values)
#
#     # get the text labels associated with the numbers
#     value_labels <- setNames(names(values), values)
#
#     subtitle_text <- glue::glue("{min(values)} = {value_labels[min_values]} and {max(values)} = {value_labels[max_values]}")
#
#
#   } else if (is.numeric(df[[x]]) && !is.null(sjlabelled::get_labels(df[[x]]))) {
#     # if x is class numeric AND DOES contain value labels
#
#
#     # get the number values for the variables
#     values <- sjlabelled::get_labels(df[[x]])
#     # get the minimum number
#     min_values <- min(values)
#     # get the maximum number
#     max_values <- max(values)
#
#     # get the text labels associated with the numbers
#     value_labels <- setNames(names(values), values)
#
#     subtitle_text <- glue::glue("{min(values)} = {value_labels[min_values]} and {max(values)} = {value_labels[max_values]}")
#
#   } else if (!is.character(df[[x]]) || !is.factor(df[[x]])) {
#     # if not character or factor
#
#     # get the mix x value
#     min_values <- min(df[[x]])
#     # get the max x value
#     max_values <- max(df[[x]])
#
#     subtitle_text <- glue::glue("The minimum value is {min_values} and the maximum value is {max_values}")
#
#   } else {
#     cli::cli_abort(
#       c(
#         "`{x_lab}` must be a vector of class {.cls numeric} with or without values or {.cls haven_labelled}",
#         x = "You've supplied a {.cls {class(x)}} vector"
#       )
#     )
#
#   }
#
#
#
#
#
#   if (missing(wt)) {
#     # if wt is missing
#
#     if (missing(group)) {
#       # if group is missing
#
#       df %>% get_means(x)
#
#
#       df %>%
#         get_means(x) %>%
#         gt::gt() %>%
#         # rename the columns
#         gt::cols_label(
#           mean = "Mean",
#           n = "N",
#           std.error = "Standard Error",
#           conf.low = "Lower CI",
#           conf.high = "Higher CI"
#         ) %>%
#         # add a title to the table using variable label if available
#         gt::tab_header(
#           title = glue::glue('Average Score of "{x_variable_label}"'),
#           subtitle = subtitle_text
#         )
#
#
#     } else {
#
#
#       # use enexpr() to capture the expressions supplied in "group"
#       # enexpr returns a naked expression of the argument supplied in "group"
#       # this is what allows the input to be either a string or a symbol
#       group <- rlang::enexpr(group)
#       # if the object supplied in "x" is not a character...
#       if (!is.character(group)) {
#
#         # capture x and convert to a symbol object with ensym()
#         #then use as_name() to make it a string
#         group <- rlang::as_name(rlang::ensym(group))
#
#       }
#
#       # determine the title of the group section
#       if (!is.null(labelled::var_label(df[[group]]))) {
#         # if x has a variable label
#
#         group_variable_label <- labelled::var_label(df[[group]])
#
#       } else {
#         # if there is no variable label
#
#         group_variable_label <- group
#
#       }
#
#       # get the length of the of the number of values in the group variable
#       # and add a 1 to it. use this with gt::tab_row_group()
#       group_length <- length(unique(df[[group]])) + 1
#
#       # calculate the mean for the general population
#       genpop_df <- df %>%
#         dplyr::summarise(
#           # calculate the mean
#           mean = mean(.data[[x]],  na.rm = TRUE),
#           # calculate the standard deviation
#           sd = sd(.data[[x]], na.rm = TRUE),
#           # get the number of respondents
#           n = dplyr::n()
#         ) %>%
#         dplyr::mutate(
#           # calculate the standard error
#           std.error = sd/sqrt(n),
#           # calculate the lower CI
#           conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#           # calculate the higher CI
#           conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#           # round all of the numbers to the second decimal
#           across(where(is.numeric), ~round(.x, 2)),
#           # add a new variable using the group as the name
#           {{group}} := "General Population"
#         )
#
#       # calculate the mean for the group variable
#       group_df <- df %>%
#         dplyr::mutate(group_f = haven::as_factor(.data[[group]])) %>%
#         # group the data with the group variable
#         dplyr::group_by(group_f) %>%
#         dplyr::summarise(
#           # calculate the mean
#           mean = mean(.data[[x]],  na.rm = TRUE),
#           # calculate the standard deviation
#           sd = sd(.data[[x]], na.rm = TRUE),
#           # get the number of respondents
#           n = n()
#         ) %>%
#         dplyr::mutate(
#           # calculate the standard error
#           std.error = sd/sqrt(n),
#           # calculate the lower CI
#           conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#           # calculate the higher CI
#           conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#           # round all of the numbers to the second decimal
#           dplyr::across(where(is.numeric), ~round(.x, 2)),
#         ) %>%
#         rename({{group}} := group_f)
#
#       # combine the two tables and make it a
#       dplyr::bind_rows(genpop_df, group_df) %>%
#         # remove the standard deviation
#         dplyr::select(-sd) %>%
#         # move n to the end of the data
#         dplyr::relocate(n, .after = conf.high) %>%
#         # moev std.error after conf.high
#         dplyr::relocate(std.error, .after = conf.high) %>%
#         # make a gt object and set the group variable to be used as row labels
#         # placed in the table stub
#         gt::gt(rowname_col = group) %>%
#         # create a row group from a collection of rows
#         gt::tab_row_group(
#           # specify the row group label
#           label = group_variable_label,
#           # specify which rows, since genpop will always be on top this should
#           # rows 2 through group_length
#           rows = c(2:group_length)
#         ) %>%
#         # reorder the group order
#         gt::row_group_order(c(NA, group_variable_label)) %>%
#         # rename the columns
#         gt::cols_label(
#           mean = "Mean",
#           n = "N",
#           std.error = "Standard Error",
#           conf.low = "Lower CI",
#           conf.high = "Higher CI"
#         ) %>%
#         # add a title to the table of the variable label
#         gt::tab_header(
#           title = glue::glue('Average Score of "{x_variable_label}"'),
#           subtitle = subtitle_text
#         )
#
#
#     }
#
#
#
#   } else {
#     # else if wt is named
#
#
#     if (missing(group)) {
#       # if group is missing
#
#     } else {
#       # if group is not missing
#
#     }
#
#
#   }
#
# }
#'
#'
#' pol_pos %>% get_mean_table(trad_n)
#'
#' get_mean_table <- function(df, var, group, wt) {
#'
#'   if (missing(wt)) {
#'
#'     if (missing(group)) {
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
#'    } else {
#'
#'      ## if var is a symbol convert it to a string
#'      # "Returns a naked expression of the variable"
#'      var <- rlang::enexpr(var)
#'      if (!is.character(var)) {
#'        # convert to a sym() object and then use as_name to make it a string
#'        var <- rlang::as_name(rlang::ensym(var))
#'      }
#'
#'      ## if group is a symbol convert it to a string
#'      # "Returns a naked expression of the variable"
#'      group <- rlang::enexpr(group)
#'      if (!is.character(group)) {
#'        # convert to a sym() object and then use as_name to make it a string
#'        group <- rlang::as_name(rlang::ensym(group))
#'      }
#'
#'      if (class(df[[group]])[1] == "haven_labelled" || class(df[[group]]) == "numeric") {
#'        df <- df %>%
#'          dplyr::mutate(.data[[group]] = haven::as_factor(.data[[group]])) %>%
#'          dplyr::count(.data[[group]])
#'      } #else if (class(df[[group]] == "numeric")) {
#'        # df <- df %>%
#'        #   dplyr::mutate(.data[[group]] := haven::as_factor(.data[[group]])) %>%
#'        #   dplyr::count(.data[[group]])
#'      #}
#'
#'      return(df)
#'
#'      # get the length of the of the number of values in the group variable
#'      # and add a 1 to it. use this with gt::tab_row_group()
#'      group_length <- length(unique(df[[group]])) + 1
#'
#'      # calculate the mean for the general population
#'      genpop <- df %>%
#'        dplyr::summarise(
#'          # calculate the mean
#'          mean = mean(.data[[var]],  na.rm = TRUE),
#'          # calculate the standard deviation
#'          sd = sd(.data[[var]], na.rm = TRUE),
#'          # get the number of respondents
#'          n = n()
#'        ) %>%
#'        dplyr::mutate(
#'          # calculate the standard error
#'          std.error = sd/sqrt(n),
#'          # calculate the lower CI
#'          conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#'          # calculate the higher CI
#'          conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#'          # round all of the numbers to the second decimal
#'          across(where(is.numeric), ~round(.x, 2)),
#'          # add a new variable using the group as the name
#'          {{group}} := "General Population"
#'        )
#'
#'      # calculate the mean for the group variable
#'      group <- df %>%
#'        # group the data with the group variable
#'        dplyr::group_by(.data[[group]]) %>%
#'        dplyr::summarise(
#'          # calculate the mean
#'          mean = mean(.data[[var]],  na.rm = TRUE),
#'          # calculate the standard deviation
#'          sd = sd(.data[[var]], na.rm = TRUE),
#'          # get the number of respondents
#'          n = n()
#'        ) %>%
#'        dplyr::mutate(
#'          # calculate the standard error
#'          std.error = sd/sqrt(n),
#'          # calculate the lower CI
#'          conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#'          # calculate the higher CI
#'          conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#'          # round all of the numbers to the second decimal
#'          across(where(is.numeric), ~round(.x, 2)),
#'        )
#'
#'      # combine the two tables and make it a
#'      bind_rows(genpop, group) %>%
#'        # remove the standard deviation
#'        select(-sd) %>%
#'        # move n to the end of the data
#'        relocate(n, .after = conf.high) %>%
#'        # moev std.error after conf.high
#'        relocate(std.error, .after = conf.high) %>%
#'        # make a gt object and set the group variable to be used as row labels
#'        # placed in the table tub
#'        gt::gt(rowname_col = group) %>%
#'        # create a row group from a collection of rows
#'        gt::tab_row_group(
#'          # specify the row group label
#'          label = var_label(df[[group]]),
#'          # specify which rows, since genpop will always be on top this should
#'          # rows 2 through group_length
#'          rows = c(2:group_length)
#'        ) %>%
#'        # reorder the group order
#'        gt::row_group_order(c(NA, var_label(df[[group]]))) %>%
#'        # rename the columns
#'        gt::cols_label(
#'          mean = "Mean",
#'          n = "N",
#'          std.error = "Standard Error",
#'          conf.low = "Lower CI",
#'          conf.high = "Higher CI"
#'        ) %>%
#'        # add a title to the table of the variable label
#'        gt::tab_header(title = var_label(df[[var]]))
#'    }
#'
#'  } else {
#'    if (missing(group)) {
#'
#'      # "Returns a naked expression of the variable"
#'      var <- rlang::enexpr(var)
#'      if (!is.character(var)) {
#'        # convert to a sym() object and then use as_name to make it a string
#'        var <- rlang::as_name(rlang::ensym(var))
#'      }
#' #'
#'      ## if wt is a symbol convert it to a string
#'      # "Returns a naked expression of the variable"
#'      wt <- rlang::enexpr(wt)
#'      if (!is.character(wt)) {
#'        # convert to a sym() object and then use as_name to make it a string
#'        wt <- rlang::as_name(rlang::ensym(wt))
#'      }
#'
#'      df %>%
#'        dplyr::summarise(
#'          # calculate the mean
#'          mean = weighted.mean(.data[[var]], .data[[wt]],  na.rm = TRUE),
#'          # calculate the standard deviation
#'          sd = sd(.data[[var]], na.rm = TRUE),
#'          # get the number of respondents
#'          n = n()
#'        ) %>%
#'        dplyr::mutate(
#'          # calculate the standard error
#'          std.error = sd/sqrt(n),
#'          # calculate the lower CI
#'          conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#'          # calculate the higher CI
#'          conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#'          # round all of the numbers to the second decimal
#'          across(where(is.numeric), ~round(.x, 2))
#'        ) %>%
#'        gt() %>%
#'        # rename the columns
#'        gt::cols_label(
#'          mean = "Mean",
#'          n = "N",
#'          std.error = "Standard Error",
#'          conf.low = "Lower CI",
#'          conf.high = "Higher CI"
#'        ) %>%
#'        # add a title to the table of the variable label
#'        gt::tab_header(title = var_label(df[[var]]))
#'
#'    } else {
#'
#'      ## if var is a symbol convert it to a string
#'      # "Returns a naked expression of the variable"
#'      var <- rlang::enexpr(var)
#'      if (!is.character(var)) {
#'        # convert to a sym() object and then use as_name to make it a string
#'        var <- rlang::as_name(rlang::ensym(var))
#'      }
#'
#'      ## if group is a symbol convert it to a string
#'      # "Returns a naked expression of the variable"
#'      group <- rlang::enexpr(group)
#'      if (!is.character(group)) {
#'        # convert to a sym() object and then use as_name to make it a string
#'        group <- rlang::as_name(rlang::ensym(group))
#'      }
#'
#'      ## if wt is a symbol convert it to a string
#'      # "Returns a naked expression of the variable"
#'      wt <- rlang::enexpr(wt)
#'      if (!is.character(wt)) {
#'        # convert to a sym() object and then use as_name to make it a string
#'        wt <- rlang::as_name(rlang::ensym(wt))
#'      }
#'
#'      #
#'      if (is.null(var_label(df[[group]]))) {
#'        group_label <- group
#'      } else {
#'        group_label <- var_label(df[[group]])
#'      }
#'
#'      # get the length of the of the number of values in the group variable
#'      # and add a 1 to it. use this with gt::tab_row_group()
#'
#'      group_length <- length(unique(df[[group]])) + 1
#'
#'      # calculate the mean for the general population
#'      genpop <- df %>%
#'        dplyr::summarise(
#'          # calculate the mean
#'          mean = weighted.mean(.data[[var]], .data[[wt]],  na.rm = TRUE),
#'          # calculate the standard deviation
#'          sd = sd(.data[[var]], na.rm = TRUE),
#'          # get the number of respondents
#'          n = n()
#'        ) %>%
#'        dplyr::mutate(
#'          # calculate the standard error
#'          std.error = sd/sqrt(n),
#'          # calculate the lower CI
#'          conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#'          # calculate the higher CI
#'          conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#'          # round all of the numbers to the second decimal
#'          across(where(is.numeric), ~round(.x, 2)),
#'          # add a new variable using the group as the name
#'          {{group}} := "General Population"
#'        )
#'
#'      # calculate the mean for the group variable
#'      group <- df %>%
#'        # group the data with the group variable
#'        dplyr::group_by(.data[[group]]) %>%
#'        dplyr::summarise(
#'          # calculate the mean
#'          mean = weighted.mean(.data[[var]], .data[[wt]],  na.rm = TRUE),
#'          # calculate the standard deviation
#'          sd = sd(.data[[var]], na.rm = TRUE),
#'          # get the number of respondents
#'          n = n()
#'        ) %>%
#'        dplyr::mutate(
#'          # calculate the standard error
#'          std.error = sd/sqrt(n),
#'          # calculate the lower CI
#'          conf.low = mean - qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#'          # calculate the higher CI
#'          conf.high = mean + qt(1 - ((1 - 0.95) / 2),  n - 1) * std.error,
#'          # round all of the numbers to the second decimal
#'          across(where(is.numeric), ~round(.x, 2)),
#'        )
#'
#'      # combine the two tables and make it a
#'      bind_rows(genpop, group) %>%
#'        # remove the standard deviation
#'        select(-sd) %>%
#'        # move n to the end of the data
#'        relocate(n, .after = conf.high) %>%
#'        # moev std.error after conf.high
#'        relocate(std.error, .after = conf.high) %>%
#'        # make a gt object and set the group variable to be used as row labels
#'        # placed in the table tub
#'        gt::gt(rowname_col = group) %>%
#'        # create a row group from a collection of rows
#'        gt::tab_row_group(
#'          # specify the row group label
#'          label = group_label,
#'          # specify which rows, since genpop will always be on top this should
#'          # rows 2 through group_length
#'          rows = c(2:group_length)
#'        ) %>%
#'        # reorder the group order
#'        gt::row_group_order(c(NA, group_label)) %>%
#'        # rename the columns
#'        gt::cols_label(
#'          mean = "Mean",
#'          n = "N",
#'          std.error = "Standard Error",
#'          conf.low = "Lower CI",
#'          conf.high = "Higher CI"
#'        ) %>%
#'        # add a title to the table of the variable label
#'        gt::tab_header(title = var_label(df[[var]]))
#'    }
#'
#'  }
#'
#' }
