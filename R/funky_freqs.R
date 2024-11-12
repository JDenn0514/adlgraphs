#' Calculate weighted frequencies
#' 
#' **THIS FUNCTION IS EXPERIMENTAL**
#'
#' Use this function to calculate simple weighted frequencies weighted grouped.
#' You can also specify a grouping variable by which you want to calculate the
#' frequencies.
#'
#' The `x`, `group`, and `wt` arguments can either be strings or symbols
#' (meaning they can have quotes or no quotes). The benefit of this is that it
#' makes it really easy to iterate this function over a list or vector of
#' variables with other functions like [map()] [purrr::map()] or [walk()]
#' [purrr::walk()] that are found in the `purrr` package.
#'
#' @param data An object of type data.frame or tibble. If piping the data into the
#'   function, this is not required.
#' @param x Either a character string or symbol. The variable with which want
#'   to get the frequencies.
#' @param group <[`tidy-select`][dplyr_tidy_select]> A selection of columns to 
#'   group the data by in addition to `treats`. This operates very similarly
#'   to `.by` from dplyr (for more info on that see [?dplyr_by][dplyr_by]). 
#' @param wt Weights. Add if you have a weighting variable and want to get
#'   weighted frequencies.
#' @param drop_zero Logical. Determines if rows with 0 should be removed 
#'   Default is `FALSE`.
#' @param na.rm Logical. Determines if NAs should be kept or removed Default is
#'   `TRUE`.



funky_freqs <- function(
  data, 
  x, 
  group = NULL, 
  wt = NULL, 
  drop_zero = FALSE,
  na.rm = TRUE
) {
  x_str <- accept_string_or_sym({{ x }})

  if (is.null(substitute(wt))) {
    wt_str <- "wts"
    warning("no weights argument given, using uniform weights of 1")
    data[[wt_str]] <- rep(1, length(data[[x_str]]))  
  } else {
    wt_str <- accept_string_or_sym({{ wt }})
    # set NAs in wt variable to zero
    data[[wt_str]][is.na(data[[wt_str]])] <- 0
  }
  
  group_sub <- substitute(group)

  if (is.null(group_sub)) {
    ### if there is no grouping variable

    # get the variable name
    x_name <- rlang::enexpr(x)
    # set data to only include x and wt, but use string version
    data <- data[c(x_str, wt_str)]

    if (isTRUE(na.rm)) {
      # if na.rm is true remove NAs 
      data <- data[stats::complete.cases(data),]
      # get the unique observations after removing NAs
      x_vals <- sort(unique(data[[x_str]]), na.last = TRUE)
    } else {
      # get the unique observations (this includes NA)
      x_vals <- sort(unique(data[[x_str]]), na.last = TRUE)
      # convert data[[x]] into a factor and add NA as a new factor level
      data[[x_str]] <- addNA(data[[x_str]])
    }

    # get the total number of observations
    len <- length(data[[x_str]])
    # get the number of observations per level (frequencies)
    freqs <- tapply(data[[wt_str]], data[[x_str]], sum, simplify = TRUE)
    # get the names from the freq array
    freq_names <- names(freqs)
    # convert freqs to a vector
    freqs <- as.vector(freqs)
    # add names to freqs
    names(freqs) <- freq_names

    if (length(x_vals) != length(freqs)) {
      # if the length of x_vals is different from the length of freqs
      # use the values in x_vals to keep certain values in freqs
      freqs <- freqs[names(freqs) %in% x_vals]
    } 

    # get pct
    pct <- sapply(freqs, \(y) y / len)
    
    # create the output tibble
    out <- tibble::tibble(
      "{ x_name }" := x_vals,
      n = freqs,
      pct = pct
    )

    if (!is.null(attr_val_labels(out[[x_str]]))) {
      # if x has value labels convert to a factor
      out[[x_str]] <- make_factor(out[[x_str]])
    }

  } else if (!is.null(group_sub)) {
    # get the group labels
    group_labs <- eval_select_by({{ group_sub }}, data)
    # get a vector of all relevant variables
    vars <- c(x_str, group_labs, wt_str)
    # keep only the relevant variables
    data <- data[c(vars)]

    if (isTRUE(na.rm)) {
      # if na.rm = TRUE remove all NAs
      data <- data[stats::complete.cases(data),]
    }
    # remove the weights variable from the vector of variables
    vars <- vars[vars != wt_str]

    # get the columns that are numeric
    vars_num <- colnames(data[vars][sapply(data[vars], is.numeric)])
    
    # get the number of observations
    freqs <- stats::xtabs(
      # use reformulate to create the formula
      stats::reformulate(termlabels = vars, response = wt_str), 
      data = data,
      na.rm = na.rm,
      addNA = TRUE
    ) 
    # create the proportions and convert it to dataframe
    props <- as.data.frame(proportions(freqs, margin = group_labs))
    # convert the table object into a dataframe
    freqs <- as.data.frame(freqs, responseName = "n")
    # create new column in freqs called pct using the column Freq from props
    freqs$pct <- props$Freq
    
    # convert the original numeric variables back into numeric
    if (length(vars_num) == 1) {
      freqs[[vars_num]] <- as.vector(sapply(freqs[[vars_num]], as.numeric, simplify = TRUE))
    } else if (length(vars_num) > 1) {
      freqs[vars_num] <- sapply(freqs[vars_num], as.numeric, simplify = TRUE)
    }
    # add value labels back to the data
    freqs[vars_num] <- add_labels(freqs, data, vars_num)
    # convert the vectors with labels to factors
    freqs[vars_num] <- label_to_factor(freqs, vars_num)
    # convert character vectors to factors
    freqs[vars] <- character_to_factor(freqs, data, vars)

    # convert freqs to data.table for speed
    freqs <- data.table::data.table(freqs)
    # set the order of the data.table with NAs last
    freqs <- data.table::setorderv(freqs, c(group_labs, x_str), na.last = TRUE)
    
    # convert to a tibble
    out <- tibble::as_tibble(freqs) 
    # 
    out <- out[,c(group_labs, x_str, "n", "pct")]

    # get the variable labels as a named list
    group_labels <- attr_var_label(data[,group_labs])
    # check to make sure the group_labels are in
    # group_labels <- group_labels[names(group_labels) %in% names(out)]
    for (x in names(group_labels)) attr(out[[x]], "label") <- group_labels[[x]]
    
  }

  if (isTRUE(drop_zero)) {
    out <- out[out$n != 0,]
  }

  class_names <- class(out)

  attr(out, "variable_label") <- attr_var_label(data[[x_str]])

  if (!is.null(attr_var_label(data[[x_str]]))) {
    attr(out[[x_str]], "label") <- attr_var_label(x_str, data)
  }

  attr(out$n, "label") <- "N"
  attr(out$pct, "label") <- "Percent"
  

  out %>% structure(class = c("adlgraphs_freqs", class_names))


}

label_to_factor <- function(data, cols) {
  # convert character vectors to factors
  lapply(
    # perform the function only over the common data frames
    cols |> setNames(nm = _), 
    # write the anonymous function
    \(y) {
      if (!is.null(attr_val_labels(data[[y]]))) {
        # if data[[y]] has value labels

        # convert data[[y]] to factor
        make_factor(data[[y]])

      } else {
        # if there aren't value labels just return data[[y]]
        data[[y]]
      }
    }
  )
}

add_labels <- function(new, old, cols) {
  # convert character vectors to factors
  lapply(
    # perform the function only over the common data frames
    cols |> setNames(nm = _), 
    # write the anonymous function
    \(y) {
      if (!is.null(attr_val_labels(old[[y]]))) {
        # if old[[y]] has levels
        # set new[[y]] as factor with levels from old[[y]]
        haven::labelled(
          new[[y]],
          labels = attr_val_labels(old[[y]])
        )
      } else {
        new[[y]]
      }
    }
  )
}


