#' Calculate the loadings in factor analysis 
#' 
#' This function creates a data frame of factor loadings from 
#' a factor analysis. It can be on an object that was created with 
#' `psych::fa()` or it can be used on a data frame. If used on a 
#' data frame, it will run it on all columns in the data frame. Also
#' works on grouped data frame if you want to check how the factor
#' loadings may change along the different levels of a group.
#' 
#' @param data Either a data created using `psych::fa()` or a 
#'   data.frame
#' @param cols  <[`tidy-select`][dplyr_tidy_select]> The variables you want 
#'   to get the correlations for. 
#' @param group <[`tidy-select`][dplyr_tidy_select]> A selection of columns to 
#'   group the data by in addition to `treats`. This operates very similarly
#'   to `.by` from dplyr (for more info on that see [?dplyr_by][dplyr_by]). 
#'   See examples to see how it operates.
#' @param labels Either a character vector or data frame. Creates a 
#'   new column called "labels" for each variable in the factor 
#'   analysis.
#' @param threshold The threshold with which to not show the factor 
#'   loadings. Default is 0.4.
#' @param print The printing method. Default is "short" which only prints
#'   a dataframe of the factor loadings. The alternative is "long" but
#'   that has not been created yet.
#' @param nfactors Number of factors to extract, default is 1.
#' @param fm Factoring method used in the factor analysis. Default is 
#'   "pa". For more information on the various methods see the `fm`
#'   argument in the `psych::fa()` function. 
#' @param rotate The type of factor rotation to perform when conducting
#'   the factor analysis. Default is "oblimin". For more information on 
#'   the different rotation methods available, check the documentation 
#'   for the `rotate` argument in `psych::fa()`.
#' @param sort Logical. If `TRUE`, the default, the loadings are sorted 
#'   largest to smallest. If `FALSE`, no sorting occurs.
#' @param decimals Number of decimals each number should be rounded to. Default
#'   is 3.
#' @param na.rm Logical. Default is `TRUE` which removes NAs prior to 
#'   calculation.
#' 
#' @returns A data.frame showing factor loadings. 
#' 
#' @examples
#' 
#' library(dplyr)
#' library(psych)
#' 
#' # first lets get our data set
#' data <- test_data %>% 
#'   select(top:run)
#' 
#' # now create the fa object
#' model <- fa(data, nfactors = 1, fm = "pa", rotate = "oblimin")
#' # get just the loadings
#' get_loadings(model)
#' # get the loadings with the variable labels based on data object
#' get_loadings(model, data)
#' 
#' # we can do all of this in one step with pipes
#' test_data %>% 
#'   # select only the variables we want in the factor analysis
#'   select(top:run) %>% 
#'   # run the factor analysis
#'   fa(., nfactors = 1, fm = "pa", rotate = "oblimin") %>% 
#'   # get the loadings
#'   get_loadings()
#' 
#' # Now let's remove the threshold for the loadings and include labels
#' test_data %>% 
#'   # select only the variables we want in the factor analysis
#'   select(top:run) %>% 
#'   # run the factor analysis
#'   fa(., nfactors = 1, fm = "pa", rotate = "oblimin") %>% 
#'   # specify threshold is 0
#'   get_loadings(threshold = 0, labels = data)
#' 
#' # alternatively, we could skip the fa step entirely like so
#' test_data %>% 
#'   # select only the variables we want in the factor analysis
#'   select(top:run) %>% 
#'   # specify number of factors, rotation, and factor method
#'   get_loadings()
#' 
#' # we can also specify the number of factors, rotation, and factoring method
#' test_data %>% 
#'   # select only the variables we want in the factor analysis
#'   select(top:run) %>% 
#'   # specify number of factors, rotation, factor method, and threshold
#'   get_loadings(nfactors = 2, rotate = "varimax", fm = "minres", threshold = 0.2) 
#' 
#' # we can also calculate the factor loadings by a grouping variable
#' test_data %>% 
#'   # select the grouping variable and the variables to be used in factor analysis
#'   select(edu_f2, top:run) %>% 
#'   # group the data
#'   group_by(edu_f2) %>% 
#'   # specify number of factors, and the threshold
#'   get_loadings(nfactors = 2, threshold = 0.2) 
#' 
#' # let's repeat the previous analysis, but use internal arguments to select the 
#' # columns to include in the factor analysis and the grouping variables
#' test_data %>% 
#'   # group indicates we want to run the factor analysis across each level in edu_f2
#'   # nfactors specifies we want two factors
#'   # threshold sets the cut off for the loadings
#'   get_loadings(
#'     # specify the variables to include in the factor analysis
#'     cols = c(top:run), 
#'     # specify the group variable, run separate factor analyses for each level
#'     group = edu_f2, 
#'     # specify two factors
#'     nfactors = 2, 
#'     # specify the loadings are cut off at |0.2|
#'     threshold = 0.2
#'   ) 
#' 
#' 
#' 
#' @export
get_loadings <- function(
  data,
  cols,
  group,
  labels = NULL, 
  threshold = 0.4, 
  print = "short",
  nfactors = 1,
  fm = "pa",
  rotate = "oblimin",
  sort = TRUE,
  decimals = 3,
  na.rm = FALSE
) {
  UseMethod("get_loadings")
}



#' @export
get_loadings.psych <- function(
  data,
  cols,
  group,
  labels = NULL, 
  threshold = 0.4, 
  print = "short",
  nfactors = 1,
  fm = "pa",
  rotate = "oblimin",
  sort = TRUE,
  decimals = 3,
  na.rm = FALSE
) {

  if (!missing(cols)) {
    cli::cli_inform(c(
      "{.arg cols} is only valid when supplying an object of class {.cls data.frame}", 
      "You've supplied an object with class {.cls {class(data)}}"
    ))
  }
  
  if (!missing(group)) {
    cli::cli_inform(c(
      "{.arg group} is only valid when supplying an object of class {.cls data.frame}", 
      "You've supplied an object with class {.cls {class(data)}}"
    ))
  }

  n <- data$factors

  # get the factor loadings, communality, and uniqueness
  out <- as.data.frame(unclass(data$loadings))
  out$variables <- rownames(out)
  out <- out[c("variables", names(out[names(out) != "variables"]))]
  rownames(out) <- NULL
  # add communality as new column
  out$communality <- data$communality
  # add uniqueness as new column
  out$uniqueness <- data$uniqueness
    
  if (!is.null(labels)) {
    if (is.character(labels)) {
      # add the labels as a new column since its a character vector
      out$labels <- labels
      # move labels after variables
      out <- out[c("variables", "labels", names(out[!names(out) %in% c("variables", "labels")]))]
    } else if (is.data.frame(labels)) {
      # get the variable labels since its a data frame
      labels <- attr_var_label(labels)
      # add the labels as a new column
      out$labels <- labels
      # move the labels column to come after the variables column
      out <- out[c("variables", "labels", names(out[!names(out) %in% c("variables", "labels")]))]
    }
      
  }

  if (is.null(labels)) {
    # get the columns with loadings in them
    loading_cols <- seq(from = 2, to = n + 1)
    # load_names <- names(out[!names(out) %in% c("variables", "labels", "communality", "uniqueness")])
  } else {
    # get the columns with loadings in them
    loading_cols <- seq(from = 3, to = n + 2)
    # load_names <- names(out[!names(out) %in% c("variables", "communality", "uniqueness")])
  }

  attr(out, "loadings_columns") <- loading_cols

  # set columns with value below the threshold as NA
  out[, loading_cols][abs(out[, loading_cols]) < threshold] <- NA
  
  if (sort) {
    # if sort is true, sort by absolute values  
    out <- sort_by(out, -abs(out[,loading_cols]))
  }

  # get the numeric columns
  num_cols <- c(loading_cols, max(loading_cols) + 1, max(loading_cols + 2))
  # round the numeric columns
  out[, num_cols] <- round(out[, num_cols], decimals)

  # add variable labels
  attr(out$communality, "label") <- "Communality"
  attr(out$uniqueness, "label") <- "Uniqueness"
  attr(out$variables, "label") <- "Variable Name"
  if (!is.null(labels)) attr(out$labels, "label") <- "Variable Label"

  # return(rev(names(out[loading_cols])))

  # # set the variable names for the loading columns as Factor X
  for (x in rev(names(out[loading_cols]))) {
    attr(out[[x]], "label") <- sub("[A-Z]+", "Factor ", x)
  }
  
  
  # set the class attributes
  class(out) <- c("adlgraphs_loadings", "tbl_df", "tbl", "data.frame")

  out

}

#' @export
get_loadings.factanal <- function(
  data,
  cols,
  group,
  labels = NULL, 
  threshold = 0.4, 
  print = "short",
  nfactors = 1,
  fm = "pa",
  rotate = "oblimin",
  sort = TRUE,
  decimals = 3,
  na.rm = FALSE
) {

  if (!missing(cols)) {
    cli::cli_inform(c(
      "{.arg cols} is only valid when supplying an object of class {.cls data.frame}", 
      "You've supplied an object with class {.cls {class(data)}}"
    ))
  }
  
  if (!missing(group)) {
    cli::cli_inform(c(
      "{.arg group} is only valid when supplying an object of class {.cls data.frame}", 
      "You've supplied an object with class {.cls {class(data)}}"
    ))
  }
  
  n <- data$factors

  # get the factor loadings, communality, and uniqueness
  out <- as.data.frame(unclass(data$loadings))
  # use the rownames to set the variables label
  out$variables <- rownames(out)
  # reorder the columns so variables is first
  out <- out[c("variables", names(out[names(out) != "variables"]))]
  rownames(out) <- NULL

  # add communality as new column
  out$communality <- 1 - data$uniqueness
  # add uniqueness as new column
  out$uniqueness <- data$uniqueness

    
  if (!is.null(labels)) {
    if (is.character(labels)) {
      # add the labels as a new column since its a character vector
      out$labels <- labels
      # move labels after variables
      out <- out[c("variables", "labels", names(out[!names(out) %in% c("variables", "labels")]))]
    } else if (is.data.frame(labels)) {
      # get the variable labels since its a data frame
      labels <- attr_var_label(labels)
      # add the labels as a new column
      out$labels <- labels
      # move the labels and column to come after the variables column
      out <- out[c("variables", "labels", names(out[!names(out) %in% c("variables", "labels")]))]
    }
      
  }

  if (is.null(labels)) {
    # get the columns with loadings in them
    loading_cols <- seq(from = 2, to = n + 1)
    # load_names <- names(out[!names(out) %in% c("variables", "labels", "communality", "uniqueness")])
  } else {
    # get the columns with loadings in them
    loading_cols <- seq(from = 3, to = n + 2)
    # load_names <- names(out[!names(out) %in% c("variables", "communality", "uniqueness")])
  }

  attr(out, "loadings_columns") <- loading_cols

  # set columns with value below the threshold as NA
  out[, loading_cols][abs(out[, loading_cols]) < threshold] <- NA
  
  if (sort) {
    # if sort is true, sort by absolute values  
    out <- sort_by(out, -abs(out[,loading_cols]))
  }

  # get the numeric columns
  num_cols <- c(loading_cols, max(loading_cols) + 1, max(loading_cols + 2))
  # round the numeric columns
  out[, num_cols] <- round(out[, num_cols], decimals)

  # add variable labels
  attr(out$communality, "label") <- "Communality"
  attr(out$uniqueness, "label") <- "Uniqueness"
  attr(out$variables, "label") <- "Variable Name"
  if (!is.null(labels)) attr(out$labels, "label") <- "Variable Label"

  # # set the variable names for the loading columns as Factor X
  for (x in seq(n)) {
    if (!missing(labels)) {
      var <- x + 2
    } else {
      var <- x + 1
    }
    attr(out[[var]], "label") <- paste("Factor", x)
  }
  
  # set the class attributes
  class(out) <- c("adlgraphs_loadings", "tbl_df", "tbl", "data.frame")

  out

}

#' @export
get_loadings.data.frame <- function(
  data,
  cols,
  group,
  labels = NULL, 
  threshold = 0.4, 
  print = "short",
  nfactors = 1,
  fm = "pa",
  rotate = "oblimin",
  sort = TRUE,
  decimals = 3,
  na.rm = FALSE
) {

  # Prepare group variables
  # if the data is grouped, get them using attr(data, "groups"), else set to NULL
  group_names <- if(inherits(data, "grouped_df")) setdiff(names(attr(data, "groups")), ".rows") else NULL
  # if group arg is missing set to NULL, else use as.character(substitute()) to capture it
  group_vars <- if (missing(group)) NULL else select_groups({{ group }}, data)
  # remove the "c" from the group_vars vector if it is there
  group_vars <- group_vars[group_vars != "c"]
  # combine group_names and group_vars for the final vector of group names
  # use unique to make sure there aren't any duplicates
  group_names <- unique(c(group_names, group_vars))
    
  # if cols is not missing, keep only the group_names and variables in cols
  if (!missing(cols)) data <- data %>% dplyr::select(tidyselect::all_of(group_names), {{ cols }})


  # get the variable labels except for the group variables
  labels <- attr_var_label(data[!names(data) %in% group_names])

  if (na.rm) data <- data[stats::complete.cases(data),]
  
  if (!is.null(group_names)) {
    # if the groups are not null

    # create a nested data frame
    nest_data <- make_nested(data, {{ group_names }})

    # iterate over each nested tibble from 
    # TODO: Add the fa object as an attribute in the output
    out <- purrr::map(
      nest_data$data,
      ~ psych::fa(
        .x,
        nfactors = nfactors,
        fm = fm,
        rotate = rotate
      ) %>% 
        get_loadings(
          labels = labels,
          threshold = threshold,
          sort = sort
        )
    ) %>% 
      stats::setNames(nest_data$name) 

    # combine the lists
    out <- vctrs::vec_rbind(!!!out, .names_to = "groups") %>% 
      tidyr::separate_wider_delim(cols = "groups", delim = "_", names = group_names)

  } else {
    out <- psych::fa(
      r = data,
      nfactors = nfactors,
      fm = fm,
      rotate = rotate
    ) %>% 
      get_loadings(
        labels = labels,
        threshold = threshold,
        sort = sort,
        decimals = decimals
      )
  }



  # add variable labels
  attr(out$communality, "label") <- "Communality"
  attr(out$uniqueness, "label") <- "Uniqueness"
  attr(out$variables, "label") <- "Variable Name"
  if ("labels" %in% names(out)) {
    attr(out$labels, "label") <- "Variable Label"
    # get the names of the loading variables
    load_names <- names(out[!names(out) %in% c(group_names, "variables", "labels", "communality", "uniqueness")])
  } else {
    # get the names of the loading variables
    load_names <- names(out[!names(out) %in% c(group_names, "variables", "communality", "uniqueness")])
  }
    

  # # set the variable names for the loading columns as Factor X
  for (x in rev(load_names)) {
    attr(out[[x]], "label") <- sub("[A-Z]+", "Factor ", x)
  }
  
  if (!is.null(group_names)) {
    # if there are groups add the value labels

    # get the variable labels as a named list
    group_labels <- attr_var_label(data[,group_names])
    # for each value in names(group_labels) add the variable label from group_labels
    for (y in names(group_labels)) attr(out[[y]], "label") <- group_labels[[y]]

  }

  # add an attribute containing the names of the grouping variables
  attr(out, "group_names") <- group_names

  # set the class attributes
  class(out) <- c("adlgraphs_loadings", "tbl_df", "tbl", "data.frame")
  
  out

}








