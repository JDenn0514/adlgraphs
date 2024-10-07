#' Calculate the loadings in factor analysis 
#' 
#' This function creates a data frame of factor loadings from 
#' a factor analysis. It can be on an object that was created with 
#' `psych::fa()` or it can be used on a data frame. If used on a 
#' data frame, it will run it on all columns in the data frame. Also
#' works on grouped data frame if you want to check how the factor
#' loadings may change along the different levels of a group.
#' 
#' @param model Either a model created using `psych::fa()` or a 
#'   data.frame
#' @param labels Either a character vector or data frame. Creates a 
#'   new column called "labels" for each variable in the factor 
#'   analysis.
#' @param threshold The threshold with which to not show the factor 
#'   loadings. Default is 0.4.
#' @param print The printing method. Default is "short" which only prints
#'   a dataframe of the factor loadings. The alternative is "long" but
#'   that has not been created yet.
#' @param nfactors Number of factors to extract, default is 1.
#' @param fm Factoring method fm="minres" will do a minimum residual as will 
#'   fm="uls". Both of these use a first derivative. fm="ols" differs very 
#'   slightly from "minres" in that it minimizes the entire residual matrix 
#'   using an OLS procedure but uses the empirical first derivative. This 
#'   will be slower. fm="wls" will do a weighted least squares (WLS) 
#'   solution, fm="gls" does a generalized weighted least squares (GLS),
#'   fm="pa" will do the principal factor solution, fm="ml" will do a 
#'   maximum likelihood factor analysis. fm="minchi" will minimize the 
#'   sample size weighted chi square when treating pairwise correlations 
#'   with different number of subjects per pair. fm ="minrank" will do a 
#'   minimum rank factor analysis. "old.min" will do minimal residual the 
#'   way it was done prior to April, 2017 (see discussion below). fm="alpha" 
#'   will do alpha factor analysis as described in Kaiser and Coffey (1965). 
#'   Default is "pa".
#' @param rotate "none", "varimax", "quartimax", "bentlerT", "equamax", 
#'   "varimin", "geominT" and "bifactor" are orthogonal rotations. 
#'   "Promax", "promax", "oblimin", "simplimax", "bentlerQ, "geominQ" and 
#'   "biquartimin" and "cluster" are possible oblique transformations of 
#'   the solution. The default is to do a oblimin transformation, although 
#'   versions prior to 2009 defaulted to varimax. SPSS seems to do a 
#'   Kaiser normalization before doing Promax, this is done here by the 
#'   call to "promax" which does the normalization before calling Promax in 
#'   GPArotation.
#' 
#' @examples
#' 
#' library(adlgraphs)
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
#'   # specify number of factors, rotation, factor method, and threshold
#'   get_loadings(nfactors = 2, rotate = "varimax", fm = "minres", threshold = 0.2) 
#' 
#' 
#' @export

get_loadings <- function(
  model,
  labels = NULL, 
  threshold = 0.4, 
  print = "short",
  nfactors = 1,
  fm = "pa",
  rotate = "oblimin"
) {
  UseMethod("get_loadings")
}

#' @export
get_loadings.default <- function(
  model,
  labels = NULL, 
  threshold = 0.4, 
  print = "short",
  nfactors = 1,
  fm = "pa",
  rotate = "oblimin"
) {

  n <- model$factors

  # get the factor loadings, communality, and uniqueness
  loadings <- tibble::as_tibble(unclass(model$loadings), rownames = "variables") %>% 
    dplyr::mutate(
      communality = model$communality,
      uniqueness = model$uniqueness
    )
    
  if (!is.null(labels)) {
    if (is.character(labels)) {

      loadings <- loadings %>% 
        dplyr::mutate(labels = labels) %>% 
        dplyr::relocate(labels, .after = "variables") 

    } else if (is.data.frame(labels)) {

      labels <- attr_var_label(labels)
      loadings <- loadings %>% 
        dplyr::mutate(labels = labels) %>% 
        dplyr::relocate(labels, .after = "variables") 
    }
      
  }

  if (is.null(labels)) {
    # get the columns with loadings in them
    loading_cols <- 2:(n + 1)
  } else {
    # get the columns with loadings in them
    loading_cols <- 3:(n + 2)
  }
  
 
  # 
  loadings[, loading_cols][abs(loadings[, loading_cols]) < threshold] <- NA

  loadings 

}

#' @export
get_loadings.data.frame <- function(
  model,
  labels = NULL, 
  threshold = 0.4, 
  print = "short",
  nfactors = 1,
  fm = "pa",
  rotate = "oblimin"
) {

  model <- model %>% 
    psych::fa(
      nfactors = nfactors,
      fm = fm,
      rotate = rotate
    )
  
  get_loadings.default(model, labels = labels, threshold = threshold)
  
  
}

#' @export
get_loadings.grouped_df <- function(
  model,
  labels = NULL, 
  threshold = 0.4, 
  print = "short",
  nfactors = 1,
  fm = "pa",
  rotate = "oblimin"
) {
  # extract the df in the "groups" attribute
  group_labs <- attr(model, "groups") %>% 
    # remove the .rows column
    dplyr::select(-`.rows`)
  # get the column names, these are the grouping variables
  group_labs <- colnames(group_labs)

  leng_groups <- length(group_labs)

  if (leng_groups > 1) {
    # if there are multiple grouping variables

    # make it a nested data set
    nest_data <- model %>% 
      tidyr::nest() %>% 
      tidyr::drop_na(everything())

    for (n in c(1:leng_groups)) {
      if (n == 1) {
        string <- paste('.data[[group_labs[1]]]')
      } else {
        string <- paste0(string, ', " - ", .data[[group_labs[', n, ']]]')
      }
      new_string = paste0("paste0(", string, ")")
    }

    # get the groups 
    # we will combine this with the correlations
    just_groups <- nest_data %>% 
      # remove the data column so it's just the groups
      dplyr::select(-data) %>% 
      # combined the grouping variabels
      dplyr::mutate(groups_combined = eval(parse(text = new_string))) %>% 
      # extract it as a vector
      dplyr::pull(groups_combined)
    
    

    # make the correlation dataframe
    load_df <- purrr::map(
      # we are iterating over the data column
      nest_data$data, 
      # use get_loadings.data.frame to get the individuals loadings
      ~get_loadings.data.frame(
        model = .x, 
        labels = labels, 
        threshold = threshold,
        nfactors = nfactors, 
        fm = fm, 
        rotate = rotate)
    ) 

    if (print == "short") {
      # name the objects in the list
      names(load_df) <- just_groups
      # create the output data frame
      # combine the list objects and make a new variable 
      # called "groups" containing the names of each list
      out <- dplyr::bind_rows(load_df, .id = "groups") %>% 
        # split up the string by the - and use the group_labs vector as the names
        tidyr::separate_wider_delim(groups, delim = " - ", names = c(group_labs)) %>% 
        # group the data by the vector group variables
        dplyr::group_by(across(all_of(group_labs))) %>% 
        # arrange by the groups
        dplyr::arrange(.by_group = TRUE)



      attr(out$variables, "label") <- "Variables"
      attr(out$communality, "label") <- "Common Variance"
      attr(out$uniqueness, "label") <- "Unique Variance"

      return(out)
    }
    

  } else {
    # if there is only one grouping variable

    # make it a nested data set
    nest_data <- model %>% 
      tidyr::nest() %>% 
      tidyr::drop_na(everything())

    # get the groups 
    # we will combine this with the correlations
    just_groups <- nest_data %>% 
      dplyr::pull(-data) 

    # make the correlation dataframe
    load_df <- purrr::map(
      # we are iterating over the data column
      nest_data$data, 
      # use get_loadings.data.frame to get the individuals loadings
      ~get_loadings.data.frame(
        model = .x, 
        labels = labels, 
        threshold = threshold,
        nfactors = nfactors, 
        fm = fm, 
        rotate = rotate)
    ) 

    if (print == "short") {
      # name the objects in the list
      names(load_df) <- just_groups
      # combine the rows and make a new label called 
      out <- dplyr::bind_rows(load_df, .id = "groups")

      # combine the columns of the grouping variable with the correlation
      # out <- dplyr::bind_cols(just_groups, load_df)
      # sort the first two columns
      out <- out %>% 
        dplyr::arrange(.by_group = TRUE)

      attr(out$variables, "label") <- "Variables"
      attr(out$communality, "label") <- "Common Variance"
      attr(out$uniqueness, "label") <- "Unique Variance"

      return(out)
    }

  }

  

}











