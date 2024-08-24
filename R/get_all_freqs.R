#' Export frequencies for a set of variables to a word doc.
#'
#' This function uses [get_freq_table()] to get the frequencies for a set of variables
#' suppplied by the user. It then outputs these frequencies to a word doc.
#'
#' @param data An object of type data.frame or tibble. If piping the data into the
#'   function, this is not required.
#' @param x A vector of variables you want to get the frequencies for.
#' @param group A character string. The first grouping variable.
#' @param wt A character string. Add if you have a weighting variable and want
#'   to get weighted frequencies
#' @param show_genpop Logical. Should there be a column showing the frequencies
#'   for the general population
#' @param file_name A character string specifying the name of the file to be
#'   created with the frequencies and where the file will be located. File must
#'   end in .docx
#'
#' @export
get_all_freqs <- function(data, x, group, wt, show_genpop, file_name) {

  if (missing(wt)) {

    if (missing(group)) {

      # get the number of variables in the x vector
      leng <- length(x)
      # create a list of the dataframes
      data_list <- rep(list(data), leng)
      # get the frequencies
      freqs <- purrr::pmap(list(data = data_list, x = x, show_genpop = show_genpop), get_freq_table)

    } else {

      # get the number of variables in the x vector
      leng <- length(x)
      # create a list of the dataframes
      data_list <- rep(list(data), leng)
      # create a vector of group
      group_list <- replicate(leng, group)
      # get the frequencies
      freqs <- purrr::pmap(list(data = data_list, x = x, group = group_list, show_genpop = show_genpop), get_freq_table)

    }

  } else {

    if (missing(group)) {

      # get the number of variables in the x vector
      leng <- length(x)
      # create a list of the dataframes
      data_list <- rep(list(data), leng)
      # create a vector of the weights
      wt_list <- replicate(leng, wt)
      # get the frequencies
      freqs <- purrr::pmap(list(data = data_list, x = x, wt = wt_list, show_genpop = show_genpop), get_freq_table)

    } else {

      # get the number of variables in the x vector
      leng <- length(x)
      # create a list of the dataframes
      data_list <- rep(list(data), leng)
      # create a vector of group
      group_list <- replicate(leng, group)
      # create a vector of the list
      wt_list <- replicate(leng, wt)
      # get the frequencies
      freqs <- purrr::pmap(list(data = data_list, x = x, group = group_list, wt = wt_list, show_genpop = show_genpop), get_freq_table)

    }

  }


  my_doc <- officer::read_docx()

  # Print the tables
  purrr::walk(freqs, write_word_table, my_doc)
  print(my_doc, target = file_name) %>% invisible()

}
