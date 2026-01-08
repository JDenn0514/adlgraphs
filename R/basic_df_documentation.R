#' Example Survey Data
#'
#' A small example dataset containing simulated survey responses with
#' survey design variables, demographics, and various question types.
#' Variables include labels and value labels as attributes, mimicking
#' data imported from SPSS or other statistical software.
#'
#' @format A tibble with 12 rows and 31 variables:
#' \describe{
#'   \item{id, id2}{Respondent identifiers}
#'   \item{grp}{Grouping variable (A, B, C)}
#'   \item{strata, strata2}{Stratification variables}
#'   \item{psu}{Primary sampling unit (1-6)}
#'   \item{ssu}{Secondary sampling unit (1-12)}
#'   \item{fpc_psu, fpc_ssu}{Finite population correction values for each stage}
#'   \item{wts, w2}{Survey weights}
#'   \item{age}{Respondent age in years}
#'   \item{income}{Annual income in dollars}
#'   \item{satisfaction_service, satisfaction_price, satisfaction_quality, satisfaction_support}{Satisfaction ratings (1-5 Likert scale)}
#'   \item{agree_recommend, agree_repurchase, agree_trust}{Agreement ratings (1-7 scale)}
#'   \item{freq_use_product, freq_visit_store, freq_contact_support}{Frequency counts (times per month)}
#'   \item{x1, x2}{Binary yes/no questions}
#'   \item{x3}{Categorical variable (1 or 2)}
#'   \item{x4}{Logical variable}
#'   \item{rating_overall, rating_value, rating_experience}{Rating scales (0-10)}
#' }
#'
#' @details
#' Each survey question variable has the following attributes:
#' - `label`: A descriptive label for the variable
#' - `labels`: Named vector of value labels
#' - `question_preface`: The question stem shown to respondents
#'
#' @examples
#' # View the data
#' basic_df
#'
#' # Access variable label
#' attr_var_label(basic_df$satisfaction_service)
#'
#' # Access value labels
#' attr_val_labels(basic_df$satisfaction_service)
#'
"basic_df"
