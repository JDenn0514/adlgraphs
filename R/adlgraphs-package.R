
#' @importFrom broom.helpers tidy_attach_model tidy_add_reference_rows
#'   tidy_add_variable_labels tidy_add_term_labels tidy_add_n
#' @importFrom cli cli_abort
#' @importFrom dplyr %>% across all_of arrange bind_cols bind_rows count
#'   everything group_by mutate rename select where
#' @importFrom forcats as_factor fct_relevel
#' @importFrom ggplot2 theme element_line element_blank element_text theme_minimal
#'   margin guide_legend geom_errorbar ggplot aes geom_col geom_text geom_label
#'   position_dodge2 position_dodge rel unit waiver element_rect position_stack
#'   facet_wrap vars ggproto FacetWrap Stat label_wrap_gen
#' @import glue
#' @importFrom graphics stars
#' @importFrom grDevices colorRampPalette rgb
#' @importFrom gt gt tab_spanner fmt_markdown tab_style
#' @importFrom gto body_add_gt
#' @importFrom haven as_factor is.labelled write_sav
#' @importFrom highcharter hc_theme hc_theme_merge
#' @importFrom jtools scale_mod
#' @importFrom labelled set_variable_labels set_value_labels
#' @importFrom marquee classic_style element_marquee
#' @importFrom mvtnorm qmvt pmvt
#' @importFrom officer body_add_par
#' @importFrom PMCMRplus dunnettTest
#' @importFrom prismatic color
#' @importFrom purrr map pmap walk
#' @importFrom readr write_csv
#' @importFrom rlang set_names sym := .data caller_arg caller_env enexpr
#' @importFrom rstudioapi isAvailable getVersion
#' @importFrom sjlabelled as_label get_labels
#' @importFrom stats qt df.residual setNames weighted.mean sd symnum
#'   na.omit lm coef quantile
#' @importFrom stringr str_replace str_detect
#' @importFrom systemfonts register_font
#' @importFrom tibble as_tibble enframe
#' @importFrom tidyr pivot_wider drop_na nest
#' @importFrom tidyselect all_of
#' @importFrom utils tail
#' @importFrom vctrs vec_ptype_abbr
#' @importFrom waldo compare
#' @importFrom withr with_seed
#' @importFrom writexl write_xlsx
#' @keywords internal
"_PACKAGE"
