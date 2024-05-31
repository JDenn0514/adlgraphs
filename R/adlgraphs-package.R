
#' @importFrom dplyr %>% across where group_by everything mutate select
#' bind_cols count rename all_of
#' @importFrom grDevices colorRampPalette rgb
#' @importFrom prismatic color
#' @importFrom graphics rect par image text
#' @importFrom ggplot2 theme element_line element_blank element_text theme_minimal
#' margin guide_legend geom_errorbar ggplot aes geom_col geom_text geom_label
#' position_dodge2 position_dodge rel unit waiver element_rect position_stack
#' facet_wrap vars ggproto FacetWrap Stat
#' @import showtext
#' @importFrom rlang set_names sym := .data caller_arg caller_env
#' @importFrom sysfonts font_add_google
#' @importFrom gt gt tab_spanner fmt_markdown tab_style
#' @importFrom cli cli_abort
#' @importFrom labelled var_label
#' @import glue
#' @importFrom utils tail
#' @importFrom stats qt df.residual setNames weighted.mean sd
#' @importFrom tidyr pivot_wider drop_na
#' @importFrom purrr pmap walk
#' @importFrom stringr str_replace str_detect
#' @importFrom haven as_factor is.labelled
#' @importFrom broom.helpers tidy_attach_model tidy_add_reference_rows
#' tidy_add_variable_labels tidy_add_term_labels tidy_add_n
#' @importFrom forcats as_factor fct_relevel
#' @importFrom gto body_add_gt
#' @importFrom jtools scale_mod
#' @importFrom officer body_add_par
#' @importFrom tibble as_tibble
#' @importFrom sjlabelled as_label get_labels
#' @importFrom waldo compare
#' @importFrom highcharter hc_theme hc_theme_merge
#' @keywords internal
"_PACKAGE"
