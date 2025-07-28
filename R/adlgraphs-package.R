
#' @importFrom broom.helpers tidy_attach_model tidy_add_reference_rows
#'   tidy_add_variable_labels tidy_add_term_labels tidy_add_n
#' @importFrom cli cli_abort
#' @importFrom data.table rbindlist
#' @importFrom dplyr %>% across all_of arrange bind_cols bind_rows count
#'   everything group_by mutate rename select where anti_join ungroup last
#' @importFrom forcats as_factor fct_relevel
#' @importFrom furrr future_map furrr_options
#' @importFrom ggplot2 theme element_line element_blank element_text theme_minimal
#'   margin guide_legend geom_errorbar ggplot aes geom_col geom_text geom_label
#'   position_dodge2 position_dodge rel unit waiver element_rect position_stack
#'   facet_wrap vars ggproto FacetWrap Stat label_wrap_gen
#' @import glue
#' @importFrom graphics stars
#' @importFrom grDevices colorRampPalette rgb
#' @importFrom gt gt tab_spanner fmt_markdown tab_style
#' @importFrom gto body_add_gt
#' @importFrom haven write_sav
#' @importFrom highcharter hc_theme hc_theme_merge
#' @importFrom jtools scale_mod
#' @importFrom lifecycle badge deprecate_soft
#' @importFrom mvtnorm qmvt pmvt
#' @importFrom officer body_add_par
#' @importFrom openxlsx2 wb_color wb_add_data wb_load wb_add_worksheet wb_workbook
#'   wb_dims wb_merge_cells wb_add_fill wb_add_font wb_add_numfmt wb_add_border 
#' @importFrom prismatic color
#' @importFrom purrr map pmap pmap_chr walk map_if keep
#' @importFrom readr write_csv
#' @importFrom rlang set_names sym := .data caller_arg caller_env enexpr
#'   sym quo_is_missing
#' @importFrom rstudioapi isAvailable getVersion
#' @importFrom stats qt df.residual setNames weighted.mean sd symnum
#'   na.omit lm coef quantile xtabs reformulate complete.cases median
#'   model.matrix lm relevel
#' @importFrom systemfonts register_font
#' @importFrom tibble as_tibble new_tibble
#' @importFrom tidyr pivot_wider drop_na nest unnest separate_wider_delim
#' @importFrom tidyselect all_of eval_select where
#' @importFrom utils tail
#' @importFrom vctrs vec_ptype_abbr vec_rbind vec_cbind vec_split 
#' @importFrom withr with_seed
#' @importFrom writexl write_xlsx
#' @keywords internal
"_PACKAGE"
