#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom broom.helpers tidy_attach_model tidy_add_reference_rows
#'   tidy_add_variable_labels tidy_add_term_labels tidy_add_n
#' @importFrom cli cli_abort
#' @importFrom data.table rbindlist
#' @importFrom dplyr %>% across anti_join arrange bind_cols bind_rows
#'   case_match case_when coalesce count distinct everything filter
#'   group_by group_vars last left_join mutate pick pull relocate rename
#'   row_number summarise select ungroup
#' @importFrom forcats as_factor fct_relevel
#' @importFrom furrr future_map furrr_options
#' @importFrom ggplot2 aes after_stat element_blank element_line
#'   element_rect element_text facet_wrap FacetWrap geom_col
#'   geom_errorbar geom_label geom_linerange geom_point geom_text
#'   ggplot ggproto ggproto_parent guide_legend label_wrap_gen margin
#'   panel_rows position_dodge position_dodge2 position_stack rel
#'   Stat theme theme_minimal unit vars waiver
#' @import glue
#' @importFrom graphics stars
#' @importFrom grDevices colorRampPalette rgb
#' @importFrom gt cell_borders cells_body cells_column_spanners cols_add
#'   fmt_markdown gt opt_table_lines px tab_header tab_spanner tab_style
#' @importFrom gto body_add_gt
#' @importFrom haven labelled read_sav write_sav
#' @importFrom highcharter hc_theme hc_theme_merge
#' @importFrom jtools scale_mod
#' @importFrom lifecycle badge deprecate_soft
#' @importFrom mvtnorm qmvt pmvt
#' @importFrom officer body_add_par
#' @importFrom openxlsx2 wb_color wb_add_data wb_load wb_add_worksheet
#'   wb_workbook wb_dims wb_merge_cells wb_add_fill wb_add_font
#'   wb_add_numfmt wb_add_border
#' @importFrom prismatic color
#' @importFrom purrr compact keep map map_if pmap pmap_chr reduce walk
#'   walk2
#' @importFrom readr write_csv
#' @importFrom rlang := .data caller_arg caller_env enexpr
#'   quo_is_missing sym
#' @importFrom rstudioapi isAvailable getVersion
#' @importFrom srvyr as_survey_design
#' @importFrom stats approxfun coef complete.cases df.residual lm median
#'   model.matrix na.omit qt quantile reformulate relevel sd setNames
#'   symnum weighted.mean weights vcov xtabs
#' @importFrom survey degf svyby svyglm svymean svytable
#' @importFrom systemfonts register_font
#' @importFrom tibble as_tibble new_tibble
#' @importFrom tidyr complete crossing drop_na nest nesting pivot_wider
#'   pivot_longer separate_wider_delim unnest
#' @importFrom tidyselect all_of eval_select everything where
#' @importFrom utils modifyList globalVariables head
#' @importFrom vctrs vec_ptype_abbr vec_rbind vec_cbind vec_split
#' @importFrom withr with_seed
#' @importFrom writexl write_xlsx
## usethis namespace: end
NULL
