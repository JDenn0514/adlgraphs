# Package index

## Graph Wrappers

These are wrapper functions around common
[ggplot2](https://ggplot2.tidyverse.org) functions. They’re designed to
make it easier to make graphs.

- [`adl_bar_plots()`](https://jdenn0514.github.io/adlgraphs/reference/adl_bar_plots.md)
  : Create bar plots in ADL's style
- [`adl_coef_plots()`](https://jdenn0514.github.io/adlgraphs/reference/adl_coef_plots.md)
  : Create coefficient plots in ADL's style
- [`adl_lollipop_plots()`](https://jdenn0514.github.io/adlgraphs/reference/adl_lollipop_plots.md)
  : Lollipop plots with optional dodging and auto label offsets
- [`stat_density_quant()`](https://jdenn0514.github.io/adlgraphs/reference/stat_density_quant.md)
  : Stat for density ridgeline plots

## Themes

Full blown [ggplot2](https://ggplot2.tidyverse.org) themes that affect
almost all visual aspects of a graph. These are similar to
[ggplot2](https://ggplot2.tidyverse.org) themes like
[`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
or
[`ggplot2::theme_gray()`](https://ggplot2.tidyverse.org/reference/ggtheme.html).

- [`theme_default()`](https://jdenn0514.github.io/adlgraphs/reference/theme_default.md)
  : Default theme
- [`theme_v_bar()`](https://jdenn0514.github.io/adlgraphs/reference/theme_v_bar.md)
  : Theme for vertical non-stacked bar plots
- [`theme_h_bar()`](https://jdenn0514.github.io/adlgraphs/reference/theme_h_bar.md)
  : Theme for non-stacked horizontal bar plots
- [`theme_h_stack()`](https://jdenn0514.github.io/adlgraphs/reference/theme_h_stack.md)
  : Theme for horizontal stacked bar plots
- [`theme_coef()`](https://jdenn0514.github.io/adlgraphs/reference/theme_coef.md)
  : Theme for coefficient plots
- [`hc_theme_default()`](https://jdenn0514.github.io/adlgraphs/reference/hc_theme_default.md)
  : The default theme for hc charts

## Other graphing stuff

- [`scale_adl()`](https://jdenn0514.github.io/adlgraphs/reference/scale_adl.md)
  : ADL color scale
- [`adl_palettes`](https://jdenn0514.github.io/adlgraphs/reference/adl_palettes.md)
  : Complete list of available adl official color palettes

## Data transformations

- [`case_match_fct()`](https://jdenn0514.github.io/adlgraphs/reference/case_match_fct.md)
  :

  `case_match` with factor levels

- [`case_when_fct()`](https://jdenn0514.github.io/adlgraphs/reference/case_when_fct.md)
  :

  `case_when` with factor levels

- [`flip_val()`](https://jdenn0514.github.io/adlgraphs/reference/flip_val.md)
  : Flip the valence of a vector

- [`make_factor()`](https://jdenn0514.github.io/adlgraphs/reference/make_factor.md)
  : Convert a labelled vector into a factor

- [`make_quarts()`](https://jdenn0514.github.io/adlgraphs/reference/make_quarts.md)
  : Create quartiles

- [`make_dicho()`](https://jdenn0514.github.io/adlgraphs/reference/make_dicho.md)
  : Make dichotomous factors

- [`make_binary()`](https://jdenn0514.github.io/adlgraphs/reference/make_binary.md)
  : Make binary variables

- [`make_nested()`](https://jdenn0514.github.io/adlgraphs/reference/make_nested.md)
  : Create a nested data frame

- [`num_rev()`](https://jdenn0514.github.io/adlgraphs/reference/num_rev.md)
  : Reverse a numeric function

- [`refactor()`](https://jdenn0514.github.io/adlgraphs/reference/refactor.md)
  : Assign or reorder factor levels manually

- [`pct_conv()`](https://jdenn0514.github.io/adlgraphs/reference/pct_conv.md)
  : Make proper percent labels

- [`make_percent()`](https://jdenn0514.github.io/adlgraphs/reference/make_percent.md)
  : Convert a numeric vector to a percent

- [`pivot_longer_values()`](https://jdenn0514.github.io/adlgraphs/reference/pivot_longer_values.md)
  : Pivot data from wide to long with value labels

## Data analysis

These are miscellaneous functions created to help with data analysis.

- [`dunnett()`](https://jdenn0514.github.io/adlgraphs/reference/dunnett.md)
  : Run Dunnett's multiple comparisons test with one control.
- [`get_coefficients()`](https://jdenn0514.github.io/adlgraphs/reference/get_coefficients.md)
  : Create a tidied tibble of regression results
- [`get_means()`](https://jdenn0514.github.io/adlgraphs/reference/get_means.md)
  : Calculate means with confidence intervals
- [`get_diffs()`](https://jdenn0514.github.io/adlgraphs/reference/get_diffs.md)
  : Calculate difference in means
- [`get_freqs()`](https://jdenn0514.github.io/adlgraphs/reference/get_freqs.md)
  : Compute weighted frequencies, optionally grouped and/or across
  multiple variables
- [`get_freq_table()`](https://jdenn0514.github.io/adlgraphs/reference/get_freq_table.md)
  : Get the frequencies as a GT table
- [`get_all_freqs()`](https://jdenn0514.github.io/adlgraphs/reference/get_all_freqs.md)
  : Export frequencies for a set of variables to a word doc.
- [`export_freqs_excel()`](https://jdenn0514.github.io/adlgraphs/reference/export_freqs_excel.md)
  : Export Frequency Tables to Excel with Professional Formatting
- [`get_corr()`](https://jdenn0514.github.io/adlgraphs/reference/get_corr.md)
  : Calculate weighted correlations
- [`get_all_corr()`](https://jdenn0514.github.io/adlgraphs/reference/get_all_corr.md)
  : Get correlations for a combination of variables
- [`get_loadings()`](https://jdenn0514.github.io/adlgraphs/reference/get_loadings.md)
  : Calculate the loadings in factor analysis
- [`prettytable()`](https://jdenn0514.github.io/adlgraphs/reference/prettytable.md)
  : Make pretty HTML tables
- [`row_means()`](https://jdenn0514.github.io/adlgraphs/reference/row_means.md)
  : Calculate row means
- [`row_sums()`](https://jdenn0514.github.io/adlgraphs/reference/row_sums.md)
  : Calculate row sums
- [`wtd_corr()`](https://jdenn0514.github.io/adlgraphs/reference/wtd_corr.md)
  : Calculate individual weighted correlations

## Attribute helpers

- [`attr_var_label()`](https://jdenn0514.github.io/adlgraphs/reference/attr_var_label.md)
  : Get variable label

- [`attr_val_labels()`](https://jdenn0514.github.io/adlgraphs/reference/attr_val_labels.md)
  : Get value labels

- [`attr_levels()`](https://jdenn0514.github.io/adlgraphs/reference/attr_levels.md)
  : Get variable label

- [`attr_note()`](https://jdenn0514.github.io/adlgraphs/reference/attr_note.md)
  : Get the note attribute

- [`attr_question_preface()`](https://jdenn0514.github.io/adlgraphs/reference/attr_question_preface.md)
  : Get the question_preface attribute

- [`attr_survey_flow()`](https://jdenn0514.github.io/adlgraphs/reference/attr_survey_flow.md)
  : Get the survey_flow attribute

- [`attr_transformation()`](https://jdenn0514.github.io/adlgraphs/reference/attr_transformation.md)
  : Get the transformation attribute

- [`flatten_labelled_vec()`](https://jdenn0514.github.io/adlgraphs/reference/flatten_labelled_vec.md)
  : Flatten a labelled vector

- [`set_question_preface()`](https://jdenn0514.github.io/adlgraphs/reference/set_question_preface.md)
  :

  Set a new attribute called `question_preface`

## Data cleaning functions

These are miscellaneous functions to help automate some data cleaning

- [`clean_demos()`](https://jdenn0514.github.io/adlgraphs/reference/clean_demos.md)
  : Clean up demographic variables
- [`remove_bogus()`](https://jdenn0514.github.io/adlgraphs/reference/remove_bogus.md)
  : Remove bots, duplicates, and/or speedsters from a data frame
- [`get_bogus()`](https://jdenn0514.github.io/adlgraphs/reference/get_bogus.md)
  : Get bogus data from a data frame
- [`export_bogus()`](https://jdenn0514.github.io/adlgraphs/reference/export_bogus.md)
  : Export data frame with only bots and duplicates

## Miscellaneous functions

- [`get_col_names()`](https://jdenn0514.github.io/adlgraphs/reference/get_col_names.md)
  : Get the column names
- [`codebook()`](https://jdenn0514.github.io/adlgraphs/reference/codebook.md)
  : Create a data codebook
- [`dunnett_helper()`](https://jdenn0514.github.io/adlgraphs/reference/dunnett_helper.md)
  : Perform Dunnett's test (mostly internal function)
- [`funky_freqs()`](https://jdenn0514.github.io/adlgraphs/reference/funky_freqs.md)
  **\[experimental\]** : Calculate weighted frequencies
- [`stars_pval()`](https://jdenn0514.github.io/adlgraphs/reference/stars_pval.md)
  : Add stars based on the p-value
- [`write_word_table()`](https://jdenn0514.github.io/adlgraphs/reference/write_word_table.md)
  : This mainly an internal package but can be used externally
- [`wtd_sd()`](https://jdenn0514.github.io/adlgraphs/reference/wtd_sd.md)
  : Calculate weighted standard deviation

## Data

- [`test_data`](https://jdenn0514.github.io/adlgraphs/reference/test_data.md)
  : dataset_title,

## Deprecated fuctions

- [`remove_bot_dupe()`](https://jdenn0514.github.io/adlgraphs/reference/remove_bot_dupe.md)
  **\[deprecated\]** : Remove bots and/or duplicates from a data frame
- [`get_bot_dupe()`](https://jdenn0514.github.io/adlgraphs/reference/get_bot_dupe.md)
  **\[deprecated\]** : Get the bots or duplicates from a data frame
- [`export_bot_dupe()`](https://jdenn0514.github.io/adlgraphs/reference/export_bot_dupe.md)
  **\[superseded\]** : Export data frame with only bots and duplicates
