

.onAttach <- function(...) {

  family <- name <- path <- NULL

  # If font registry already contains roboto core, set use_robotoy == TRUE
  fonts_present <- systemfonts::registry_fonts() %>%
    dplyr::filter(family %in% adlgraphs_global$preferred_font) %>%
    nrow() >= 5

  assign("use_roboto",
         fonts_present,
         envir = adlgraphs_global)


  # Else, find and register necessary roboto variants using systemfonts (or,
  # alternatively, find them manually in ~/Library/Fonts). Then, if font
  # registry contains roboto core, set use_roboto == TRUE.
  if(!get("use_roboto", envir = adlgraphs_global)){
    roboto_paths <- dplyr::filter(systemfonts::system_fonts(), family == "Roboto")
    roboto_paths <- roboto_paths[["path"]]

    # On some OSX systems (e.g. pkgdown GHA VM) system_fonts() cannot find fonts
    # installed in the user fonts directory. In any case where system_fonts()
    # sees no roboto fonts, if `user_dir` exists, it too is checked for fonts.
    user_dir <- paste0(Sys.getenv("HOME"), "/Library/Fonts")
    if(length(roboto_paths) == 0 & dir.exists(user_dir)){
      roboto_paths <- list.files(user_dir, full.names = TRUE)
      roboto_paths <- grep("Roboto-", roboto_paths, value = TRUE)
    }

    # Register preferred fonts using the paths found above. This will only be
    # attempted if at least 10 paths are found, as 10 distinct faces are needed
    # to register all possible variants of the three needed fonts. If the
    # correct face cannot be found, `find_path` will error and the try object
    # will fail before `use_roboto` is set to TRUE.
    if (length(roboto_paths) >= 5){
      try({

        # register preferred strong font (roboto Semibold), with variants
        systemfonts::register_font(
          name = adlgraphs_global$preferred_font$heavy,
          plain = find_path("Roboto-Medium", roboto_paths),
          bold = find_path("Roboto-Black", roboto_paths)
        )

        # register preferred regular font (roboto Medium), with variants
        systemfonts::register_font(
          name = adlgraphs_global$preferred_font$regular,
          plain = find_path("Roboto-Regular", roboto_paths),
          bold = find_path("Roboto-Black", roboto_paths)
        )

        # register preferred light font (roboto Book), with variants
        systemfonts::register_font(
          name = adlgraphs_global$preferred_font$light,
          plain = find_path("Roboto-Light", roboto_paths),
          bold = find_path("Roboto-Bold", roboto_paths)
        )

        packageStartupMessage(paste0(
          "adlgraphs has registered the following fonts for use in this R session:\n   ",
          paste(adlgraphs_global$preferred_font, collapse = ", ")
        ))

        assign("use_roboto",
               TRUE,
               envir = adlgraphs_global)
      })
    }
  }
  # If roboto is available...
  if(get("use_roboto", envir = adlgraphs_global)){
    # ... Update font names
    assign(
      "font",
      list(
        heavy   = list(family = adlgraphs_global$preferred_font$heavy, face = "plain"),
        bold    = list(family = adlgraphs_global$preferred_font$bold, face = "plain"),
        regular = list(family = adlgraphs_global$preferred_font$regular, face = "plain"),
        light   = list(family = adlgraphs_global$preferred_font$light, face = "plain")
      ),
      envir = adlgraphs_global
    )

    # ... and check on rstudio graphics
    if (Sys.getenv("RSTUDIO") == 1){
      if(getOption("RStudioGD.backend") != "ragg"){
        options(RStudioGD.backend = "ragg")
        packageStartupMessage(paste(
          "adlgraphs has set RStudio graphics to `ragg` for the current session.",
          "You can make this change permanent:\n   ",
          "Tools > Global Options > General > Graphics > Graphics Device > Backend == 'AGG'."
        ))
      }
    }
    # Otherwise, notify user
  } else {
    packageStartupMessage(
      "adlgraphs cannot locate Roboto fonts, so adlgraphs themes will use your default sans-serif font."
    )
  }

}


utils::globalVariables(
  c(
    "estimate", "wts", ".", "adl_palettes", "lower", "upper", "x", "correlation",
    "trad_avg_n", "x_title", "estimate", "std.error", "var_class",
    "contrasts_type", "conf.high", "conf.low", "ss", "p.value", "label", "x_f",
    ".data", "n", "pct", "General Population", "pct_lab", "group_f", "sd",
    "value_label", "race_1", "race_2", "race_3", "race_4", "race_5", "race_6",
    "hispanic", "white_b", "black_b", "aapi_b", "native_b", "other_b", "race_f",
    "sex", "edu", "income", "age", "ideology", "partyid8", "partyid7",
    "progressive", "religpew", "religpew_f", "religpew_evan_f", "born_again",
    "jewish", "reltrad_f", "std.err", "t.value", "var_label", "mena_b", 
    "start_date", "current_year", "age_n", ".rows", "data", "groups_combined", 
    "groups", "DistributionChannel", "distribution_channel", "Q_RecaptchaScore",
    "q_recaptcha_score", "Q_RelevantIDDuplicate", "q_relevant_id_duplicate", 
    "bot", "duplicate", "Var1", "Var2", "error_call", "bots", "speedsters"
  )
)
