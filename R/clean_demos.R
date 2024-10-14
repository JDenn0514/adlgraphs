#' Clean up demographic variables
#'
#' This function makes it easier to clean demographic data but it really only
#' works when the data is set up in a very specific way. This is designed to
#' work with how ADL programs its surveys and to work in case any of the
#' variables listed below are not found in the data.
#'
#' @param df A dataframe or tibble. This can be piped in like with a normal
#'   \code{\link[ggplot2]{ggplot}} function.
#'
#' @export
#'



clean_demos <- function(
  df
) {

  if ("race_1" %in% colnames(df)) {

    df <- clean_race(df, "race_1")

  }

  if ("race_2" %in% colnames(df)) {

    df <- clean_race(df, "race_2")

  }

  if ("race_3" %in% colnames(df)) {

    df <- clean_race(df, "race_3")

  }

  if ("race_4" %in% colnames(df)) {

    df <- clean_race(df, "race_4")

  }

  if ("race_5" %in% colnames(df)) {

    df <- clean_race(df, "race_5")

  }

  if ("race_6" %in% colnames(df)) {

    df <- clean_race(df, "race_6")

  } 

  if ("race_7" %in% colnames(df)) {
    
    df <- clean_race(df, "race_7")

  }

  if ("asian_b" %in% colnames(df) && "hawaiian_b" %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(
        aapi_b = dplyr::case_when(
          asian_b == 1 | hawaiian_b == 1 ~ 1,
          .default = 0
        )
      )
  }

  if ("hispanic" %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(
        hispanic_b = dplyr::case_match(
          hispanic,
          1 ~ 1,
          .default = 0
        )
      )
  }

  if (
    "white_b" %in% colnames(df) &&
    "black_b" %in% colnames(df) &&
    "aapi_b" %in% colnames(df) &&
    "native_b" %in% colnames(df) &&
    "other_b" %in% colnames(df) 
  ) {

    df <- df %>%
      dplyr::mutate(
        # get people of multiple races
        race_mult_n = white_b + black_b + aapi_b + native_b + other_b,
        # create the racial gropus
        race_f = adlgraphs::case_when_fct(
          race_mult_n > 1 | other_b == 1 | native_b == 1 ~ "Multi/Other",
          hispanic_b == 1 ~ "Hispanic",
          black_b == 1 ~ "Black",
          aapi_b == 1 ~ "AAPI",
          white_b == 1 ~ "White"
        ),
        # reorder the levels
        race_f = refactor(race_f, c("White", "Black", "Hispanic", "AAPI", "Multi/Other")),
      ) %>%
      labelled::set_variable_labels(race_f = "Race/Ethnicity")

  } else if (
    "white_b" %in% colnames(df) &&
    "black_b" %in% colnames(df) &&
    "aapi_b" %in% colnames(df) &&
    "native_b" %in% colnames(df) &&
    "mena_b" %in% colnames(df)
  ) {
    df <- df %>%
      dplyr::mutate(
        # get people of multiple races
        race_mult_n = white_b + black_b + aapi_b + native_b + mena_b,
        # create the racial gropus
        race_f = adlgraphs::case_when_fct(
          race_mult_n > 1 | mena_b == 1 | native_b == 1 ~ "Multi/Other",
          hispanic_b == 1 ~ "Hispanic",
          black_b == 1 ~ "Black",
          aapi_b == 1 ~ "AAPI",
          white_b == 1 ~ "White"
        ),
        # reorder the levels
        race_f = refactor(race_f, c("White", "Black", "Hispanic", "AAPI", "Multi/Other"))
      ) %>%
      labelled::set_variable_labels(race_f = "Race/Ethnicity")
  }

  if ("sex" %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(

        ## gender
        # making a three category variable
        gender_f3 = make_factor(sex),
        # making a binary numeric
        gender_f2 = dplyr::case_match(
          sex,
          1 ~ "A man",
          2 ~ "A woman"
        )
      ) %>%
      labelled::set_variable_labels(
        gender_f2     = "Gender",
        gender_f3     = "Gender"
      )
  }

  if ("edu" %in% colnames(df)) {

    df <- df %>%
      dplyr::mutate(
        ## EDU
        # create the five group education variable
        edu_f = make_factor(edu),
        # making a binary factor
        edu_f2 = adlgraphs::case_when_fct(
          edu < 4 ~ "No College Degree",
          edu > 3 ~ "At Least a Bachelor's Degree"
        )
      ) %>%
      labelled::set_variable_labels(
        edu_f = "Highest Education Level",
        edu_f2 = "College Graduate"
      )

  }

  if ("income" %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(
        # making income a nine group variable
        income_n9 = dplyr::case_match(
          income,
          1 ~ 1,
          2 ~ 2,
          3 ~ 3,
          4 ~ 4,
          5 ~ 5,
          c(6:7) ~ 6,
          c(8:10) ~ 7,
          11 ~ 8,
          12 ~ 9
        ),
        # make it a 9 levelfactor
        income_f9 = adlgraphs::case_when_fct(
          income == 1 ~ "Less than $10,000",
          income == 2 ~ "$10,000 to $19,999",
          income == 3 ~ "$20,000 to $29,999",
          income == 4 ~ "$30,000 to $39,999",
          income == 5 ~ "$40,000 to $49,999",
          income %in% c(6:7) ~ "$50,000 to $69,999",
          income %in% c(8:10) ~ "$70,000 to $99,999",
          income == 11 ~ "$100,000 to $149,999",
          income == 12 ~ "$150,000 or more"
        )
      ) %>%
      labelled::set_variable_labels(
        income_f9 = "Income",
        income_n9 = "Income"
      )
  }

  if ("age" %in% colnames(df)) {
    if (!is.null(attr_val_labels(df$age))) {
      df <- df %>%
        dplyr::mutate(
          # age, adding 17 so the ages start at 18
          age_n = 17 + age %>% 
            structure(label = "Age")
        )
    } else if (is.null(df$age)) {
      df <- df %>% 
        dplyr::mutate(
          age_n = age %>% structure(label = "Age")
        )
    }
    df <- df %>% 
      dplyr::mutate(
        # make a new age variable to be used in survey weighting
        age_f6 = adlgraphs::case_when_fct(
          age_n %in% c(18:24) ~ "18-24",
          age_n %in% c(25:34) ~ "25-34",
          age_n %in% c(35:44) ~ "35-44",
          age_n %in% c(45:54) ~ "45-54",
          age_n %in% c(55:64) ~ "55-64",
          age_n > 64 ~ "65+"
        ) %>% 
          # set the label
          structure(
            label = "Age Cohort",
            note = "Six Categories"
          ),
        # create a nw four group age variable
        age_f4 = adlgraphs::case_when_fct(
          age_n < 31 ~ "18-30",
          age_n %in% c(31:49) ~ "31-49",
          age_n %in% c(50:64) ~ "50-64",
          age_n > 64 ~ "65+"
        ) %>% 
          # set the label
          structure(
            label = "Age Cohort",
            note = "Four Categories"
          )
      )
        

    if ("start_date" %in% colnames(df)) {
      df <- df %>% 
        dplyr::mutate(
          # set the current year using lubridate
          current_year = lubridate::year(start_date) %>% 
            # set the label
            structure(label = "Year study was conducted"),
          # set the birth year based on their age and the current year
          birth_year = current_year - age_n %>% 
            # set the label
            structure(label = "Birth Year"),
          # set teh generations using birth year
          gens_f = adlgraphs::case_when_fct(
            birth_year > 1996 ~ "Gen Z",
            birth_year %in% c(1981:1996) ~ "Millennial",
            birth_year %in% c(1965:1980) ~ "Gen X",
            birth_year < 1965 ~ "Baby Boomers/ Silent Generation"
          ) %>% 
            # set the label
            structure(label = "Age Cohort/Generation")
        )
      } else {
      # use the age variable to set the generations
      df <- df %>% 
        dplyr::mutate(
          gens_f = adlgraphs::case_when_fct(
            age_n < 27 ~ "Gen Z",
            age_n %in% c(27:43) ~ "Millennials",
            age_n %in% c(44:59) ~ "Gen X",
            age_n > 59 ~ "Baby Boomers/ Silent Generation"
          ) %>% 
            structure(label = "Age Cohort/Generation")
        )
      }
  }


  if ("ideology" %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(
        ## Politics
        # 5 group political ideology
        ideo_f = make_factor(ideology),
        # set up the order and make it three groups
        ideo_f3 = adlgraphs::case_when_fct(
          ideology %in% c(1, 2) ~ "Liberal",
          ideology == 3 ~ "Moderate",
          ideology %in% c(4, 5) ~ "Conservative"
        )
      ) %>%
      labelled::set_variable_labels(
        ideo_f = "Political Ideology",
        ideo_f3 = "Political Ideology"
      )

  }

  if ("partyid8" %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(
        # convert to a factor
        pid_f8 = make_factor(partyid8),
        # set the order and make three groups
        pid_f3 = adlgraphs::case_when_fct(
          partyid8 %in% c(1:3) ~ "Democrat",
          partyid8 %in% c(4,8) ~ "Independent",
          partyid8 %in% c(5:7) ~ "Republican",
        )
      ) %>%
      labelled::set_variable_labels(
        pid_f8 = "Political Partisanship",
        pid_f3 = "Political Partisanship"
      )

  }

  if ("partyid7" %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(
        # convert to a factor
        pid_f7 = make_factor(partyid7),
        # set the order and make three groups
        pid_f3 = adlgraphs::case_when_fct(
          partyid7 %in% c(1:3) ~ "Democrat",
          partyid7 %in% c(4) ~ "Independent",
          partyid7 %in% c(5:7) ~ "Republican",
        )
      ) %>%
      labelled::set_variable_labels(
        pid_f7 = "Political Partisanship",
        pid_f3 = "Political Partisanship"
      )

  }

  if ("progressive" %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(
        # convert progressive to a factor
        progressive_f = make_factor(progressive),
        # convert to a dichotomous factor
        progressive_f2 = dplyr::case_match(
          progressive,
          c(1:2) ~ "Yes",
          c(3:4) ~ "No"
        )
      ) %>%
      labelled::set_variable_labels(
        progressive_f = "Politically Progressive",
        progressive_f2 = "Politically Progressive"
      )
  }

  if ("religpew"  %in% colnames(df) && "born_again" %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(
        ## Religion
        # simple conversion to a factor
        religpew_f = make_factor(religpew),
        # make the reltrad groups
        religpew_evan_f = dplyr::case_when(
          religpew %in% c(1, 5) & born_again == 1 ~ "Evangelical Protestant",
          religpew %in% c(1, 5) & born_again != 1 ~ "Non-Evangelical Protestant",
          .default = paste(religpew_f)
        ),
        # make the reltrad variable
        reltrad_f = dplyr::case_when(
          religpew %in% c(11:13) ~ "No Religion",
          religpew %in% c(3:4, 7:10) ~ "Other Faith",
          .default = paste(religpew_evan_f)
        ),
        reltrad_f = refactor(
          reltrad_f,
          c(
            "Non-Evangelical Protestant",
            "Evangelical Protestant",
            "Catholic",
            "No Religion",
            "Jewish",
            "Other Faith",
            "Something Else (Please specify)"
          )
        ),

        # make a dichotomous born_again variable
        born_again_f2 = make_factor(born_again),
      ) %>%
      labelled::set_variable_labels(
        reltrad_f = "Religious Identity",
        born_again = "Evangelical/Born Again Christian",
      )

  }

  if ("jewish" %in% colnames(df)) {

    df <- df %>% dplyr::mutate(jewish_f2 = make_factor(jewish))

  }

  return(df)
}
