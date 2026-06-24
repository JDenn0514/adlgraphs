library(officer)
library(broom)

# Define the function
ugly_regressions <- function(dataset, participant_race, ordinal_dvs, binary_dvs, control_vars, iv, interaction_terms) {
  # Create a new Word document
  doc <- read_docx()
  
  # Add a title to the document
  doc <- doc %>%
    body_add_par("Regression Analysis Results", style = "heading 1")
  
  # Function to perform linear regression
  perform_linear_regression <- function(dv, formula, model_name) {
    model <- lm(as.formula(formula), data = dataset)
    doc <<- doc %>%
      body_add_par(model_name, style = "heading 2") %>%
      body_add_par(paste(capture.output(summary(model)), collapse = "\n"), style = "Normal") %>%
      body_add_par("", style = "Normal")  # Add a blank line for spacing
    return(model)
  }
  
  # Function to perform logistic regression
  perform_logistic_regression <- function(dv, formula, model_name) {
    model <- glm(as.formula(formula), data = dataset, family = binomial)
    doc <<- doc %>%
      body_add_par(model_name, style = "heading 2") %>%
      body_add_par(paste(capture.output(summary(model)), collapse = "\n"), style = "Normal") %>%
      body_add_par("", style = "Normal")  # Add a blank line for spacing
    return(model)
  }
  
  # Analyze ordinal DVs
  doc <- doc %>%
    body_add_par("Ordinal Dependent Variables", style = "heading 2")
  
  for (dv in ordinal_dvs) {
    # Bivariate model
    perform_linear_regression(dv, paste(dv, "~", iv), paste("Bivariate Linear Regression:", dv, "~", iv))
    
    # Multivariate model with race and controls
    formula <- paste(dv, "~", iv, "+", participant_race, "+", paste(control_vars, collapse = " + "))
    perform_linear_regression(dv, formula, paste("Multivariate Linear Regression:", dv, "~", iv, "+ race + controls"))
    
    # Interaction with race
    formula <- paste(dv, "~", iv, "*", participant_race, "+", paste(control_vars, collapse = " + "))
    perform_linear_regression(dv, formula, paste("Interaction Linear Regression:", dv, "~", iv, "* race + controls"))
    
    # Interaction with other terms
    for (interaction in interaction_terms) {
      formula <- paste(dv, "~", iv, "*", interaction, "+", participant_race, "+", paste(control_vars, collapse = " + "))
      perform_linear_regression(dv, formula, paste("Interaction Linear Regression:", dv, "~", iv, "*", interaction, "+ race + controls"))
    }
  }
  
  # Analyze binary DVs
  doc <- doc %>%
    body_add_par("Binary Dependent Variables", style = "heading 2")
  
  for (dv in binary_dvs) {
    # Bivariate model
    perform_logistic_regression(dv, paste(dv, "~", iv), paste("Bivariate Logistic Regression:", dv, "~", iv))
    
    # Multivariate model with race and controls
    formula <- paste(dv, "~", iv, "+", participant_race, "+", paste(control_vars, collapse = " + "))
    perform_logistic_regression(dv, formula, paste("Multivariate Logistic Regression:", dv, "~", iv, "+ race + controls"))
    
    # Interaction with race
    formula <- paste(dv, "~", iv, "*", participant_race, "+", paste(control_vars, collapse = " + "))
    perform_logistic_regression(dv, formula, paste("Interaction Logistic Regression:", dv, "~", iv, "* race + controls"))
    
    # Interaction with other terms
    for (interaction in interaction_terms) {
      formula <- paste(dv, "~", iv, "*", interaction, "+", participant_race, "+", paste(control_vars, collapse = " + "))
      perform_logistic_regression(dv, formula, paste("Interaction Logistic Regression:", dv, "~", iv, "*", interaction, "+ race + controls"))
    }
  }
  
  # Save the document
  print(doc, target = "regression_analysis_results.docx")
}

# Example usage:
# ugly_regressions(dataset, 'race', c('ordinal_dv1', 'ordinal_dv2'), c('binary_dv1'), c('control1', 'control2'), 'iv', c('interaction1', 'interaction2'))
