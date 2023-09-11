
#' A function for coefficient plots
#'
#' This function creates a theme for coefficient plots.
#'
#'@export

theme_coef <- function() {
  theme_minimal() +
    theme(
      # adjust the overall text font family, size and lineheight
      text = element_text(family = "L", size = 12, lineheight = 1.1),
      # make the plot title TW font, bold, centered horizontally, and add bottom margin
      plot.title = element_text(family = "TW", face = "bold", hjust = 0.5,
                                margin = margin(b = 15)),
      # make plot subtitle TW font, left adjusted, add bottom margin
      plot.subtitle = element_text(family = "TW", hjust = 0,
                                   margin = margin(b = 15)),
      # add
      strip.text = element_text(face = "bold", size = 12, hjust = 0.5,
                                margin = margin(t = 10, b = 5)),
      plot.margin = margin(15, 15, 15, 15),
      panel.spacing.x = unit(2, "lines"),
      legend.position = "right",
      legend.text = element_text(lineheight = 1.1,
                                 margin = ggplot2::margin(b = 10, t = 10)),
      axis.title.x = element_text(margin = ggplot2::margin(t = 10)),
      panel.grid = element_blank(),
      panel.grid.major.x = element_line(linewidth = 0.5, color = "gray75"),
      panel.grid.minor.x = element_line(linewidth = 0.25, color = "gray90")
    )
}

#' A function for horizontal stacked bar plots
#'
#' This function creates the theme for horizontal stacked bar plots
#'
#' @export
theme_h_stack <- function() {
  theme_minimal() +
    theme(
      # adjust formatting for all text
      text = element_text(family = "L", size = 12, lineheight = 1.1),
      # adjust plot title formatting
      plot.title = element_text(family = "TW", face = "bold", hjust = 0.5,
                                # add space beneath title
                                margin = margin(b = 10)),
      # center title over entire plot
      plot.title.position = "plot",
      # adjust plot subtitle formatting
      plot.subtitle = element_text(family = "TW", hjust = 0,
                                   # add space beneath subtitle
                                   margin = margin(b = 15)),
      # adjust plot caption formatting
      plot.caption = element_text(hjust = 1, size = 8, lineheight = 1.05,
                                  # add space above caption
                                  margin = margin(10)),
      # add space around graph
      plot.margin = margin(15, 15, 15, 15),
      # remove x-axis text
      axis.text = element_blank(),
      # adjust space margin around legend labels
      legend.text = element_text(margin = margin(r = 5, l = -3)),
      # adjust space around entire legend
      legend.margin = margin(b = 0),
      # legend position at the top
      legend.position = "top",
      # remove all grid lines
      panel.grid = element_blank()
    )
}



#' A function for simple horizontal bar plots
#'
#' This function creates the theme for simple horizontal bar plots
#'
#' @export

# Theme for horizontal bar plots
theme_h_bar <- function() {
  theme_minimal() +
    theme(
      text = element_text(family = "L", size = 12, lineheight = 1.1),
      plot.title = element_text(family = "TW", face = "bold", hjust = 0.5,
                                margin = margin(b = 10)),
      # center title over entire plot
      plot.title.position = "plot",
      plot.subtitle = element_text(family = "TW", hjust = 0,
                                   margin = margin(b = 15)),
      panel.grid = element_blank(),
      legend.position = "none",
      axis.text.x = element_blank(),
      plot.margin = margin(15, 15, 15, 15)
    )
}

#' A function for simple vertical bar plots
#'
#' This function creates the theme for simple vertical bar plots
#'
#' @export
theme_v_bar <- function() {
  theme_minimal() +
    theme(
      text = element_text(family = "L", size = 12, lineheight = 1.1),
      plot.title = element_text(family = "TW", face = "bold", hjust = 0.5,
                                margin = margin(b = 10)),
      # center title over entire plot
      plot.title.position = "plot",
      plot.subtitle = element_text(family = "TW", hjust = 0,
                                   margin = margin(b = 15)),
      plot.caption = element_text(hjust = 1, size = 8, margin = margin(5), lineheight = 1.05),
      # make sure there is space around the graph
      plot.margin = margin(15, 15, 15, 15),
      axis.text.x = element_text(margin = margin(b = 10), lineheight = 1.1),
      axis.text.y = element_blank(),
      axis.title.x = element_text(margin = margin(b = 10),
                                  lineheight = 1.1),
      #axis.title.y = element_text(margin = margin(r = 10)),
      legend.text = element_text(margin = margin(r = 10, l = -3)),
      # legend position
      legend.position = "top",
      panel.grid = element_blank(),
    )
}







