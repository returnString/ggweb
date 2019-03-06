library(shiny)
library(ggweb)

comparison <- function(name) {
  fluidRow(
    column(width = 6, plotOutput(paste0(name, "_img"))),
    column(width = 6, chartOutput(paste0(name, "_web")))
  )
}

fluidPage(
  titlePanel("ggplot web test"),
  inputPanel(
    dateRangeInput("date_range", "Date range", start = "2018-01-01", end = "2018-01-31")
  ),
  comparison("geombar_faceted"),
  comparison("geombar_single"),
  comparison("geombar_single_categorical"),
  comparison("geombar_dodged"),
  comparison("geombar_stacked"),
  comparison("geomline_single"),
  comparison("geomline_split")
)
