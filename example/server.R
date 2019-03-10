library(shiny)
library(ggweb)
library(ggplot2)
library(dplyr)
library(tidyr)
library(future)
library(promises)
library(magrittr)

plan(multiprocess)

function(input, output) {
  day_sequence <- reactive({ seq(input$date_range[[1]], input$date_range[[2]], "days") })

  var_over_time <- reactive({
    source <- data.frame(date = day_sequence(),
                         var1 = rnorm(length(day_sequence()), mean = 1000, sd = 100),
                         var2 = rnorm(length(day_sequence()), mean = 1000, sd = 300),
                         var3 = runif(length(day_sequence()), min = 900, max = 1100))

    source %>%
      gather("type", "var", -date)
  })

  var_over_time_async <- reactive({
    data <- var_over_time()

    future({
      data
    })
  })

  geombar_faceted_plot <- reactive({
    ggplot(var_over_time(), aes(date, var)) +
      geom_line() +
      facet_wrap(~ type)
  })

  geombar_single_plot <- reactive({
    var_over_time_async() %...>% {
      to_plot <- .
      to_plot %<>% filter(type == "var1")
      ggplot(to_plot, aes(date, var)) +
        geom_col() +
        scale_y_continuous(labels = function(x) format(x, big.mark = ",")) +
        labs(title = "Single bars", x = "Date", y = "Some Variable")
    }
  })

  geombar_single_categorical_plot <- reactive({
    ggplot(var_over_time(), aes(type, fill = type)) +
      geom_bar() +
      labs(title = "Single bars (categorical)", x = "Var", y = "Count")
  })

  geombar_dodged_plot <- reactive({
    ggplot(var_over_time(), aes(date, var, fill = type)) +
      geom_col(position = "dodge") +
      scale_y_continuous(labels = function(x) format(x, big.mark = ",")) +
      labs(title = "Dodged bars", x = "Date", y = "Some Variable")
  })

  geombar_stacked_plot <- reactive({
    ggplot(var_over_time(), aes(date, var, fill = type)) +
      geom_col() +
      labs(title = "Stacked bars", x = "Date", y = "Some Variable")
  })

  geomline_single_plot <- reactive({
    ggplot(var_over_time() %>% filter(type == "var1"), aes(date, var)) +
      geom_line() +
      labs(title = "Split lines", x = "Date", y = "Some Variable")
  })

  geomline_split_plot <- reactive({
    ggplot(var_over_time(), aes(date, var, colour = type)) +
      geom_line() +
      labs(title = "Split lines", x = "Date", y = "Some Variable")
  })

  geompoint <- reactive({
    ggplot(var_over_time(), aes(date, var)) +
      geom_point() +
      labs(title = "Scatterplot", x = "Date", y = "Some Variable")
  })

  output$geombar_faceted_img <- renderPlot({
    geombar_faceted_plot()
  })

  output$geombar_single_img <- renderPlot({
    geombar_single_plot()
  })

  output$geombar_single_categorical_img <- renderPlot({
    geombar_single_categorical_plot()
  })

  output$geombar_dodged_img <- renderPlot({
    geombar_dodged_plot()
  })

  output$geombar_stacked_img <- renderPlot({
    geombar_stacked_plot()
  })

  output$geomline_single_img <- renderPlot({
    geomline_single_plot()
  })

  output$geomline_split_img <- renderPlot({
    geomline_split_plot()
  })

  output$geompoint_img <- renderPlot({
    geompoint()
  })

  output$geombar_faceted_web <- renderChart({
    geombar_faceted_plot()
  })

  output$geombar_single_web <- renderChart({
    geombar_single_plot()
  })

  output$geombar_single_categorical_web <- renderChart({
    geombar_single_categorical_plot()
  })

  output$geombar_dodged_web <- renderChart({
    geombar_dodged_plot()
  })

  output$geombar_stacked_web <- renderChart({
    geombar_stacked_plot()
  })

  output$geomline_single_web <- renderChart({
    geomline_single_plot()
  })

  output$geomline_split_web <- renderChart({
    geomline_split_plot()
  })

  output$geompoint_web <- renderChart({
    geompoint()
  })
}
