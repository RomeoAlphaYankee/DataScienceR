library(dplyr)
library(ggplot2)
library(shiny)

data("gapminder")

ui <- fluidPage(
  selectInput(inputId = "year", 
              label = "Choose a Year", 
              choices = as.numeric(names(table(gapminder$year))),
              selected = 2007),
  plotOutput("plot"))

server <- function(input, output) {
  output$plot <- renderPlot({
    gapminder %>%
      filter(year == input$year) %>%
      ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
      geom_point(aes(size = pop)) +
      scale_x_continuous(trans = "log10") +
      labs(x = "GDP Per Capita", y = "Life Expectancy", title = "Life Expectancy as a Function of GDP Per Capita", color = "Continent", size = "Population")
    })
}

shinyApp(ui = ui, server = server)
