library(dplyr)
library(ggplot2)
library(shiny)
library(gapminder)
data("gapminder")

ui <- fluidPage(
  titlePanel(title = "Gapminder Dataset"),
  sidebarPanel(
    selectInput(inputId = "year", 
                label = "Choose a Year", 
                choices = as.numeric(names(table(gapminder$year))),
                selected = 2007),
    tableOutput("table")),
  mainPanel(
    plotOutput("plot"))
  )

server <- function(input, output) {
  
  output$table <- renderTable({
    gapminder %>%
      filter(year == input$year) %>%
      group_by(continent) %>%
      summarize(GDPperCap = mean(gdpPercap), LifeExp = mean(lifeExp)) %>%
      arrange(desc(LifeExp))
  })
  
  output$plot <- renderPlot({
    gapminder %>%
      filter(year == input$year) %>%
      ggplot(aes(x = gdpPercap, y = lifeExp, color = continent)) +
      geom_point(aes(size = pop)) +
      scale_x_continuous(trans = "log10") +
      labs(x = "GDP Per Capita (log10 scale)", y = "Life Expectancy", title = "Life Expectancy as a Function of GDP Per Capita", color = "Continent", size = "Population")
    })
}

shinyApp(ui = ui, server = server)
