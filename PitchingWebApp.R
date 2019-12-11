library(dplyr)
library(ggplot2)
library(shiny)
# library(plotly)
# library(paletteer)


ui <- fluidPage(
  titlePanel(title = "Zack Greinke Situational Pitch Selection 2015"),
  sidebarPanel(
    selectInput(inputId = "bat", label = "Batter",
                choices = c("R", "L"),
                selected = "R"),
    selectInput(inputId = "count", label = "Pitch Count", 
                choices = c("0-0", "0-1", "0-2", "1-0", "1-1", "1-2", "2-0", "2-1", "2-2", "3-0", "3-1", "3-2"), 
                selected = "0-0"),
    selectInput(inputId = "outs", label = "Outs", choices = c(0, 1, 2), selected = 0),
    tableOutput("table"),
    width = 2),
  mainPanel(
    plotOutput("plot"),
    width = 10) 
  )

server <- function(input, output) {
  output$table <- renderTable({
    greinke %>%
      filter(batter_stand == input$bat, bs_count == input$count, outs == input$outs) %>%
      select(pitch_type) %>%
      table %>% prop.table() %>% round(3)
  })
  output$plot <- renderPlot({
    greinke %>%
      filter(batter_stand == input$bat, bs_count == input$count, outs == input$outs) %>%
      ggplot(aes(x = px, y = pz, color = pitch_type, size = start_speed)) +
      geom_point(alpha = 0.5) +
      coord_equal() +
      annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.4, alpha = 0.2, color = "blue", alpha = 0.4, fill = 0.1) +
      geom_hline(yintercept = 0, color = "orange") + 
      scale_color_manual(name = "Pitch Type",
                         values = c("CU" = "blue", "CH" = "cornflowerblue", "FF" = "red2", "FT" = "red", "SL" = "black"),
                         labels = c("CU" = "Curveball", "CH" = "Change-up", "FF" = "4-Seam Fastball", "FT" = "2-Seam Fastball", "SL" = "Slider")) +
      labs(x = "Horizontal Location (ft. from plate)",
           y = "Vertical Location (ft.)",
           color = "Pitch Type",
           size = "Speed")
    })
}

shinyApp(ui = ui, server = server)

# Save as app.R in one directory with all needed files

### Deployment
# rsconnect::deployApp('~/DataFile/R_Files/R_Scripts/DataScience/directory_name')