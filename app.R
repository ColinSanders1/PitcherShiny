library(baseballr)
library(shiny)
library(dplyr)
library(plotly)
library(readxl)
library(openxlsx)

tryCatch({
  data <- read.xlsx("IDtable.xlsx")
}, error = function(e) {
  print(e)
})

player_id <- function(last_name, first_name, data) {
  id <- data$key_mlbam[data$name_last == last_name & data$name_first == first_name]
  if (length(id) == 0) {
    return("player not found")
  } else {
    return(id)
  }
}

ui <- fluidPage(
  titlePanel("MLB Pitcher Report"),
  sidebarLayout(
    sidebarPanel(
      textInput("first_name", "First Name", ""),
      textInput("last_name", "Last Name", ""),
      actionButton("search_btn", "Retrieve Data")
    ),
    mainPanel(
      plotlyOutput("pitch_movement_plot"), 
      plotlyOutput("pitch_location_plot"),
      tableOutput("Avg_Stats1"),
      tableOutput("Avg_Stats2"),
      plotlyOutput("Pitch_perc_combined_str"),
      plotlyOutput("Pitch_perc_combined_ball")
    )
  )
)

server <- function(input, output, session) {
  player_data <- eventReactive(input$search_btn, {
    last_name <- input$last_name
    first_name <- input$first_name
    player_id_result <- player_id(last_name, first_name, data)
    
    if (player_id_result == "player not found") {
      return(NULL)  
    } else {
      dat <- scrape_statcast_savant(
        start_date = '2023-03-25',
        end_date = Sys.Date(),
        playerid = player_id_result,
        player_type = "pitcher"
      )
      return(dat)
    }
  })
  
  observeEvent(input$search_btn, {
    updateSelectInput(session, "selected_name", choices = unique(player_data()$player_name))
  })
  
  output$selected_output <- renderText({
    selected_name <- input$selected_name
    selected_name
  })
  output$Avg_Stats1 <- renderTable({
    player_selected_data1 <- player_data()
    
    if (!is.null(player_selected_data1)) {
      player_avg_data1 <- player_selected_data1 %>%
        group_by(player_name, pitch_name) %>%
        summarize(
          Average_SpinRate = mean(release_spin_rate, na.rm = TRUE),
          Average_Velocity = mean(release_speed, na.rm = TRUE),
          Average_Extension = mean(release_extension, na.rm = TRUE),
          Average_ReleaseHeight = mean(release_pos_y, na.rm = TRUE),
        )
      player_avg_data1
    }
  })
  
  output$Avg_Stats2 <- renderTable({
    player_selected_data2 <- player_data()
    
    vyf = -sqrt(player_selected_data2$vy0^2 - (2 * player_selected_data2$ay * (50 - 17/12)))
    t = (vyf - player_selected_data2$vy0) / player_selected_data2$ay
    vzf = player_selected_data2$vz0 + (player_selected_data2$az * t)
    player_selected_data2$vaa = -atan(vzf / vyf) * (180 / pi)
    
    if (!is.null(player_selected_data2)) {
      player_avg_data2 <- player_selected_data2 %>%
        group_by(player_name, pitch_name) %>%
        summarize(Average_Vertical_Approach = mean(vaa, na.rm = TRUE),
                  XWOBA = mean(estimated_woba_using_speedangle, na.rm = TRUE),
                  Average_Exit_Velocity = mean(launch_speed, na.rm = TRUE),
                  Average_Launch_Angle = mean(launch_angle, na.rm = TRUE)
        )
      player_avg_data2
    }
  })
  
  pitch_location_data <- reactive({
    player_selected_data <- player_data()
    
    if (!is.null(player_selected_data)) {
      return(player_selected_data)
    }
    return(NULL)
  })
  
  pitch_counts <- reactive({
    player_selected_data <- player_data()
    
    if (!is.null(player_selected_data)) {
      pitch_counts_data <- player_selected_data %>%
        group_by(pitch_name) %>%
        summarise(Pitch_Count = n())
      
      pitch_counts_data <- pitch_counts_data %>%
        mutate(Percentage = (Pitch_Count / sum(Pitch_Count)) * 100)
      
      return(pitch_counts_data)
    }
  })
  
  pitch_percentages_by_strikes <- reactive({
    player_selected_data <- player_data()
    
    if (!is.null(player_selected_data)) {
      pitch_counts_data <- player_selected_data %>%
        group_by(pitch_name, strikes, stand) %>%
        summarise(Pitch_Count = n())
      
      pitch_counts_data <- pitch_counts_data %>%
        group_by(strikes, stand) %>%
        mutate(Percentage = (Pitch_Count / sum(Pitch_Count)) * 100)
      
      return(pitch_counts_data)
    }
  })
  
  pitch_percentages_by_balls <- reactive({
    player_selected_data <- player_data()
    
    if (!is.null(player_selected_data)) {
      pitch_counts_data <- player_selected_data %>%
        group_by(pitch_name, balls, stand) %>%
        summarise(Pitch_Count = n())
      
      pitch_counts_data <- pitch_counts_data %>%
        group_by(balls, stand) %>%
        mutate(Percentage = (Pitch_Count / sum(Pitch_Count)) * 100)
      
      return(pitch_counts_data)
    }
  })
  
  pitch_percentages_by_stand <- reactive({
    player_selected_data <- player_data()
    
    if (!is.null(player_selected_data)) {
      pitch_counts_data <- player_selected_data %>%
        group_by(pitch_name, stand) %>%
        summarise(Pitch_Count = n())
      
      pitch_percentages_data <- pitch_counts_data %>%
        group_by(stand) %>%
        mutate(Percentage = (Pitch_Count / sum(Pitch_Count)) * 100)
      
      return(pitch_percentages_data)
    }
  })
  
  
  output$pitch_movement_plot <- renderPlotly({
    player_selected_data <- player_data()
    
    if (!is.null(player_selected_data)) {
      pitch_movement_data <- player_selected_data
      
      plot <- plot_ly(data = pitch_movement_data, x = ~pfx_x, y = ~pfx_z, type = 'scatter',
                      mode = 'markers', text = ~paste(pitch_name, " (", pitch_counts()$Pitch_Count[match(pitch_name, pitch_counts()$pitch_name)], ")", sep = ""), 
                      color = ~pitch_name, colors = 'Set1', marker = list(size = 5))
      
      plot <- plot %>% layout(
        title = "Pitch Movement by Type",
        xaxis = list(title = "Horizontal Movement (pfx_x)"),
        yaxis = list(title = "Vertical Movement (pfx_z)")
      )
      
      return(plot)
    }
  })
  
  output$pitch_location_plot <- renderPlotly({
    pitch_location_data_selected <- pitch_location_data()
    
    if (!is.null(pitch_location_data_selected)) {
      location_plot <- plot_ly(
        data = pitch_location_data_selected,
        x = ~plate_x,
        y = ~plate_z,
        type = 'scatter',
        mode = 'markers',
        text = ~paste(pitch_name, " (", pitch_counts()$Pitch_Count[match(pitch_name, pitch_counts()$pitch_name)], ")", sep = ""),
        color = ~pitch_name,
        colors = 'Set1',
        marker = list(size = 3)
      )
      
      location_plot <- location_plot %>% layout(
        title = "Pitch Location in the Strike Zone",
        xaxis = list(title = "Horizontal Location (plate_x)"),
        yaxis = list(title = "Vertical Location (plate_z)")
      )
      
      return(location_plot)
    }
  })
  
  output$Pitch_perc_combined_str <- renderPlotly({
    pitch_percentages_strikes <- pitch_percentages_by_strikes()
    
    if (!is.null(pitch_percentages_strikes)) {
      plot <- ggplot(data = pitch_percentages_strikes, aes(x = factor(strikes), y = Percentage, fill = pitch_name)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.8) +
        labs(title = "Pitch Type Percentages by Strike Count",
             x = "Strikes",
             y = "Percentage (%)",
             fill = "Pitch Name") +
        theme_minimal() +
        facet_wrap(~stand, ncol = 2) 
      
      plot <- ggplotly(plot)
      
      return(plot)
    }
  })
  
  output$Pitch_perc_combined_ball <- renderPlotly({
    pitch_percentages_balls <- pitch_percentages_by_balls()
    
    if (!is.null(pitch_percentages_balls)) {
      plot <- ggplot(data = pitch_percentages_balls, aes(x = factor(balls), y = Percentage, fill = pitch_name)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.8) +
        labs(title = "Pitch Type Percentages by Ball Count",
             x = "Balls",
             y = "Percentage (%)",
             fill = "Pitch Name") +
        theme_minimal() +
        facet_wrap(~stand, ncol = 2)  
      
      plot <- ggplotly(plot)
      
      return(plot)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
