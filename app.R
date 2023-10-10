library(baseballr)
library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(readxl)
library(openxlsx)

tryCatch({
  data <- read.xlsx("~/IDtable.xlsx")
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
  titlePanel("Pitcher Report"),
  sidebarLayout(
    sidebarPanel(
      textInput("last_name", "Last Name", ""),
      textInput("first_name", "First Name", ""),
      actionButton("search_btn", "Retrieve Data")
    ),
    mainPanel(
      plotlyOutput("pitch_movement_plot"), 
      plotlyOutput("pitch_location_plot"),
      tableOutput("Avg_Stats"),
      plotlyOutput("Pitch_perc_by_ball_count"),
      plotlyOutput("Pitch_perc_by_strike_count")
    )
  )
)

server <- function(input, output, session) {
  player_data <- eventReactive(input$search_btn, {
    last_name <- input$last_name
    first_name <- input$first_name
    player_id_result <- player_id(last_name, first_name, data)
    
    if (player_id_result == "player not found") {
      return(NULL)  # Player not found, return NULL
    } else {
      # Fetch pitch data for the selected player based on their ID
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
    # Update the choices in the selectInput based on the retrieved player data
    updateSelectInput(session, "selected_name", choices = unique(player_data()$player_name))
  })
  
  output$selected_output <- renderText({
    selected_name <- input$selected_name
    selected_name
  })
  
  output$Avg_Stats <- renderTable({
    player_selected_data <- player_data()
    
    if (!is.null(player_selected_data)) {
      # Retrieve data for the selected player and create the player_avg_data table
      player_avg_data <- player_selected_data %>%
        group_by(player_name, pitch_name) %>%
        summarize(
          Average_SpinRate = mean(release_spin_rate, na.rm = TRUE),
          Average_Velocity = mean(release_speed, na.rm = TRUE),
          Average_Extension = mean(release_extension, na.rm = TRUE),
          Average_ReleaseHeight = mean(release_pos_y, na.rm = TRUE),
          XWOBA = mean(estimated_woba_using_speedangle, na.rm = TRUE),
          Average_Exit_Velocity = mean(launch_speed, na.rm = TRUE),
          Average_Launch_Angle = mean(launch_angle, na.rm = TRUE)
        )
      player_avg_data
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
      
      # Calculate the percentage for each pitch type
      pitch_counts_data <- pitch_counts_data %>%
        mutate(Percentage = (Pitch_Count / sum(Pitch_Count)) * 100)
      
      return(pitch_counts_data)
    }
  })
  
  # Create a summary table to count pitch types by strikes
  pitch_counts_strikes <- reactive({
    player_selected_data <- player_data()
    
    if (!is.null(player_selected_data)) {
      pitch_counts_data <- player_selected_data %>%
        group_by(pitch_name, strikes) %>%
        summarise(Pitch_Count = n())
      
      # Calculate the percentage for each pitch type by strike count
      pitch_counts_data <- pitch_counts_data %>%
        group_by(strikes) %>%
        mutate(Percentage = (Pitch_Count / sum(Pitch_Count)) * 100)
      
      return(pitch_counts_data)
    }
  })
  
  # Create a summary table to count pitch types by balls
  pitch_counts_balls <- reactive({
    player_selected_data <- player_data()
    
    if (!is.null(player_selected_data)) {
      pitch_counts_data <- player_selected_data %>%
        group_by(pitch_name, balls) %>%
        summarise(Pitch_Count = n())
      
      # Calculate the percentage for each pitch type by ball count
      pitch_counts_data <- pitch_counts_data %>%
        group_by(balls) %>%
        mutate(Percentage = (Pitch_Count / sum(Pitch_Count)) * 100)
      
      return(pitch_counts_data)
    }
  })
  
  # Create a scatter plot for pitch movement with labels for pitch counts
  output$pitch_movement_plot <- renderPlotly({
    player_selected_data <- player_data()
    
    if (!is.null(player_selected_data)) {
      pitch_movement_data <- player_selected_data
      
      # Create a scatter plot using plotly with color based on pitch_name and labels for pitch counts
      plot <- plot_ly(data = pitch_movement_data, x = ~pfx_x, y = ~pfx_z, type = 'scatter',
                      mode = 'markers', text = ~paste(pitch_name, " (", pitch_counts()$Pitch_Count[match(pitch_name, pitch_counts()$pitch_name)], ")", sep = ""), 
                      color = ~pitch_name, colors = 'Set1', marker = list(size = 5))
      
      # Customize the plot layout
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
  
  output$Pitch_perc_by_strike_count <- renderPlotly({
    bar_chart <- plot_ly(data = pitch_counts_strikes(), x = ~factor(strikes), y = ~Percentage,
                         type = 'bar', color = ~pitch_name, text = ~paste(pitch_name, ": ", Percentage, "%"),
                         marker = list(line = list(width = 2))) %>%
      layout(
        title = "Pitch Type Percentages by Strike Count",
        xaxis = list(title = "Strikes"),
        yaxis = list(title = "Percentage (%)"),
        barmode = 'stack'
      )
    
    return(bar_chart)
  })
  
  # Create a bar chart for pitch type percentages by ball count
  output$Pitch_perc_by_ball_count <- renderPlotly({
    bar_chart <- plot_ly(data = pitch_counts_balls(), x = ~factor(balls), y = ~Percentage,
                         type = 'bar', color = ~pitch_name, text = ~paste(pitch_name, ": ", Percentage, "%"),
                         marker = list(line = list(width = 2))) %>%
      layout(
        title = "Pitch Type Percentages by Ball Count",
        xaxis = list(title = "Balls"),
        yaxis = list(title = "Percentage (%)"),
        barmode = 'stack'
      )
    
    return(bar_chart)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

