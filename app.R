library(baseballr)
library(dplyr)
library(plotly)
library(formattable)

dat <- scrape_statcast_savant_pitcher_all("2023-03-25", Sys.Date(), pitcherid = NULL)

vyf = -sqrt(dat$vy0^2 - (2 * dat$ay * (50 - 17/12)))
t = (vyf - dat$vy0) / dat$ay
vzf = dat$vz0 + (dat$az * t)
dat$vaa = -atan(vzf / vyf) * (180 / pi)

dat$pitch_type[which(dat$pitch_type == 'CH')] <- "Changeup"
dat$pitch_type[which(dat$pitch_type == 'CU')] <- "Curveball"
dat$pitch_type[which(dat$pitch_type == 'FC')] <- "Cutter"
dat$pitch_type[which(dat$pitch_type == 'FF')] <- "Four seam"
dat$pitch_type[which(dat$pitch_type == 'FS')] <- "Split Flinger"
dat$pitch_type[which(dat$pitch_type == 'FT')] <- "Two-Seam"
dat$pitch_type[which(dat$pitch_type == 'KC')] <- "Kuckle-Curve"
dat$pitch_type[which(dat$pitch_type == 'SI')] <- "Sinker"
dat$pitch_type[which(dat$pitch_type == 'SL')] <- "Slider"
dat$pitch_type[which(dat$pitch_type == "ST")] <- "Sweeper"
dat$pitch_type[which(dat$pitch_type == "FO")] <- "Forkball"
dat$pitch_type[which(dat$pitch_type == "KN")] <- "Knuckleball"

library(shiny)

ui <- fluidPage(
  titlePanel("Pitcher Report"),
  fluidRow(
    column(3,
           selectInput("selected_name", "Select a Name:", choices = unique(dat$player_name))
    ),
    column(9,
           mainPanel(
             plotlyOutput("pitch_movement_plot"),  # Move the plot to the left side
             textOutput("selected_output"),
             tableOutput("Avg_Stats"),
             tableOutput("Percentile_Rankings")  # Add a new table for percentile rankings
           )
    )
  )
)

server <- function(input, output) {
  output$selected_output <- renderText({
    selected_name <- input$selected_name
  })
  
  output$Avg_Stats <- renderTable({
    selected_name <- input$selected_name
    player_avg_data <- dat %>%
      filter(player_name == selected_name) %>%
      group_by(player_name, pitch_type) %>%
      summarize(
        Average_SpinRate = mean(release_spin_rate, na.rm = TRUE),
        Average_Velocity = mean(release_speed, na.rm = TRUE),
        Average_Extension = mean(release_extension, na.rm = TRUE),
        Average_ReleaseHeight = mean(release_pos_y, na.rm = TRUE),
        Average_VAA = mean(vaa, na.rm = TRUE),
        XBA = mean(estimated_ba_using_speedangle, na.rm = TRUE),
        XWOBA = mean(estimated_woba_using_speedangle, na.rm = TRUE),
        Average_Exit_Velocity = mean(launch_speed, na.rm = TRUE),
        Average_Launch_Angle = mean(launch_angle, na.rm = TRUE)
      )
    player_avg_data
  })
  
  # Calculate percentile rankings
  percentile_rankings <- reactive({
    selected_name <- input$selected_name
    dat %>%
      filter(player_name == selected_name) %>%
      summarise(
        Percentile_SpinRate = ecdf(release_spin_rate)(mean(release_spin_rate, na.rm = TRUE)),
        Percentile_Velocity = ecdf(release_speed)(mean(release_speed, na.rm = TRUE)),
        Percentile_Extension = ecdf(release_extension)(mean(release_extension, na.rm = TRUE)),
        Percentile_ReleaseHeight = ecdf(release_pos_y)(mean(release_pos_y, na.rm = TRUE)),
        Percentile_VAA = ecdf(vaa)(mean(vaa, na.rm = TRUE)),
        Percentile_XBA = ecdf(estimated_ba_using_speedangle)(mean(estimated_ba_using_speedangle, na.rm = TRUE)),
        Percentile_XWOBA = ecdf(estimated_woba_using_speedangle)(mean(estimated_woba_using_speedangle, na.rm = TRUE)),
        Percentile_ExitVelocity = ecdf(launch_speed)(mean(launch_speed, na.rm = TRUE)),
        Percentile_LaunchAngle = ecdf(launch_angle)(mean(launch_angle, na.rm = TRUE))
      )
  })
  
  # Display percentile rankings in a table
  output$Percentile_Rankings <- renderTable({
    percentile_rankings()
  })
  
  # Create a summary table to count pitch types
  pitch_counts <- reactive({
    selected_name <- input$selected_name
    dat %>%
      filter(player_name == selected_name) %>%
      group_by(pitch_type) %>%
      summarise(Pitch_Count = n())
  })
  
  # Create a scatter plot for pitch movement with labels for pitch counts
  output$pitch_movement_plot <- renderPlotly({
    selected_name <- input$selected_name
    pitch_movement_data <- dat %>%
      filter(player_name == selected_name)
    
    # Create a scatter plot using plotly with color based on pitch_type and labels for pitch counts
    plot <- plot_ly(data = pitch_movement_data, x = ~pfx_x, y = ~pfx_z, type = 'scatter',
                    mode = 'markers', text = ~paste(pitch_type, " (", pitch_counts()$Pitch_Count[match(pitch_type, pitch_counts()$pitch_type)], ")", sep = ""), 
                    color = ~pitch_type, colors = 'Set1', marker = list(size = 10))
    
    # Customize the plot layout
    plot <- plot %>% layout(
      title = "Pitch Movement by Type",
      xaxis = list(title = "Horizontal Movement (pfx_x)"),
      yaxis = list(title = "Vertical Movement (pfx_z)")
    )
    
    return(plot)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
