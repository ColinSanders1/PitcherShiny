library(baseballr)
library(shiny)
library(dplyr)
library(plotly)
library(readxl)
library(openxlsx)
library(ggplot2)
library(DBI)
library(RSQLite)

tryCatch({
  data <- read.xlsx("IDtable.xlsx")
  print("IDtable loaded successfully")
}, error = function(e) {
  print("Error loading IDtable:")
  print(e)
})

player_id <- function(last_name, first_name, data) {
  id <- data$player_id[data$last_name == last_name & data$first_name == first_name]
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
      selectInput("selected_year", "Select Year", choices = 2023:as.numeric(format(Sys.Date(), "%Y"))),
      actionButton("search_btn", "Retrieve Data"),
      verbatimTextOutput("pitcher_type_display"), 
      uiOutput("pitch_type_ui"),
      selectInput("selected_count", "Select Count", 
                  choices = c("All", "0-0", "0-1", "0-2", "1-0", "1-1", "1-2", "2-0", "2-1", "2-2", "3-0", "3-1", "3-2"), 
                  selected = "All")
    ),
    mainPanel(
      plotlyOutput("pitch_movement_plot"),
      tableOutput("Avg_Stats1"),
      tableOutput("Avg_Stats2"),
      fluidRow(
        column(6, plotlyOutput("Combined_Pitch_Perc_L")),
        column(6, plotlyOutput("Combined_Pitch_Perc_R")),
      plotOutput("pitch_location_heatmap")
  )
))
)

server <- function(input, output, session) {
  player_data <- eventReactive(input$search_btn, {
    player_id_result <- player_id(input$last_name, input$first_name, data)
    if (player_id_result == "player not found") return(NULL)
    
    con <- dbConnect(RSQLite::SQLite(), "statcast_db.sqlite")
    query <- paste0("SELECT * FROM mlbdata WHERE pitcher_id = ", player_id_result, " AND year = ", input$selected_year)
    dat <- dbGetQuery(con, query)
    dbDisconnect(con)
    
    if (nrow(dat) == 0) return(NULL)
    
    zone_percentage <- dat %>%
      group_by(pitch_name) %>%
      summarize(zone_percentage = round(mean(plate_x >= -1 & plate_x <= 1 & plate_z >= 1.6 & plate_z <= 3.4, na.rm = TRUE) * 100, 2))
    
    strike_counts <- dat %>%
      filter(description %in% c("called_strike", "swinging_strike")) %>%
      group_by(pitch_name) %>%
      summarize(
        called_strikes = sum(description == "called_strike"),
        swinging_strikes = sum(description == "swinging_strike")
      )
    
    pitch_counts <- dat %>%
      group_by(pitch_name) %>%
      summarize(num_pitches = n(), usage_percentage = (n() / nrow(dat)) * 100)
    
    csw_percentage <- merge(pitch_counts, strike_counts, by = "pitch_name", all = TRUE) %>%
      mutate(csw_percentage = ((called_strikes + swinging_strikes) / num_pitches) * 100)
    
    list(data = dat, zone_percentage = zone_percentage, csw_percentage = csw_percentage)
  })
  pitch_percentages_by_strikes <- reactive({
    player_selected_data <- player_data()$data
    
    if (!is.null(player_selected_data)) {
      pitch_counts_data <- player_selected_data %>%
        group_by(pitch_name, strikes, bat_side) %>%
        summarise(Pitch_Count = n())
      
      pitch_counts_data <- pitch_counts_data %>%
        group_by(strikes, bat_side) %>%
        mutate(Percentage = (Pitch_Count / sum(Pitch_Count)) * 100)
      
      return(pitch_counts_data)
    }
  })
  
  pitch_percentages_by_balls <- reactive({
    player_selected_data <- player_data()$data
    
    if (!is.null(player_selected_data)) {
      pitch_percentages_data <- player_selected_data %>%
        group_by(pitch_name, balls, bat_side) %>%
        summarise(Pitch_Count = n())
      
      pitch_percentages_data <- pitch_percentages_data %>%
        group_by(balls, bat_side) %>%
        mutate(Percentage = (Pitch_Count / sum(Pitch_Count)) * 100)
      
      return(pitch_percentages_data)
    }
  })
  
  combined_pitch_percentages <- reactive({
    pitch_percentages_strikes <- pitch_percentages_by_strikes()
    pitch_percentages_balls <- pitch_percentages_by_balls()
    
    if (!is.null(pitch_percentages_strikes) && !is.null(pitch_percentages_balls)) {
      combined_data <- merge(pitch_percentages_strikes, pitch_percentages_balls, 
                             by = c("pitch_name", "bat_side"), suffixes = c("_strikes", "_balls"))
      
      
      combined_data$ball_strike_count <- paste0("B", combined_data$balls, "-S", combined_data$strikes)
      
      return(combined_data)
    } else {
      return(NULL)
    }
  })
  
  observe({
    if (!is.null(player_data()$data)) {
      pitch_names <- unique(player_data()$data$pitch_name)
      output$pitch_type_ui <- renderUI({
        tagList(
          selectInput("selected_heatmap_pitch_type", "Select Heatmap Pitch Type", choices = pitch_names)
        )
      })
    }
  })
  
  four_seam_pitcher_type <- reactive({
    dat <- player_data()$data
    if (is.null(dat)) return(NULL)
    
    fs_data <- dat %>%
      filter(pitch_name == "4-Seam Fastball") %>%
      mutate(
        spin_eff = abs(sin(spin_axis * pi / 180)),
        spin_type = ifelse(spin_eff < 0.689, "Supinator", "Pronator"),
        arm_angle_bucket = case_when(
          arm_angle > 60 ~ "Over-the-Top",
          arm_angle > 45 ~ "High Â¾",
          arm_angle > 30 ~ "Â¾",
          arm_angle > 15 ~ "Low Â¾",
          arm_angle >= 0 ~ "Sidearm",
          arm_angle < 0 ~ "Submarine",
          TRUE ~ "Unknown"
        ),
        pitcher_type = paste(spin_type, arm_angle_bucket, sep = " ")
      )
    
    if (nrow(fs_data) > 0) {
      return(fs_data$pitcher_type[which.max(table(fs_data$pitcher_type))])
    } else {
      return("No 4-Seam Data")
    }
  })
  
  output$pitcher_type_display <- renderText({
    req(four_seam_pitcher_type())
    paste("Pitcher Type:", four_seam_pitcher_type())
  })
  
  output$Avg_Stats1 <- renderTable({
    dat <- player_data()$data
    if (!is.null(dat)) {
      dat %>%
        group_by(pitch_name) %>%
        summarize(
          Usage_Percentage = (n() / nrow(dat)) * 100,
          Average_SpinRate = mean(release_spin_rate, na.rm = TRUE),
          Average_Velocity = mean(release_speed, na.rm = TRUE),
          Average_Extension = mean(extension, na.rm = TRUE),
          Average_ReleaseHeight = mean(release_pos_z, na.rm = TRUE),
          Average_ArmAngle = mean(arm_angle, na.rm = TRUE)
        ) %>%
        filter(!is.na(pitch_name) & pitch_name != "")
    }
  })
  
  output$Avg_Stats2 <- renderTable({
    dat <- player_data()$data
    zp <- player_data()$zone_percentage
    csw <- player_data()$csw_percentage
    if (!is.null(dat)) {
      vyf <- -sqrt(dat$vy0^2 - (2 * dat$ay * (50 - 17/12)))
      t <- (vyf - dat$vy0) / dat$ay
      vzf <- dat$vz0 + (dat$az * t)
      dat$vaa <- -atan(vzf / vyf) * (180 / pi)
      
      dat %>%
        group_by(pitch_name) %>%
        summarize(
          Average_VAA = mean(vaa, na.rm = TRUE),
          XWOBA = mean(expected_woba, na.rm = TRUE),
          Average_Exit_Velocity = mean(launch_speed, na.rm = TRUE),
          Average_Launch_Angle = mean(launch_angle, na.rm = TRUE)
        ) %>%
        filter(!is.na(pitch_name) & pitch_name != "") %>%
        left_join(zp, by = "pitch_name") %>%
        left_join(csw %>% select(pitch_name, csw_percentage), by = "pitch_name")
    }
  })
  
  output$pitch_location_heatmap <- renderPlot({
    dat <- player_data()$data
    if (!is.null(dat) && !is.null(input$selected_heatmap_pitch_type)) {
      filtered <- dat %>% filter(pitch_name == input$selected_heatmap_pitch_type)
      
      if (input$selected_count != "All") {
        count_parts <- strsplit(input$selected_count, "-")[[1]]
        balls <- as.numeric(count_parts[1])
        strikes <- as.numeric(count_parts[2])
        filtered <- filtered %>% filter(balls == balls, strikes == strikes)
      }
      if (nrow(filtered) > 0) {
        ggplot(filtered, aes(x = plate_x, y = plate_z)) +
          stat_density_2d(aes(fill = after_stat(density)), geom = 'raster', contour = FALSE) +
          scale_fill_gradientn(colours = c("blue", "white", "red")) +
          annotate("rect", xmin = -1, xmax = 1, ymin = 1.6, ymax = 3.4, fill = NA, color = "black", alpha = 0.1) +
          ylim(1, 4) +
          xlim(-1.8, 1.8) +
          theme_classic() +
          xlab("Horizontal Pitch Location") +
          ylab("Vertical Pitch Location") +
          ggtitle("Pitch Location Heat Map", subtitle = "Catcher's Perspective")
      }
    }
  })
  
  output$pitch_movement_plot <- renderPlotly({
    dat <- player_data()$data
    if (!is.null(dat)) {
      plot_ly(data = dat, x = ~pfx_x, y = ~pfx_z, type = 'scatter',
              mode = 'markers', text = ~paste(pitch_name), color = ~pitch_name,
              colors = 'Set1', marker = list(size = 5)) %>%
        layout(title = "Pitch Movement by Type",
               xaxis = list(title = "Horizontal Movement (pfx_x)"),
               yaxis = list(title = "Vertical Movement (pfx_z)"))
    }
  })
  
  output$Combined_Pitch_Perc_L <- renderPlotly({
    dat <- player_data()$data
    if (!is.null(dat)) {
      lefty <- dat %>% filter(bat_side == "L")
      
      if (input$selected_count != "All") {
        lefty <- lefty %>% filter(paste(balls, strikes, sep = "-") == input$selected_count)
      }
      
      counts <- lefty %>%
        group_by(pitch_name) %>%
        summarize(count = n()) %>%
        mutate(perc = count / sum(count) * 100)
      
      plot_ly(counts, labels = ~pitch_name, values = ~perc, type = 'pie') %>%
        layout(title = paste('Pitch Type % vs LHH (Count:', input$selected_count, ')'), showlegend = TRUE)
    }
  })
  
  output$Combined_Pitch_Perc_R <- renderPlotly({
    dat <- player_data()$data
    if (!is.null(dat)) {
      righty <- dat %>% filter(bat_side == "R")
      
      if (input$selected_count != "All") {
        righty <- righty %>% filter(paste(balls, strikes, sep = "-") == input$selected_count)
      }
      
      counts <- righty %>%
        group_by(pitch_name) %>%
        summarize(count = n()) %>%
        mutate(perc = count / sum(count) * 100)
      
      plot_ly(counts, labels = ~pitch_name, values = ~perc, type = 'pie') %>%
        layout(title = paste('Pitch Type % vs RHH (Count:', input$selected_count, ')'), showlegend = TRUE)
    }
  })
}

shinyApp(ui = ui, server = server)
