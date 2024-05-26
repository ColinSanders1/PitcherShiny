library(baseballr)
library(shiny)
library(dplyr)
library(plotly)
library(readxl)
library(openxlsx)
library(ggplot2)

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
      selectInput("selected_year", "Select Year", choices = 2016:as.numeric(format(Sys.Date(), "%Y"))),
      actionButton("search_btn", "Retrieve Data"),
      uiOutput("pitch_type_ui")
    ),
    mainPanel(
      plotlyOutput("pitch_movement_plot"),
      plotOutput("pitch_location_heatmap"),
      tableOutput("Avg_Stats1"),
      tableOutput("Avg_Stats2"),
      plotlyOutput("Combined_Pitch_Perc_L"),
      plotlyOutput("Combined_Pitch_Perc_R"),
      tableOutput("CSW_Table")
    )
  )
)

server <- function(input, output, session) {
  player_data <- reactive({
    req(input$search_btn)
    last_name <- input$last_name
    first_name <- input$first_name
    player_id_result <- player_id(last_name, first_name, data)
    
    if (player_id_result == "player not found") {
      return(NULL)  
    } else {
      dat <- scrape_statcast_savant(
        start_date = paste(input$selected_year, "-03-25", sep = ''),  
        end_date = paste(input$selected_year, "-12-12", sep = ''),  
        playerid = player_id_result,
        player_type = "pitcher"
      )
      return(dat)
    }
  })
  
  observeEvent(input$search_btn, {
    updateSelectInput(session, "selected_name", choices = unique(player_data()$player_name))
  })
  
  observe({
    if (!is.null(player_data())) {
      pitch_names <- unique(player_data()$pitch_name)
      output$pitch_type_ui <- renderUI({
        tagList(
          selectInput("selected_heatmap_pitch_type", "Select Heatmap Pitch Type", choices = pitch_names)
        )
      })
    }
  })
  
  output$Avg_Stats1 <- renderTable({
    player_selected_data1 <- player_data()
    
    if (!is.null(player_selected_data1)) {
      player_avg_data1 <- player_selected_data1 %>%
        group_by(pitch_name) %>%
        summarize(
          Num_pitches = n(),
          Average_SpinRate = mean(release_spin_rate, na.rm = TRUE),
          Average_Velocity = mean(release_speed, na.rm = TRUE),
          Average_Extension = mean(release_extension, na.rm = TRUE),
          Average_ReleaseHeight = mean(release_pos_y, na.rm = TRUE)
        ) %>%
        filter(!is.na(pitch_name) & pitch_name != "")
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
        group_by(pitch_name) %>%
        summarize(Average_VAA = mean(vaa, na.rm = TRUE),
                  XWOBA = mean(estimated_woba_using_speedangle, na.rm = TRUE),
                  Average_Exit_Velocity = mean(launch_speed, na.rm = TRUE),
                  Average_Launch_Angle = mean(launch_angle, na.rm = TRUE)
        )%>%
        filter(!is.na(pitch_name) & pitch_name != "")
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
  
  output$pitch_location_heatmap <- renderPlot({
    player_selected_data <- player_data()
    
    if (!is.null(player_selected_data) && !is.null(input$selected_heatmap_pitch_type)) {
      filtered_data <- player_selected_data %>%
        filter(pitch_name == input$selected_heatmap_pitch_type)
      
      if (nrow(filtered_data) > 0) {
        plot <- ggplot(filtered_data, aes(x = plate_x, y = plate_z)) +
          stat_density_2d(aes(fill = after_stat(density)), geom = 'raster', contour = FALSE) +
          scale_fill_gradientn(colours = c("blue", "white", "red")) +
          annotate("rect", xmin = -1, xmax = 1, ymin = 1.6, ymax = 3.4, fill = NA, color = "black", alpha = 0.1) +
          ylim(1, 4) +
          xlim(-1.8, 1.8) +
          theme_classic() +
          xlab("Horizontal Pitch Location") +
          ylab("Vertical Pitch Location") +
          ggtitle("Pitch Location Heat Map", subtitle = "Catcher's Perspective") +
          guides(fill = guide_colorbar(title = "Density"))
        
        return(plot)
      } else {
        return(NULL)
      }
    }
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
  
  pitch_percentages_by_strikes <- reactive({
    player_selected_data <- player_data()
    
    if (!is.null(player_selected_data)) {
      pitch_counts_data <- player_selected_data %>%
        group_by(pitch_name, strikes, stand) %>%
        summarise(Pitch_Count = n())
      
      # Calculate the percentage for each pitch type by strike count and stand
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
      
      # Calculate the percentage for each pitch type by ball count and stand
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
  
 combined_pitch_percentages <- reactive({
    pitch_percentages_strikes <- pitch_percentages_by_strikes()
    pitch_percentages_balls <- pitch_percentages_by_balls()
    
    if (!is.null(pitch_percentages_strikes) && !is.null(pitch_percentages_balls)) {
      # Merge data
      combined_data <- merge(pitch_percentages_strikes, pitch_percentages_balls, 
                             by = c("pitch_name", "stand"), suffixes = c("_strikes", "_balls"))
      
      # Create a new variable for combined ball and strike counts
      combined_data$ball_strike_count <- paste0("B", combined_data$balls, "-S", combined_data$strikes)
      
      return(combined_data)
    } else {
      return(NULL)
    }
  })
  
 output$Combined_Pitch_Perc_L <- renderPlotly({
   combined_data <- combined_pitch_percentages()
   
   if (!is.null(combined_data)) {
     filtered_data <- combined_data[combined_data$stand == "L",]
     
     plot <- ggplot(data = filtered_data, aes(x = ball_strike_count, y = Percentage_strikes, fill = pitch_name)) +
       geom_bar(stat = "identity", position = "dodge", width = 0.8) +
       geom_bar(aes(x = ball_strike_count, y = Percentage_balls, fill = pitch_name), 
                stat = "identity", position = "dodge", width = 0.8) +
       labs(title = "Pitch Type Percentages by Count vs LHB",
            x = "Count",
            y = "Percentage (%)",
            fill = "Pitch Name") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
             axis.title = element_text(size = 10),  # Increase font size of axis labels
             legend.title = element_text(size = 12),  # Increase font size of legend title
             legend.text = element_text(size = 10),  # Increase font size of legend text
             plot.title = element_text(size = 12)) +  # Increase font size of plot title
       guides(fill = guide_legend(reverse = TRUE))  # Reverse legend order for better readability
     
     plot <- ggplotly(plot, tooltip = "text")
     
     return(plot)
   }
 })
 
 output$Combined_Pitch_Perc_R <- renderPlotly({
   combined_data <- combined_pitch_percentages()
   
   if (!is.null(combined_data)) {
     filtered_data <- combined_data[combined_data$stand == "R",]
     
     plot <- ggplot(data = filtered_data, aes(x = ball_strike_count, y = Percentage_strikes, fill = pitch_name)) +
       geom_bar(stat = "identity", position = "dodge", width = 0.8) +
       geom_bar(aes(x = ball_strike_count, y = Percentage_balls, fill = pitch_name), 
                stat = "identity", position = "dodge", width = 0.8) +
       labs(title = "Pitch Type Percentages by Count vs RHB",
            x = "Count",
            y = "Percentage (%)",
            fill = "Pitch Name") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
             axis.title = element_text(size = 10),  # Increase font size of axis labels
             legend.title = element_text(size = 12),  # Increase font size of legend title
             legend.text = element_text(size = 10),  # Increase font size of legend text
             plot.title = element_text(size = 12)) +  # Increase font size of plot title
       guides(fill = guide_legend(reverse = TRUE))  # Reverse legend order for better readability
     
     
     plot <- ggplotly(plot, tooltip = "text")
     
     return(plot)
   }
   })
     
   output$CSW_Table <- renderTable({
       player_selected_data <- player_data()
       
       if (!is.null(player_selected_data)) {
         strike_counts <- player_selected_data %>%
           filter(description %in% c("called_strike", "swinging_strike")) %>%
           group_by(pitch_name) %>%
           summarize(
             called_strikes = sum(description == "called_strike"),
             swinging_strikes = sum(description == "swinging_strike")
           )
         
         pitch_counts <- player_selected_data %>%
           group_by(pitch_name) %>%
           summarize(num_pitches = n())
         
         merged_dat <- merge(pitch_counts, strike_counts, by = "pitch_name", all = TRUE)
         
         merged_dat$csw_percentage <- ((merged_dat$called_strikes + merged_dat$swinging_strikes) / merged_dat$num_pitches) * 100
         
         merged_dat <- merged_dat[merged_dat$pitch_name != "", ]
         
         merged_dat$usage <- merged_dat$num_pitches / sum(merged_dat$num_pitches) * 100

         result <- merged_dat[, c("pitch_name", "num_pitches", "usage", "csw_percentage")]
         
         return(result)
   }
 })
}
 

shinyApp(ui = ui, server = server)
