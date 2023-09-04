library(baseballr)
library(dplyr)
dat<-scrape_statcast_savant_pitcher_all("2023-03-25", Sys.Date(), pitcherid = NULL)
vyf = -sqrt(dat$vy0^2-(2*dat$ay*(50-17/12)))
t=(vyf-dat$vy0)/dat$ay
vzf = dat$vz0 + (dat$az*t)
dat$vaa=-atan(vzf/vyf)*(180/pi)


library(shiny)

ui <- fluidPage(
  titlePanel("Dropdown Example"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_name", "Select a Name:", choices = unique(dat$player_name))
    ),
    mainPanel(
      textOutput("selected_output"),
      tableOutput("Avg_Stats")
    )
  )
)

server <- function(input, output) {
  output$selected_output <- renderText({
    selected_name <- input$selected_name
    paste("You selected:", selected_name)
  })
  output$Avg_Stats <- renderTable({
    selected_name <- input$selected_name
    player_avg_data <- dat %>%
      filter(player_name == selected_name) %>%
      group_by(player_name,pitch_type) %>%
      summarize(
        Average_SpinRate = mean(release_spin_rate),
        Average_Velocity = mean(release_speed),
        Average_Extension = mean(release_extension),
        Average_ReleaseHeight = mean(release_pos_y),
        Average_VAA = mean(vaa)
      )
    player_avg_data
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
