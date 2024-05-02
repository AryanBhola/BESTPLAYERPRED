library(shiny)

# Define UI
ui <- fluidPage(
  style = "background-color:#fffcee ;",
  titlePanel(
    div(style = "text-align: center;",
        tags$h1("TOP FOOTBALL PLAYERS", style = "color: #202F56; font-size: 50px; font-family: 'Comic Sans MS', cursive;")
        
    )
  ),
  
  # Sidebar layout
  sidebarLayout(
    position = "left",  # Position sidebar on the left
    sidebarPanel(
      style = "background-color: #ffffff;
      border:4px solid #FFF5Ef;",
      
      # Add radio buttons to select player position
      radioButtons("position", tags$h3("SELECT PLAYER POSITION :", style = "font-size: 25px; color: #202F56; font-family: 'Comic Sans MS', cursive;"),
                   choices = c("Striker", "Midfielder", "Center Back","Wing Backs","Central Defensive Midfielders","Central Midfielder","Wingers"),
                   selected = "Striker"),
      
     tags$style(HTML("
        .radio label {
          font-family: Helvetica, sans-serif; 
          color:#202F56 ;
          font-size: 22px; font-family: 'Comic Sans MS', cursive;
          
        }
      "))
  ),
    
    # Main panel
    mainPanel(
      # Strikers plot
      position = "right",
      div(
        style = "background-color: #ffffff; border: 4px solid #FFF5Ef; padding: 50px; margin-bottom: 20px;",
        plotOutput("selected_plot")
      )
      
    )
  )
)
