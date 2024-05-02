# Define server logic
function(input, output) {
  
  # Load your dataset 'male_players' here
  
  # Filter data for a specific update date
  players <- male_players
  players <- players[players$update_as_of == "22-09-2023", ]
  
  # List of useless variables to remove
  useless_variables <- c("fifa_version", "dob", "preferred_foot", "work_rate",
                         "fifa_update", "body_type", "nationality_id", "nationality_name", 
                         "nation_team_id", "nation_position", "update_as_of", "short_name", 
                         "club_team_id", "club_name", "club_position", "player_id", "player_url", 
                         "club_joined_date", "club_contract_valid_until_year", "club_loaned_from", 
                         "club_jersey_number", "nation_jersey_number", "real_face", 
                         "release_clause_eur", "player_tags", "player_traits", "value_eur", 
                         "wage_eur", "league_id", "league_name", "league_level", 
                         "ls", "st", "rs", "lw", "lf", "cf", "rf", "rw", "lam", "cam", "ram", 
                         "lm", "lcm", "cm", "rcm", "rm", "lwb", "ldm", "cdm", "rdm", "rwb", 
                         "lb", "lcb", "cb", "rcb", "rb", "gk", "goalkeeping_diving", 
                         "goalkeeping_handling", "goalkeeping_kicking", "goalkeeping_positioning", 
                         "goalkeeping_reflexes", "goalkeeping_speed")
  
  # Remove useless variables
  players <- players[, !(names(players) %in% useless_variables)]
  
  # Filter out goalkeepers
  onfield_players <- players[players$player_positions != "GK",]
  onfield_players <- replace(onfield_players, is.na(data), 0)
  
  # Split player positions
  max_length <- max(sapply(strsplit(onfield_players$player_positions, ","), length))
  df_split <- do.call(rbind, lapply(strsplit(onfield_players$player_positions, ","), function(x) {
    if (length(x) < max_length) {
      c(x, rep(NA, max_length - length(x)))
    } else {
      x
    }
  }))
  df_split <- as.data.frame(df_split)
  colnames(df_split) <- paste0("prefered_position", 1:max_length)
  onfield_players <- cbind(onfield_players, df_split)
  
  # Remove string characters
  cols <- setdiff(names(onfield_players), c("preferred_position1", "long_name"))
  onfield_players[, cols] <- lapply(onfield_players[, cols], function(x) {
    if (is.character(x)) eval(x) else x
  })
  
  # Calculate striker rating
  a <- 1
  b <- 2
  c <- 3
  d <- 4
  
  onfield_players$att_striker <- (
    b * onfield_players$weak_foot + 
      b * onfield_players$skill_ball_control + 
      a * onfield_players$mentality_vision + 
      b * onfield_players$mentality_aggression + 
      b * onfield_players$movement_agility + 
      a * onfield_players$skill_curve + 
      a * onfield_players$power_long_shots + 
      d * onfield_players$movement_balance + 
      d * onfield_players$attacking_finishing + 
      d * onfield_players$attacking_heading_accuracy + 
      c * onfield_players$power_jumping + 
      c * onfield_players$skill_dribbling
  ) / (3 * a + 4 * b + 2 * c + 3 * d)
  
  # Calculate midfielder rating
  onfield_players$att_midfielder <- (
    d * onfield_players$skill_ball_control +
      c * onfield_players$skill_dribbling +
      a * onfield_players$defending_marking_awareness +
      b * onfield_players$attacking_short_passing +
      b * onfield_players$mentality_vision +
      c * onfield_players$attacking_crossing +
      a * onfield_players$skill_long_passing +
      c * onfield_players$movement_acceleration +
      d * onfield_players$movement_sprint_speed +
      c * onfield_players$power_stamina +
      b * onfield_players$movement_agility +
      b * onfield_players$movement_balance +
      d * onfield_players$mentality_interceptions +
      b * onfield_players$attacking_heading_accuracy +
      c * onfield_players$power_long_shots +
      b * onfield_players$attacking_finishing
  ) / (4 * a + 6 * b + 4 * c + 5 * d)
  
  # Calculate center back rating
  onfield_players$att_CB <- (
    d * onfield_players$movement_reactions +
      c * onfield_players$mentality_interceptions +
      d * onfield_players$defending_sliding_tackle +
      d * onfield_players$defending_standing_tackle +
      b * onfield_players$mentality_vision +
      b * onfield_players$mentality_composure +
      b * onfield_players$attacking_crossing +
      a * onfield_players$attacking_short_passing +
      b * onfield_players$skill_long_passing +
      c * onfield_players$movement_acceleration +
      b * onfield_players$movement_sprint_speed +
      d * onfield_players$power_stamina +
      d * onfield_players$power_jumping +
      d * onfield_players$attacking_heading_accuracy +
      b * onfield_players$power_long_shots +
      d * onfield_players$defending_marking_awareness +
      c * onfield_players$mentality_aggression
  ) / (6 * b + 3 * c + 7 * d)
  # Calculate wing back rating
  onfield_players$wing_backs <- (
    b * onfield_players$skill_ball_control +  
      a * onfield_players$dribbling +
      a * onfield_players$defending_marking_awareness +
      d * onfield_players$defending_sliding_tackle +
      d * onfield_players$defending_standing_tackle +
      a * onfield_players$mentality_positioning +
      c * onfield_players$mentality_vision +
      c * onfield_players$attacking_crossing +
      b * onfield_players$attacking_short_passing +
      c * onfield_players$skill_long_passing +
      d * onfield_players$movement_acceleration +
      d * onfield_players$movement_sprint_speed +
      c * onfield_players$power_stamina +
      b * onfield_players$movement_agility +
      b * onfield_players$movement_balance +
      d * onfield_players$mentality_interceptions +
      c * onfield_players$defending_standing_tackle +
      b * onfield_players$attacking_heading_accuracy +
      a * onfield_players$power_long_shots +
      b * onfield_players$attacking_finishing
  ) / (4 * a + 2 * b + 4 * c + 4 * d)
  
  # Calculate central defensive midfielder rating
  onfield_players$central_DM <- (
    d * onfield_players$skill_ball_control +  
      c * onfield_players$skill_dribbling +
      a * onfield_players$defending_marking_awareness +
      b * onfield_players$passing +
      b * onfield_players$mentality_vision +
      c * onfield_players$attacking_crossing +
      a * onfield_players$attacking_short_passing +
      b * onfield_players$skill_long_passing +
      b * onfield_players$movement_acceleration +
      c * onfield_players$movement_sprint_speed +
      c * onfield_players$power_stamina +
      b * onfield_players$movement_agility +
      b * onfield_players$movement_balance +
      d * onfield_players$mentality_interceptions +
      c * onfield_players$defending_standing_tackle +
      b * onfield_players$attacking_heading_accuracy +
      a * onfield_players$power_long_shots +
      b * onfield_players$attacking_finishing +
      c * onfield_players$mentality_positioning
  ) / (4 * a + 6 * b + 3 * c + 5 * d)
  
  # Calculate central midfielder/playmaker rating
  onfield_players$central_M1 <- (
    d * onfield_players$skill_ball_control +  
      c * onfield_players$skill_dribbling +
      a * onfield_players$defending_marking_awareness +
      b * onfield_players$passing +
      c * onfield_players$mentality_vision +
      c * onfield_players$attacking_crossing +
      a * onfield_players$attacking_short_passing +
      b * onfield_players$skill_long_passing +
      b * onfield_players$movement_acceleration +
      c * onfield_players$movement_sprint_speed +
      c * onfield_players$power_stamina +
      b * onfield_players$movement_agility +
      b * onfield_players$movement_balance +
      d * onfield_players$mentality_interceptions +
      c * onfield_players$defending_standing_tackle +
      b * onfield_players$attacking_heading_accuracy +
      a * onfield_players$power_long_shots +
      b * onfield_players$attacking_finishing +
      c * onfield_players$mentality_positioning
  ) / (3 * a + 7 * b + 7 * c + 2 * d)
  
  # Calculate winger rating
  onfield_players$wingers <- (
    b * onfield_players$skill_ball_control +  
      a * onfield_players$dribbling +
      a * onfield_players$defending_marking_awareness +
      b * onfield_players$passing +
      c * onfield_players$mentality_vision +
      c * onfield_players$attacking_crossing +
      b * onfield_players$attacking_short_passing +
      a * onfield_players$skill_long_passing +
      c * onfield_players$movement_acceleration +
      d * onfield_players$movement_sprint_speed +
      c * onfield_players$power_stamina +
      b * onfield_players$movement_agility +
      b * onfield_players$movement_balance +
      a * onfield_players$mentality_interceptions +  
      a * onfield_players$defending_standing_tackle +       
      b * onfield_players$attacking_heading_accuracy +
      c * onfield_players$power_long_shots +
      b * onfield_players$attacking_finishing
  ) / (4 * a + 6 * b + 4 * c + 5 * d)
  
  
  # Select top 5 players for each position
  strikers <- head(onfield_players[order(-onfield_players$att_striker), ], 5)
  midfielders <- head(onfield_players[order(-onfield_players$att_midfielder), ], 5)
  center_backs <- head(onfield_players[order(-onfield_players$att_CB), ], 5)
  wing_backs <-head(onfield_players[order(-onfield_players$wing_backs), ], 5)
  central_DM <- head(onfield_players[order(-onfield_players$ central_DM), ], 5)
  central_M1 <- head(onfield_players[order(-onfield_players$ central_M1), ], 5)
  wingers <- head(onfield_players[order(-onfield_players$wingers), ], 5)
  # Render outputs based on selected option
  output$selected_plot <- renderPlot({
    req(input$position)
    if (input$position == "Striker") {
      ggplot(data = strikers, aes(x = long_name, y = att_striker)) +
        geom_bar(stat = "identity", fill = "#202f56") +
        labs(title = "Top 5 Strikers",
             x = "Player Name",
             y = "Striker Rating") +
        theme_bw() +theme(axis.text.x = element_text(angle = 45, hjust = 1,size=15),
                          axis.text.y = element_text(size = 12, color = "#202f56"),
                          plot.title = element_text(color = "#202f56",size = 25,hjust=0.5)) +
        coord_flip()
    } else if (input$position == "Midfielder") {
      ggplot(data = midfielders, aes(x = long_name, y = att_midfielder)) +
        geom_bar(stat = "identity", fill = "#202F56") +
        labs(title = "Top 5 Midfielders",
             x = "Player Name",
             y = "Midfielder Rating") +
        theme_bw() +theme(axis.text.x = element_text(angle = 45, hjust = 1,size=15),
                          axis.text.y = element_text(size = 12, color = "#202f56"),
                          plot.title = element_text(color = "#202f56",size = 25,hjust=0.5)) +
        coord_flip()
    }else if (input$position == "Wing Backs") {
      ggplot(data = wing_backs, aes(x = long_name, y = wing_backs)) +
        geom_bar(stat = "identity", fill = "#202f56") +
        labs(title = "Top 5 Wing Backs",
             x = "Player Name",
             y = "Midfielder Rating") +
        theme_bw() +theme(axis.text.x = element_text(angle = 45, hjust = 1,size=15),
                          axis.text.y = element_text(size = 12, color = "#202f56"),
                          plot.title = element_text(color = "#202f56",size = 25,hjust=0.5)) +
        coord_flip()
    }else if (input$position == "Central Defensive Midfielders") {
      ggplot(data = central_DM , aes(x = long_name, y = central_DM)) +
        geom_bar(stat = "identity", fill = "#202f56") +
        labs(title = "Top 5 Central Defensive Midfielders",
             x = "Player Name",
             y = "Midfielder Rating") +
        theme_bw() +theme(axis.text.x = element_text(angle = 45, hjust = 1,size=15),
                          axis.text.y = element_text(size = 12, color = "#202f56"),
                          plot.title = element_text(color = "#202f56",size = 25,hjust=0.5)) +
        coord_flip()
    }else if (input$position == "Central Midfielder") {
      ggplot(data = central_M1 , aes(x = long_name, y = central_M1)) +
        geom_bar(stat = "identity", fill = "#202f56") +
        labs(title = "Top 5 Central Midfielders",
             x = "Player Name",
             y = "Midfielder Rating") +
        theme_bw() +theme(axis.text.x = element_text(angle = 45, hjust = 1,size=15),
                          axis.text.y = element_text(size = 12, color = "#202f56"),
                          plot.title = element_text(color = "#202f56",size = 25,hjust=0.5)) +
        coord_flip()
    }else if (input$position == "Wingers") {
      ggplot(data = wingers , aes(x = long_name, y = wingers)) +
        geom_bar(stat = "identity", fill = "#202f56") +
        labs(title = "Top 5 Wingers",
             x = "Player Name",
             y = "Midfielder Rating") +
        theme_bw() +theme(axis.text.x = element_text(angle = 45, hjust = 1,size=15),
                          axis.text.y = element_text(size = 12, color = "#202f56"),
                          plot.title = element_text(color = "#202f56",size = 25,hjust=0.5)) +
        coord_flip()
    }
    else if (input$position == "Center Back") {
      ggplot(data = center_backs, aes(x = long_name, y = att_CB)) +
        geom_bar(stat = "identity", fill = "#202f56") +
        labs(title = "Top 5 Center Backs",
             x = "Player Name",
             y = "CB Rating") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1,size=15),
              axis.text.y = element_text(size = 12, color = "#202f56"),
      plot.title = element_text(color = "#202f56",size = 25,hjust=0.5)) +
        coord_flip()
    }
  })
}
