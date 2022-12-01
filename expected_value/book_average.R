average_books = function(league_ml) {
  
  get_book_data_clean = function(league) {
    load("betting_data/book_id_data.rdata")
    
    book_data_clean = BOOK_ID_DATA %>%
      filter(!(id %in% c(266, 251, 1434)))
    
    # removes the state from display name passed in
    remove_state = function(display_name, state) {
      if (length(state) > 1) {
        str_replace(display_name, regex("Canada$"), "")
      } else {
        pattern = paste(state,"$",sep = "")
        str_replace(display_name, regex(pattern), "")
      }
    }
    
    # cleans up cases not caught by the regex
    clean_parent_name = function(id, parent_name) {
      if (id == 1073) {
        return("Caesars")
      } else if (id == 19){
        return("BetMGM")
      } else if (id == 1074) {
        return("Betway")
      } else if (id == 1072) {
        return("888Sport")
      } else (
        return(parent_name)
      )
    }
    
    # sets the parent name as the display name without the states
    book_data_clean = book_data_clean %>%
      mutate(parent_name = map2_chr(display_name, states, remove_state))
    
    # cleans up cases not caught by regex
    book_data_clean = book_data_clean %>%
      mutate(parent_name = map2_chr(id, parent_name, clean_parent_name))
    
    # remove space at the end
    book_data_clean = book_data_clean %>%
      mutate(parent_name = str_replace(parent_name, regex(" $"), ""))
    
    return(book_data_clean)
  }
  
  
  book_data_clean = get_book_data_clean()
  
  parent_name_data = book_data_clean %>%
    select(c(id, parent_name)) %>%
    mutate(book_id = id) %>%
    select(-c(id))
  
  league_ml = left_join(league_ml, parent_name_data, by = "book_id")
  
  prob.to.line = function(prob) {
    if_else(prob > .5,
            -1 * prob * 100 / (1 - prob),
            100 / prob- 100)
  }
  
  league_ml = league_ml %>%
    group_by(id, parent_name) %>%
    summarise(home_team_win = first(home_team_win),
              brier_score = mean(brier_score),
              season = first(season),
              home_prob_fair = mean(home_prob_fair),
              away_prob_fair = mean(away_prob_fair),
              home_prob_vig = mean(home_prob_vig),
              away_prob_vig = mean(away_prob_vig),
              ml_home = prob.to.line(mean(home_prob_vig)),
              ml_away = prob.to.line(mean(away_prob_vig))) %>%
    ungroup() %>%
    mutate(league = "ncaaf")
  
  parent_name_list = league_ml$parent_name %>% unique()
  
  parent_name_df = as.data.frame(
    cbind(
      c(1:length(parent_name_list)), 
      parent_name_list)
  )
  
  colnames(parent_name_df) = c("book_id", "parent_name")
  
  book_legality = book_data_clean %>%
    select(c(parent_name, is_legal)) %>%
    distinct() %>%
    group_by(parent_name) %>%
    summarise(is_legal = length(which(is_legal)) == 1)
  
  parent_name_df = left_join(parent_name_df, book_legality, by = "parent_name")

  
  league_ml = left_join(league_ml, parent_name_df, by = "parent_name")
  
  league_ml = league_ml %>%
    mutate(book_id = as.numeric(book_id))
  
  return(league_ml)
}