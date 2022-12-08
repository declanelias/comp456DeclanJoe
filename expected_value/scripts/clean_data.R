
clean_data = function(league) {

  load_book = function(league) {
    file = paste("betting_data/", league, "_raw.rdata", sep = "")
    env = new.env()
    nm = load(file, env)
    return(env[[nm]])
  }
  
  
  line.to.prob = function(line) {
    
    if_else(line < 0,
            abs(line) / (abs(line) + 100) * 100,
            100 / (line + 100) * 100)
    
  }
  
  implied.prob = function(line1, line2) {
    prob1 = line.to.prob(line1)
    prob2 = line.to.prob(line2)
    
    return(prob1 / (prob1 + prob2))
  }
  

  league_raw = load_book(league)
  
  league_raw = league_raw %>%
    filter(type == "game")
  
  
  league_ml = league_raw %>%
    select(-c(teams, draw, total, away_total, away_over, away_under, home_total, 
              home_over, home_under, line_status,
              spread_away_line, spread_home_line, over, under))
  
  league_ml = league_ml %>%
    filter(!is.na(winning_team_id))
  
  league_ml = league_ml %>%
    mutate(home_prob_fair = implied.prob(ml_home, ml_away),
           away_prob_fair = 1 - home_prob_fair,
           home_team_win = if_else(winning_team_id == home_team_id, TRUE, FALSE),
           home_prob_vig = line.to.prob(ml_home) / 100,
           away_prob_vig = line.to.prob(ml_away) / 100,
           brier_score = (home_prob_fair - home_team_win) ^ 2)
  
  
  consensus_data = league_ml %>% filter(book_id == 15)
  
  consensus_data = consensus_data %>%
    mutate(consensus = home_prob_fair) %>%
    select(id, consensus)
  
  league_ml = right_join(league_ml, consensus_data, by = "id")
  
  league_ml = league_ml %>%
    filter(!is.na(consensus))
  
  league_ml = league_ml %>%
    mutate(home_prob_fair = if_else( abs(home_prob_fair - consensus) > .2,
                                     1 - home_prob_fair,
                                     home_prob_fair),
           away_prob_fair = 1 - home_prob_fair)
  
  league_ml = league_ml %>%
    mutate(temp = if_else(abs(home_prob_vig - consensus) > .2,
                          home_prob_vig,
                          away_prob_vig),
           home_prob_vig = if_else(abs(home_prob_vig - consensus) > .2,
                                   away_prob_vig,
                                   home_prob_vig),
           away_prob_vig =  temp)

  
  league_ml = league_ml %>%
    mutate(home_prob_fair = ifelse( abs(home_prob_fair - consensus) > .2,
                                    NA,
                                    home_prob_fair),
           home_prob_fair = ifelse(home_prob_vig + away_prob_vig > 1.1 | home_prob_vig + away_prob_vig < 1,
                                   NA,
                                   home_prob_fair)) %>%
    filter(!is.na(home_prob_fair))
  
  return(league_ml)
}

final_clean = function(ml_data, start_year, end_year, n) {
  ml_data = ml_data %>%
    filter(!is.na(parent_name))
  
  # Remove consensus and open
  ml_data = ml_data %>%
    filter(!(parent_name %in% c("Consensus", "Open")))
  
  
  # Only get the books with 150+ observations every year from 2018-2022
  ml_data = ml_data %>%
    filter(season > (start_year - 1)) %>%
    group_by(season, parent_name) %>%
    filter(n() >= n) %>%
    ungroup(season) %>%
    filter(all(start_year:end_year %in% season)) %>%
    ungroup()
  
  # Only get games that have a line for each observation
  
  ml_data = ml_data %>%
    group_by(id) %>%
    filter(all(ml_data$parent_name %>% unique() %in% parent_name)) %>%
    ungroup()
  
  return(ml_data)
}
