library(dplyr)
library(httr)
library(urltools)
library(tidyverse)
library(purrr)

get_book_id_data = function() {
  url = "https://api.actionnetwork.com/web/v1/books"
  user_agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/106.0.0.0 Safari/537.36"
  resp = GET(url, user_agent(user_agent))
  book_df = jsonlite::fromJSON(content(resp, "text"))$books
  book_df = book_df %>%
    unnest(meta)
  book_df = book_df %>% 
    select(-c(logos, website, deeplink, 
              is_fastbet_enabled_app, 
              is_fastbet_enabled_web, 
              is_supported, secondary_color))
  return(book_df)
}

BOOK_ID_DATA = get_book_id_data()


# Cleans the raw data frame
clean_df = function(df, week, season) {
  
  # select the rows we need
  df = df %>%
    select(id, start_time, away_team_id, home_team_id, winning_team_id, league_name, season, teams, boxscore, odds, teams)
  
  # add final scores
  df$home_points = df$boxscore$total_home_points
  df$away_points = df$boxscore$total_away_points
  
  # remove unnecessary rows
  df = df %>%
    select(-c(boxscore))
  
  # unnest the odds dataframe
  
  for (i in 1:length(df$odds)) {
    odds = df$odds[[i]]
    if ("meta" %in% colnames(odds)) {
      df$odds[[i]] = odds %>%
        select(-c(meta))
    }
  }
  
  df = df %>% unnest(c(odds))
  
  # remove unnecessary variables in the odds
  df = df %>%
    select(-c(ml_home_public, ml_away_public, spread_home_public, spread_away_public, total_under_public, total_over_public, ml_home_money, ml_away_money, 
              spread_home_money, total_over_money, total_under_money, num_bets, type, inserted, line_status))
  
  # add the week
  df = df %>%
    mutate(week = week,
           season = season)
  
  return(df)
}


# Checks the is_clv parameter for the get_football_df and get_other_sports df. 
# 
# Param url: url for ActionNetwork website
# Param is_clv: logical value indicating whether the url is for closing line value or just for the game
#
# Returns a url to be used that indic
check_is_clv = function(is_clv, url) {
  if (class(is_clv) == class(TRUE)) {
    if (!is_clv) {
      url = url %>% 
        param_set("period", "game")
    }
  } else {
    stop("is_clv should be a boolean")
  }
  
  return(url)
}



get_football_df = function(league, books, start_year, end_year,  is_clv) {
  user_agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/106.0.0.0 Safari/537.36"
  url = paste("https://api.actionnetwork.com/web/v1/scoreboard/", league, "?", sep = "")
  url = check_is_clv(is_clv, url)
  
  df = NULL  
  for (season in start_year:end_year) {
    
    for (week in 1:20) {
      temp_url = param_set(url, "week", week) %>% 
        param_set("season", season) %>% 
        param_set("bookIds", paste(books, collapse = ","))
      resp = GET(temp_url, user_agent(user_agent))
      resp_df = jsonlite::fromJSON(content(resp, "text"))$games
      if (length(resp_df) != 0 & "odds" %in% colnames(resp_df) & "boxscore" %in% colnames(resp_df)) {
        df = rbind(df, clean_df(resp_df, week, season))
        print(paste("finished week",week, season))
      }
    }
  }
  
  return(df)
}


format_date = function(date) {
  date = as.character(date)
  date = gsub("-", "", date)
  return(date)
}


get_days = function(league, start_year) {
  days = c()
  if (start_year == 2019) {
    if (league == "nba") {
      start_date = "10-22"
      end_date = "10-11"
      end_year = 2020
    } else if (league == "nhl") {
      start_date = "10-02"
      end_date = "09-30"
      end_year = 2020
    }
  } else if (league %in% c("nhl", "nba")) {
    start_date = "10-01"
    end_date = "07-01"
    end_year = start_year + 1
  }else if (league == "mlb"){
    start_date = "03-01"
    end_date = "11-10"
    end_year = start_year
  } else if (league == "ncaab") {
    start_date = "10-01"
    end_date = "04-01"
    end_year = start_year + 1
  }
  else {
    stop("Wrong league provided. Needs to be one of 'nhl', 'nba', 'ncaab', 'mlb'")
  }
  
  start_date = as.Date(paste(start_year, start_date, sep = "-"))
  end_date = as.Date(paste(end_year, end_date, sep = "-"))
  
  return(format_date(seq(start_date, end_date, by = "days")))
}


get_other_sport_df = function(league, books, start_year, end_year, is_clv) {
  url = paste("https://api.actionnetwork.com/web/v1/scoreboard/", league, "?", sep = "")
  url = check_is_clv(is_clv, url)
  user_agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/106.0.0.0 Safari/537.36"
  
  df = NULL
  
  for (season in start_year : end_year) {
    
    for (day in get_days(league, season)) {
      temp_url = param_set(url, "date", day) %>% param_set("bookIds", paste(books, collapse = ","))
      resp = try(GET(temp_url, user_agent(user_agent)))
      if (class(resp) != "try-error"){
        resp_df = jsonlite::fromJSON(content(resp, "text"))$games
        temp_df = try(clean_df(resp_df, day, season), silent = TRUE)
        if (class(temp_df) != "try-error") {
          df = rbind(df, temp_df)
        }
      }
      print(paste("finished day",day))
    }
    print(paste("finished season", season))
    
  }
  
  return(df)
}

check_params = function(leagues, books, start_year, end_year, is_clv) {
  LEAGUES = c("nfl", "ncaaf", "nhl", "mlb", "ncaab", "nba")
  BOOKS = BOOK_ID_DATA$id
  
  for (book in books) {
    if(!(book %in% BOOKS)) {
      stop("book_id is not valid")
    }
  }
  
  for (league in leagues) {
    if (!(league %in% leagues)) {
      stop(paste("league param needs to be one of ", LEAGUES, collapse = ", "))
    }
  }
  
  if (start_year > 2022 | end_year > 2022) {
    stop("Cannot get data past 2022")
  }
  
  if (class(is_clv) != class(TRUE)) {
    stop("is_clv must be of class logical")
  }
}

get_betting_data = function(leagues, books, start_year, end_year, is_clv) {
  if (tolower(leagues) == "all"){
    leagues = c("nfl", "ncaaf", "nhl", "mlb", "ncaab", "nba")
  }
  
  if (tolower(books) == "all") {
    books = BOOK_ID_DATA$id
  }
  check_params(leagues, books, start_year, end_year, is_clv)
  
  df = NULL
  for (league in leagues) {
    print("-----------------------------------------------")
    print(paste("STARTING ", league))
    print("-----------------------------------------------")
    
    if (league %in% c("ncaaf", "nfl")) {
      temp_df = get_football_df(league, books, start_year, end_year, is_clv)
    } else {
      temp_df = get_other_sport_df(league, books, start_year, end_year, is_clv)
    }
    df = rbind(df, temp_df)
  }
  
  return(df)
}
