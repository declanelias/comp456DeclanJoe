private = new.env()

private$data_train = NULL
private$data_test = NULL
private$data_test_long = NULL
private$data_train_long = NULL
private$ml_data = NULL
private$test_year = NULL
private$exclusion_year = NULL
private$brier_train = NULL

constructor = function(ml_data, test_year, exclusion_year= NULL) {
  private$ml_data = ml_data
  private$test_year = test_year
  private$exclusion_year = exclusion_year
  
  private$clean_data()
  private$set_data()
}

private$clean_data = function() {
  ml_data = private$ml_data
  ml_data = ml_data %>%
    filter(!is.na(parent_name))
  
  # Remove consensus and open
  ml_data = ml_data %>%
    filter(!(parent_name %in% c("Consensus", "Open")))
  
  
  # Only get the books with 150+ observations every year from 2018-2022
  ml_data = ml_data %>%
    filter(season > 2018) %>%
    group_by(season, parent_name) %>%
    filter(n() >= 150) %>%
    ungroup(season) %>%
    filter(all(2019:2022 %in% season)) %>%
    ungroup()
  
  # Only get games that have a line for each observation
  
  ml_data = ml_data %>%
    group_by(id) %>%
    filter(all(ml_data$parent_name %>% unique() %in% parent_name)) %>%
    ungroup()
  
  private$ml_data = ml_data
}

private$set_data = function() {
  ml_data = private$ml_data
  test_year = private$test_year
  exclusion_year = private$exclusion_year
  
  data_test = ml_data %>%
    filter(season == test_year)
  
  if (is_null(exclusion_year)) {
    data_train = ml_data %>%
      filter(season != test_year)
  } else {
    data_train = ml_data %>%
      filter(season != test_year,
             season != exclusion_year)
  }
  
  private$data_train = data_train
  private$data_train_long = private$data_train %>%
    select(-c(brier_score, away_prob_fair, home_prob_vig, 
              away_prob_vig, ml_home, ml_away, parent_name)) %>%
    pivot_wider(names_from = book_id, values_from = home_prob_fair)
  
  private$data_test = data_test
  private$data_test_long = private$data_test %>%
    select(-c(brier_score, away_prob_fair, home_prob_vig, 
              away_prob_vig, ml_home, ml_away, parent_name)) %>%
    pivot_wider(names_from = book_id, values_from = home_prob_fair)
  
  private$brier_train = private$get_brier_score(data_train)
  
  lockBinding(c("data_train", "data_test", 
                "data_test_long", "ml_data", 
                "test_year", "exclusion_year"), 
              private)
}

private$get_brier_score = function(df) {
  brier_data = df %>%
    group_by(book_id) %>%
    summarise(brier_score = mean(brier_score),
              n = n())
  
  brier_ref = (brier_data %>%
                 filter(book_id != 5000) %>%
                 summarise(brier_score = mean(brier_score)))$brier_score
  
  get_brier_ss = function(brier_score) {
    return(1 -  brier_score / brier_ref)
  }
  
  
  brier_data = brier_data %>%
    group_by(book_id) %>%
    mutate(brier_skill_score = get_brier_ss(brier_score)) %>% 
    ungroup()
  
  return(brier_data)
}

fit_top_n_books_model = function(n, weighted_average) {
  
  if (class(weighted_average) != "logical") {
    stop("Weighted average needs to be TRUE or FALSE")
  }
  
  data_train = private$data_train
  data_test_long = private$data_test_long
  brier_train = private$brier_train
  
  if (n == tolower("all") | n > nrow(brier_train)) {
    n = brier_train %>%
      filter(brier_skill_score > 0) %>%
      nrow()
  }
  
  brier_train = brier_train %>%
    arrange(desc(brier_skill_score)) %>%
    head(n) %>%
    select(c(book_id, brier_skill_score))

  model_books = brier_train$book_id
  
  if (weighted_average) {
    brier_ss = brier_train$brier_skill_score
    column_names = colnames(data_test_long)
    
    model_books_data = data_test_long[,which(column_names %in% model_books)]
    weighted_average = data.matrix(model_books_data) %*% brier_ss / sum(brier_ss)
    
    data_test_long$.pred_TRUE = weighted_average
    name = paste("Weighted Average of Top", n, "Books")
  } else {
    data_test_long$.pred_TRUE = rowMeans(data_test_long[,which(colnames(data_test_long) %in% model_books)])
    name = paste("Average of Top", n, "Books")
  }
  
  data_test_long = data_test_long %>%
    mutate(.pred_win = make_two_class_pred(1 - .pred_TRUE, levels(as.factor(home_team_win)), threshold = .5))
  
  return(
    structure(
      list(
        model_books, 
        data_test_long, 
        private$calc_metrics(data_test_long, name), 
        NULL),
      .Names = c("books", name, "metrics", "coefficients")
      )
    )
}

fit_logistic_model = function(n) {
  brier_train = private$brier_train
  
  if (n == tolower("above average")) {
    n = brier_train %>%
      filter(brier_skill_score > 0) %>%
      nrow()
    
    model_books = brier_train %>%
      filter(brier_skill_score > 0) %>%
      getElement("book_id")
    
  } else if (n == tolower("all") | n > nrow(brier_train)) {
    n = brier_train %>%
      nrow()
    
    model_books = brier_train %>%
      getElement("book_id")
    
  } else {
    model_books = brier_train %>%
      arrange(desc(brier_skill_score)) %>%
      head(n) %>%
      getElement("book_id")
  }
  
  logreg_train = private$data_train_long %>%
    mutate(home_team_win = as.factor(home_team_win)) %>%
    select(-c(id, season, league))
  
  column_names = colnames(logreg_train)
  home_team_win = logreg_train$home_team_win
  logreg_train = logreg_train[,which(column_names %in% model_books)]
  logreg_train$home_team_win = home_team_win
  
  
  logreg_test = private$data_test_long %>%
    mutate(home_team_win = as.factor(home_team_win))
  
  logistic_spec = logistic_reg() %>%
    set_engine("glm") %>%
    set_mode("classification")
  
  logistic_rec = recipe(home_team_win ~ ., data = logreg_train)
  
  log_wf = workflow() %>%
    add_recipe(logistic_rec) %>%
    add_model(logistic_spec)
  
  log_fit = fit(log_wf, data = logreg_train)
  
  logistic_output = logreg_test %>%
    bind_cols(predict(log_fit, new_data = logreg_test, type = 'prob'))
  
  logistic_output = logistic_output %>%
    mutate(.pred_win = make_two_class_pred(.pred_FALSE, levels(home_team_win), threshold = .5))
  
  name = paste("Logistic Regression with Top", n, "Books")
  
  return(
    structure(
      list(
        model_books, 
        logistic_output, 
        private$calc_metrics(logistic_output, name), 
        log_fit %>% tidy()),
      .Names = c("books", name, "metrics", "coefficients")
    )
  )
}

private$calc_metrics = function(output, model_name) {
  confusion_matrix = output  %>%
    conf_mat(truth = home_team_win, estimate = .pred_win)
  
  true_negative = confusion_matrix$table[1]
  false_negative = confusion_matrix$table[3]
  true_positive = confusion_matrix$table[4]
  false_positive = confusion_matrix$table[2]
  
  sens = true_positive / (true_positive + false_positive)
  spec = true_negative / (true_negative + false_negative)
  accuracy = (true_positive + true_negative) / (true_positive + false_positive + true_negative + false_negative)
  
  brier_score = (as.logical(output$home_team_win) - output$.pred_TRUE) ^ 2 %>% mean()
  
  names = c("true_negative", "false_negative", "true_positive", "false_positive", "sens", "spec", "accuracy", "brier_score")
  values = c(true_negative, false_negative, true_positive, false_positive, sens, spec, accuracy, brier_score)
  
  model_name = rep(model_name, 8)
  
  return(as.data.frame(cbind(names, values, model_name)))
  
}


