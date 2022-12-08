private = new.env()

private$data_train = NULL
private$data_test = NULL
private$data_test_long = NULL
private$data_train_long = NULL
private$ml_data = NULL
private$test_year = NULL
private$exclusion_year = NULL
private$brier_train = NULL

private$model_books = NULL
private$n_book_train_data = NULL

constructor = function(ml_data, test_year, exclusion_year= NULL) {
  private$ml_data = ml_data
  private$test_year = test_year
  private$exclusion_year = exclusion_year
  
  private$set_data()
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

fit_top_n_books_model = function(n, weighted_average, test = FALSE) {
  
  if (class(weighted_average) != "logical") {
    stop("Weighted average needs to be TRUE or FALSE")
  }
  
  data_train_long = private$data_train_long
  brier_train = private$brier_train
  
  if (n == "all") {
    n = brier_train %>%
      nrow()
  } else if (n == "above average") {
    n = brier_train %>%
      filter(brier_skill_score > 0) %>%
      nrow()
  }
  
  brier_train = brier_train %>%
    arrange(desc(brier_skill_score)) %>%
    head(n) %>%
    select(c(book_id, brier_skill_score))

  model_books = brier_train$book_id
  
  if (test) {
    data_train_long = private$data_test_long
  }
  
  if (weighted_average) {
    brier_ss = brier_train$brier_skill_score
    column_names = colnames(data_train_long)
    
    model_books_data = data_train_long[,which(column_names %in% model_books)]
    weighted_average = data.matrix(model_books_data) %*% brier_ss / sum(brier_ss)
    
    data_train_long$.pred_TRUE = weighted_average
    name = paste("Weighted Average of Top", n, "Books")
  } else {
    data_train_long$.pred_TRUE = rowMeans(data_train_long[,which(colnames(data_train_long) %in% model_books)])
    name = paste("Average of Top", n, "Books")
  }
  
  
  # data_train_long = data_train_long %>%
    # mutate(.pred_win = make_two_class_pred(1 - .pred_TRUE, levels(as.factor(home_team_win)), threshold = .5))
  
  return(
    structure(
      list(
        model_books, 
        data_train_long, 
        private$calc_metrics(data_train_long, name), 
        NULL),
      .Names = c("books", name, "metrics", "coefficients")
      )
    )
}

fit_logistic_model = function(n) {
  set.seed(123)
  private$set_top_n_books(n)
  
  model_books = private$model_books
  n = length(model_books)
  
  logreg_train = private$n_book_train_data %>%
    mutate(home_team_win = as.factor(home_team_win))
  
  data_cv10 <- vfold_cv(logreg_train, v = 10)
  
  logistic_spec = logistic_reg() %>%
    set_engine("glm") %>%
    set_mode("classification")
  
  logistic_rec = recipe(home_team_win ~ ., data = logreg_train) %>%
    step_normalize(all_numeric_predictors())
  
  log_wf = workflow() %>%
    add_recipe(logistic_rec) %>%
    add_model(logistic_spec)
  
  log_fit <- fit(log_wf, data = logreg_train)
  
  logistic_output = logreg_train %>%
    bind_cols(predict(log_fit, new_data = logreg_train, type = 'prob'))
  
  logistic_output = logistic_output %>%
    mutate(.pred_win = make_two_class_pred(.pred_FALSE, levels(as.factor(home_team_win)), threshold = .5))
  
  name = paste("Logistic Regression with Top", n, "Books")
  
  log_cv_fit = fit_resamples(
    log_wf, 
    resamples = data_cv10,
    metrics = metric_set(sens, yardstick::spec, accuracy, brier_score),
    control = control_resamples(save_pred = TRUE, event_level = 'second'))
  
  return(
    structure(
      list(
        model_books, 
        logistic_output, 
        collect_metrics(log_cv_fit) %>%
          mutate(metric = .metric, value = mean, model_name = name) %>%
          select(metric, value, model_name), 
        log_fit),
      .Names = c("books", name, "metrics", "coefficients")
    )
  )
}

fit_reg_log_model = function(lasso) {
  set.seed(123)

  private$set_top_n_books("all")
  
  model_books = private$model_books

  logreg_train = private$n_book_train_data %>%
    mutate(home_team_win = as.factor(home_team_win))
  
  logreg_cv10 <- vfold_cv(logreg_train, v = 10)
  
  logistic_lasso_spec_tune <- logistic_reg() %>%
    set_engine('glmnet') %>%
    set_args(mixture = as.numeric(lasso), penalty = tune()) %>%
    set_mode("classification")
  
  logistic_rec <- recipe(home_team_win ~ ., data = logreg_train) %>%
    step_normalize(all_numeric_predictors())
  
  log_lasso_wf = workflow() %>%
    add_recipe(logistic_rec) %>%
    add_model(logistic_lasso_spec_tune)
  
  penalty_grid = grid_regular(
    penalty(range = c(-3, 1)), #log10 transformed 
    levels = 30)
  
  tune_output <- tune_grid( 
    log_lasso_wf, # workflow
    resamples = logreg_cv10, # cv folds
    metrics = metric_set(brier_score),
    control = control_resamples(save_pred = TRUE, event_level = 'second'),
    grid = penalty_grid # penalty grid defined above
  )
  
  best_se_penalty = select_by_one_std_err(tune_output, metric = 'brier_score', desc(penalty))
  
  final_fit_se <- finalize_workflow(log_lasso_wf, best_se_penalty) %>% # incorporates penalty value to workflow 
    fit(data = logreg_train)
  
  final_fit_se %>% tidy()
  
  log_lasso_output = logreg_train %>%
    bind_cols(predict(final_fit_se, new_data = logreg_train, type = 'prob'))
  
  log_lasso_output = log_lasso_output %>%
    mutate(.pred_win = make_two_class_pred(.pred_FALSE, levels(as.factor(home_team_win)), threshold = .5))
  
  name = if_else(lasso, "Lasso", "Ridge")
  
  return(
    structure(
      list(
        model_books, 
        log_lasso_output, 
        private$calc_metrics(log_lasso_output, name), 
        final_fit_se),
      .Names = c("books", name, "metrics", "coefficients")
    )
  )
  
}

private$calc_metrics = function(output, model_name) {
  
  brier_score = (as.logical(output$home_team_win) - output$.pred_TRUE) ^ 2 %>% mean()
  
  metric = c("brier_score")
  value = c( brier_score)
  
  model_name = rep(model_name, 8)
  
  return(as.data.frame(cbind(metric, value, model_name)))
  
}

private$old_calc_metrics = function(output, model_name) {
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
  
  metric = c("true_negative", "false_negative", "true_positive", "false_positive", "sens", "spec", "accuracy", "brier_score")
  value = c(true_negative, false_negative, true_positive, false_positive, sens, spec, accuracy, brier_score)
  
  model_name = rep(model_name, 8)
  
  return(as.data.frame(cbind(metric, value, model_name)))
  
}

private$set_top_n_books = function(n) {
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
  
  train = private$data_train_long %>%
  mutate(home_team_win = as.factor(home_team_win)) %>%
  select(-c(id, season, league))
  
  column_names = colnames(train)
  home_team_win = train$home_team_win
  train = train[,which(column_names %in% model_books)]
  train$home_team_win = home_team_win
  
  private$model_books = model_books
  private$n_book_train_data = train
}

brier_score_vec <- function(truth, 
                            estimate,
                            estimator = NULL,
                            event_level = "first",
                            na_rm = TRUE) {
  
  estimator <- finalize_estimator(truth, estimator, metric_class = "brier_score")
  
  brier_score_impl <- function(truth, estimate) {
    mean((truth - estimate) ^ 2)
  }
  
  metric_vec_template(
    metric_impl = brier_score_impl,
    truth = as.numeric(truth) - 1, 
    estimate = estimate,
    estimator = estimator,
    na_rm = na_rm,
    cls = "numeric"
  )
  
}


brier_score <- function(data, ...) {
  UseMethod("brier_score")
}

brier_score <- new_prob_metric(brier_score, direction = "minimize")

brier_score.data.frame <- function(data, truth, estimate, ..., estimator = NULL, na_rm = TRUE, event_level = "first") {
  
  metric_summarizer(
    metric_nm = "brier_score",
    metric_fn = brier_score_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate), 
    na_rm = na_rm,
    event_level = event_level
  )
  
}
