# BART Modelling ----------------------------------------------------------

# Empty list to store models
models <- list()

if(renew_models){
  
  for (model in unique(nested_data$model_number)){
    
    # |---- Data ----
    
    data <- nested_data %>% filter(model_number == model)
    article_no <- data %>% select(article_no) %>% unnest()
    zg <- data %>% select(zg_values) %>% unnest()
    xmat <- data %>% select(xmat) %>% unnest()
    outliers <- data %>% select(outlier) %>% unnest()
    
    # Dependant and exogenous variables that will be used for modelling
    xmat <- xmat %>%
      mutate(across(where(~ all(. %in% c(0, 1))), as.factor)) %>%
      # select(zg, where(~!any(is.na(.)) & is.numeric(.) | is.factor(.) | is.character(.))) %>%
      select(zg, where(~is.numeric(.) | is.factor(.) | is.character(.))) %>%
      select(-c("article_no", "model_nr")) %>%
      as.data.frame()
    
    # |---- BART modelling ----
    y_xmat <- xmat %>% pull(zg)
    df_xmat <- xmat %>% select(-zg)
    
    # Model
    set_bart_machine_num_cores(4)
    set.seed(seed)
    
    # Search for optimal hyperparameters (Time consuming, Re-Run only if data changes)
    # Square-Root transformation for dependent variable
    bart_machine_cv <- bartMachineCV(df_xmat, 
                                     sqrt(y_xmat),
                                     use_missing_data = TRUE,
                                     seed = seed, 
                                     k_cvs = c(1, 2, 3),
                                     k_folds = 10,
                                     num_tree_cvs = c(seq(50, 200, by = 25)), 
                                     num_iterations_after_burn_in=2000, 
                                     num_burn_in = 300, 
                                     serialize = TRUE,
                                     nu_q_cvs = list(c(3, 0.9), c(3, 0.99), c(10, 0.75)))
    summary(bart_machine_cv)
    
    plot_convergence_diagnostics(bart_machine_cv)
    check_bart_error_assumptions(bart_machine_cv)
    investigate_var_importance(bart_machine_cv, num_replicates_for_avg = 20)
    plot_y_vs_yhat(bart_machine_cv, prediction_intervals = TRUE)
    
    # Saving model
    models[paste0("model_nr_",model, "_BART")] <- list(bart_machine_cv) 
    gc()
    
  } 
  
  # Saving models to log folder
  save(models, file = paste0("logs/models.RData"))
  
} else load(BART_models)