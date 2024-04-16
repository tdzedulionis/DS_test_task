# Predicting -----------------------------------------------------------------------------------------------------

if (renew_models == T){
  
  predictions <- list()
  
  for (model in unique(nested_data$model_number)){
    
    # |---- Data ----
    
    data <- nested_data %>% filter(model_number == model)
    article_no <- data %>% select(article_no) %>% unnest()
    fxmat <- data %>% select(fxmat) %>% unnest()
    outliers <- data %>% select(outlier) %>% unnest()
    
    fxmat <- fxmat %>%
      mutate(across(where(~ all(. %in% c(0, 1))), as.factor)) %>%
      select(zg, where(~is.numeric(.) | is.factor(.) | is.character(.))) %>%
      select(-c("article_no", "model_nr")) %>%
      as.data.frame()
    
    # |---- Predictions ----
    df_fxmat <- select(fxmat, -zg)
    
    prediction <- predict(models[[paste0("model_nr_",model, "_BART")]], df_fxmat)^2
    prediction <- data.frame(zg = prediction,
                             date = data %>% select(fxmat) %>% unnest() %>% select(date))
    
    # Saving variables importance information
    var_importance <- investigate_var_importance(models[[paste0("model_nr_",model, "_BART")]],
                                                 plot = F, type = "trees")
    
    # Prediction intervals 
    pred_interval <- calc_prediction_intervals(models[[paste0("model_nr_",model, "_BART")]], df_fxmat) %>%
      .[["interval"]] %>%
      replace(., . < 0, 0 ) %>%
      data.frame() %>%
      mutate(date = data %>% select(fxmat) %>% unnest() %>% pull(date),
             pi_lower_bd = pi_lower_bd^2,
             pi_upper_bd = pi_upper_bd^2)
    
    predictions[paste0("model_nr_",model, "_prediction")] <- list(prediction)
    
    predictions[paste0("model_nr_",model, "_var_importance")] <- list(var_importance)
    
    predictions[paste0("model_nr_",model, "_pred_intervals")] <- list(pred_interval)
  }
  
  # Saving newest predictions as default
  save(predictions, file = paste0("logs/predictions.RData"))
} else load(predictions_path)


# Plotting ----------------------------------------------------------------

# Plot forecasted values
forecasted_plots <- lapply(unique(nested_data$model_number), function(x){
  
  actual_data <- get_plot_data(x) %>% select(zg, date) %>% mutate(type = "Actual")
  predicted_data <- predictions[[paste0("model_nr_",x, "_prediction")]] %>%
    mutate(type = "Forecasted")
  pred_intervals = predictions[[paste0("model_nr_",x, "_pred_intervals")]]
  
  plot_forecast(actual = actual_data,
                predicted = predicted_data,
                pred_interval = pred_intervals,
                model_nr = x)
  
})

grid.arrange(grobs = forecasted_plots, ncol = 2)

# Variable importance plots

for(model in unique(nested_data$model_number)){
  pdf(file = paste0("plots/model_",model,"_VAR_Importance.pdf"))
  plot_var_importance(model, bottom_margin = 12)
  dev.off()
}
