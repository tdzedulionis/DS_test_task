# Functions ---------------------------------------------------------------

# Identify outliers using IQR method
outliers <- function(x) {
  col <- which(sapply(x, is.numeric))
  q <- quantile(x[,col], c(0.25, 0.75), na.rm = T)
  iqr <- q[2] - q[1]
  lower <- q[1] - 1.5 * iqr
  upper <- q[2] + 1.5 * iqr
  x <- x[x[,col] < lower | x[,col] > upper,]
  return(x)
}

# Extract data for plotting
get_plot_data <- function(x){
  
  df <- nested_data %>%
    filter(model_number == x) %>%
    .[["xmat"]] %>%
    .[[1]] %>%
    mutate(date = as.Date(date))
  
  return(df)
  
}

# Plot forecasts

plot_forecast <- function(predicted, actual, pred_interval, model_nr){
  
  df_combined <- rbind(
    actual, 
    predicted
  ) %>%
    left_join(pred_interval)
  
  # Plotting
  ggplot(df_combined, aes(x = date, y = zg, color = type, linetype = type)) +
    geom_line() +
    labs(title = paste0("Original vs. Forecasted Values \n Model nr. ", model_nr),
         x = "Date",
         y = "ZG") +
    geom_ribbon(aes(x = date, ymin = pi_lower_bd, ymax = pi_upper_bd),
                color = NA, fill = "#888888", alpha = 0.5, show.legend = F) +
    geom_line(data = df_combined[df_combined$type == "Forecasted", ], aes(x = date, y = zg), 
              color = "#D55E00", linetype = "solid") +  # Adjusted line style for forecasted values
    theme_minimal() +
    theme(legend.position = "top",
          plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
          axis.title = element_text(face = "bold", size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size = 12)) +
    scale_color_manual(values = c("Actual" = "#0072B2", "Forecasted" = "#D55E00")) +
    scale_linetype_manual(values = c("Actual" = "solid", "Forecasted" = "solid")) 
  
  
}

# BART Plots for variable importance
plot_var_importance <- function(x, bottom_margin){
  
  x_title <- paste0("Model - ", x)
  x <- predictions[[paste0("model_nr_",model, "_var_importance")]]
  num_replicates_for_avg = 5
  x$avg_var_props <- x$avg_var_props[which(x$avg_var_props > 0)]
  x$sd_var_props <- x$sd_var_props[which(x$sd_var_props > 0)]
  
  par(mar = c(bottom_margin, 6, 3, 0))
  if (is.na(x$sd_var_props[1])) {
    moe = 0
  }
  else {
    moe = 1.96 * x$sd_var_props/sqrt(num_replicates_for_avg)
  }
  bars = barplot(x$avg_var_props, names.arg = names(x$avg_var_props), 
                 las = 2, ylab = "Inclusion Proportion", col = "gray", 
                 ylim = c(0, max(x$avg_var_props + moe)))
  conf_upper = x$avg_var_props + 1.96 * x$sd_var_props/sqrt(num_replicates_for_avg)
  conf_lower = x$avg_var_props - 1.96 * x$sd_var_props/sqrt(num_replicates_for_avg)
  segments(bars, x$avg_var_props, bars, conf_upper, col = rgb(0.59, 
                                                              0.39, 0.39), lwd = 3)
  segments(bars, x$avg_var_props, bars, conf_lower, col = rgb(0.59, 
                                                              0.39, 0.39), lwd = 3)
  par(mar = c(5.1, 4.1, 4.1, 2.1))
  title(main = paste0(x_title, "\nVariable importance graph"))
}
