# EDA ---------------------------------------------------------------------
# Histograms
hist_plots <- lapply(unique(nested_data$model_number), function(x){
  
  plot_data <- get_plot_data(x)
  
  ggplot(data = plot_data, aes(x = zg)) +
    geom_histogram(fill = "#336699", color = "white", bins = 20) +
    labs(
      title = paste("Distribution of ZG - Article no:", unique(plot_data$article_no)),
      x = "",
      y = ""
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10)
    )
  
})

# |--------
# Analyzing temporal trends
trend_plots <- lapply(unique(nested_data$model_number), function(x){
  
  plot_data <- get_plot_data(x)
  
  outliers <- nested_data %>%
    filter(model_number == x) %>%
    .[["outlier"]] %>%
    .[[1]] %>%
    mutate(date = as.Date(date)) %>%
    na.omit()
  
  ggplot(plot_data, aes(x = date, y = zg)) +
    geom_line(color = "#336699") +
    geom_point(data = outliers, aes(color = "Holiday"),color = "red", size = 3) +  # Adding points for holidays
    labs(
      title = paste("Time Series of ZG - Article no:", unique(plot_data$article_no)),
      x = "Date",
      y = "ZG"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()
    )
  
})

# |--------

# Check how `zg` is affected by weekday
weekday_plots <- lapply(unique(nested_data$model_number), function(x){
  
  plot_data <- get_plot_data(x)
  
  ggplot(plot_data, aes(x = factor(weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), y = zg)) +
    geom_boxplot(fill = "#336699", color = "#999999", alpha = 0.7) +
    labs(
      title = paste("Distribution of ZG by Weekday -", unique(plot_data$article_no)),
      x = "",
      y = ""
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      legend.position = "none"
    )
})

# |--------

# Check how promo combinations affect average of 'ZG' for each of article no
promo_plots <- lapply(unique(nested_data$model_number), function(x){
  
  plot_data <- get_plot_data(x) %>%
    mutate(date = as.Date(date), 
           # Create a new column that represents the combination of all promos
           promo_combo = paste0("Promo 1: ", promo_1, 
                                ", Promo 2: ", promo_2,
                                ", Promo 3: ", promo_3,
                                ", Promo 4: ", promo_4)) %>%
    group_by(promo_combo) %>%
    summarize(mean_zg = mean(zg)) %>%
    arrange(mean_zg)
  
  ggplot(plot_data, aes(x = reorder(promo_combo, -mean_zg), y = mean_zg)) +
    geom_bar(stat = "identity", fill = "#336699", width = 0.7) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    ) +
    labs(
      x = "", 
      y = "",
      title = paste("Mean `ZG` by Promo Combination (Bar Chart) -", 
                    unlist(nested_data[nested_data$model_number == x, "article_no"]))
    )
  
})

# |--------
promo_holiday_plots <- lapply(unique(nested_data$model_number), function(x){
  
  plot_data <- get_plot_data(x)
  
  # Create a dataframe
  df <- data.frame(
    date = plot_data$date,
    sales = plot_data$zg,
    promo = rowSums(plot_data[, c("promo_1", "promo_2", "promo_3", "promo_4")]),
    holiday = plot_data$is_holiday
  )
  
  ggplot(data = df,
         aes(x = date, y = sales, color = factor(promo), group = 1)) + 
    geom_line(size = 1) +
    geom_point(data = subset(df, holiday != "0"), aes(color = "Holiday"),color = "red", size = 3) +  # Adding points for holidays
    theme_bw() + 
    labs(title = paste0("Impact of Promos and holidays on 'ZG` - article no. ",
                        unlist(nested_data[nested_data$model_number == x, "article_no"])),
         x = "",
         y = "",
         color = "How many promos are active") +
    theme(legend.position = "bottom", 
          plot.title = element_text(hjust = 0.5))
  
})

# Plot --------------------------------------------------------------------

grid.arrange(grobs = hist_plots, ncol = 2)
grid.arrange(grobs = trend_plots, ncol = 2)
grid.arrange(grobs = weekday_plots, ncol = 2)
grid.arrange(grobs = promo_plots, ncol = 2)
grid.arrange(grobs = promo_holiday_plots, ncol = 2)

# Save plots
pdf("plots/hist_plots.pdf", width=12, height=5)
grid.arrange(grobs = hist_plots, ncol = 2)
dev.off()

pdf("plots/trend_plots.pdf", width=14, height=5)
grid.arrange(grobs = trend_plots, ncol = 2)
dev.off()

pdf("plots/weekday_plots.pdf", width=12, height=5)
grid.arrange(grobs = weekday_plots, ncol = 2)
dev.off()

pdf("plots/promo_plots.pdf", width=15, height=20)
grid.arrange(grobs = promo_plots, nrows = 4, ncol = 1)
dev.off()

pdf("plots/promo_plots(1).pdf", width=10, height=15)
grid.arrange(grobs = promo_plots[1:2], nrows = 2, ncol = 1)
dev.off()

pdf("plots/promo_plots(2).pdf", width=10, height=15)
grid.arrange(grobs = promo_plots[3:4], nrows = 2, ncol = 1)
dev.off()

pdf("plots/promo_holiday_plots.pdf", width=17, height=8)
grid.arrange(grobs = promo_holiday_plots, ncol = 2)
dev.off()
