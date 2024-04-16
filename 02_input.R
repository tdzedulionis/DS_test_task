# Input -------------------------------------------------------------------

# |---- Read data ----
# Main data
data <- readxl::read_excel(data_path, 
                           sheet = "data")

# Holidays in Lithuania
holidays <- holidays(
  countries = "Lithuania",
  years = unique(year(as.Date(data$date))),
  quiet = FALSE,
  include_regions = FALSE
) %>%
  select(holiday, holiday_name, holiday_type) %>%
  mutate(is_holiday = 
           case_when(
             holiday_type == "National holiday" ~ 2,
             holiday_type == "Observance" ~ 1,
             TRUE ~ 0
           )) %>%
  select(holiday, is_holiday) %>%
  group_by(holiday) %>%
  # If there are 2 holidays on same day, filter more important one
  filter(is_holiday == max(is_holiday)) %>%
  ungroup()

# Modify the data
data <- data %>%
  mutate(
    date = as.Date(date),
    # Assign a model number for each of article_no
    model_nr = match(article_no, unique(article_no)),
    # Add column for weekdays 
    weekday = weekdays(date)) %>%
  # Add holidays
  left_join(holidays, by = c("date" = "holiday")) %>%
  mutate(is_holiday = as.factor(ifelse(is.na(is_holiday), 0, is_holiday))) %>%
  mutate(month = as.factor(month(date)))

# |---- Create nested data ----

nested_data <- map_df(unique(data$model_nr), function(x){
  
  # Number of article
  article_no <- data %>%
    filter(model_nr == x) %>%
    pull(article_no) %>%
    unique()
  
  # Known data (will be used for modelling)
  xmat <- data %>% 
    filter(model_nr == x & !is.na(zg))
  
  # Unknown data (needs to be modelled)
  fxmat <- data %>% 
    filter(model_nr == x & is.na(zg))
  
  # All values of zg
  zg_values <- data %>% 
    filter(model_nr == x) %>%
    select(zg, date)
  
  # Detect and mark outliers
  outlier <- zg_values %>%
    outliers()
  
  # Descriptive statistics
  desc_stats <- zg_values %>%
    pull(zg) %>%
    summary
  
  
  return(tibble(
    model_number = x,
    article_no = article_no %>% list(),
    zg_values = zg_values %>% list(),
    xmat = xmat %>% list(),
    fxmat = fxmat %>% list(),
    outlier = outlier %>% list(),
    desc_stats = desc_stats %>% list()
  ))
}
)
