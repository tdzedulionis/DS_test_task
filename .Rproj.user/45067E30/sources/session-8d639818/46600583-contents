# Finalise ----------------------------------------------------------------

wb <- createWorkbook()
for(model in nested_data$model_number){
  sheet_name <- paste0(model,
                       " - article no. ",
                       nested_data %>%
                         filter(model_number == model) %>%
                         pull(article_no) %>% .[[1]])
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name,
            predictions[[paste0("model_nr_",model, "_prediction")]])
}
saveWorkbook(wb, file = paste0("output/", "final_output_",Sys.Date(),".xlsx"), overwrite = TRUE)