# R @4.1.2 is used

rm(list = ls())
options(java.parameters="-Xmx5000m")

# For reproducability, renv is used.
# renv::activate()
# renv::restore()

# Load libraries ----------------------------------------------------------

packages <- c("tidyverse",
              "gridExtra",
              "lares",
              "bartMachine",
              "openxlsx")

lapply(packages, function(x){
  if(!requireNamespace(x, quietly = TRUE)) install.packages(x); 
  suppressPackageStartupMessages(library(x, character.only = T))}
)

# Source ------------------------------------------------------------------

# functions
source("00_functions.R")

# set constants
source("01_set_constants.R")

# Input
source("02_input.R")

# Exploratory data analysis (EDA)
source("03_eda.R")

# Modelling
source("04_modelling.R")

# Predictions
source("05_predictions.R")

# Finalise
source("06_finalise.R")
