library(shinythemes)
library(flexdashboard)
library(shinyWidgets)
library(shiny)
library(plotly)
library(tidyverse)
library(tidymodels)
library(modeltime)
library(timetk)   
library(lubridate)
library(readr)
library(dplyr)
library(tidyr)
library(glmnet)
library(DT)
library(randomForest)
options(scipen=999)

data_daily  <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")

covid19_confirmed_global <- reactive({
  
  data_daily <- data_daily[data_daily$location == input$country,]
  time_series_covid19_confirmed_global <- data_daily %>% select(date, input$dimension) %>% drop_na(input$dimension)
  time_series_covid19_confirmed_global <- aggregate(time_series_covid19_confirmed_global[input$dimension], by=list(date=time_series_covid19_confirmed_global$date), FUN=sum)
  
  covid19_confirmed_global <- time_series_covid19_confirmed_global %>%
    select(date, input$dimension) %>%
    rename(value=input$dimension)
  
  covid19_confirmed_global$date <- as.Date(covid19_confirmed_global$date, format = "%Y-%m-%d")
  covid19_confirmed_global
})

covid19_confirmed_global_tbl <- reactive({
  
  covid19_confirmed_global_tbl <- covid19_confirmed_global()
  as.tibble(covid19_confirmed_global_tbl)
})

splits <- reactive({
  covid19_confirmed_global_tbl() %>%
    time_series_split(assess = paste(input$testing, "days"), cumulative = TRUE)
})

calibration_table <- reactive({
  
  model_fit_arima <- arima_reg() %>%
    set_engine("auto_arima") %>%
    fit(value ~ date, training(splits()))
  
  # model_fit_arima
  
  model_fit_prophet <- prophet_reg() %>%
    set_engine("prophet", yearly.seasonality = input$yearly_seasonality, weekly.seasonality = input$weekly_seasonality, daily.seasonality = input$daily_seasonality) %>%
    fit(value ~ date, training(splits()))
  
  # model_fit_prophet
  
  recipe_spec <- recipe(value ~ date, training(splits())) %>%
    step_timeseries_signature(date) %>%
    step_rm(contains("am.pm"), contains("hour"), contains("minute"),
            contains("second"), contains("xts")) %>%
    step_fourier(date, period = 365, K = 5) %>%
    step_dummy(all_nominal())
  
  # recipe_spec %>% prep() %>% juice()
  
  model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
    set_engine("glmnet")
  
  workflow_fit_glmnet <- workflow() %>%
    add_model(model_spec_glmnet) %>%
    add_recipe(recipe_spec %>% step_rm(date)) %>%
    fit(training(splits()))
  
  model_spec_rf <- rand_forest(trees = input$random_forest_trees, min_n = input$random_forest_min_n) %>%
    set_engine("randomForest")
  
  workflow_fit_rf <- workflow() %>%
    add_model(model_spec_rf) %>%
    add_recipe(recipe_spec %>% step_rm(date)) %>%
    fit(training(splits()))
  
  model_spec_prophet_boost <- prophet_boost() %>%
    set_engine("prophet_xgboost", yearly.seasonality = input$yearly_seasonality, weekly.seasonality = input$weekly_seasonality, daily.seasonality = input$daily_seasonality) 
  
  workflow_fit_prophet_boost <- workflow() %>%
    add_model(model_spec_prophet_boost) %>%
    add_recipe(recipe_spec) %>%
    fit(training(splits()))
  
  # workflow_fit_prophet_boost
  
  model_table <- modeltime_table(
    model_fit_arima, 
    model_fit_prophet,
    workflow_fit_glmnet,
    workflow_fit_rf,
    workflow_fit_prophet_boost
  ) 
  
  # model_table
  
  calibration_table <- model_table %>%
    modeltime_calibrate(testing(splits()))
  
  calibration_table
})