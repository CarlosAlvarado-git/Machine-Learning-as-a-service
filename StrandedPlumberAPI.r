library(plumber)
library(caret)
library(jsonlite)
library(ipred)
library(e1071)
library(yaml)
library(dplyr)
library(tidyr)
library(lubridate)

#Generador de Logs
logge <- function(req, res){
  # boole <- length(req$args)
  d <- Sys.time()
  y <-list('usuario' = Sys.getenv("USERNAME"),
           'end_point' = req$PATH_INFO,
           'user_agent'=req$HTTP_USER_AGENT,
           'time' = d, 
           'payload'=req$body, 
           'output' = res$body
  )
  archivo <- toJSON(y, force = TRUE)
  
  wd <- getwd()
  
  dir <- paste0(wd,"/logs","/year=", year(d), "/month=", month(d), "/day=", day(d))
  
  dir.create(dir, recursive = TRUE)
  
  write(archivo, file = paste0(dir,"/",as.integer(d),".json"), append = TRUE)
}

# Utilise post method to send JSON unseen data, in the same 
# format as our dataset

#--------------------------------------------------
# Read in model 
#--------------------------------------------------
model <- readr::read_rds("tb_model.rds")
model$modelInfo

#* Predict whether a patient is a stranded patient
#* @post /predict

function(req, res){
  
  resultado <- data.frame(predict(model, newdata = as.data.frame(req$body), type="prob"))
  res$body <- resultado
  logge(req,res)
  resultado
}

#* Ruta de test
#* @post /test_data
function(req, res){
  test_data_file <- as.data.frame(req$body$file$parsed)
  #browser()
  test_data_file <- test_data_file %>% mutate(stranded_class = factor(stranded_class)) %>% drop_na()
  predict <- predict(model, test_data_file, type="raw")
  predict_probs <- predict(model, test_data_file, type="prob")
  predictions <- cbind(predict, predict_probs)
  db_append_table <- ConfusionTableR::binary_class_cm(predictions$predict, test_data_file[, names(test_data_file) %in% c("stranded_class")])
  
  data_csv_test <- db_append_table$record_level_cm %>% 
    select('Pred_Not.Stranded_Ref_Not.Stranded', 'Pred_Stranded_Ref_Not.Stranded', 'Pred_Not.Stranded_Ref_Stranded', 'Pred_Stranded_Ref_Stranded',
           'Balanced.Accuracy', 'Accuracy', 'Precision', 'Recall', 'Specificity')
  
  res$body <- data_csv_test
  logge(req,res)
  data_csv_test
}


#* @plumber
function(pr){
  pr %>% 
    pr_set_api_spec(yaml::read_yaml("openapi.yaml"))
}
