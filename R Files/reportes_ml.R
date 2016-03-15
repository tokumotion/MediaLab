library(dplyr); library(tidyr)

reports <- function(network, brand, start_date, stop_date){
  source("~/Github/Prueba/R Files/functions_ml.R")
  switch(network,
         facebook = facebook(brand, start_date, stop_date),
         analytics = analytics(brand, start_date, stop_date),
         adwords = adwords(brand, start_date, stop_date))
}