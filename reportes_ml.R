reports <- function(network, brand, start_date, stop_date){
  source("~/Github/Prueba/functions_ml.R")
  switch(network,
         facebook = facebook(brand, start_date, stop_date),
         analytics = analytics(brand, start_date, stop_date),
         adwords = adwords(brand, start_date, stop_date))
}

reports("analytics", "nissan", "2016-01-01", "2016-01-31")
