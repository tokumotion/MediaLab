library(fbRads); library(httr); library(dplyr); library(tidyr)
library(magrittr); library(lubridate); library(jsonlite)

load("~/GitHub/Prueba/fb_token")

token <- fb_token$credentials$access_token
acct <- "101146170039639"

fbad_init(acct, token, version = fb_api_version())

fblista <- fbad_list_campaign(fields = c("id", "name", "objective", 
                                         "start_time", "stop_time"))
fblista[which(grepl("caral", fblista$name, ignore.case = TRUE) &
                fblista$start_time >= "2016-01-30"),]
fblista[,c(4:5)] <- lapply(fblista[,c(4:5)], ymd_hms)

fblista %<>%
  filter(grepl("caral", fblista$name, ignore.case = TRUE),
         start_time >= "2016-01-30")

# POST_ENGAGEMENT identical to WEBSITE_CLICKS

POST_ENGAGEMENT <- filter(fblista, objective == "POST_ENGAGEMENT")

?fb_insights

test_2 <- lapply(fblista$id,
       function(id) fb_insights(target = id, 
                                level = "adgroup",
                                fields = toJSON(c("campaign_group_name",
                                                  "campaign_name", "spend",
                                                  "frequency",
                                                  "impressions","reach",
                                                  "ctr", "cpm", "cpc",
                                                  "action_values",
                                                  "cost_per_unique_action_type",
                                                  "unique_actions")),
                                job_type = "async"))

test_2 <- do.call("rbind", lapply(test_2, data.frame))

# Sólo funciona cuando el número de filas en las listas son identicas

# Otra forma de abrir el JSON con un for loop (SACRILEGIO)

x <- data.frame()
for(i in 1:nrow(test_2)) {
  w = data.frame(rep(test_2[i, !sapply(test_2, is.list)], 
               sapply(test_2$cost_per_unique_action_type[i], nrow),
               length.out = length(test_2[i, !sapply(test_2, is.list)])), 
           lapply(test_2[i, sapply(test_2, is.list)], data.frame))
  x = rbind(x, w)
  rm(w)
}

write.table(x, "~/GitHub/Prueba/tabla_facebook.csv", sep = ",", row.names = F)

# Esta es otra alternativa (más eficiente)
test_2 <- tidyr::unnest(test_2)

# Cuando tenemos que jalar listas de las listas (tenemos que hacer un loop
# para que jale todas las columnas del data frame)
do.call("rbind",mapply(cbind, 
                       a = test_2[sapply(test_2, is.list)],
                       test_2$unique_actions, 
                       test_2$cost_per_unique_action_type,
                       SIMPLIFY = FALSE, USE.NAMES = FALSE))

do.call("rbind", lapply(test_2$unique_actions, data.frame))

names(test)

names(test) <- c("Campaña", "Objetivo", "Impresiones", "Clicks", 
                 "Inversión", "Clicks a la web", "Alcance", "CTR",
                 "CTR Unico", "CPC", "CPM")

write.csv(test, "fb_caral.csv", row.names = FALSE)
