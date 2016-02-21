# El objetivo de la función es descargar la data del tráfico
# generado desde Facebook a las webs de los clientes.
# Las variables que vamos a usar son: clicks, CTR,
# impresiones, costo por click y costo total.

# De ahora en adelante, debemos taggear cada página y cada
# link para poder cruzar la información con la de la bbdd.

facebook <- function(brand, start_date, stop_date) {
	require(RFacebook); require(fbRads)
	load("~/Documents/fb_token")

	# Cuenta de Business Manager de MediaLab
	acct <- "101146170039639"

	# Escogemos la cuenta que vamos a analizar
  switch(brand,
         caral = "838478606179463",
         nissan = "227015499643",
         transitemos = "374014132682491")

  # Concatenamos las fechas en el formato de Graph API
  dates <- paste("&since=", start_date, "&until=", stop_date,
  	sep = "")

  # Pedimos las interacciones por tipo y filtramos los clicks
  # a la web. Además pedimos las impresiones de la página para
  # unir ambas columnas y crear un data.frame. Finalmente,
  # creamos una columna final con el CTR.

  clicks <- getInsights(brand, 
  	token = fb_token, 
  	period = "day",
  	parms = dates, 
  	metric = "page_consumptions_by_consumption_type_unique")
  clicks <- sum(clicks$value[which(clicks$variable == "link clicks")])

  impressions <- getInsights(brand, 
  	token = fb_token, 
  	period = "day",
  	parms = dates, 
  	metric = "page_impressions_unique")
  impressions <- sum(impressions$value)

  # Vector con las impresiones del mes, clicks y CTR
  table <- cbind(impressions, clicks, CTR = clicks/impressions)

  # Activamos la cuenta de Business Manager
  fbad_init(acct, 
  	fb_token$credentials$access_token, 
  	version = fb_api_version())

  fblist <- fbad_list_campaign(fields = "id", "name", 
  	"objective", "start_time", "stop_time")

  # Filtro campañas relevantes
  fblist <- fblist[which(grepl(brand, fblist$name, 
  	ignore.case = TRUE)|
  	fblist$start_time >= start_time|
  	fblist$stop_time <= stop_time),]

  variables <- c("campaign_group_name", "campaign_name", "spend", "frequency",
                 "impressions","reach", "ctr", "cpm", "cpc", "action_values",
                 "cost_per_unique_action_type", "unique_actions")

  fbtable <- lapply(fblist$id, 
                    function(id) fb_insights(target = id, 
                                             level = "adgroup",
                                             fields = toJSON(variables,
                                                             job_type = 'async')))

  # Convertimos la lista que nos devuelve el API en un dataframe
  fbtable <- do.call('rbind', lapply(fbtable, data.frame))
  fb_data <- data.frame()
  for(i in 1:nrow(test_2)) {
    w = data.frame(rep(fbtable[i, !sapply(fbtable, is.list)], 
                       sapply(fbtable$cost_per_unique_action_type[i], nrow),
                       length.out = length(fbtable[i, !sapply(test_2, is.list)])), 
                   lapply(fbtable[i, sapply(fbtable, is.list)], data.frame))
    fb_data = rbind(fb_data, w)
    rm(w)
  }
}

