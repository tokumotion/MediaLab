# El objetivo de la función es descargar la data del tráfico
# generado desde Facebook a las webs de los clientes.
# Las variables que vamos a usar son: clicks, CTR,
# impresiones, costo por click y costo total.

# De ahora en adelante, debemos taggear cada página y cada
# link para poder cruzar la información con la de la bbdd.

facebook <- function(brand, start_date, stop_date) {
	require(Rfacebook); require(fbRads)
	load("~/GitHub/Prueba/fb_token")

	# Cuenta de Business Manager de MediaLab
	acct <- "101146170039639"

	# Escogemos la cuenta que vamos a analizar
  brand <- switch(brand,
                  caral = "838478606179463",
                  nissan = "227015499643",
                  transitemos = "374014132682491")

  # Concatenamos las fechas en el formato de Graph API
	# Formato de fechas: "yyyy-mm-dd"
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
  table <- data.frame(cbind(start_date, stop_date, impressions, 
                            clicks, CTR = clicks/impressions))

  # Activamos la cuenta de Business Manager
  token <- fb_token$credentials$access_token
  fbad_init(acct, token, version = fb_api_version())

  fblist <- fbad_list_campaign(fields = c("id", "name",
                                          "objective", "start_time", "stop_time"))

  # Filtro campañas relevantes
  fblist <- fblist[which(grepl(brand, fblist$name,
  	ignore.case = TRUE)|
  	fblist$start_time >= start_date|
  	fblist$stop_time <= stop_date),]

  fbtable <- lapply(fblist$id,
                    function(id) fb_insights(target = id, 
                                             level = "adgroup",
                                             fields = toJSON(c("campaign_group_name",
                                                               "campaign_name", "spend",
                                                               "frequency",
                                                               "impressions","reach",
                                                               "ctr", "cpm", "cpc",
                                                               "action_values",
                                                               "cost_per_unique_action_type",
                                                               "unique_actions"),
                                             job_type = "async")))

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
  return(table ,fb_data)
}

analytics <- function(brand, start_date, stop_date) {
  require(RGoogleAnalytics)
  
  load("~/GitHub/Prueba/token_file")
  
  ValidateToken(token)
  
  proyectos <- c("Uptown", "Los Prados", "Coral Tower", "Montealto")
  segmentos <- c("gaid::6ADy07WQTlKccJftIlb8pA",
                 "gaid::Z7euUR8kTdOP820qSORXyQ",
                 "gaid::4zFWUKVrQRCNO5AKEo72Mw",
                 "gaid::Ipl22d0CStqYs8gPlDt-yA")
  conversion.rates <- c("ga:goal1ConversionRate", "ga:goal2ConversionRate",
                        "ga:goal3ConversionRate", "ga:goal5ConversionRate")
  completions <- c("ga:goal1Completions", "ga:goal2Completions", "ga:goal3Completions",
                   "ga:goal5Completions")
  tabla <- data.frame(proyectos, segmentos, conversion.rates, completions)
  rm(list=c("proyectos", "segmentos", "conversion.rates", "completions"))
  tabla <- data.frame(lapply(tabla, as.character), stringsAsFactors = FALSE)
  
  caral_segment <- function(segment){
    tabla <- tabla[which(tabla$proyectos == segment),]
    query.list <- Init(start.date = "2016-01-01",
                       end.date = "2016-01-31",
                       dimensions = "ga:channelGrouping, ga:source,
                       ga:medium, ga:month",
                       metrics = c("ga:sessions, ga:percentNewSessions,
                                   ga:newUsers, ga:bounceRate, ga:pageviewsPerSession,
                                   ga:avgSessionDuration", tabla$conversion.rates,
                                   tabla$completions),
                       max.results = 10000,
                       segments = tabla$segmentos,
                       table.id = "ga:94329203")
    ga.query <- QueryBuilder(query.list)
    ga.data <- GetReportData(ga.query, token)
  }

  brand <- switch(brand,
                  nissan = {
                    query.list <- Init(start.date = start_date,
                                       end.date = stop_date,
                                       dimensions = "ga:channelGrouping, ga:source,
                                       ga:medium, ga:month",
                                       metrics = "ga:sessions, ga:percentNewSessions,
                                       ga:newUsers, ga:bounceRate, ga:pageviewsPerSession,
                                       ga:avgSessionDuration, ga:goal1ConversionRate,
                                       ga:goal1Completions",
                                       max.results = 10000,
                                       table.id = "ga:105243153")
                    ga.query <- QueryBuilder(query.list)
                    ga.data <- GetReportData(ga.query, token)
                    return(ga.data)
                  },
                  caral = {
                    datos <- lapply(tabla$proyectos ,caral_segment)
                    return(datos)
                  })
}

adwords <- function(brand, start_date, stop_date) {
  require(RAdwords)
  load("~/GitHub/Prueba/ML_Adwords")
  metricas <- metrics("CAMPAIGN_PERFORMANCE_REPORT")
  metricas <- as.character(metricas)
  cuentas <- c("291-886-0053", "964-275-3771")
  caral_adwords <- function(x) {
    report <- statement(metricas[c(36, 96, 11, 86, 66, 43, 33, 27,
                                   23, 21, 57, 51, 108, 109, 123)], 
                        report = "CAMPAIGN_PERFORMANCE_REPORT",
                        start = start_date, end = stop_date)
    tabla <- getData(clientCustomerId = x, 
                     google_auth = Adwords, statement = report)
    Search <- tabla[which(tabla$Network == "Search Network"),]
    Display <- tabla[which(tabla$Network == "Display Network"),]
    return(list(Search, Display))
  }
  brand <- switch(brand,
                  nissan = {
                    report <- statement(metricas[c(36, 96, 11, 86, 66, 43, 33, 27,
                                                   23, 21, 57, 51, 108, 109, 123)], 
                                        report = "CAMPAIGN_PERFORMANCE_REPORT",
                                        start = start_date, end = stop_date)
                    tabla <- getData(clientCustomerId = "509-793-1822", 
                                   google_auth = Adwords, statement = report)
                    Search <- tabla[which(tabla$Network == "Search Network"),]
                    Display <- tabla[which(tabla$Network == "Display Network"),]
                    return(list(Search, Display))
                  },
                  caral = {
                    datos <- lapply(cuentas ,caral_adwords)
                    return(datos)
                    }
                  )
}