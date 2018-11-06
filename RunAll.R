list.of.packages <- c("imputeTS","countrycode","eurostat","dplyr","tidyr","plotly","stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

Sys.setenv("plotly_username"="lkiss711")
Sys.setenv("plotly_api_key"="8wkDxOfqdxC6ArhWfePv")


library("eurostat")
library("dplyr")
library("tidyr")
library("stringr")
library("countrycode")
library("zoo")
library("imputeTS")
library("forecast")


id_year <- "nrg_chddr2_a"
id <- "nrg_chddr2_m"
data <- get_eurostat(id,time_format = "date")
data_year <- get_eurostat(id_year,time_format = "raw")

data_year_all_HDD <- data_year[,2:5]
data_year_all_HDD <- dplyr::filter(data_year_all_HDD,indic_nrg == "HDD" & nchar(as.character(geo)) == 4)
data_year_to_pl_HDD <- dplyr::filter(data_year,indic_nrg == "HDD" & nchar(as.character(geo)) == 2)


data_year_all_CDD <- data_year[,2:5]
data_year_all_CDD <- dplyr::filter(data_year_all_CDD,indic_nrg == "CDD" & nchar(as.character(geo)) == 4)

spread_data_year_HDD <- spread(data_year_all_HDD[2:4],geo,values)
spread_data_year_CDD <- spread(data_year_all_CDD[2:4],geo,values)

data_all_HDD <- data[,2:5]
data_all_HDD <- dplyr::filter(data_all_HDD,indic_nrg == "HDD" & nchar(as.character(geo)) == 4)

data_all_CDD <- data[,2:5]
data_all_CDD <- dplyr::filter(data_all_CDD,indic_nrg == "CDD" & nchar(as.character(geo)) == 4)

spread_data_HDD <- spread(data_all_HDD[2:4],geo,values)
spread_data_CDD <- spread(data_all_CDD[2:4],geo,values)

NUTS2_codes <- unique(data_year_to_pl_HDD[,"geo"])
NUTS2_codes$eustat_codes <- label_eurostat(NUTS2_codes[,"geo"], fix_duplicated = TRUE)

data_year_to_plot_HDD <- spread(data_year_to_pl_HDD[,3:5],geo,values)
data_year_to_map_HDD <- spread(data_year_to_pl_HDD[,3:5],time,values)
data_year_to_map_HDD$country_mean = rowMeans(data_year_to_map_HDD[,2:45], na.rm = TRUE)
data_year_to_map_HDD$iso_code <- countrycode(data_year_to_map_HDD$geo,"eurostat","iso3c")

data_ts <- dplyr::filter(data,indic_nrg == "HDD" & nchar(as.character(geo)) == 2)
data_ts <- data_ts[,3:5]
data_ts <- spread(data_ts,geo,values)


library("plotly")

p_ts <- plot_ly(data = data_year_to_plot_HDD, x = ~time)
p_ts <-   add_lines(p_ts,y = ~AT ,name = 'AT', visible = T, line = list(width = 4, color = "#00587b"))
p_ts <- p_ts %>%   layout(title = 'Heating Degree Days in Europe by country')

for(ds in NUTS2_codes$geo){

  if(ds != 'AT'){
      p_ts <-   add_lines(p_ts,y =  eval(parse(text = paste("y = data_year_to_plot_HDD$",ds)),)
                          ,name = ds, visible = F)

      }
 
}

visible_list <- list()
for(i in 1:length(NUTS2_codes$geo)){
  visible_sublist <- list()
  for(j in 1:length(NUTS2_codes$geo)){
    if(i == j){
      visible_sublist[[j]] = T
    }else{
      visible_sublist[[j]] = F
    }          
  }
  visible_list[[i]] = visible_sublist
}


p_ts <- 
(p_ts %>% 
  layout(
    updatemenus = list(
      list(yanchor = 'auto',buttons =list(
        list(method = "restyle",args = list("visible", visible_list[[1]]),label = NUTS2_codes$geo[[1]]),
        list(method = "restyle",args = list("visible", visible_list[[2]]),label = NUTS2_codes$geo[[2]]),
        list(method = "restyle",args = list("visible", visible_list[[3]]),label = NUTS2_codes$geo[[3]]),
        list(method = "restyle",args = list("visible", visible_list[[4]]),label = NUTS2_codes$geo[[4]]),
        list(method = "restyle",args = list("visible", visible_list[[5]]),label = NUTS2_codes$geo[[5]]),
        list(method = "restyle",args = list("visible", visible_list[[6]]),label = NUTS2_codes$geo[[6]]),
        list(method = "restyle",args = list("visible", visible_list[[7]]),label = NUTS2_codes$geo[[7]]),
        list(method = "restyle",args = list("visible", visible_list[[8]]),label = NUTS2_codes$geo[[8]]),
        list(method = "restyle",args = list("visible", visible_list[[9]]),label = NUTS2_codes$geo[[9]]),
        list(method = "restyle",args = list("visible", visible_list[[10]]),label = NUTS2_codes$geo[[10]]),
        list(method = "restyle",args = list("visible", visible_list[[11]]),label = NUTS2_codes$geo[[11]]),
        list(method = "restyle",args = list("visible", visible_list[[12]]),label = NUTS2_codes$geo[[12]]),
        list(method = "restyle",args = list("visible", visible_list[[13]]),label = NUTS2_codes$geo[[13]]),
        list(method = "restyle",args = list("visible", visible_list[[14]]),label = NUTS2_codes$geo[[14]]),
        list(method = "restyle",args = list("visible", visible_list[[15]]),label = NUTS2_codes$geo[[15]]),
        list(method = "restyle",args = list("visible", visible_list[[16]]),label = NUTS2_codes$geo[[16]]),
        list(method = "restyle",args = list("visible", visible_list[[17]]),label = NUTS2_codes$geo[[17]]),
        list(method = "restyle",args = list("visible", visible_list[[18]]),label = NUTS2_codes$geo[[18]]),
        list(method = "restyle",args = list("visible", visible_list[[19]]),label = NUTS2_codes$geo[[19]]),
        list(method = "restyle",args = list("visible", visible_list[[20]]),label = NUTS2_codes$geo[[20]]),
        list(method = "restyle",args = list("visible", visible_list[[21]]),label = NUTS2_codes$geo[[21]]),
        list(method = "restyle",args = list("visible", visible_list[[22]]),label = NUTS2_codes$geo[[22]]),
        list(method = "restyle",args = list("visible", visible_list[[23]]),label = NUTS2_codes$geo[[23]]),
        list(method = "restyle",args = list("visible", visible_list[[24]]),label = NUTS2_codes$geo[[24]]),
        list(method = "restyle",args = list("visible", visible_list[[25]]),label = NUTS2_codes$geo[[25]]),
        list(method = "restyle",args = list("visible", visible_list[[26]]),label = NUTS2_codes$geo[[26]]),
        list(method = "restyle",args = list("visible", visible_list[[27]]),label = NUTS2_codes$geo[[27]]),
        list(method = "restyle",args = list("visible", visible_list[[28]]),label = NUTS2_codes$geo[[28]])
)))))
p_ts

options(browser = 'false')
api_create(p_ts, filename = "hdd_ts_by_country")



p_map <- data_year_to_map_HDD %>% 
  plot_geo(
    locationmode = 'eu countries'
  ) %>% 
  add_trace(
    z = data_year_to_map_HDD$`1974`,
    locations = data_year_to_map_HDD$iso_code, color = data_year_to_map_HDD$country_mean, name = 1974,
    colors = 'Greens', visible = T
  ) %>%
  colorbar(title = "Average HDD") %>%
  layout(geo = list(
    scope = 'europe'
      ),
    title='Average Heating Degree Days by country between 1974 and 2017<br>
      Source: <a href="http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=nrg_chddr2_m&lang=en">Eurostat</a>'
    )

p_map

options(browser = 'false')
api_create(p_map, filename = "hdd_average_by_country")

# i <- 1
# for(year in colnames(data_year_to_map_HDD[2:46])){
#     p_map <-   add_trace(p_map,z =  eval(parse(text = paste0("y = data_year_to_map_HDD[",i,"]")),)
#                         ,name = year,colors = 'Greens', visible = F)
#     
# i <- i+1
# }
# 
# p_map


# visible_list <- list()
# for(year in colnames(data_year_to_map_HDD[2:46])){
#   visible_sublist <- list()
#   for(j in  colnames(data_year_to_map_HDD[2:46])){
#     if(year == j){
#       visible_sublist[[j]] = T
#     }else{
#       visible_sublist[[j]] = F
#     }          
#   }
#   visible_list[[year]] = visible_sublist
# }
# 
# 
# 
# p_map <- 
#   (p_map %>% 
#      layout(
#        updatemenus = list(
#          list(yanchor = 'auto',buttons =list(
#            list(method = "restyle",args = list("visible", visible = visible_list[[1]]),label = colnames(data_year_to_map_HDD[2:46])[[1]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[2]]),label = colnames(data_year_to_map_HDD[2:46])[[2]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[3]]),label = colnames(data_year_to_map_HDD[2:46])[[3]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[4]]),label = colnames(data_year_to_map_HDD[2:46])[[4]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[5]]),label = colnames(data_year_to_map_HDD[2:46])[[5]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[6]]),label = colnames(data_year_to_map_HDD[2:46])[[6]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[7]]),label = colnames(data_year_to_map_HDD[2:46])[[7]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[8]]),label = colnames(data_year_to_map_HDD[2:46])[[8]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[9]]),label = colnames(data_year_to_map_HDD[2:46])[[9]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[10]]),label = colnames(data_year_to_map_HDD[2:46])[[10]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[11]]),label = colnames(data_year_to_map_HDD[2:46])[[11]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[12]]),label = colnames(data_year_to_map_HDD[2:46])[[12]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[13]]),label = colnames(data_year_to_map_HDD[2:46])[[13]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[14]]),label = colnames(data_year_to_map_HDD[2:46])[[14]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[15]]),label = colnames(data_year_to_map_HDD[2:46])[[15]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[16]]),label = colnames(data_year_to_map_HDD[2:46])[[16]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[17]]),label = colnames(data_year_to_map_HDD[2:46])[[17]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[18]]),label = colnames(data_year_to_map_HDD[2:46])[[18]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[19]]),label = colnames(data_year_to_map_HDD[2:46])[[19]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[20]]),label = colnames(data_year_to_map_HDD[2:46])[[20]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[21]]),label = colnames(data_year_to_map_HDD[2:46])[[21]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[22]]),label = colnames(data_year_to_map_HDD[2:46])[[22]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[23]]),label = colnames(data_year_to_map_HDD[2:46])[[23]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[24]]),label = colnames(data_year_to_map_HDD[2:46])[[24]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[25]]),label = colnames(data_year_to_map_HDD[2:46])[[25]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[26]]),label = colnames(data_year_to_map_HDD[2:46])[[26]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[27]]),label = colnames(data_year_to_map_HDD[2:46])[[27]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[28]]),label = colnames(data_year_to_map_HDD[2:46])[[28]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[29]]),label = colnames(data_year_to_map_HDD[2:46])[[29]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[30]]),label = colnames(data_year_to_map_HDD[2:46])[[30]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[31]]),label = colnames(data_year_to_map_HDD[2:46])[[31]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[32]]),label = colnames(data_year_to_map_HDD[2:46])[[32]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[33]]),label = colnames(data_year_to_map_HDD[2:46])[[33]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[34]]),label = colnames(data_year_to_map_HDD[2:46])[[34]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[35]]),label = colnames(data_year_to_map_HDD[2:46])[[35]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[36]]),label = colnames(data_year_to_map_HDD[2:46])[[36]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[37]]),label = colnames(data_year_to_map_HDD[2:46])[[37]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[38]]),label = colnames(data_year_to_map_HDD[2:46])[[38]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[39]]),label = colnames(data_year_to_map_HDD[2:46])[[39]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[40]]),label = colnames(data_year_to_map_HDD[2:46])[[40]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[41]]),label = colnames(data_year_to_map_HDD[2:46])[[41]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[42]]),label = colnames(data_year_to_map_HDD[2:46])[[42]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[43]]),label = colnames(data_year_to_map_HDD[2:46])[[43]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[44]]),label = colnames(data_year_to_map_HDD[2:46])[[44]]),
#            list(method = "restyle",args = list("visible", visible = visible_list[[45]]),label = colnames(data_year_to_map_HDD[2:46])[[45]])
#          ) 
#         )
#       )
#      )
#     )
# 
# p_map  
# 

source("Functions.R")

parameters <- det_common_order(data_ts)
parameters <- det_max_order(parameters)


trends_by_country <- data_ts



for(country in colnames(data_ts)[2:length(colnames(data_ts))]){
  eval(parse(text = paste0("ts_temp <- ts(data_ts$",country,", start = c(1974,1),frequency = 12)")))
  ts_temp <- na.ma(ts_temp)
  x <- decompose(ts_temp)
  eval(parse(text = paste0("trends_by_country$",country," <- x$trend")))
  # deseasonal <- seasadj(x)
  # y <- auto.arima(deseasonal)
  # y$arma
}

p_trend <- plot_ly(data = trends_by_country, x = ~time)
p_trend <-   add_lines(p_trend,y = ~AT ,name = 'AT', visible = T, line = list(width = 4, color = "#00587b"))
p_trend <- p_trend %>%   layout(title = 'Trends in Heating Degree Days in Europe by country')


for(ds in NUTS2_codes$geo){
  
  if(ds != 'AT'){
    p_trend <-   add_lines(p_trend,y =  eval(parse(text = paste("y = trends_by_country$",ds)),)
                           ,name = ds, visible = F)
    
  }
  
}

visible_list <- list()
for(i in 1:length(NUTS2_codes$geo)){
  visible_sublist <- list()
  for(j in 1:length(NUTS2_codes$geo)){
    if(i == j){
      visible_sublist[[j]] = T
    }else{
      visible_sublist[[j]] = F
    }          
  }
  visible_list[[i]] = visible_sublist
}


p_trend <- 
  (p_trend %>% 
     layout(
       updatemenus = list(
         list(yanchor = 'auto',buttons =list(
           list(method = "restyle",args = list("visible", visible_list[[1]]),label = NUTS2_codes$geo[[1]]),
           list(method = "restyle",args = list("visible", visible_list[[2]]),label = NUTS2_codes$geo[[2]]),
           list(method = "restyle",args = list("visible", visible_list[[3]]),label = NUTS2_codes$geo[[3]]),
           list(method = "restyle",args = list("visible", visible_list[[4]]),label = NUTS2_codes$geo[[4]]),
           list(method = "restyle",args = list("visible", visible_list[[5]]),label = NUTS2_codes$geo[[5]]),
           list(method = "restyle",args = list("visible", visible_list[[6]]),label = NUTS2_codes$geo[[6]]),
           list(method = "restyle",args = list("visible", visible_list[[7]]),label = NUTS2_codes$geo[[7]]),
           list(method = "restyle",args = list("visible", visible_list[[8]]),label = NUTS2_codes$geo[[8]]),
           list(method = "restyle",args = list("visible", visible_list[[9]]),label = NUTS2_codes$geo[[9]]),
           list(method = "restyle",args = list("visible", visible_list[[10]]),label = NUTS2_codes$geo[[10]]),
           list(method = "restyle",args = list("visible", visible_list[[11]]),label = NUTS2_codes$geo[[11]]),
           list(method = "restyle",args = list("visible", visible_list[[12]]),label = NUTS2_codes$geo[[12]]),
           list(method = "restyle",args = list("visible", visible_list[[13]]),label = NUTS2_codes$geo[[13]]),
           list(method = "restyle",args = list("visible", visible_list[[14]]),label = NUTS2_codes$geo[[14]]),
           list(method = "restyle",args = list("visible", visible_list[[15]]),label = NUTS2_codes$geo[[15]]),
           list(method = "restyle",args = list("visible", visible_list[[16]]),label = NUTS2_codes$geo[[16]]),
           list(method = "restyle",args = list("visible", visible_list[[17]]),label = NUTS2_codes$geo[[17]]),
           list(method = "restyle",args = list("visible", visible_list[[18]]),label = NUTS2_codes$geo[[18]]),
           list(method = "restyle",args = list("visible", visible_list[[19]]),label = NUTS2_codes$geo[[19]]),
           list(method = "restyle",args = list("visible", visible_list[[20]]),label = NUTS2_codes$geo[[20]]),
           list(method = "restyle",args = list("visible", visible_list[[21]]),label = NUTS2_codes$geo[[21]]),
           list(method = "restyle",args = list("visible", visible_list[[22]]),label = NUTS2_codes$geo[[22]]),
           list(method = "restyle",args = list("visible", visible_list[[23]]),label = NUTS2_codes$geo[[23]]),
           list(method = "restyle",args = list("visible", visible_list[[24]]),label = NUTS2_codes$geo[[24]]),
           list(method = "restyle",args = list("visible", visible_list[[25]]),label = NUTS2_codes$geo[[25]]),
           list(method = "restyle",args = list("visible", visible_list[[26]]),label = NUTS2_codes$geo[[26]]),
           list(method = "restyle",args = list("visible", visible_list[[27]]),label = NUTS2_codes$geo[[27]]),
           list(method = "restyle",args = list("visible", visible_list[[28]]),label = NUTS2_codes$geo[[28]])
         )))))
p_trend

options(browser = 'false')
api_create(p_ts, filename = "hdd_trend_by_country")


# 
# 
# forecast_by_country <- data_ts
# 
# 
# for(country in colnames(data_ts)[2:length(colnames(data_ts))]){
#   eval(parse(text = paste0("ts_temp <- ts(data_ts$",country,", start = c(1974,1),frequency = 12)")))
#   ts_temp <- na.ma(ts_temp)
#   x <- decompose(ts_temp)
#   eval(parse(text = paste0("trends_by_country$",country," <- x$trend")))
#   # deseasonal <- seasadj(x)
#   # y <- auto.arima(deseasonal)
#   # y$arma
#   
# }
# 
# fit.xts <- auto.arima(qxts$y)
# forecast_length <- 30
# fore.xts <- forecast(fit.xts, h=forecast_length)
# 
# 
# fore.dates <- seq(as.POSIXct(data_ts$time[length(data_ts$time)], origin='1970-01-01'), by=data_ts$time[length(data_ts$time)] - data_ts$time[length(data_ts$time)-1], len=forecast_length)
# 
# p <- plot_ly()
# 
# p <- plot_ly() %>%
#   add_lines(x = as.POSIXct(data_ts$time, origin='1970-01-01'), y = data_ts$AT,
#             color = I("black"), 
#             name = "observed", 
#             marker=list(mode='lines')) %>% 
#   add_lines(x = fore.dates, y = seas_fcast$mean, color = I("blue"), name = "prediction") %>%
#   add_ribbons(x = fore.dates, 
#               ymin = seas_fcast$lower[, 2], 
#               ymax = seas_fcast$upper[, 2],
#               color = I("gray95"), 
#               name = "95% confidence") %>%
#   add_ribbons(p, 
#               x = fore.dates, 
#               ymin = seas_fcast$lower[, 1], 
#               ymax = seas_fcast$upper[, 1],
#               color = I("gray80"), name = "80% confidence")
# 
# p
# 
# 
# 
