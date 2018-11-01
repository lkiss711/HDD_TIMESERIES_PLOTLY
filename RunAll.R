list.of.packages <- c("countrycode","eurostat","dplyr","tidyr","plotly","stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library("eurostat")
library("dplyr")
library("tidyr")
library("stringr")
library("countrycode")


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
data_year_to_map_HDD$iso_code <- countrycode(data_year_to_map_HDD$geo,"eurostat","iso3c")


library("plotly")


p_ts <- plot_ly(data = data_year_to_plot_HDD, x = ~time)
p_ts <-   add_lines(p_ts,y = ~AT ,name = 'AT', visible = T)
for(ds in NUTS2_codes$geo){
  if(ds != 'AT'){
    p_ts <-   add_lines(p_ts,y =  eval(parse(text = paste("y = data_year_to_plot_HDD$",ds)),) ,name = ds, visible = F)
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


ts_proba <- ts(spread_data_HDD[,2], start = c(1974,1), frequency = 12)
dec_ts <- decompose(ts_proba)

p_map <- data_year_to_map_HDD %>% 
  plot_geo(
    locationmode = 'eu countries'
  ) %>% 
  add_trace(
    z = data_year_to_map_HDD$country_mean,locations = data_year_to_map_HDD$iso_code, visible = T
  ) %>%
  colorbar(title = "Average HDD") %>%
  layout(geo = list(
    scope = 'europe'
      ),
    title='Average Heating Degree Days by country between 1974 and 2017<br>
      Source: <a href="http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=nrg_chddr2_m&lang=en">Eurostat</a>'
    )
  


p_map  

data_year_to_map_HDD$country_mean = rowMeans(data_year_to_map_HDD[,2:45], na.rm = TRUE)
