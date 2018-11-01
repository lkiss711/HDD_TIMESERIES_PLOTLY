list.of.packages <- c("eurostat","dplyr","tidyr","plotly","stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library("eurostat")
library("dplyr")
library("tidyr")
library("stringr")


id_year <- "nrg_chddr2_a"
id <- "nrg_chddr2_m"
data <- get_eurostat(id,time_format = "date")
data_year <- get_eurostat(id_year,time_format = "raw")

data_year_all_HDD <- data_year[,2:5]
data_year_all_HDD <- dplyr::filter(data_year_all_HDD,indic_nrg == "HDD" & nchar(as.character(geo)) == 4)
data_year_to_plot_HDD <- dplyr::filter(data_year,indic_nrg == "HDD" & nchar(as.character(geo)) == 2)


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

NUTS2_codes <- unique(data_year_to_plot_HDD[,"geo"])
NUTS2_codes$eustat_codes <- label_eurostat(NUTS2_codes[,"geo"], fix_duplicated = TRUE)

data_year_to_plot_HDD <- spread(data_year_to_plot_HDD[,3:5],geo,values)

library("plotly")


p_ts <- plot_ly(data = data_year_to_plot_HDD, x = ~time)
p_ts <-   add_lines(p_ts,y = ~AT ,name = 'AT', visible = T)
for(ds in NUTS2_codes$geo){
  if(ds != 'AT'){
    p_ts <-   add_lines(p_ts,y =  eval(parse(text = paste("y = data_year_to_plot_HDD$",ds)),) ,name = ds, visible = F)
  }
}  

p_ts <- 
(p_ts %>% 
  layout(
    updatemenus = list(
      list(yanchor = 'auto',buttons =list(
        list(method = "restyle",args = list("visible", list(T,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F)),label = 'AT'),
        list(method = "restyle",args = list("visible", list(F,T,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F)),label = 'BE'),
        list(method = "restyle",args = list("visible", list(F,F,T,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F)),label = 'BG'),
        list(method = "restyle",args = list("visible", list(F,F,F,T,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F)),label = 'CY'),
        list(method = "restyle",args = list("visible", list(F,F,F,F,T,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F)),label = 'CZ'),
        list(method = "restyle",args = list("visible", list(F,F,F,F,F,T,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F)),label = 'DE'),
        list(method = "restyle",args = list("visible", list(F,F,F,F,F,F,T,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F)),label = 'DK'),
        list(method = "restyle",args = list("visible", list(F,F,F,F,F,F,F,T,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F)),label = 'EE'),
        list(method = "restyle",args = list("visible", list(F,F,F,F,F,F,F,F,T,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F)),label = 'EL'),
        list(method = "restyle",args = list("visible", list(F,F,F,F,F,F,F,F,F,T,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F)),label = 'ES'),
        list(method = "restyle",args = list("visible", list(F,F,F,F,F,F,F,F,F,F,T,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F)),label = 'FI'),
        list(method = "restyle",args = list("visible", list(F,F,F,F,F,F,F,F,F,F,F,T,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F)),label = 'FR'),
        list(method = "restyle",args = list("visible", list(F,F,F,F,F,F,F,F,F,F,F,F,T,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F)),label = 'HR'),
        list(method = "restyle",args = list("visible", list(F,F,F,F,F,F,F,F,F,F,F,F,F,T,F,F,F,F,F,F,F,F,F,F,F,F,F,F)),label = 'HU'),
        list(method = "restyle",args = list("visible", list(F,F,F,F,F,F,F,F,F,F,F,F,F,F,T,F,F,F,F,F,F,F,F,F,F,F,F,F)),label = 'IE'),
        list(method = "restyle",args = list("visible", list(F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,T,F,F,F,F,F,F,F,F,F,F,F,F)),label = 'IT'),
        list(method = "restyle",args = list("visible", list(F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,T,F,F,F,F,F,F,F,F,F,F,F)),label = 'LT'),
        list(method = "restyle",args = list("visible", list(F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,T,F,F,F,F,F,F,F,F,F,F)),label = 'LU'),
        list(method = "restyle",args = list("visible", list(F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,T,F,F,F,F,F,F,F,F,F)),label = 'LV'),
        list(method = "restyle",args = list("visible", list(F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,T,F,F,F,F,F,F,F,F)),label = 'MT'),
        list(method = "restyle",args = list("visible", list(F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,T,F,F,F,F,F,F,F)),label = 'NL'),
        list(method = "restyle",args = list("visible", list(F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,T,F,F,F,F,F,F)),label = 'PL'),
        list(method = "restyle",args = list("visible", list(F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,T,F,F,F,F,F)),label = 'PT'),
        list(method = "restyle",args = list("visible", list(F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,T,F,F,F,F)),label = 'RO'),
        list(method = "restyle",args = list("visible", list(F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,T,F,F,F)),label = 'SE'),
        list(method = "restyle",args = list("visible", list(F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,T,F,F)),label = 'SI'),
        list(method = "restyle",args = list("visible", list(F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,T,F)),label = 'SK'),
        list(method = "restyle",args = list("visible", list(F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,T)),label = 'UK')           
)))))
p_ts





ts_proba <- ts(spread_data_HDD[,2], start = c(1974,1), frequency = 12)
dec_ts <- decompose(ts_proba)

