library(plyr)

det_common_order <- function(y_local){
  
  
  parameters <- list()
  coef_list <- list()
  
  
  for(i in 1:ncol(y_local)){
    y1 <- y_local[,i]
    
    AuModel <- auto.arima(y1,  max.p=5, max.q=5,
                          max.P=3, max.Q=3, max.order=8, max.d=2, max.D=1,
                          start.p=1, start.q=1, start.P=1, start.Q=1)
    
    
    
    parameters[[i]] <- AuModel$arma
    coef_list[[i]] <- AuModel$coef
    new_col <- length(parameters[[i]])+1
    parameters[[i]][[new_col]] <- coef_list[[i]][[length(coef_list[[i]])]]
    print(parameters[[i]])
  }
  
  df_parameters <- as.data.frame(matrix(unlist(parameters),nrow=ncol(y_local),byrow = T))
  
  df_parameters <- setNames(df_parameters, c("p", "q", "P", "Q", "m", "d", "D","drift"))
  
  return(df_parameters)
}

det_max_order <- function(df_orders){
  
  orders <- count(df_orders, vars = c("p", "q", "P", "Q", "m", "d", "D"))
  
  orders <- arrange(orders,desc(orders[,"freq"]))
  
  common_order <- orders[1,1:ncol(orders)-1]
  
  colnames(common_order) <- c("p", "q", "P", "Q", "m", "d", "D")
  
  return(common_order)
}


plot_forecast <- function(country){
  forecast_length <- 30
  fore.dates <- seq(as.POSIXct(data_ts$time[length(data_ts$time)], origin='1970-01-01'), by=data_ts$time[length(data_ts$time)] - data_ts$time[length(data_ts$time)-1], len=forecast_length)
  
  p <- plot_ly()
  
  p <- plot_ly() %>%
    add_lines(x = as.POSIXct(data_ts$time, origin='1970-01-01'), y = data_ts$country,
              color = I("black"), 
              name = "observed", 
              marker=list(mode='lines')) %>% 
    add_lines(x = fore.dates, y = seas_fcast$mean, color = I("blue"), name = "prediction") %>%
    add_ribbons(x = fore.dates, 
                ymin = seas_fcast$lower[, 2], 
                ymax = seas_fcast$upper[, 2],
                color = I("gray95"), 
                name = "95% confidence") %>%
    add_ribbons(p, 
                x = fore.dates, 
                ymin = seas_fcast$lower[, 1], 
                ymax = seas_fcast$upper[, 1],
                color = I("gray80"), name = "80% confidence")
  return(p)
  
}