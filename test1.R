install.packages("forecast")

library(forecast)
library(tseries)

# ===================================================

monthly_list <- read.csv("E:/Rtimeline/monthly_list_final.csv")
monthly_list <- monthly_list[-1:-72,]
monthly_list <- monthly_list[-133:-144,]

monthly_list_ts <- monthly_list[14]
monthly_list_timeseries <- ts(monthly_list_ts, start = c(2006,1), deltat = 1/12 )
plot.ts(monthly_list_timeseries)

par(mfrow=c(2,1))

fithot <- HoltWinters(monthly_list_timeseries, gamma = FALSE, l.start = 0)
plot(fithot)
fithot2 <- forecast.HoltWinters(fithot, h=3)
plot(fithot2)


plot(monthly_list_ts[,1],type="l")
monthly_list_ts.1 <- filter(monthly_list_ts[,1],filter=rep(1/9,9))
monthly_list_ts.2 <- filter(monthly_list_ts[,1],filter=rep(1/25,25))
monthly_list_ts.3 <- filter(monthly_list_ts[,1],filter=rep(1/41,41))
lines(monthly_list_ts.1,col="red")
lines(monthly_list_ts.2,col="purple")
lines(monthly_list_ts.3,col="blue")

adf.test(monthly_list_timeseries)
a1 <- auto.arima(monthly_list_timeseries,trace = TRUE)
a2 <- arima(monthly_list_timeseries,order = c(0,1,1),seasonal = c(1,0,1))
a3 <- forecast.Arima(a2,h=12,level=c(25))
plot(a3)

#=======================================================================================

monthly_list <- read.csv("E:/Rtimeline/monthly_list_final.csv")

monthly_list_ts <- monthly_list_train[i]
monthly_list_timeseries <- ts(monthly_list_ts, start = c(2006,1), deltat = 1/12 )
plot.ts(monthly_list_timeseries)

#=====================================================================================

plot(decompose(monthly_list_timeseries))


#=====================================================================================

#檢查是否為穩定數據
adf.test(monthly_list_timeseries)

#須先平穩數據,用一階差分
diff1 <- diff(monthly_list_timeseries,1)
#看是否有滿足 p < 0.05
adf.test(diff1)
acf(diff1,lag.max = 24)
Box.test(diff1)
a4 <- auto.arima(diff1, approximation= T, trace = T)
a5 <- arima(monthly_list_timeseries,order = c(1,0,3), seasonal = c(0,1,0))
a6 <- forecast.Arima(a5,h=4,level=c(25))
plot(a6)
Box.test(a6$residuals)

plot(decompose(a6$residuals))

#====================================================================

monthly_list_train <- monthly_list[-129:-133,]


#1 寫入單筆資料
#2 轉換成時間序列
#3 檢查是否穩定 
#4 不穩定用一階差分
#5 使用arima判定最佳模型
#6 寫出最佳模型
#7 產生模型
#8 預測幾月幾期
#9 寫出預測幾月幾期

d_list <- NULL
arima_list <- NULL
forecast_list <- NULL

arma_list <- NULL

# sigma2_list <- NULL
# loglik_list <- NULL
# aic_list <- NULL

for(i in 2:ncol(monthly_list)){
  monthly_list_ts <- monthly_list[i]
  # year <- 2006
  # for(j in 1:11){
  #   if(sum(monthly_list_ts[1:12,])/12)
  # }
  monthly_list_timeseries <- ts(monthly_list_ts, start = c(2006,1), deltat = 1/12 )
  adf_test <- adf.test(monthly_list_timeseries)
  box_test <- Box.test(monthly_list_timeseries)
  
  d <- 0
  if( adf_test$p.value >= 0.05 ||  box_test$p.value >= 0.05){
    d <- 1
    diff <- diff(monthly_list_timeseries, d)
    for(k in 1:5){
      
      adf_test <- adf.test(diff)
      box_test <- Box.test(diff)
      if( adf_test$p.value >= 0.05 ||  box_test$p.value >= 0.05){
        d <- d + 1
        diff <- diff(monthly_list_timeseries, d)
        adf_test <- adf.test(diff)
        box_test <- Box.test(diff)
      }
    }
    a1 <- auto.arima(diff, approximation= T, trace = T)
  }else{
    a1 <- auto.arima(monthly_list_timeseries, stepwise = F, trace = T)
  }
  
  # diff_num <- 0
  # if(d >= 2){
  #    diff_num <- diff_num + 1
  # }
  # alt_diff <- 0
  # if( (a1$arma[3] == 0) && (a1$arma[4] == 0)){
  #   alt_diff <- diff_num
  # }
  a2 <- arima(monthly_list_timeseries,order = c(a1$arma[1], a1$arma[6], a1$arma[2]), seasonal = c(a1$arma[3], a1$arma[7], a1$arma[4]), method = "ML")
  a3 <- forecast.Arima(a2, h=4, level=c(25))
  plot(a3)
  
  d_list <- rbind(d_list, d)
  arima_list <- rbind(arima_list, a3$method)
  
  arma_list <- rbind(arma_list, a1$arma)
  
  # sigma2_list <- rbind(sigma2_list, a3$sigma2)
  # loglik_list <- rbind(loglik_list, a3$model$loglik)
  # aic_list <- rbind(aic_list, a3$model$aic)
  
  a3_month_forecast <- a3$mean[1:12]
  forecast_list <- cbind(forecast_list, a3_month_forecast)
  
  # if(i == 2){
  #   name <- paste(i, colnames(monthly_list_train)[i],".png")
  #   
  #   png(name, width = 480, height = 320)
  # }else{
  # name <- paste(i, colnames(monthly_list_train)[i+1],".png")
  # 
  # png(name, width = 480, height = 320)
  # }
}

total_train <- cbind(d_list, arima_list, arma_list)
# total_train <- cbind(d_list, arima_list, sigma2_list, loglik_list, aic_list)

# total <- cbind(d_list, arima_list)

print(arima_list)
print(forecast_list)

#=====================================================================================


monthly_list_part1 <- monthly_list
monthly_list_part1 <- monthly_list_part1[,-1]

monthly_list_part2 <- forecast_list
monthly_list_part2 <- monthly_list_part2[-5:-12,]
monthly_list_part2 <- cbind(monthly_list_part2, matrix(0, nrow = 4, ncol = 75))
monthly_list_part2 <- as.data.frame(monthly_list_part2, stringsAsFactors = F)
colnames(monthly_list_part2) <- colnames(monthly_list_part1)

monthly_list_comb <- rbind(monthly_list_part1, monthly_list_part2)

write.table(monthly_list_comb, file = "monthly_list_comb.csv", sep = ",", row.names = F)

#========================================================================================


monthly_list_combT <- NULL
monthly_list_comb_1 <- NULL

for(i in 1:ncol(monthly_list_comb)){
  monthly_list_comb_1 <- c(monthly_list_comb_1, colnames(monthly_list_comb)[i])
}
monthly_list_comb_1 <- c("景點",monthly_list_comb_1)
monthly_list_comb_1 <- as.data.frame(monthly_list_comb_1, stringsAsFactors = F)


monthly_list_title <- NULL
for(i in 2006:2017){
  year_id <- i
  for( j in 1:12){
    month_id <- j
    if(j <= 9){
      month_id <- paste("0", month_id, sep = "")
    }
    monthly_list_title <- rbind(monthly_list_title, paste(year_id, month_id, sep = ""))
  }
}
monthly_list_title <- monthly_list_title[-137:-144]
monthly_list_title <- c("景點", monthly_list_title)


T_monthly_list_comb <- t(monthly_list_comb)
T_monthly_list_comb <- as.data.frame(T_monthly_list_comb, stringsAsFactors = F)
T_monthly_list_comb <- cbind.data.frame(rownames(T_monthly_list_comb), T_monthly_list_comb, stringsAsFactors = F)

colnames(T_monthly_list_comb) <- monthly_list_title

write.table(T_monthly_list_comb, file = "monthly_list_comb_final_v2.csv", sep = ",", row.names = F)


T_monthly_list_comb <- rbind.data.frame(colnames(T_monthly_list_comb), T_monthly_list_comb, stringsAsFactors = F)

write.table(T_monthly_list_comb, file = "monthly_list_comb_final.csv", sep = ",", row.names = F)

#============================================================================================================


T_monthly_list_comb_T <- t(T_monthly_list_comb)
T_monthly_list_comb_T <- as.data.frame(T_monthly_list_comb_T, stringsAsFactors = F)

T_monthly_list_comb_T <- cbind.data.frame(rownames(T_monthly_list_comb_T), T_monthly_list_comb_T, stringsAsFactors = F)

write.table(T_monthly_list_comb_T, file = "monthly_list_comb_final_T.csv", sep = ",", row.names = F)

#=============================================================================================================

#差分表
d_list <- NULL
#最佳模型表
arima_list <- NULL
#最佳模型參數
arma_list <- NULL
#預測表
forecast_list <- NULL

# for(i in 2:ncol(monthly_list)){
for(i in 2:30){
  monthly_list_ts <- monthly_list[i]
  monthly_list_timeseries <- ts(monthly_list_ts, start = c(2006,1), deltat = 1/12 )
  #adf_test是指Dickey-Fuller Test
  adf_test <- adf.test(monthly_list_timeseries)
  #box_test是指測定時序是否為隨機波動
  box_test <- Box.test(monthly_list_timeseries)
  
  d <- 0
  if( adf_test$p.value >= 0.05 ||  box_test$p.value >= 0.05){
    d <- 1
    diff <- diff(monthly_list_timeseries, d)
    for(k in 1:5){
      
      adf_test <- adf.test(diff)
      box_test <- Box.test(diff)
      if( adf_test$p.value >= 0.05 ||  box_test$p.value >= 0.05){
        d <- d + 1
        diff <- diff(monthly_list_timeseries, d)
        adf_test <- adf.test(diff)
        box_test <- Box.test(diff)
      }
    }
    a1 <- auto.arima(diff, approximation= T, trace = T)
  }else{
    a1 <- auto.arima(monthly_list_timeseries, stepwise = F, trace = T)
  }

  a2 <- arima(monthly_list_timeseries,order = c(a1$arma[1], a1$arma[6], a1$arma[2]), seasonal = c(a1$arma[3], a1$arma[7], a1$arma[4]), method = "ML")
  a3 <- forecast.Arima(a2, h=4, level=c(25))
  plot(a3)
  
  d_list <- rbind(d_list, d)
  arima_list <- rbind(arima_list, a3$method)
  
  arma_list <- rbind(arma_list, a1$arma)

  a3_month_forecast <- a3$mean[1:12]
  forecast_list <- cbind(forecast_list, a3_month_forecast)
  
  
  if(i == 2){
    name <- paste(i, colnames(monthly_list_train)[i],".png")

    png(name, width = 480, height = 320)
  }else{
  name <- paste(i, colnames(monthly_list_train)[i+1],".png")

  png(name, width = 480, height = 320)
  }
}

total_train_fix <- cbind(d_list, arima_list, arma_list)

total_train <- cbind(d_list, arima_list, arma_list)