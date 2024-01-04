library(readxl)
library(demography)
library(timeSeries)
library(ggplot2)
library(reshape2)
library(writexl)

# Read data 
data1 = read_excel("Proccessed2 Angola.xlsx", sheet = "mu (1) a")
data2 = read_excel("Proccessed2 Angola.xlsx", sheet = "lx angola")

# Convert data1 ke data frame dan hilangkan variable x
mortality_data = as.data.frame(data1)
mortality_data = subset(mortality_data, select = -x)

# Convert data2 ke vector dan samain dimensi seperti mortality_data
population_data = data.frame(data2)
population_data= subset(population_data, select = -x)

age_groups = 1:99
years = 1990:2019

# Create demogdata object
dem_mortality = demogdata(
  data = mortality_data,
  ages = age_groups,
  years = years,
  label = "Mortality Data",
  name = "Mortality",
  pop = population_data,
  type = "mortality"
)

# Fit to model
lc_model = lca(dem_mortality)

# Plot the model
plot(lc_model)
plot(lc_model$age,lc_model$ax,type="l",lwd=2)
plot(lc_model$age,lc_model$bx,type="l",lwd=2)
plot(lc_model$year,lc_model$kt,type="l",lwd=2)

# 10 year forecast
plot(forecast(lc_model, h = 10))

hm = forecast(lc_model, h = 10)
hm$model$Mortality[]
plot(hm$age ,hm$model$Mortality[,1], type = "l", col = colors()[26])

for (i in 2:30){
  lines(hm$age ,hm$model$Mortality[,i], type = "l", col = colors()[26+i])
}
#length(hm$model$Mortality[,1])
#max(hm$model$Mortality[1,])
#which(hm$model$Mortality == 0.000163, arr.ind = TRUE)

for (i in 1:30){
  print(which(hm$model$Mortality == min(hm$model$Mortality[i,]), arr.ind = TRUE))
}
  
#x = 20, t = 1995
a = lc_model$ax[20]+lc_model$bx[20]*lc_model$kt[6]
a

# Residual
residue = exp(lc_model[["fitted"]][["y"]])
n_residue = residue - data1
lc = subset(n_residue, select = -x)

# Heatmap
heatmap_data = as.matrix(lc)
heatmap(heatmap_data, Rowv = NA, Colv = NA,
        col = colorRampPalette(c("green", "yellow", "red"))(1000),
        main = "Heatmap of lc",
        xlab = "X-axis Label",
        ylab = "Y-axis Label",
        scale = "none")
write_xlsx(lc, "C:\\Users\\seraf\\Documents\\lc_data.xlsx")
#---------------------------------------------------------------------------------------
# Forecasting Lee Carter (new miu)
library(timeSeries)
library(forecast)
library(xts)
library(TSA)
kt = lc_model$kt
kt_ts = ts(kt)
plot(kt_ts)

auto.arima(kt_ts)

mod1 = Arima(kt_ts, order = c(2,1,1), include.drift = TRUE)
predict = forecast(mod1, h = 10)
new_kt = cbind(mod1$fitted, predict$mean)
plot(new_kt)

new_kt = matrix(new_kt, nrow = 99, ncol = 10)
dim(new_kt)

predict$mean
vector = matrix(predict$mean, nrow = 99, ncol = 10)

new_miu = lc_model$ax + lc_model$bx * vector
nmiu = exp(new_miu)
nmiu
nmiu = as.data.frame(nmiu)
nmiu
write_xlsx(nmiu, "C:\\Users\\62821\\Documents\\miu (1).xlsx")

#---------------------------------------------------------------------------------------
# mu (tau)
data3 = read_excel("Proccessed2 Angola.xlsx", sheet = "mu (tau) a")
data4 = read_excel("Proccessed2 Angola.xlsx", sheet = "lx angola")

# Convert data1 ke data frame dan hilangkan variable x
mortality_data2 = as.data.frame(data3)
mortality_data2 = subset(mortality_data2, select = -x)

# Convert data2 ke vector dan samain dimensi seperti mortality_data
population_data2 = data.frame(data4)
population_data2= subset(population_data2, select = -x)

age_groups = 1:99
years = 1990:2019

# Create demogdata object
dem_mortality2 = demogdata(
  data = mortality_data2,
  ages = age_groups,
  years = years,
  label = "Mortality Data",
  name = "Mortality",
  pop = population_data2,
  type = "mortality"
)

# Fit to model
lc_model2 = lca(dem_mortality2)

# Plot the model
plot(lc_model2)
plot(lc_model2$age,lc_model$ax,type="l",lwd=2)
plot(lc_model2$age,lc_model$bx,type="l",lwd=2)
plot(lc_model2$year,lc_model$kt,type="l",lwd=2)

# 10 year forecast
plot(forecast(lc_model2, h = 10))

hm2 = forecast(lc_model2, h = 10)
hm2$model$Mortality[]
plot(hm2$age ,hm2$model$Mortality[,1], type = "l", col = colors()[26])

for (i in 2:30){
  lines(hm2$age ,hm2$model$Mortality[,i], type = "l", col = colors()[26+i])
}
#length(hm$model$Mortality[,1])
#max(hm$model$Mortality[1,])
#which(hm$model$Mortality == 0.000163, arr.ind = TRUE)

for (i in 1:30){
  print(which(hm2$model$Mortality == min(hm2$model$Mortality[i,]), arr.ind = TRUE))
}

#x = 20, t = 1995
a2 = lc_model2$ax[20]+lc_model2$bx[20]*lc_model2$kt[6]
a2

# Residual
residue2 = exp(lc_model2[["fitted"]][["y"]])
n_residue2 = residue2 - data3
lc2 = subset(n_residue2, select = -x)

# Heatmap
heatmap_data2 = as.matrix(lc2)
heatmap(heatmap_data2, Rowv = NA, Colv = NA,
        col = colorRampPalette(c("green", "yellow", "red"))(1000),
        main = "Heatmap of lc",
        xlab = "X-axis Label",
        ylab = "Y-axis Label",
        scale = "none")
write_xlsx(lc2, "C:\\Users\\seraf\\Documents\\lc_data2.xlsx")
#---------------------------------------------------------------------------------------
# Forecasting Lee Carter (new miu)
library(TSA)
kt2 = lc_model2$kt
kt_ts2 = ts(kt2)
plot(kt_ts2)

auto.arima(kt_ts2) 

mod2 = Arima(kt_ts, order = c(0,1,0), include.drift = TRUE)
predict2 = forecast(mod2, h = 10)
new_kt2 = cbind(mod2$fitted, predict2$mean)
plot(new_kt2)

new_kt2 = matrix(new_kt2, nrow = 99, ncol = 10)
dim(new_kt2)

predict2$mean
vector2 = matrix(predict2$mean, nrow = 99, ncol = 10)

new_miu2 = lc_model2$ax + lc_model2$bx * vector2
nmiu2 = exp(new_miu2)
nmiu2
nmiu2 = as.data.frame(nmiu2)
nmiu2
write_xlsx(nmiu2, "C:\\Users\\62821\\Documents\\miu (tau).xlsx")

#-------------------------------------------------------------------
# New qx (tau)
qx_tau = 1 - exp(-nmiu2)
write_xlsx(qx_tau, "C:\\Users\\62821\\Documents\\qx_tau.xlsx")

