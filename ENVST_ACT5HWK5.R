#### Activity 5----- 
# read in greenhouse gas data from reservoirs
ghg <- read.csv("/cloud/project/activity05/Deemer_GHG_Data.csv")

ETdat <- read.csv("/cloud/project/activity05/activity06/ETdata.csv")


#load packages 
#install.packages(c("dplyr","ggplot2","olsrr","PerformanceAnalytics", "forecast"))

library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)
library(forecast)


# log transform methane fluxes
ghg$log.ch4 <- log(ghg$ch4+1)

ghg$log.age <- log(ghg$age)
ghg$log.DIP <- log(ghg$DIP+1)
ghg$log.precip <- log(ghg$precipitation)

unique(ghg$Region) 

# binary variable for boreal region
ghg$BorealV <- ifelse(ghg$Region == "Boreal",1,0)
# binary variable for tropical region
ghg$TropicalV <- ifelse(ghg$Region == "Tropical",1,0)

# binary variable for alpine region
ghg$AlpineV <- ifelse(ghg$Alpine == "yes",1,0)

# binary variable for known hydropower
ghg$HydroV <- ifelse(ghg$hydropower == "yes",1,0)

# multiple regression
# creates a model object
mod.full <- lm(log.ch4 ~ airTemp+
                 log.age+mean.depth+
                 log.DIP+
                 log.precip+ BorealV, data=ghg) #uses the data argument to specify dataframe

summary(mod.full) 

#checking assumptions 
res.full <- rstandard(mod.full)
fit.full <- fitted.values(mod.full)

# qq plot
qqnorm(res.full, pch=19, col="grey50")
qqline(res.full)

# shapiro-wilks test
shapiro.test(res.full)

plot(fit.full,res.full, pch=19, col="grey50")
abline(h=0)

# isolate continuous model variables into data frame:

reg.data <- data.frame(ghg$airTemp,
                       ghg$log.age,ghg$mean.depth,
                       ghg$log.DIP,
                       ghg$log.precip)

# make a correlation matrix 
chart.Correlation(reg.data, histogram=TRUE, pch=19)

# run stepwise
full.step <- ols_step_forward_aic(mod.full)
# view table
full.step 

# check full model
full.step$model

# plot AIC over time
plot(full.step )

# prediction with interval for predicting a point
predict.lm(mod.full, data.frame(airTemp=20,log.age=log(2),
                                mean.depth=15,log.DIP=3,
                                log.precip=6, BorealV=0),
           interval="prediction")

# look at prediction with 95% confidence interval of the mean

predict.lm(mod.full, data.frame(airTemp=20,log.age=log(2),
                                mean.depth=15,log.DIP=3,
                                log.precip=6, BorealV=0),
           interval="confidence")








####Activity 6---- 
unique(ETdat$crop)


#install.packages(c("lubridate"))

library(lubridate)


# average fields for each month for almonds
almond <- ETdat %>% # ET data
  filter(crop == "Almonds") %>% # only use almond fields
  group_by(date) %>% # calculate over each date
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE)) # average fields

# visualize the data
ggplot(almond, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

# almond ET time series
almond_ts <- ts(almond$ET.in, # data
                start = c(2016,1), #start year 2016, month 1
                #first number is unit of time and second is observations within a unit
                frequency= 12) # frequency of observations in a unit


# decompose almond ET time series
almond_dec <- decompose(almond_ts)
# plot decomposition
plot(almond_dec)

acf(na.omit(almond_ts), # remove missing data
    lag.max = 24) # look at 2 years (24 months)

pacf.plot <- pacf(na.omit(almond_ts))


almond_y <- na.omit(almond_ts)
model1 <- arima(almond_y , # data 
                order = c(1,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model1

model4 <- arima(almond_y , # data 
                order = c(4,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model4


# calculate fit
AR_fit1 <- almond_y - residuals(model1) 
AR_fit4 <- almond_y - residuals(model4)
#plot data
plot(almond_y)
# plot fit
points(AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","darkgoldenrod4"),
       bty="n")

newAlmond <- forecast(model4)
newAlmond

#make dataframe for plotting
newAlmondF <- data.frame(newAlmond)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newAlmondF$dateF <- ymd(paste(years,"/",month,"/",1))


# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = almond, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(almond$date[1]),newAlmondF$dateF[24])+  # Plotting original data
  geom_line(data = newAlmondF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newAlmondF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")










########Homework 5------- 
#question 1
ghg_trans.co2 <- ((1)/(ghg$co2+1000))

mod.full <- lm(ghg_trans.co2 ~ 
                 mean.depth+
                 airTemp+
                 log.age+
                 log.DIP+
                 log.precip+
                 log.ch4,
                 data = ghg)
summary(mod.full)
str(mod.full)

regTable <- summary(mod.full)$coefficients 
write.csv(regTable, "/cloud/project/reg_out.csv") #more-> export 






#question 2 - Pistachios

ETall <- ETdat %>%
  group_by(date, crop) %>%
  summarise(ET.in = mean(Ensemble.ET))


Pistachios <- ETall %>% 
  filter(crop == "Pistachios")


Pistachios_ts <- na.omit(ts(Pistachios$ET.in,
                    start = c(2016,1),
                    frequency = 12))

Pistachio_decompose <- decompose(Pistachios_ts)
plot(Pistachio_decompose)
    
Pistachio_decompose

acf(na.omit(ts(Pistachios$ET.in),
               start=c(2016,1),
               frequency=12),
    main = "Pistachio",
    lag.max=24)

pacf(na.omit(ts(Pistachios$ET.in),
            start=c(2016,1),
            frequency=12),
    lag.max=24)

ModelPista <- arima(Pistachios_ts,
                order=c(1,0,0))






#question 2 - almonds 
Almonds <- ETall %>% 
  filter(crop == "Almonds")


Almonds_ts <- na.omit(ts(Almonds$ET.in,
                            start = c(2016,1),
                            frequency = 12))

Almonds_decompose <- decompose(Almonds_ts)
plot(Almonds_decompose)
Almonds_decompose

acf(na.omit(ts(Almonds$ET.in),
            start=c(2016,1),
            frequency=12),
    main = "Almonds",
    lag.max=24)

pacf(na.omit(ts(Almonds$ET.in),
             start=c(2016,1),
             frequency=12),
     lag.max=24)

ModelAlmonds <- arima(Almonds_ts,
                order=c(1,0,0))






#question 2 - fallow/idle fields
FIC <- ETall %>% 
  filter(crop == "Fallow/Idle Cropland")


FIC_ts <- na.omit(ts(FIC$ET.in,
                         start = c(2016,1),
                         frequency = 12))

FIC_decompose <- decompose(FIC_ts)
plot(FIC_decompose)
FIC_decompose

acf(na.omit(ts(FIC$ET.in),
            start=c(2016,1),
            frequency=12),
    main = "Fallow and Idle Cropland",
    lag.max=24)

pacf(na.omit(ts(FIC$ET.in),
             start=c(2016,1),
             frequency=12),
     lag.max=24)

ModelFIC <- arima(FIC_ts,
                order=c(1,0,0))





#question 2 - corn 
Corn <- ETall %>% 
  filter(crop == "Corn")


Corn_ts <- na.omit(ts(Corn$ET.in,
                            start = c(2016,1),
                            frequency = 12))

Corn_decompose <- decompose(Corn_ts)
plot(Corn_decompose)
Corn_decompose

acf(na.omit(ts(Corn$ET.in),
            start=c(2016,1),
            frequency=12),
    main = "corn",
    lag.max=24)

pacf(na.omit(ts(Corn$ET.in),
             start=c(2016,1),
             frequency=12),
     lag.max=24)

ModelCorn <- arima(Corn_ts,
                order=c(1,0,0))






#Question 2 - Table Grapes
Grapes <- ETall %>% 
  filter(crop == "Grapes (Table/Raisin)")


Grapes_ts <- na.omit(ts(Grapes$ET.in,
                      start = c(2016,1),
                      frequency = 12))

Grapes_decompose <- decompose(Grapes_ts)
plot(Grapes_decompose)
Grapes_decompose

acf(na.omit(ts(Grapes$ET.in),
            start=c(2016,1),
            frequency=12),
    main = "Grapes",
    lag.max=24)

pacf(na.omit(ts(Grapes$ET.in),
             start=c(2016,1),
             frequency=12),
     lag.max=24)

Model5 <- arima(Grapes_ts,
                order=c(1,0,0))









#question 3 - Pistachios
pacf.plot <- pacf(na.omit(Pistachios_ts))

Pista_y <- na.omit(Pistachios_ts)
modelPista <- arima(Pista_y , # data 
                order = c(1,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
modelPista



modelPistaF <- arima(Pista_y , # data 
                order = c(4,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
modelPistaF


newPista <- forecast(modelPistaF)
newPista


#make dataframe for plotting
newPistaF <- data.frame(newPista)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newPistaF$dateF <- ymd(paste(years,"/",month,"/",1))


# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = Pistachios, aes(x = ymd(date), y = ET.in))+
  ggtitle("Pistachio Forcast")+
  xlim(ymd(Pistachios$date[1]),newPistaF$dateF[24])+  # Plotting original data
  geom_line(data = newPistaF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newPistaF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")






#question 3 - Fallow/ Idle
pacf.plot <- pacf(na.omit(FIC_ts))

FIC_y <- na.omit(FIC_ts)
modelFIC <- arima(FIC_y , # data 
                order = c(1,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
modelFIC


modelFICF <- arima(FIC_y , # data 
                order = c(4,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
modelFICF

newFIC <- forecast(modelFICF)
newFIC

#make dataframe for plotting
newFICF <- data.frame(newFIC)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newFICF$dateF <- ymd(paste(years,"/",month,"/",1))


# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = FIC, aes(x = ymd(date), y = ET.in))+
  ggtitle("Fallow/Idle Fields Forcast")+
  xlim(ymd(FIC$date[1]),newFICF$dateF[24])+  # Plotting original data
  geom_line(data = newFICF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newFICF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")
