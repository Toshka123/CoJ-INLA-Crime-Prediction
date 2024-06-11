
############################################################################################################################################
############################################################################################################################################


# integer-valued first-order autoregressive process (INAR(1))
# AR1 fitted with INLA
# RW1 can also be fitted.




##############################################################################################################################################


# packages to install

install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLA)

#inla.list.models()
install.packages("sf")
install.packages("rgdal")
install.packages("maptools")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("kableExtra")
install.packages("sp")
install.packages("tmap")
install.packages("viridis")
install.packages("corrplot")
install.packages("gghighlight")
install.packages("spacetime")
install.packages("lubridate")
install.packages("MASS")
install.packages("lmtest")
install.packages("FRK")
install.packages("jtools")
install.packages("viridisLite")
install.packages("usethis")
install.packages("devtools")
install.packages("DClusterm")
install.packages("maps")
install.packages("tibble")

# libraries
library(tibble)
library(maps)
library("devtools")
library(tidyverse)
# Nice tables
library(kableExtra)
# Simple features (a standardised way to encode vector data ie. points, lines, polygons)
library(sf) 
# Spatial objects conversion
library(sp) 
# Thematic maps
library(tmap) 
# Nice colour schemes
library(viridis) 
# Obtain correlation coefficients
library(corrplot)
# Highlight data on plots
library(gghighlight)
# Analysing spatio-temporal data
#library(STRbook)
library(spacetime)
# Date parsing and manipulation
library(lubridate)
# Applied statistics
library(MASS)
# Statistical tests for linear regression models
library(lmtest)
# Fit spatial random effects models
library(FRK)
# Exportable regression tables
library(jtools)
library('sf')
library(rgdal)
library(maptools)
library(dplyr)
library(ggplot2)
library(INLA)
library(Matrix)
library(foreach)
library(parallel)
library(sp)
library('sf')
library(rgdal)
library(maptools)
library(dplyr)
library(ggplot2)
library(jtools)
library("devtools")
library(MASS) 
library("DClusterm")
library("parallel")
library("spacetime")
library("DCluster")
library("boot")
library("spdep")
library("spData")
#x <- readOGR(dsn=".", layer="coj_police_bounds")

###############################################################################

################################################################### load data ###########################################################


setwd("D:\\Rdata1")                  # create a working directory
mydata <- read.csv("SAPS_quarterly_data_CoJ_average.csv", header=T)
#newdata <- read.csv("newdata.csv", header=T)
#View(mydata)
attach(mydata)
mydata

hist(mydata$Average_crime)
hist(mydata$Average_crime)

###############################################################################


##################################################################################


#Check the distribution of case.names

hist(mydata$Average_crime)


############################################################################################################################################

# Load shapefile
shapename <- readOGR("C:/Users/PMokilane/OneDrive - csir.co.za/Desktop/Crime monitoring tool - material/Joburg police GIS data", "coj_police_bounds")
shapename
plot(shapename)

glimpse(shapename)

########################################################################################

# preparation of dataset 
# The number of stations ad quarters specified

######################################################################################### 
station=43
quarter=4
mu = matrix(nrow=43, ncol=4)
sd_env = 47.29463 # 
##########################################################################################

# selecting variables from the imported dataset

############################################################################################

env  = matrix(rnorm(quarter*station, mean=89.42595, sd=sd_env), nrow=quarter, ncol=station)

dataset <- data.frame(
  Average_crime = Average_crime,
  log_crime=log(Average_crime),
  quarter = rep(seq_len(quarter), station),
  station = rep(seq_len(station), each = quarter),
  set_data=set_data,
  lon=lon,
  lat=lat,
  quarter1,
  quarter2,
  quarter3,
  quarter4,
  Quantile,
  Crime_stats,
  station_name,
  population=as.numeric(pop),
  #LocID,
  env = as.vector(env))
env


############################################################################################

# subseting data for model training purposes

############################################################################################

dataset1<-dataset[1:817,]

dataset1

# calculate average
mean(dataset1$Average_crime)
## [1] 89.42595
# calculate stardard deviation
sd(dataset1$Average_crime)
# 47.29463
##############################################################################################

# The number of stations ad quarters specified

##############################################################################################

station=43
quarter=4
mu = matrix(nrow=43, ncol=4)

sd_env = 47.29463 # the env

##############################################################################################
# variable env was never used in the model but just kept in the data

##############################################################################################


env  = matrix(rnorm(station*quarter, mean=89.42595, sd=sd_env), nrow=quarter, ncol=station)
env

dataset <- data.frame(
  Average_crime = Average_crime,
  log_crime=log(Average_crime),
  quarter = rep(seq_len(quarter), station),
  station = rep(seq_len(station), each = quarter),
  set_data=set_data,
  lon=lon,
  lat=lat,
  quarter1,
  quarter2,
  quarter3,
  quarter4,
  Quantile,
  Crime_stats,
  station_name,
  population=as.numeric(pop),
  #LocID,
  env = as.vector(env))

###################################### Subseting ##########################################################


# subset data so that the Average_crime values are replaced by "NA" for model testing

#######  These are only 4th quarter of the lastest year ##################################################

##########################################################################################################

s04to04<-dataset[818:860,]
s04to04

###############################################################################################

s04to05 <- data.frame(
  Average_crime = NA,
  quarter = rep(seq_len(quarter), station),
  log_crime=log(Average_crime),
  station = rep(seq_len(station), each = quarter),
  set_data=set_data,
  lon=lon,
  lat=lat,
  quarter1,
  quarter2,
  quarter3,
  quarter4,
  #Avg_per_station=Avg_per_station,
  Quantile,
  Crime_stats,
  station_name,
  population=as.numeric(pop),
  #LocID,
  env = as.vector(env))
env

# Replace Average_crime in this data testing dataset

s04to04<-s04to05[818:860,]
s04to04

#####################The integrated nested Laplace approximation (INLA)       #######################################################
############################### combine the two data sets and  run the model  #######################################################

appendedDf <- rbind(dataset1, s04to04)
appendedDf
length(appendedDf)
tail(appendedDf)
head(appendedDf)


###########################################################################################################

#model for prediction
# Quantile === grouped stations according to levels of crime (1, 2 , 3 , 4)

#model.ar1 <- inla(Average_crime ~ 1 + Quantile + f(quarter, model = "ar1", replicate = station, 
#                                                  hyper = list(prec = list(param = c(10, 100)))), data = appendedDf,
#                control.predictor = list(compute = TRUE),
#               control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
#              control.family = list(hyper = list(prec = list(param = c(10, 100)))),
#             family = "gaussian" )




######################################## random work model ###########################################################


#model.ar2 <- inla(Average_crime ~ 1 + Quantile + f(quarter, model = "rw1", replicate = station, 
#                                                  hyper = list(prec = list(param = c(10, 100)))), data = appendedDf,
#                control.predictor = list(compute = TRUE),
#               control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
#              control.family = list(hyper = list(prec = list(param = c(10, 100))),
#                                   family = "gaussian"))

########################################################################## RW1 ###################################################################

#nm.adj <- poly2nb(shapename)
#adj.mat <- as(nb2mat(nm.adj, style = "B"), "Matrix")

#model.ar2 <- inla(Average_crime ~ Quantile + f(quarter, model = "rw1", replicate = station, hyper = list(prec = list(param = c(10, 100))))+
#                   f(as.numeric(station), model = "besag", graph = adj.mat, hyper = list(prec = list(param = c(10, 100)))),E= Expected, data= appendedDf,
#                control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE),
#               control.family = list(hyper = list(prec = list(param = c(10, 100)))),
#              family = "gaussian")

#fittedValues<-model.ar2$summary.fitted.values$mean
#fittedValues
#COMPNT_NM

#out<-data.frame(cbind(appendedDf$station_name,fittedValues,model.ar2$summary.fitted.value$"0.025quant", model.ar2$summary.fitted.value$"0.5quant", model.ar2$summary.fitted.value$"0.975quant",appendedDf$quarter,appendedDf$station,appendedDf$population, appendedDf$Crime_stats))
#out_sample<-out[818:860,]#
#names(out)<-c("COMPNT_NM","fittedValues","0.025quant","0.5quant","0.975quant","quarter","station","population","Crime_stats")
#write.csv(out,"INLA_output_bases7.csv")  # writing the file out to excel
#out
#names(out)
#Subset only out of sample predictions


l#ength(out)

#out_sample<-out[818:860,]
#out_sample


#library(Metrics)

#mape(as.numeric(out_sample$Crime_stats), as.numeric(out_sample$fittedValues))

######################################## spatio - temporal model ### chosen model #################################################################

#### Assumption, the covariance structure is separable (spatial and temporal)
################# we also assume adjacency, The spatial relationship follows a Gaussian process (average of polygons sharing borders with, variance=proportion of precision)


# data(appendedDf)##### type=B or type= W

nm.adj <- poly2nb(shapename)
adj.mat <- as(nb2mat(nm.adj, style = "B"), "Matrix")
#adj.mat

model.ar2 <- inla(Average_crime ~ Quantile + f(quarter, model = "ar1", replicate = station, hyper = list(prec = list(param = c(10, 100))))+
                    f(as.numeric(station), model = "besag", graph = adj.mat, hyper = list(prec = list(param = c(10, 100)))),E= Expected, data= appendedDf,
                  control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE),verbose=TRUE,
                  control.family = list(hyper = list(prec = list(param = c(10, 100)))),
                  family = "gaussian")



fittedValues<-model.ar2$summary.fitted.values$mean
fittedValues
COMPNT_NM

out<-data.frame(cbind(appendedDf$station_name,fittedValues,model.ar2$summary.fitted.value$"0.025quant", model.ar2$summary.fitted.value$"0.5quant", model.ar2$summary.fitted.value$"0.975quant",appendedDf$quarter,appendedDf$station,appendedDf$population, appendedDf$Crime_stats))
#out_sample<-out[818:860,]#
names(out)<-c("COMPNT_NM","fittedValues","0.025quant","0.5quant","0.975quant","quarter","station","population","Crime_stats")
write.csv(out,"INLA_output_bases7.csv")  # writing the file out to excel
out
names(out)
#Subset only out of sample predictions


length(out)

out_sample<-out[818:860,]
out_sample


library(Metrics)
mape(as.numeric(out_sample$Crime_stats), as.numeric(out_sample$fittedValues))

###################################################################################check predictions at this records############

model.ar2$summary.fitted.values[818,]
model.ar2$summary.fitted.values[819 ,]
model.ar2$summary.fitted.values[820,]
model.ar2$summary.fitted.values[821 ,]

summary(model.ar2) 
out_sample1<-model.ar2[1:817,]

plot(model.ar2)

model.ar2$cpo$cpo
model.ar2$cpo$pit
hist(model.ar2$cpo)

plot(model.ar2$marginals.fixed[[1]], type = "l", 
     xlab = expression(Intercept), ylab = "density")


##################################
model.ar2$summary.fixed
summary(model.ar2)


names(crimes)
summary(crimes)
fittedValues<-model.ar2$summary.fitted.values$mean
fittedValue<- cbind(model.ar2$summary.fitted.values$mean,model.ar2$summary.fitted.value$"0.025quant", model.ar2$summary.fitted.value$"0.5quant", model.ar2$summary.fitted.value$"0.975quant")
fittedValue


length(fittedValues)
length(model.ar2$Average_crime)

#predict(model, newdata=s04to04, interval = "confidence", se=TRUE)
s04to04

out<-data.frame(cbind(appendedDf$station_name,fittedValues,model.ar2$summary.fitted.value$"0.025quant",model.ar2$summary.fitted.value$"0.5quant",model.ar2$summary.fitted.value$"0.975quant", appendedDf$Crime_stats,appendedDf$quarter,appendedDf$station, appendedDf$population))
#out_sample<-out[818:860,]#
names(out)<-c("COMPNT_NM","fittedValues","0.025quant","0.5quant","0.975quant","Crime_stats","quarter","station","population")
write.csv(out,"INLA_output_bases7.csv")  # writing the file out to excel
out
names(out)
#Subset only out of sample predictions

out_sample<-out[818:860,]
out_sample

attach(out_sample)
out_sample<- out_sample[-c(16,20),]

test<-out_sample[c('fittedValues', 'Crime_stats', 'station','quarter','population')]
#newdata <- test[818:860,]
#attach(newdata)
attach(test)
View(test)
test<- test[-c(16,20),]
attach(test)
test

#class(test$population)
#as.numeric(test$population)
#test$pop_size=test$fittedValues/as.numeric(test$population)
#test$predicted_rates= 10000*(fittedValues/population)
#test$crime_rates= 10000*(test1$Crime_stats/(test1$pop))

class(fittedValues)

######################################################## comparing predicted values with actual crime count #########################


plot(test$fittedValues, test$Crime_stats, main="Scatterplot",ylab="predicted crime",xlab="actual crime stats", pch=1)
abline(coef = c(-17, 1))


shapename
head(shapename$COMPNT_NM)
head(out_sample$station_name)
################################################  merge  #################################################################################
out_sample

mergedData <- merge(shapename,out_sample, by=c('nam' = 'COMPNT_NM'))
mergedData

#mergedData <- shapename %>% 
# merge(out_sample,by='COMPNT_NM')
#mergedData %>% as.data.frame %>%  filter(!is.na(fittedValues)) %>% nrow 

glimpse(mergedData)
glimpse(out_sample)
glimpse(shapename)

####################################################### Plotting two maps "actual crime" and "predicted" #############################################################################

plot(mergedData)
invisible(text(coordinates(mergedData), labels=as.character(mergedData$COMPNT_NM), cex=0.6))

spplot(mergedData, zcol='fittedValues')
spplot(mergedData, zcol='Crime_stats')


############################################################### other possible models###########################

################################################################################################################

##################################################################

# build basis functions

G <- auto_basis(data = mydata[,c("lon","lat")] %>%
                  SpatialPoints(),           # To sp obj
                nres = 1,                         # One resolution
                type = "Gaussian")                # Gaussian BFs

# basis functions evaluated at data locations are then the covariates

S <- eval_basis(basis = G,                       # basis functions
                s = mydata[,c("lon","lat")] %>%
                  as.matrix()) %>%            # conv. to matrix
  as.matrix()                                 # conv. to matrix
colnames(S) <- paste0("B", 1:ncol(S)) # assign column names
S

# selecting variables

reg_df <- cbind(mydata, S) %>%
  dplyr::select(station,quarter,lon, Quantile,lat, Average_crime, B1:B12)## correct
reg_df

# subseting data for model training

#s01to03<- subset(reg_df, quarter=="Q1" & quarter=="Q2",  select=c(station,quarter,lon, lat, Average_crime, B1:B12))
#s01to03

s01to03 <- reg_df[1:817,]
s01to03

##################################################################

# Fit Linear Regression

##################################################################

eq1 <- Average_crime ~ lon + lat + .
lm_m <- lm(formula = eq1, 
           data = dplyr::select(s01to03, -station))
lm_m %>% summary()

##################################################################

# estimate a poisson model

##################################################################

poisson_m1 <- glm(eq1,
                  family = poisson("log"), # Poisson + log link
                  data = dplyr::select(s01to03, -station))
poisson_m1 %>% summary()

# subset data for testing

#s04to04<- subset(reg_df, quarter=="Q4" & quarter== "Q4",  select=c(station,quarter,lon, lat, B1:B12))
#s04to04

s04to04 <- reg_df[818:860,]
s04to04

# use 'predict()' to run model on new data
pred<- predict(poisson_m1, newdata = s04to04, type = "response")
pred

appendeding <- cbind(s04to04,pred)
appendeding

library(Metrics)
mape(as.numeric(appendeding$Average_crime), as.numeric(appendeding$pred))

plot(appendeding$pred, appendeding$Average_crime, main="Scatterplot",ylab="predicted crime",xlab="actual crime stats", pch=1)

########################################################## ploting   #########################################
##################################################################################################################
####################################################### Plotting two maps "actual crime" and "predicted" #############################################################################

mergedData <- merge(shapename,out_sample, by=c('nam' = 'COMPNT_NM'))
#mergedData

dim(shapename)
dim(out_sample)
#mergedData <- shapename %>% 
# merge(out_sample,by='COMPNT_NM')
#mergedData %>% as.data.frame %>%  filter(!is.na(fittedValues)) %>% nrow 

#glimpse(mergedData)
#glimpse(out_sample)
#glimpse(shapename)

library(shiny)
library(ggplot2)
library(sp)
library(sf)
library(geoR)
library(gstat)
#plot(mergedData)
A3<- c("EVA", "HIL", "MEA", "DED", "DIE", "PRI", "ALB", "ELD", "GER", "KAG",  "RAN" ,"ALE" , "BED","BRA", "MID", "SAN", "LAN", "MON", "PRO", "ROS", "BRI", "MUL","ENN", "JAB", "KLI", "ERA", "DOO", "BRA", "KRU", "SAN","SAND", "EDE", "SOP", "DIE", "OLI", "TEM" , "LEN", "LENS", "IVO", "DOU", "CLE", "MOR", "LEN", "ROO", "HON","FLO", "JHB", "FAI", "BEK", "JEP", "BOO", "ORL", "ORA", "RAN", "LIN", "DOB", "RAB", "NOR", "PAR", "YEO", "MOF", "OLI", "KLI", "TEM" , "NAL", "SEB")         
mergedData1 <- mergedData[which(mergedData@data$Area %in% A3),]
#mergedData1
AreaL = list("sp.text", coordinates(mergedData1), as.character(mergedData1@data$Area),col="white", cex=0.7,font=2)
#AreaL
#View(mergedData1)
spplot(mergedData1, zcol='fittedValues', sp.layout=list(AreaL),col="black", cex=0.7,font=2)
#spplot(mer

###############################################################################################################
###############################################################################################################


#test dispersion

poisson_m1$deviance / poisson_m1$df.residual

#value 19.79438 is creater than one

# data to output

data <- data.frame(station= s04to04$station, predicted=predict(poisson_m1, newdata = s04to04, type = "response"))

# Output predicted data

names(data)<-c("station","predicted")
write.csv(data,"Crime_predic_output_poisson.csv")  # writing the file out to excel

plot(appendeding$fittedValues, appendeding$Crime_stats, main="Scatterplot",ylab="predicted crime",xlab="actual crime stats", pch=1)

############################################################################################################################

# to take care of dispersion we use

# estimate a quasipoisson model

##################################################################

qpoisson_m1 <- glm(eq1,
                   family = quasipoisson("log"), # QuasiPoisson + log link
                   data = dplyr::select(s01to03, -station))
qpoisson_m1 %>% summary()

s01to03


# data to output

data <- data.frame(station= s04to04$station, predicted=predict(qpoisson_m1, newdata = s04to04, type = "response"))
data

# Output predicted data

names(data)<-c("station","predicted")
write.csv(data,"Crime_predic_output2.csv")  # writing the file out to excel

##################################################################

# estimate a negative binomial model, another option

##################################################################

nb_m1 <- glm.nb(eq1, 
                data = dplyr::select(s01to03, -station))
nb_m1

# use 'predict()' to run model on new data
pred<- predict(nb_m1, newdata = s04to04, type = "response")
pred

##################################################################




#Include basis funcions

###################################################################


################################################################### load data ###########################################################


setwd("D:\\Rdata1")                  # create a working directory
mydata <- read.csv("SAPS_quarterly_data_CoJ_average.csv", header=T)
#newdata <- read.csv("newdata.csv", header=T)
#View(mydata)
attach(mydata)
mydata

hist(mydata$Average_crime)
hist(mydata$Average_crime)



###############################################################################

# build basis functions

G <- auto_basis(data = mydata[,c("lon","lat")] %>%
                  SpatialPoints(),           # To sp obj
                nres = 1,                         # One resolution
                type = "Gaussian")                # Gaussian BFs

# basis functions evaluated at data locations are then the covariates

S <- eval_basis(basis = G,                       # basis functions
                s = mydata[,c("lon","lat")] %>%
                  as.matrix()) %>%            # conv. to matrix
  as.matrix()                                 # conv. to matrix
colnames(S) <- paste0("B", 1:ncol(S)) # assign column names
S

# selecting variables


reg_df <- cbind(mydata, S) %>%
  dplyr::select(station_name,fin_year,year, quarter, locID, ipd, quarter1, quarter2, quarter3, quarter4,  lat,   lon, Average_crime, Avg,set_data, Quantile, Crime_stats, CLUSTER,   station, Area,   pop, Cluster4, cluster5, Quantile1,  station1, crime_count,Crime_pop, B1:B12)## correct
reg_df

appen <- cbind(mydata, S)
appen
# subseting data for model training

#s01to03<- subset(reg_df, quarter=="Q1" & quarter=="Q2",  select=c(station,quarter,lon, lat, Average_crime, B1:B12))
#s01to03

#s01to03 <- reg_df[1:817,]
s#01to03


##################################################################################


#Check the distribution of case.names

hist(mydata$Average_crime)


############################################################################################################################################

# Load shapefile
shapename <- readOGR("C:/Users/PMokilane/OneDrive - csir.co.za/Desktop/Crime monitoring tool - material/Joburg police GIS data", "coj_police_bounds")
shapename
plot(shapename)

glimpse(shapename)

########################################################################################

# preparation of dataset 
# The number of stations ad quarters specified

######################################################################################### 
station=43
quarter=4
mu = matrix(nrow=43, ncol=4)
sd_env = 47.29463 # 
##########################################################################################

# selecting variables from the imported dataset

############################################################################################

env  = matrix(rnorm(quarter*station, mean=89.42595, sd=sd_env), nrow=quarter, ncol=station)

reg_df <- data.frame(
  Average_crime = Average_crime,
  log_crime=log(Average_crime),
  quarter = rep(seq_len(quarter), station),
  station = rep(seq_len(station), each = quarter),
  set_data=set_data,
  lon=lon,
  lat=lat,
  quarter1,
  quarter2,
  quarter3,
  quarter4,
  Quantile,
  Crime_stats,
  station_name,
  population=as.numeric(pop),
  #LocID,
  appen$B1,
  appen$B2,
  appen$B3,
  appen$B4,
  appen$B5,
  appen$B6,
  appen$B7,
  appen$B8,
  appen$B9,
  appen$B10,
  appen$B11,
  appen$B12,
  env = as.vector(env))
env


############################################################################################

# subseting data for model training purposes

############################################################################################

dataset1<-reg_df[1:817,]

dataset1

# calculate average
mean(dataset1$Average_crime)
## [1] 89.42595
# calculate stardard deviation
sd(dataset1$Average_crime)
# 47.29463
##############################################################################################

# The number of stations ad quarters specified

##############################################################################################

station=43
quarter=4
mu = matrix(nrow=43, ncol=4)

sd_env = 47.29463 # the env

##############################################################################################
# variable env was never used in the model but just kept in the data

##############################################################################################


env  = matrix(rnorm(station*quarter, mean=89.42595, sd=sd_env), nrow=quarter, ncol=station)
env

dataset <- data.frame(
  Average_crime = Average_crime,
  log_crime=log(Average_crime),
  quarter = rep(seq_len(quarter), station),
  station = rep(seq_len(station), each = quarter),
  set_data=set_data,
  lon=lon,
  lat=lat,
  quarter1,
  quarter2,
  quarter3,
  quarter4,
  Quantile,
  Crime_stats,
  station_name,
  population=as.numeric(pop),
  #LocID,
  appen$B1,
  appen$B2,
  appen$B3,
  appen$B4,
  appen$B5,
  appen$B6,
  appen$B7,
  appen$B8,
  appen$B9,
  appen$B10,
  appen$B11,
  appen$B12,
  env = as.vector(env))

###################################### Subseting ##########################################################


# subset data so that the Average_crime values are replaced by "NA" for model testing

#######  These are only 4th quarter of the lastest year ##################################################

##########################################################################################################

s04to04<-reg_df[818:860,]
s04to04

###############################################################################################

s04to05 <- data.frame(
  Average_crime = NA,
  quarter = rep(seq_len(quarter), station),
  log_crime=log(Average_crime),
  station = rep(seq_len(station), each = quarter),
  set_data=set_data,
  lon=lon,
  lat=lat,
  quarter1,
  quarter2,
  quarter3,
  quarter4,
  #Avg_per_station=Avg_per_station,
  Quantile,
  Crime_stats,
  station_name,
  population=as.numeric(pop),
  #LocID,
  appen$B1,
  appen$B2,
  appen$B3,
  appen$B4,
  appen$B5,
  appen$B6,
  appen$B7,
  appen$B8,
  appen$B9,
  appen$B10,
  appen$B11,
  appen$B12,
  env = as.vector(env))
env

s04to05
# Replace Average_crime in this data testing dataset

s04to04<-s04to05[818:860,]
s04to04

#####################The integrated nested Laplace approximation (INLA)       #######################################################
############################### combine the two data sets and  run the model  #######################################################

appendedDf <- rbind(dataset1, s04to04)
appendedDf
length(appendedDf)
tail(appendedDf)
head(appendedDf)


###########################################################################################################

######################################## spatio - temporal model ### chosen model #################################################################

#### Assumption, the covariance structure is separable (spatial and temporal)
################# we also assume adjacency, The spatial relationship follows a Gaussian process (average of polygons sharing borders with, variance=proportion of precision)


# data(appendedDf)##### type=B or type= W

nm.adj <- poly2nb(shapename)
adj.mat <- as(nb2mat(nm.adj, style = "B"), "Matrix")
#adj.mat

model.ar2 <- inla(Average_crime ~ Quantile + appen.B1:appen.B12+f(quarter, model = "ar1", replicate = station, hyper = list(prec = list(param = c(10, 100))))+
                    f(as.numeric(station), model = "besag", graph = adj.mat, hyper = list(prec = list(param = c(10, 100)))),E= Expected, data= appendedDf,
                  control.predictor = list(compute = TRUE),control.compute = list(dic = TRUE, cpo = TRUE, waic = TRUE),
                  control.family = list(hyper = list(prec = list(param = c(10, 100)))),
                  family = "gaussian")



fittedValues<-model.ar2$summary.fitted.values$mean
fittedValues
COMPNT_NM

out<-data.frame(cbind(appendedDf$station_name,fittedValues,model.ar2$summary.fitted.value$"0.025quant", model.ar2$summary.fitted.value$"0.5quant", model.ar2$summary.fitted.value$"0.975quant",appendedDf$quarter,appendedDf$station,appendedDf$population, appendedDf$Crime_stats))
#out_sample<-out[818:860,]#
names(out)<-c("COMPNT_NM","fittedValues","0.025quant","0.5quant","0.975quant","quarter","station","population","Crime_stats")
write.csv(out,"INLA_output_bases7.csv")  # writing the file out to excel
out
names(out)
#Subset only out of sample predictions
out_sample<-out[1:817,]
mape(as.numeric(out_sample$Crime_stats), as.numeric(out_sample$fittedValues))



length(out)

out_sample<-out[818:860,]
out_sample


library(Metrics)

mape(as.numeric(out_sample$Crime_stats), as.numeric(out_sample$fittedValues))



