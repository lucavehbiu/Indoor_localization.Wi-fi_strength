#Project - Wi-fi, Indoor localization using signal strengths
  
#Luca Vehbiu
  
#28-01-2019



#Import, cleaning and pre-processing

##Load libraries
library(pacman)
p_load(dplyr, ggplot2, readr, lubridate, anytime, tidyr, caret, gridExtra, modelr, mlbench, plotly, gridExtra)
setwd("/Users/Luca/Desktop")

#Read and load data
data <- read_csv("trainingData.csv")
validation_set <- read.csv("validationData.csv")

load("no.outliers.RDA")
load("validationWAP.RDA")
load("luca.RDA")
load('train.RDA') 
load('test.RDA')



##Convert timestamp
data$TIMESTAMP <- anytime(data$TIMESTAMP)
data <- data %>% mutate(day = weekdays(TIMESTAMP), date = date(TIMESTAMP))

valid$TIMESTAMP <- anytime(valid$TIMESTAMP)
valid <- valid %>% mutate(date = date(TIMESTAMP), day = weekdays(TIMESTAMP))


##Remove outliers; signal strength of WAPS
outlier <- data
outlier_right <- outlier[, 521:529]
outlier_left <- outlier[, 1:520]

outlier_left <- as.data.frame(
  lapply(outlier_left, function(x){ifelse(x >= -25 & x <=0 ,  NA, x+0)} ))

outlier_left <- as.data.frame(
  lapply(outlier_left, function(x){ifelse(x > -105 & x <= -103 ,  NA, x+0)} ))

outlier_left <- as.data.frame(lapply(outlier_left, function(x){ifelse(x == -105 ,  -103 , x+0)} ))

outlier <- cbind(outlier_left, outlier_right) #bind to have all variables in one data.frame
outlier <- drop_na(outlier) #drop outliers

valid <- as.data.frame(lapply(valid, function(x) {ifelse(x == 100, -103, x+0)})) #put -105 as -103 for validation


##Remove zero variance variables(WAPS)
valid.zero_var <- which(
  apply(
    valid[, 1:520], 2, function(x) all(x == -103)) == T) #zero var WAPS from validation

valid <- valid[, -valid.zero_var] #remove them from validation
new.data <- new.data[, -valid.zero_var] #remove them from training set

data.zero_var <- which(
  apply(
    new.data, 2, function(x) all(x == -103)) == T) #zero WAPS from training set

valid <- valid[, -data.zero_var] #remove them from validation
new.data <- new.data[, -data.zero_var] #remove them from training set


##Group variables by longitude and latitude to remove duplicates (coordinates) by averaging them
no.dupli <- new.data  #do it for training set
no.dupli <- transform(no.dupli, 
                      Cluster_ID = as.numeric(interaction(LATITUDE, LONGITUDE, drop= T))) %>% 
  group_by(Cluster_ID, FLOOR) %>% 
  summarise_if(is.numeric, mean)

no.dupli <- dplyr:: ungroup(no.dupli, Cluster_ID)


no.dupli_valid <- valid #do it for the validation set as well
no.dupli_valid <- transform(no.dupli_valid, 
                            Cluster_ID = as.numeric(interaction(LATITUDE, LONGITUDE, drop= T))) %>% 
  group_by(Cluster_ID, FLOOR, BUILDINGID) %>% 
  summarise_if(is.numeric, median)

no.dupli_valid <- dplyr:: ungroup(no.dupli_valid, Cluster_ID)


##Scale and center by rows
logged.and.scaled <- abs(no.dupli)  # conver all positive values
logged.and.scaled[, 3:314] <- log(logged.and.scaled[, 3:314]) #log the WAP strengths

logged.and.scaled_right <- logged.and.scaled %>% 
  select(BUILDINGID, FLOOR, LONGITUDE, LATITUDE, PHONEID, USERID, RELATIVEPOSITION, TIMESTAMP) #divide them

logged.and.scaled <-  as.data.frame(t(apply(logged.and.scaled[,3:314], 1, 
                                            function(x) { (x-mean(x)) / sd(x)}))) #scale and center each row

logged.and.scaled <- bind_cols(logged.and.scaled, logged.and.scaled_right) 
#bind them, to have all col. in one  data.frame


#do it for validation as well
logged.and.scaled_valid <- abs(no.dupli_valid)
logged.and.scaled_valid[, 4:315] <- log(logged.and.scaled_valid[, 4:315]) #log 

logged.and.scaled_valid_right <- logged.and.scaled_valid %>% 
  select(SPACEID, PHONEID, USERID, RELATIVEPOSITION, TIMESTAMP, BUILDINGID, FLOOR, LONGITUDE, LATITUDE) 
#divide them


logged.and.scaled_valid <- as.data.frame(t(apply(logged.and.scaled_valid[,4:315], 1, 
                                                 function(x) {(x - mean(x)) / sd(x)}))) 
#scale and center each row


logged.and.scaled_valid <- bind_cols(logged.and.scaled_valid, logged.and.scaled_valid_right) 
#bind them











































