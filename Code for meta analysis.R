# R code for meta analysis
# Jiawei Zhang 
# Under the project of "Association between Long-Term Exposure to Air Pollution and COVID-19 Mortality: Investigation of potential collider bias"

# Install and Loading package ####
install.packages("readxl")
install.packages("meta")
install.packages("dmetar")
install.packages("dplyr")

library(readxl)
library(meta)
library(dmetar)
library(dplyr)

# Loading data ####
data <- read_excel("H:/PhD/Paper 1/Collider bias/Meta.xlsx")

# set working direction ####
setwd("H:/PhD/Paper 1/Collider bias/Res")
names(data)
str(data)

# Standarization of the effect estimate ####
# calculating STANDARD ERROR from 95% CONFIDENCE INTERVAL
# pay attention to the unit in original papers, and finally convert to study-specific IQR increase
# 1 ppb = 1.88 ug/m3 in NO2;

data$UniteIncreOri <- data$UniteIncre
data$UniteIncre <- ifelse(data$PolMetric=='ppb',data$UniteIncreOri*1.88,data$UniteIncreOri)

data$betaIQR <- (log(data$RR)/(data$UniteIncre))*data$PollutIQR
data$lowerIQR <- (log(data$LRR)/(data$UniteIncre))*data$PollutIQR
data$upperIQR <- (log(data$HRR)/(data$UniteIncre))*data$PollutIQR 
data$seIQR <- (data$upperIQR - data$lowerIQR)/(2*1.96)

data$Author <- paste0(sub(" .*","",data$FirstAuthor)," et al. ",data$Year)
data$Npopula <- format(data$Npop,big.mark=',')
data$Ncases <- format(round(data$Ncase),big.mark=',')

# NO2 ####
Data_NO2 <- subset(data, Pollutant=='NO2')
names(Data_NO2)

# random effect meta analysis 
m2_random_NO2 <- metagen(TE=betaIQR, 
                         seTE=seIQR,
                         data=Data_NO2,
                         studlab=paste(Author),
                         level = 0.95,
                         comb.fixed = F,
                         comb.random = T,
                         method.ci = "z",
                         method.tau = "SJ",
                         hakn = TRUE, 
                         sm="RR")

# stratified analysis 
sub2_NO2 <- update.meta(m2_random_NO2, 
                        byvar = Population, 
                        comb.random = TRUE, 
                        comb.fixed = FALSE)
sub2_NO2

# Plot
meta::forest(sub2_NO2,
             sortvar = 1 / Data_NO2$Npop,
             studlab = TRUE,
             comb.random = T,
             overall = T,
             prediction =F,
             subgroup = F,
             subgroup.hetstat = T
)

# PM2.5 ####
Data_PM25 <- subset(data, grepl('PM2.5',Pollutant))

# random effect meta analysis 
m2_random_PM25 <- metagen(TE=betaIQR,
                          seTE=seIQR,
                          data=Data_PM25,
                          studlab=paste(Author),
                          level = 0.95,
                          comb.fixed = F,
                          comb.random = T,
                          method.ci = "z",
                          method.tau = "SJ",
                          sm="RR"
)

# stratified analysis 
sub2_PM25 <- update.meta(m2_random_PM25, 
                         byvar = Population, 
                         comb.random = TRUE, 
                         comb.fixed = FALSE)
sub2_PM25

# Plot
meta::forest(sub2_PM25,
             sortvar = 1 / Data_PM25$Npop,
             studlab = TRUE,
             comb.random = T,
             overall = T,
             prediction = F,
             subgroup = F,
             subgroup.hetstat = T)