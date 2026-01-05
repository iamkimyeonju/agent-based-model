
library(readxl)
library(xts)
library(zoo)
library(lubridate)
library(dplyr)
library(schoolmath)
library(moments)
library(ggplot2)
library(tidyverse)

setwd("G:/My Drive/Multi-purpose-dam-inflow")


# Data Preprosessing

## 1. 보령댐 Inflow Data & Demand

for (i in 1998:2023) {assign(paste0("BR_",i),read_excel(paste0("BR_",i,".xlsx"),skip=2))}

BR <- rbind(BR_2023,BR_2022,BR_2021,BR_2020,BR_2019,BR_2018,BR_2017,BR_2016,BR_2015,BR_2014,
            BR_2013,BR_2012,BR_2011,BR_2010,BR_2009,BR_2008,BR_2007,BR_2006,BR_2005,
            BR_2004,BR_2003,BR_2002,BR_2001,BR_2000,BR_1999,BR_1998)


names(BR) = c("Date","Water Level of Dam(EL.m)","Water Level of spillway(EL.m)","Storage(MCM)","Storage rate(%)","Precipitation(mm)",
              "Inflow(m^3/s)","Natural Inflow(m^3/s)","Pipe Inflow(m^3/s)","Total Release(m^3/s)","Energy 1(m^3/s)","Hydrology(m^3/s)",
              "Agriculture(m^3/s)","Maintain River 2(m^3/s)","Convelve(m^3/s)","Energy 2(m^3/s)")

BR$`Natural Inflow(m^3/s)`[is.negative(BR$`Natural Inflow(m^3/s)`)] <- 0
BR$`Inflow(m^3/s)`[is.negative(BR$`Inflow(m^3/s)`)] <- 0
BR$`Inflow(m^3/s)` <- BR$`Inflow(m^3/s)`*60*60*24/1000000 
BR$Date <- as.Date(BR$Date,'%Y-%m-%d')

BR_inflow <- xts(BR$`Inflow(m^3/s)`,order.by = BR$Date) %>%
  apply.monthly(sum) 

Agr_br <- 4.7
Ind_br <- 90.6
Env_br <- 11.3

Total_demand_br <- Agr_br + Ind_br + Env_br

## 2. 용담댐 Inflow Data & Demand


for (i in 2001:2023) {assign(paste0("Yongdam_",i),read_excel(paste0("Yongdam_",i,".xlsx"),skip=2))}

Yongdam <- rbind(Yongdam_2023,Yongdam_2022,Yongdam_2021,Yongdam_2020,Yongdam_2019,Yongdam_2018,
                 Yongdam_2017,Yongdam_2016,Yongdam_2015,Yongdam_2014,Yongdam_2013,Yongdam_2012,
                 Yongdam_2011,Yongdam_2010,Yongdam_2009,Yongdam_2008,Yongdam_2007,Yongdam_2006,
                 Yongdam_2005,Yongdam_2004,Yongdam_2003,Yongdam_2002,Yongdam_2001)


names(Yongdam) = c("Date","Water Level of Dam(EL.m)","Water Level of spillway(EL.m)","Storage(MCM)","Storage rate(%)","Precipitation(mm)",
                   "Inflow(m^3/s)","Natural Inflow(m^3/s)","Total Release(m^3/s)","Energy 1(m^3/s)","Energy 2(m^3/s)","Hydrology(m^3/s)",1:3)

Yongdam$`Natural Inflow(m^3/s)`[is.negative(Yongdam$`Natural Inflow(m^3/s)`)] <- 0
Yongdam$`Inflow(m^3/s)`[is.negative(Yongdam$`Inflow(m^3/s)`)] <- 0
Yongdam$`Inflow(m^3/s)` <- Yongdam$`Inflow(m^3/s)`*60*60*24/1000000 
Yongdam$Date <- as.Date(Yongdam$Date,'%Y-%m-%d')

Yongdam_inflow <- xts(Yongdam$`Inflow(m^3/s)`,order.by = Yongdam$Date) %>%
  apply.monthly(sum) 

Agr_yd <- 0
Ind_yd <- 492.7
Env_yd <- 157.7

Total_demand_yd <- Agr_yd + Ind_yd + Env_yd

## 3. 부안댐 Inflow Data & Demand


for (i in 1997:2023) {assign(paste0("Buan_",i),read_excel(paste0("Buan_",i,".xlsx"),skip=2))}

Buan <- rbind(Buan_2023,Buan_2022,Buan_2021,Buan_2020,Buan_2019,Buan_2018,Buan_2017,Buan_2016,Buan_2015,Buan_2014,Buan_2013,Buan_2012,Buan_2011,Buan_2010,
              Buan_2009,Buan_2008,Buan_2007,Buan_2006,Buan_2005,Buan_2004,Buan_2003,Buan_2002,Buan_2001,Buan_2000,Buan_1999,Buan_1998,
              Buan_1997)

names(Buan) = c("Date","Water Level of Dam(EL.m)","Water Level of spillway(EL.m)","Storage(MCM)","Storage rate(%)","Precipitation(mm)",
                "Inflow(m^3/s)","Natural Inflow(m^3/s)","Total Release(m^3/s)","Energy 1(m^3/s)","Overflow(m^3/s)","Emergency Release",
                "Maintain River (m^3/s)","Agriculture(m^3/s)","1(m^3/s)")

Buan$`Natural Inflow(m^3/s)`[is.negative(Buan$`Natural Inflow(m^3/s)`)] <- 0
Buan$`Inflow(m^3/s)`[is.negative(Buan$`Inflow(m^3/s)`)] <- 0
Buan$`Inflow(m^3/s)` <- Buan$`Inflow(m^3/s)`*60*60*24/1000000 
Buan$Date <- as.Date(Buan$Date,'%Y-%m-%d')

Buan_inflow <- xts(Buan$`Inflow(m^3/s)`,order.by = Buan$Date) %>%
  apply.monthly(sum) 

Agr_ba <- 6.6
Ind_ba <- 28.5
Env_ba <- 0

Total_demand_ba <- Agr_ba + Ind_ba + Env_ba

## 4. 대청댐 Inflow Data & Demand


for (i in 1996:2023) {assign(paste0("Daecheong_",i),read_excel(paste0("Daecheong_",i,".xlsx"),skip=2))}

Daecheong <- rbind(Daecheong_2023,Daecheong_2022,Daecheong_2021,Daecheong_2020,Daecheong_2019,Daecheong_2018,Daecheong_2017,Daecheong_2016,Daecheong_2015,Daecheong_2014,
                   Daecheong_2013,Daecheong_2012,Daecheong_2011,Daecheong_2010,Daecheong_2009,Daecheong_2008,Daecheong_2007,Daecheong_2006,
                   Daecheong_2005,Daecheong_2004,Daecheong_2003,Daecheong_2002,Daecheong_2001,Daecheong_2000,Daecheong_1999,Daecheong_1998,
                   Daecheong_1997,Daecheong_1996)

names(Daecheong) = c("Date","Water Level of Dam(EL.m)","Water Level of spillway(EL.m)","Storage(MCM)","Storage rate(%)","Precipitation(mm)",
                     "Inflow(m^3/s)","Natural Inflow(m^3/s)","Yongdam Release(m^3/s)","Total Release(m^3/s)","Energy 1(m^3/s)",
                     "Hydrology(m^3/s)","Maintain River(m^3/s)","Fish","additional","Daejoen","Chungju1(m^3/s)","Chungju1(m^3/s)")

Daecheong$`Natural Inflow(m^3/s)`[is.negative(Daecheong$`Natural Inflow(m^3/s)`)] <- 0
Daecheong$`Inflow(m^3/s)`[is.negative(Daecheong$`Inflow(m^3/s)`)] <- 0
Daecheong$`Inflow(m^3/s)` <- Daecheong$`Inflow(m^3/s)`*60*60*24/1000000 
Daecheong$Date <- as.Date(Daecheong$Date,'%Y-%m-%d')


Daecheong_inflow <- xts(Daecheong$`Inflow(m^3/s)`,order.by = Daecheong$Date) %>%
  apply.monthly(sum) 

Agr_dc <- 349
Ind_dc <- 1300
Env_dc <- 0

Total_demand_dc <- Agr_dc + Ind_dc + Env_dc

# Inflow Analysis

library("reshape2") 

## Original data plot

value1 <- data.frame(
  cbind(
    BR_inflow,
    Yongdam_inflow,
    Buan_inflow,
    Daecheong_inflow))

Geum_inflow1 <- data.frame(
    Date = c(index(Daecheong_inflow)),
    value1) %>% 
  melt(id.vars="Date")

ggplot(Geum_inflow1,aes(x=Date,y=value,color=variable)) +
  geom_line() + 
  theme_bw()

## Standardization data plot

value2 <- data.frame(
  cbind(
    scale(BR_inflow),
    scale(Yongdam_inflow),
    scale(Buan_inflow),
    scale(Daecheong_inflow)))

Geum_inflow2 <- data.frame(
  Date = c(index(Daecheong_inflow)),
  value2) %>% 
  melt(id.vars="Date")

ggplot(Geum_inflow2,aes(x=Date,y=value,color=variable)) +
  geom_line() + 
  theme_bw()

## Normalization data plot
norm <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

value3 <- data.frame(
  cbind(
    norm(BR_inflow),
    norm(Yongdam_inflow),
    norm(Buan_inflow),
    norm(Daecheong_inflow)))

Geum_inflow3 <- data.frame(
  Date = c(index(Daecheong_inflow)),
  value3) %>% 
  melt(id.vars="Date")

ggplot(Geum_inflow3,aes(x=Date,y=value,color=variable)) +
  geom_line() + 
  theme_bw()

## log data plot

value4 <- data.frame(
  cbind(
    log(BR_inflow),
    log(Yongdam_inflow),
    log(Buan_inflow),
    log(Daecheong_inflow)))

Geum_inflow4 <- data.frame(
  Date = c(index(Daecheong_inflow)),
  value4) %>% 
  melt(id.vars="Date")

ggplot(Geum_inflow4,aes(x=Date,y=value,color=variable)) +
  geom_line() + 
  theme_bw() 








