# ------------------------------------------------------------------------------
# 작성자: 김연주
# 금강유역 Agent-based_Geum 파일
# 문의 or 오류 신고 : iamkimyeonju@gmail.com
# 최종 수정일 : 2025년 7월 13일
# ------------------------------------------------------------------------------

# UConn computer
#setwd("C:/Users/pqh24002/OneDrive - University of Connecticut/University of Seoul/3차년도/Research/코드n자료")

# Private computer
#setwd("C:/Users/iamki/OneDrive - University of Connecticut/University of Seoul/3차년도/Research/코드n자료")
#source("function_for_Geum.R")

# install.packages("naniar")
# install.packages("lubridate")
# install.packages("zoo")
# install.packages("ggplot2")
# install.packages("reshape2")
# install.packages("reshape")
# install.packages("DescTools")

library(naniar)
library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(reshape2)
library(reshape)
library(DescTools)

Agent_Geum_Network_with_Diversion <- function(supply_scenario = 1, demand_scenario = 1,  opt_year = 10, ABM=TRUE){
  
  #supply_scenario = 1; demand_scenario = 1; ABM=TRUE; opt_year = 39
  

# General setting ---------------------------------------------------------

  supply_scenario <- supply_scenario # 공급시나리오 supply_scenario
  demand_scenario <- demand_scenario # 수요시나리오 demand_scenario
  #drought_scenario <- drought_scenario # 몇번째 가뭄시나리오를 사용할 것 인가?
  
  opt_year <- opt_year
  opt <- 365*opt_year
  Month <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")
  Agr_total <- 0; Ind_total <- 0; Env_total <- 0;
  Node_A <- 0; 
  df_nodeA <- matrix(NA, nrow=opt)
  Node_B <- 0; 
  df_nodeB <- matrix(NA, nrow=opt)
  Node_C <- 0; 
  df_nodeC <- matrix(NA, nrow=opt)
  Node_D <- 0; 
  df_nodeD <- matrix(NA, nrow=opt)
  Node_E <- 0
  df_nodeE <- matrix(NA, nrow=opt)
  Node_F <- 0
  df_nodeF <- matrix(NA, nrow=opt)
  Outlet1 <- 0
  df_outlet1 <- matrix(NA,nrow = opt)
  Outlet2 <- 0
  df_outlet2 <- matrix(NA,nrow = opt)
  
  ## Return rate
  alpha_Agr = 0.35; alpha_Ind = 0.65; alpha_Dom = 0.65; alpha_Env = 1.00; 
  alpha_Pow = 1.00; alpha_Intake <- (alpha_Ind + alpha_Dom)/2
  
  ## Inflow 
  runoff_code <- list(용담댐 = c(300101,300102,300103,300104,300105,300106,300107,300108),
                      용담댐하류 = c(300201),
                      무주남대천 = c(300301,300302,300303),
                      영동천 = c(300401,300402,300403,300404,300405,300406),
                      초강 = c(300501,300502,300503),
                      대청댐상류 = c(300601),
                      보청천 = c(300701,300702,300703,300704,300705,300706),
                      대청댐 = c(300801,300802,300803,300804,300805),
                      갑천 = c(300901,300902,300903,300904,300905,300906),
                      대청댐하류 = c(301001,301002),
                      미호천 = c(301101,301102,301104,301105,301106,301107,301108,
                              301109,301110,301111,301112,301113,301114,301115),
                      백곡댐 = c(301103),
                      # 301103 미호천 - 백곡댐 표준유역
                      금강공주 = c(301201,301202,301203,301204,301205,301206,301207,
                               301208,301209,301210,301211,301212,301213,301214),
                      논산천 = c(301301,301302,301303,301304,301305),
                      금강하구언 = c(301401,301402,301403),
                      보령댐 = c(320305))


  
  runoff_array <- read_csv("Netflow_1981-2020.csv") 
  runoff_array[, 2:ncol(runoff_array)] <- runoff_array[, 2:ncol(runoff_array)] * 86400 / 1e6 #CMS --> m^3/day --> MCM/day
  runoff_array$Time <- as.Date(runoff_array$Time)
  
  runoff_array <- runoff_array %>% filter(Time >= as.Date("1981-10-01")) %>% filter(Time <= as.Date("2022-09-30"))
  
  inflow_3001 <- subset(runoff_array,select= as.character(runoff_code$용담댐)) %>% rowSums() %>% as.data.frame()
  inflow_3002 <- subset(runoff_array,select= as.character(runoff_code$용담댐하류)) %>% rowSums() %>% as.data.frame()
  inflow_3003 <- subset(runoff_array,select= as.character(runoff_code$무주남대천)) %>% rowSums() %>% as.data.frame()
  inflow_3004 <- subset(runoff_array,select= as.character(runoff_code$영동천)) %>% rowSums() %>% as.data.frame()
  inflow_3005 <- subset(runoff_array,select= as.character(runoff_code$초강)) %>% rowSums() %>% as.data.frame()
  inflow_3006 <- subset(runoff_array,select= as.character(runoff_code$대청댐상류)) %>% rowSums() %>% as.data.frame()
  inflow_3007 <- subset(runoff_array,select= as.character(runoff_code$보청천)) %>% rowSums() %>% as.data.frame()
  inflow_3008 <- subset(runoff_array,select= as.character(runoff_code$대청댐)) %>% rowSums() %>% as.data.frame()
  inflow_3009 <- subset(runoff_array,select= as.character(runoff_code$갑천)) %>% rowSums() %>% as.data.frame()
  inflow_3010 <- subset(runoff_array,select= as.character(runoff_code$대청댐하류)) %>% rowSums() %>% as.data.frame()
  inflow_3011 <- subset(runoff_array,select= as.character(runoff_code$미호천)) %>% rowSums() %>% as.data.frame()
  inflow_3012 <- subset(runoff_array,select= as.character(runoff_code$금강공주)) %>% rowSums() %>% as.data.frame()
  inflow_3013 <- subset(runoff_array,select= as.character(runoff_code$논산천)) %>% rowSums() %>% as.data.frame()
  inflow_3014 <- subset(runoff_array,select= as.character(runoff_code$금강하구언)) %>% rowSums() %>% as.data.frame()
  inflow_301103 <- subset(runoff_array,select= as.character(runoff_code$백곡댐)) %>% rowSums() %>% as.data.frame()
  inflow_320305 <- subset(runoff_array,select= as.character(runoff_code$보령댐)) %>% rowSums() %>% as.data.frame()
  
  
  # breaker 
  breaker <- data.frame(month =  c(10,11,12,1,2,3,4,5,6,7,8,9), days = c(31,30,31,31,28,31,30,31,30,31,31,30))
  breaker <- do.call(rbind, replicate(opt_year, breaker, simplify = FALSE))
  breaker['ABM'] <- cumsum(breaker["days"])
  breaker[12*opt_year,'ABM'] <- NA
  
  # 다목적댐
  df_dam <- df_intake <- matrix(NA,nrow = opt,ncol = 5)
  demand_dam <- matrix(NA,nrow = opt,ncol = 5)
  
  ## 용담댐
  Dam1_Stor <- 815.0*0.7 # 총 저수용량 * 0.7 (MCM)
  dam1_Stor_Min <- 815.0*0.1 # 총 저수용량 * 0.1 (MCM)
  dam1_Stor_Max <- 815.0* 0.9 # 총 저수용량 * 0.9 (MCM)
  R1_Agr <- 0; R1_Ind <- 0; R1_Env <- 0;
  df_dam1_Stor <- matrix(NA, nrow = opt)
  df_dam1_Spillway <- matrix(NA, nrow = opt)
  df_R1 <- matrix(NA, nrow= opt, ncol=3, dimnames = list(1:opt, c('Agr','Ind','Env')))
  Dam1 <- NA
  Dam1_Spillway <- 0
  Release_Yongdam <- sweep(read.csv("Yongdam_Release.csv")[2:length(read.csv("Yongdam_Release.csv"))],1,c(31, 30, 31,31, 28, 31, 30, 31, 30, 31, 31, 30),FUN="/")
  Release_Yongdam <- as.data.frame(lapply(Release_Yongdam, rep, c(31, 30, 31, 31, 28, 31, 30, 31, 30, 31, 31, 30)))
  Release_Yongdam[,6:length(Release_Yongdam)] <- Release_Yongdam[,6:length(Release_Yongdam)] * supply_scenario
  Yongdam <- Release_Yongdam
  Release_Yongdam <- do.call(rbind, replicate(opt_year, Release_Yongdam, simplify = FALSE))
  demand_dam[,1] <- Release_Yongdam$A_LV0 + Release_Yongdam$I_LV0 + Release_Yongdam$E_LV0
  
  ## 대청댐
  Dam2_Stor <- 1490.0*0.7 # 총 저수용량 * 0.7 (MCM)
  dam2_Stor_Min <- 1490.0*0.1 # 총 저수용량 * 0.1 (MCM)
  dam2_Stor_Max <- 1490.0* 0.9 # 총 저수용량 * 0.9 (MCM)
  R2_Agr = 0; R2_Ind = 0; R2_Env = 0;
  df_dam2_Stor <- matrix(NA,nrow =  opt)
  df_dam2_Spillway <- matrix(NA,nrow =  opt)
  df_R2 <- matrix(NA, nrow= opt, ncol=3, dimnames = list( 1:opt,c('Agr', 'Ind','Env')))
  Dam2 <- NA
  Dam2_Spillway <- 0
  Release_Daecheong <- sweep(read.csv("Daecheong_Release.csv")[2:length(read.csv("Daecheong_Release.csv"))],1,c(31, 30, 31,31, 28, 31, 30, 31, 30, 31, 31, 30),FUN="/")
  Release_Daecheong <- as.data.frame(lapply(Release_Daecheong, rep, c(31, 30, 31,31, 28, 31, 30, 31, 30, 31, 31, 30)))
  Release_Daecheong[,6:length(Release_Daecheong)] <- Release_Daecheong[,6:length(Release_Daecheong)] * supply_scenario
  Daecheong <- Release_Daecheong
  Release_Daecheong <- do.call(rbind, replicate(opt_year, Release_Daecheong, simplify= FALSE))
  demand_dam[,2] <- Release_Daecheong$A_LV0 + Release_Daecheong$I_LV0 + Release_Daecheong$E_LV0
  
  ## 보령댐
  Dam3_Stor <- 116.9*0.7 # 총 저수용량 * 0.7 (MCM)
  dam3_Stor_Min <- 116.9*0.1 # 총 저수용량 * 0.1 (MCM)
  dam3_Stor_Max <- 116.9* 0.9 # 총 저수용량 * 0.9 (MCM)
  R3_Agr = 0; R3_Ind = 0; R3_Env = 0;
  df_dam3_Stor <- matrix(NA,nrow =opt)
  Dam3 <- NA
  df_dam3_Spillway <- matrix(NA,nrow =  opt)
  df_R3 <- matrix(NA, nrow=opt, ncol=3, dimnames = list( 1:opt,c('Agr', 'Ind','Env')))
  waterway <- 3.45 # 보령댐 도수로
  df_waterway <- matrix(NA,nrow=opt)
  Dam3_Spillway <- 0
  Release_Boryeong <- sweep(read.csv("Boryeong_Release.csv")[2:length(read.csv("Boryeong_Release.csv"))],1,c(31, 30, 31,31, 28, 31, 30, 31, 30, 31, 31, 30),FUN="/")
  Release_Boryeong <- as.data.frame(lapply(Release_Boryeong, rep,c(31, 30, 31,31, 28, 31, 30, 31, 30, 31, 31, 30)))
  Release_Boryeong[,6:length(Release_Boryeong)] <- Release_Boryeong[,6:length(Release_Boryeong)] * supply_scenario
  Boryeong <- Release_Boryeong
  Release_Boryeong <- do.call(rbind, replicate(opt_year, Release_Boryeong , simplify = FALSE))
  demand_dam[,3] <- Release_Boryeong$A_LV0 + Release_Boryeong$I_LV0 + Release_Boryeong$E_LV0
  
  # 농업용 저수지
  ## 백곡저수지
  Dam4_Stor <- as.numeric(read.csv("농업기반시설 시설제원.csv") %>% filter(표준코드 == 4375010009) %>% 
                            select(유효저수량) * 1000 /1000000 ) # 유효저수량
  dam4_Stor_Nor <- as.numeric(read.csv("농업기반시설 시설제원.csv") %>% filter(표준코드 == 4375010009) %>% 
                                select(유효저수량) * 1000 /1000000)  # 유효저수량
  dam4_Stor_Min <- as.numeric(read.csv("농업기반시설 시설제원.csv") %>% filter(표준코드 == 4375010009) %>% 
                                select(사수량) * 1000 /1000000) # 사수량
  dam4_Stor_Max <- as.numeric(read.csv("농업기반시설 시설제원.csv") %>% filter(표준코드 == 4375010009) %>% 
                                select(총저수량) * 1000 /1000000) # 유효저수량
  df_R4 <- matrix(NA, nrow=opt, ncol=1, dimnames = list( 1:opt,c('Agr')))
  df_dam4_Stor <- matrix(NA,nrow =  opt)
  df_dam4_Spillway <- matrix(NA,nrow =  opt)
  demand_dam[,4] <- rep(c(rep(0,212), rep(0.217, 153)),opt_year) 
  Dam4_Spillway <- 0
  Dam4 <- NA
  
  ## 탑정저수지
  Dam5_Stor <- as.numeric(read.csv("농업기반시설 시설제원.csv") %>% filter(표준코드 == 4423010045) %>% 
                            select(유효저수량) * 1000 /1000000) # 유효저수량
  dam5_Stor_Nor <- as.numeric(read.csv("농업기반시설 시설제원.csv") %>% filter(표준코드 == 4423010045) %>% 
                                select(유효저수량) * 1000 /1000000)  # 유효저수량
  dam5_Stor_Min <- as.numeric(read.csv("농업기반시설 시설제원.csv") %>% filter(표준코드 == 4423010045) %>% 
                                select(사수량) * 1000 /1000000) # 사수량
  dam5_Stor_Max <- as.numeric(read.csv("농업기반시설 시설제원.csv") %>% filter(표준코드 == 4423010045) %>% 
                                select(총저수량) * 1000 /1000000) # 총 저수용량
  df_R5 <- matrix(NA, nrow=opt, ncol=1, dimnames = list( 1:opt,c('Agr')))
  df_dam5_Stor <- matrix(NA,nrow =  opt)
  df_dam5_Spillway <- matrix(NA,nrow =  opt)
  demand_dam[,5] <- rep(c(rep(0,212), rep(0.2904, 153)),opt_year) 
  Dam5_Spillway <- 0
  Dam5 <- NA
  
  org_demand_dam <- demand_dam
  
  # 발전용수
  river_intake <- read.csv("하천수허가량(금강홍수통제소).csv")
  df_power <- matrix(NA,nrow = opt,ncol = 3)
  demand_power <- matrix(NA,nrow = opt,ncol = 3)
  demand_power[,1] <- filter(river_intake,시설물명 == "무주양수발전소") %>%
    select(허가량.m..일.) %>% as.numeric()
  demand_power[,2] <- filter(river_intake,시설물명 == "방우리수력발전소") %>%
    select(허가량.m..일.) %>% as.numeric()
  demand_power[,3] <- filter(river_intake,시설물명 == "금강수력발전") %>%
    select(허가량.m..일.) %>% as.numeric()
  demand_power <- (demand_power/1000000)*0.5 * demand_scenario # m^3/day --> MCM/day , 하천수허가량의 50%만 쓴다고 가정
  for(i in 1:3){
  assign(paste0("Power",i),NA)}
  org_demand_power <- demand_power
  
  ## Power 1 : 무주양수발전소 (하부댐 기준)
  Power1_Stor <- as.numeric(3.686)
  Power1_Stor_Nor <- as.numeric(3.686)
  Power1_Stor_Min <- as.numeric(3.686 *0.5)
  Power1_Stor_Max <- as.numeric(6.685)
  df_Power1 <- matrix(NA, nrow=opt , ncol=1)
  df_power1_Stor <- matrix(NA, nrow=opt)
  df_power1_Spillway <- matrix(NA,nrow=opt)
  Power1_Spillway <- 0
  
  # 취수장
  ## Intake
  Intake.name <- list(Intake1 = c("삼락취수장"),
                      Intake2 = c("구천취수장", "무풍취수장","설천취수장","무주취수장"),
                      Intake3 = c("영동취수장"),
                      Intake4 = c("모동취수장","궁촌취수장"),
                      Intake5 = c("옥천취수장"),
                      Intake6 = c("교사취수장","원남취수장","청산취수장"),
                      Intake7 = c("안남취수장"),
                      Intake8 = c("중리취수장"),
                      Intake9 = c("국전취수장"),
                      Intake10 = c("삼정취수장"),
                      Intake11 = c("병천취수장","현도취수장"),
                      Intake12 = c("백곡취수장"))
  
  Intake_demand <- read.csv("intake_filter_reservoir.csv") %>%
    group_by(INTAKE1NM) %>%
    summarise(USE1M = sum(USE1M),
              USE2M = sum(USE2M),
              USE3M = sum(USE3M),
              USE4M = sum(USE4M),
              USE5M = sum(USE5M),
              USE6M = sum(USE6M),
              USE7M = sum(USE7M),
              USE8M = sum(USE8M),
              USE9M = sum(USE9M),
              USE10M = sum(USE10M),
              USE11M = sum(USE11M),
              USE12M = sum(USE12M)) %>%
    filter(INTAKE1NM %in% unlist(Intake.name))
  
  df_intake <- matrix(NA,nrow = opt,ncol = 12)
  demand_intake <- matrix(NA,nrow = 365,ncol = 12)
  days_in_month <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  
  for(i in 1:12){
    assign(paste0("Intake",i),NA)
    value <- Intake_demand %>%
      filter(INTAKE1NM %in% Intake.name[[i]]) %>% 
      select(-1) %>% 
      colSums() %>% 
      as.data.frame()
    for (mth in 1:12){
      assign(paste0("U",mth,"M"), rep(value[mth,]/days_in_month[mth],days_in_month[mth])) # ton/month --> ton/day
    }
    demand_intake[,i] <- c(U1M,U2M,U3M,U4M,U5M,U6M,U7M,U8M,U9M,U10M,U11M,U12M) # ton/day
  }
  
  demand_intake <- demand_intake/1000000 # m^3/day --> MCM/day
  demand_intake <- rbind(demand_intake[yday("2025-10-01"):365,],demand_intake[1:yday("2025-09-30"),]) # 10월 시작
  demand_intake <- demand_intake
  #demand_intake[,2] <- rowSums(Yongdam[,5:7]) #용담댐: 취수장 자료가 아닌 운영룰 기반
  #demand_intake[,9] <- rowSums(Daecheong[,6:8]) #대청댐: 취수장 자료가 아닌 운영룰 기반
  #demand_intake[,15] <- rowSums(Boryeong[,6:8]) #보령댐: 취수장 자료가 아닌 운영룰 기반
  demand_intake <- do.call(rbind, replicate(opt_year, demand_intake, simplify = FALSE)) * demand_scenario
  org_demand_intake <- demand_intake 
  
  # 농업용수
  ## 하천수 허가량 관개기에만 농업용수 사용, 비관개기에는 0
  df_agr <- matrix(NA,nrow = opt,ncol = 13)
  demand_agr <- matrix(NA,nrow = 365,ncol = 13)
  river_agr <- data.frame(유역 = c("용담댐","용담댐하류","무주남대천","영동천",
                                 "초강","대청댐상류","보청천","갑천",
                                 "대청댐하류","미호천","금강공주","논산천",
                                 "금강하구언"),
                          이용현황 = c(76106.10,6951.30,25862.00,57247.00,83853.70,
                                   12024.80,77021.60,67460.50,23540.70,
                                   379454.50,386994.80,212032.90, 237368.20)/1000) %>% # MCM/year
    mutate(관개기이용 = 이용현황/length(yday("2025-05-01"):yday("2025-09-30"))) #MCM/day (관개기)
  
  for (i in 1:13){ 
    assign(paste0("Agr",i),NA)
    demand_agr[yday("2025-05-01"):yday("2025-09-30"),i] <- river_agr$관개기이용[i]
  }
  demand_agr[1:yday("2025-04-30"),] <- 0 #비관개기 0
  demand_agr[yday("2025-10-01"):365,] <- 0 #비관개기 0
  demand_agr <- rbind(demand_agr[yday("2025-10-01"):365,],demand_agr[1:yday("2025-09-30"),]) # 10월 시작
  demand_agr <- do.call(rbind, replicate(opt_year, demand_agr, simplify = FALSE)) * demand_scenario
  org_demand_agr <- demand_agr
  df_want <- matrix(NA,nrow = opt,ncol = 2)
  df_help <- matrix(NA,nrow = opt,ncol = 2)

# Water balance -----------------------------------------------------------

  for(i in 1:c(365*opt_year)){ #Start from October
    
    #for(i in 1:273){ #Start from October
    ### Intake 1
    if(inflow_3001[i,] >= demand_intake[i,1]){
      Intake1 <- demand_intake[i,1]
    }else{
      Intake1 <- inflow_3001[i,]
    }
    df_intake[i,1] <- Intake1
    
    ### Yongdam dam
    if(Dam1_Stor > dam1_Stor_Max){ #Flood
      Dam1_Spillway <- Dam1_Stor + inflow_3001[i,] - Intake1 + (Intake1 * alpha_Intake) - dam1_Stor_Max
      R1_Agr <- Release_Yongdam$A_LV0[i]
      R1_Ind <- Release_Yongdam$I_LV0[i]
      R1_Env <- Release_Yongdam$E_LV0[i]
    }else if(Dam1_Stor >= Release_Yongdam$LV1[i]){ #normal(LV0)
      Dam1_Spillway <- 0
      R1_Agr <- Release_Yongdam$A_LV0[i]
      R1_Ind <- Release_Yongdam$I_LV0[i]
      R1_Env <- Release_Yongdam$E_LV0[i]
    } else if(Dam1_Stor >= Release_Yongdam$LV2[i]){ #Interest(LV1)
      Dam1_Spillway <- 0
      R1_Agr <- Release_Yongdam$A_LV1[i]
      R1_Ind <- Release_Yongdam$I_LV1[i]
      R1_Env <- Release_Yongdam$E_LV1[i]
    } else if(Dam1_Stor >= Release_Yongdam$LV3[i]){ #Watch(LV2)
      Dam1_Spillway <- 0
      R1_Agr <- Release_Yongdam$A_LV2[i]
      R1_Ind <- Release_Yongdam$I_LV2[i] 
      R1_Env <- Release_Yongdam$E_LV2[i] 
    } else if(Dam1_Stor >= Release_Yongdam$LV4[i]){ #Alert(LV3)
      Dam1_Spillway <- 0
      R1_Agr <- Release_Yongdam$A_LV3[i] 
      R1_Ind <- Release_Yongdam$I_LV3[i] 
      R1_Env <- Release_Yongdam$E_LV3[i] 
    } else if(Dam1_Stor >= dam1_Stor_Min){ #Serious(LV4)
      Dam1_Spillway <- 0
      R1_Agr <- Release_Yongdam$A_LV4[i]
      R1_Ind <- Release_Yongdam$I_LV4[i]
      R1_Env <- Release_Yongdam$E_LV4[i] 
    } else { #Under LWL
      Dam1_Spillway <- 0
      R1_Agr <- 0
      R1_Ind <- 0
      R1_Env <- 0
    }
    Dam1_Stor <- Dam1_Stor - (R1_Agr + R1_Ind + R1_Env) + inflow_3001[i,] - Intake1 +
      (Intake1 * alpha_Intake) - Dam1_Spillway
    Dam1 <- R1_Agr + R1_Ind + R1_Env
    df_dam[i,1] <- Dam1 
    
    ### Agr 1
    if(R1_Agr * alpha_Agr + R1_Env * alpha_Env + R1_Ind * alpha_Ind + Dam1_Spillway >= demand_agr[i,1]-R1_Agr){
      Agr1 <- demand_agr[i,1]-R1_Agr
    }else if(R1_Agr * alpha_Agr + R1_Env * alpha_Env + R1_Ind * alpha_Ind + Dam1_Spillway > 0){
      Agr1 <- R1_Agr * alpha_Agr + R1_Env * alpha_Env + R1_Ind * alpha_Ind + Dam1_Spillway
    }else {
      Agr1 <- 0
    }
    df_agr[i,1] <- Agr1
    
    ## 3002 용담댐하류 
    ### Agr 2
    if(R1_Agr * alpha_Agr + R1_Env * alpha_Env + R1_Ind * alpha_Ind + 
       Dam1_Spillway - Agr1 + Agr1*alpha_Agr + inflow_3002[i,] >= demand_agr[i,2]){
      Agr2 <- demand_agr[i,2]
    }else if (R1_Agr * alpha_Agr + R1_Env * alpha_Env + R1_Ind * alpha_Ind + 
              Dam1_Spillway - Agr1 + Agr1*alpha_Agr + inflow_3002[i,] > 0){
      Agr2 <- R1_Agr * alpha_Agr + R1_Env * alpha_Env + R1_Ind * alpha_Ind + 
        Dam1_Spillway - Agr1 + Agr1*alpha_Agr + inflow_3002[i,]
    }else {
      Agr2 <- 0
    }
    df_agr[i,2] <- Agr2
    
    ## 3003 무주남대천
    ### Power 1
    # 무주남대천 양수발전소의 레벨을 찾아서 여기에 넣고 작은 저수지라고 생각하고
    # 여기도 저수지 라고 보고 코딩을 한다
    
    
    
    if(Power1_Stor >= Power1_Stor_Max){ #총 저수량 이상
      Power1_Spillway <- Power1_Stor + inflow_3003[i,] - Power1_Stor_Max
      Power1 <- demand_power[i,1]
    }else if(Power1_Stor >= Power1_Stor_Nor){ #총 저수량 이하 유효저수량 이상
      Power1_Spillway <- 0
      Power1 <- demand_power[i,1]
    }else if(Power1_Stor >= Power1_Stor_Min){ #유효저수량 이하 사수량 이상
      Power1_Spillway <- 0
      Power1 <- demand_power[i,1] * 0.8
    }else{ #사수량 이하
      Power1_Spillway <- 0
      Power1 <- 0
    }
    Power1_Stor <- Power1_Stor + inflow_3003[i,]  - Power1_Spillway
    df_power[i,1] <- Power1
    
    ### Intake 2
    if( Power1 + Power1 *alpha_Pow + Power1_Spillway >= demand_intake[i,2]){
      Intake2 <- demand_intake[i,2]
    }else if (Power1 + Power1 *alpha_Pow + Power1_Spillway >0){
      Intake2 <-  Power1 + Power1 *alpha_Pow
    }else {
      Intake2 <- 0
    }
    df_intake[i,2] <- Intake2
    
    ### Agr 3
    if(Power1 + (Power1 *alpha_Pow)- Intake2 + (Intake2*alpha_Intake) >= demand_agr[i,3]){
      Agr3 <- demand_agr[i,3]
    }else if (Power1 + Power1 *alpha_Pow - Intake2 + (Intake2*alpha_Intake) > 0){
      Agr3 <- Power1 + Power1 *alpha_Pow - Intake2 + (Intake2*alpha_Intake)
    }else {
      Agr3 <- 0
    }
    df_agr[i,3] <- Agr3
    
    ## 3004 영동천
    ### Node A
    Node_A = (R1_Agr * alpha_Agr) + (R1_Env * alpha_Env) + (R1_Ind * alpha_Ind) + 
              Dam1_Spillway - Agr1 + (Agr1*alpha_Agr) + inflow_3002[i,] - Agr2 + 
              (Agr2 *alpha_Agr) + Power1_Spillway - Power1 + (Power1 *alpha_Pow) - Intake2 + 
              (Intake2*alpha_Intake) - Agr3 + (Agr3 * alpha_Agr) + inflow_3004[i,]
    
    ### Power 2
    if( Node_A >= demand_power[i,2]){
      Power2 <- demand_power[i,2]
    }else if (Node_A > 0){
      Power2 <- Node_A
    }else{
      Power2 <- 0
    }
    df_power[i,2] <- Power2
    
    ### Intake 3  
    if(Node_A - Power2 + (Power2 *alpha_Pow) >= demand_intake[i,3]){
      Intake3 <- demand_intake[i,3]
    }else if(Node_A - Power2 + (Power2 *alpha_Pow) > 0){
      Intake3 <- Node_A - Power2 + (Power2 *alpha_Pow)
    }else{
      Intake3 <- 0
    }
    df_intake[i,3] <- Intake3
    
    ### Agr 4
    if(Node_A - Power2 + (Power2 *alpha_Pow) - Intake3 + (Intake3 * alpha_Intake) >= demand_agr[i,4]){
      Agr4 <- demand_agr[i,4]
    }else if(Node_A - Power2 + (Power2 *alpha_Pow) - Intake3 + (Intake3 * alpha_Intake) > 0){
      Agr4 <- Node_A - Power2 + (Power2 *alpha_Pow) - Intake3 + (Intake3 * alpha_Intake)
    }else{
      Agr4 <- 0
    }
    df_agr[i,4] <- Agr4
    
    ## 3005 초강
    ### Intake 4
    if(inflow_3005[i,] >= demand_intake[i,4]){
      Intake4 <- demand_intake[i,4]
    }else if(inflow_3005[i,] > 0 ){
      Intake4 <- inflow_3005[i,]
    }else{
      Intake4 <- 0
    }
    df_intake[i,4] <- Intake4
    
    ### Agr 5
    if(inflow_3005[i,] - Intake4 + (Intake4 * alpha_Intake) >= demand_agr[i,5]){
      Agr5 <- demand_agr[i,5]
    }else if(inflow_3005[i,] - Intake4 + Intake4 * alpha_Intake > 0){
      Agr5 <- inflow_3005[i,] - Intake4 + Intake4 * alpha_Intake
    }else{
      Agr5 <- 0
    }
    #df_agr[i,5] <- Agr5
    
    ### Dashed Node B 
    Node_B_dash <- Node_A - Power2 + (Power2 * alpha_Pow) - Intake3 + (Intake3 * alpha_Intake) - 
      Agr4 + (Agr4 * alpha_Agr) +  inflow_3006[i,]   
    
    
    ### New diversion
    want5 <-  demand_agr[i,5] - Agr5
    
    if (Node_B_dash >= want5){
      help5 <- want5
    }else{
      help5 <- 0
    }
    
    df_want[i,1] <- want5
    df_help[i,1] <- help5
    df_agr[i,5] <- Agr5 + help5
    
    ### Node B
    Node_B <- Node_A - Power2 + (Power2 * alpha_Pow) - Intake3 + (Intake3 * alpha_Intake) - 
              Agr4 + (Agr4 * alpha_Agr) + inflow_3005[i,] - Intake4 + (Intake4 * alpha_Intake) - 
              Agr5 + (Agr5 * alpha_Agr) + inflow_3006[i,] - help5   
    
    ## 3006 대청댐상류 
    ### Intake 5
    if(Node_B >= demand_intake[i,5]){
      Intake5 <- demand_intake[i,5]
    }else if(Node_B > 0){
      Intake5 <- Node_B
    }else {
      Intake5 <- 0
    }
    df_intake[i,5] <- Intake5
    
    ### Agr 6
    if(Node_B - Intake5 + (Intake5*alpha_Intake) >= demand_agr[i,6]){
      Agr6 <- demand_agr[i,6]
    }else if(Node_B - Intake5 + (Intake5*alpha_Intake) > 0){
      Agr6 <- Node_B - Intake5 + (Intake5*alpha_Intake)
    }else{
      Agr6 <- 0
    }
    df_agr[i,6] <- Agr6
    
    ## 3007 보청천
    ## Intake 6
    if(inflow_3007[i,] >= demand_intake[i,6]){
      Intake6 <- demand_intake[i,6]
    }else if (inflow_3007[i,] > 0){
      Intake6 <- inflow_3007[i,]
    }else{
      Intake6 <- 0
    }
    df_intake[i,6] <- Intake6
    
    ### Agr 7
    if(inflow_3007[i,] - Intake6 + (Intake6 * alpha_Intake) >= demand_agr[i,7]){
      Agr7 <- demand_agr[i,7]
    }else if (inflow_3007[i,] - Intake6 + (Intake6 * alpha_Intake) > 0){
      Agr7 <- inflow_3007[i,] - Intake6 + (Intake6 * alpha_Intake)
    }else{
      Agr7 <- 0
    }
    #df_agr[i,7] <- Agr7
    
    ### Dashed Node C
    Node_C_dash <- Node_B - Intake5 + (Intake5 * alpha_Intake) - Agr6 + (Agr6 * alpha_Agr) 
    
    ### New diversion
    want7 <-  demand_agr[i,7] - Agr7
    
    if (Node_C_dash >= want7){
      help7 <- want7
    }else{
      help7 <- 0
    }
    
    df_want[i,2] <- want7
    df_help[i,2] <- help7
    df_agr[i,7] <- Agr7 + help7
    
    
    ### Node C
    Node_C <- Node_B - Intake5 + (Intake5 * alpha_Intake) - Agr6 + (Agr6 * alpha_Agr) + 
              inflow_3007[i,] - Intake6 + (Intake6 * alpha_Intake) - Agr7 + (Agr7 * alpha_Agr) - help7 
    
    ### Intake 7
    if(Node_C >= demand_intake[i,7]){
      Intake7 <- demand_intake[i,7]
    }else if (Node_C > 0){
      Intake7 <- Node_C
    }else{
      Intake7 <- 0
    }
    df_intake[i,7] <- Intake7
    
    ## 3008 대청댐
    ### Daecheong Dam
    if(Dam2_Stor > dam2_Stor_Max){ #Flood
      Dam2_Spillway <- Dam2_Stor + inflow_3008[i,] + Node_C - Intake8 + (Intake8 * alpha_Intake) - dam2_Stor_Max
      R2_Agr <- Release_Daecheong$A_LV0[i]
      R2_Ind <- Release_Daecheong$I_LV0[i]
      R2_Env <- Release_Daecheong$E_LV0[i]
    }else if(Dam2_Stor >= Release_Daecheong$LV1[i]){ #normal(LV0)
      Dam2_Spillway <- 0
      R2_Agr = Release_Daecheong$A_LV0[i]
      R2_Ind = Release_Daecheong$I_LV0[i]
      R2_Env = Release_Daecheong$E_LV0[i]
    } else if(Dam2_Stor >= Release_Daecheong$LV2[i]){ #Interest(LV1)
      Dam2_Spillway <- 0
      R2_Agr = Release_Daecheong$A_LV1[i]
      R2_Ind = Release_Daecheong$I_LV1[i]
      R2_Env = Release_Daecheong$E_LV1[i]
    } else if(Dam2_Stor >= Release_Daecheong$LV3[i]){ #Watch(LV2)
      Dam2_Spillway <- 0
      R2_Agr = Release_Daecheong$A_LV2[i]
      R2_Ind = Release_Daecheong$I_LV2[i] 
      R2_Env = Release_Daecheong$E_LV2[i] 
    } else if(Dam2_Stor >= Release_Daecheong$LV4[i]){ #Alert(LV3)
      Dam2_Spillway <- 0
      R2_Agr = Release_Daecheong$A_LV3[i]  
      R2_Ind = Release_Daecheong$I_LV3[i] 
      R2_Env = Release_Daecheong$E_LV3[i]
    } else if(Dam2_Stor >= dam2_Stor_Min){ #Serious(LV4)
      Dam2_Spillway <- 0
      R2_Agr = Release_Daecheong$A_LV4[i]  
      R2_Ind = Release_Daecheong$I_LV4[i]  
      R2_Env = Release_Daecheong$E_LV4[i] 
    } else { #Under LWL
      Dam2_Spillway <- 0
      R2_Agr = 0
      R2_Ind = 0
      R2_Env = 0
    }
    Dam2_Stor <- Dam2_Stor - (R2_Agr + R2_Env + R2_Ind) + inflow_3008[i,] + Node_C - Intake7 + (Intake7 * alpha_Intake) - Dam2_Spillway
    Dam2 <- R2_Agr + R2_Ind + R2_Env
    df_dam[i,2] <- Dam2
    
    ### Intake 8
    if((R2_Agr * alpha_Agr) + (R2_Ind * alpha_Ind) + (R2_Env * alpha_Env) + Dam2_Spillway >= demand_intake[i,8]){
      Intake8 <- demand_intake[i,8]
    }else if ((R2_Agr * alpha_Agr) + (R2_Ind * alpha_Ind) + (R2_Env * alpha_Env) + Dam2_Spillway > 0){
      Intake8 <- (R2_Agr * alpha_Agr) + (R2_Ind * alpha_Ind) + (R2_Env * alpha_Env) + Dam2_Spillway
    }else{
      Intake8 <- 0
    }
    df_intake[i,8] <- Intake8
    
    ### Intake 9
    if((R2_Agr * alpha_Agr) + (R2_Ind * alpha_Ind) + (R2_Env * alpha_Env) + 
       Dam2_Spillway -Intake8 + (Intake8 * alpha_Intake) >= demand_intake[i,9]){
      Intake9 <- demand_intake[i,9]
    }else if ((R2_Agr * alpha_Agr) + (R2_Ind * alpha_Ind) + (R2_Env * alpha_Env) + 
              Dam2_Spillway -Intake8 + (Intake8 * alpha_Intake) > 0){
      Intake9 <- (R2_Agr * alpha_Agr) + (R2_Ind * alpha_Ind) + (R2_Env * alpha_Env) + 
                  Dam2_Spillway -Intake8 + (Intake8 * alpha_Intake)
    }else{
      Intake9 <- 0
    }
    df_intake[i,9] <- Intake9
    
    ### Intake 10
    if((R2_Agr * alpha_Agr) + (R2_Ind * alpha_Ind) + (R2_Env * alpha_Env) + 
       Dam2_Spillway - Intake8 + (Intake8 * alpha_Intake) - Intake9  >= demand_intake[i,10]){
      Intake10 <- demand_intake[i,10]
    }else if((R2_Agr * alpha_Agr) + (R2_Ind * alpha_Ind) + (R2_Env * alpha_Env) + 
             Dam2_Spillway - Intake8 + (Intake8 * alpha_Intake) - Intake9 > 0){
      Intake10 <- (R2_Agr * alpha_Agr) + (R2_Ind * alpha_Ind) + (R2_Env * alpha_Env) + 
                  Dam2_Spillway - Intake8 + (Intake8 * alpha_Intake) - Intake9
    }else{
      Intake10 <- 0
    }
    df_intake[i,10] <- Intake10
    
    ## 3009 갑천
    ### Agr 8
    if(inflow_3009[i,] >= demand_agr[i,8]){
      Agr8 <- demand_agr[i,8]
    }else if(inflow_3009[i,] > 0 ){
      Agr8 <- inflow_3009[i,]
    }else{
      Agr8 <- 0
    }
    df_agr[i,8] <- Agr8
    
    ### Node D
    Node_D <- (R2_Agr * alpha_Agr) + (R2_Ind * alpha_Ind) + (R2_Env * alpha_Env) -
              Intake8 + (Intake8 * alpha_Intake) - Intake9 - Intake10 + (Intake10 * alpha_Intake) +
              inflow_3009[i,] - Agr8 + (Agr8 * alpha_Agr) + inflow_3010[i,]
    
    ## 3010 대청댐 하류
    ### Agr 9
    if(Node_D >= demand_agr[i,9]){
      Agr9 <- demand_agr[i,9]
    }else if (Node_D > 0){
      Agr9 <- Node_D
    }else{
      Agr9 <- 0
    }
    df_agr[i,9] <- Agr9
    
    ### Power 3
    if(Node_D - Agr9 + (Agr9 * alpha_Agr) >= demand_power[i,3]){
      Power3 <- demand_power[i,3]
    }else if (Node_D - Agr9 + (Agr9 * alpha_Agr) > 0){
      Power3 <- Node_D - Agr9 + (Agr9 * alpha_Agr)
    }else{
      Power3 <- 0
    }
    df_power[i,3] <- Power3
    
    #view(demand_power)
    #view(df_power)
    
    ## 301103 백곡댐
    ### Intake 12
    if(inflow_301103[i,] >= demand_intake[i,12]){
      Intake12 <- demand_intake[i,12]
    }else if(inflow_301103[i,] > 0){
      Intake12 <- inflow_301103[i,]
    }else{
      Intake12 <- 0
    }
    df_intake[i,12] <- Intake12
    
    ### 백곡저수지 Dam4
    if(Dam4_Stor >= dam4_Stor_Max){ #총 저수량 이상
      Dam4_Spillway <- Dam4_Stor + inflow_301103[i,] - Intake12 + (Intake12 * alpha_Intake) - dam4_Stor_Max
      R4_Agr <- demand_dam[i,4]
    }else if(Dam4_Stor >= dam4_Stor_Nor){ #총 저수량 이하 유효저수량 이상
      Dam4_Spillway <- 0
      R4_Agr <- demand_dam[i,4]
    }else if(Dam4_Stor >= dam4_Stor_Min){ #유효저수량 이하 사수량 이상
      Dam4_Spillway <- 0
      R4_Agr <- demand_dam[i,4] * 0.8
    }else{ #사수량 이하
      Dam4_Spillway <- 0
      R4_Agr <- 0
    }
    Dam4_Stor <- Dam4_Stor + inflow_301103[i,] - Intake12 + (Intake12 * alpha_Intake)  - Dam4_Spillway
    Dam4 <- R4_Agr
    df_dam[i,4] <- R4_Agr
    
    ## 미호천 3011
    ### Intake 11
    if(R4_Agr * alpha_Agr + Dam4_Spillway + inflow_3011[i,] >= demand_intake[i,11]){
      Intake11 <- demand_intake[i,11]
    }else if(R4_Agr * alpha_Agr + Dam4_Spillway + inflow_3011[i,] > 0){
      Intake11 <- R4_Agr * alpha_Agr + Dam4_Spillway + inflow_3011[i,]
    }else{
      Intake11 <- 0
    }
    df_intake[i,11] <- Intake11
    
    ### Agr 10
    if(R4_Agr * alpha_Agr + Dam4_Spillway + inflow_3011[i,] + (Intake9 * alpha_Intake) - Intake11 + (Intake11 * alpha_Intake) >= demand_agr[i,10]- R4_Agr){
      Agr10 <- demand_agr[i,10] - R4_Agr
    }else if (R4_Agr * alpha_Agr + Dam4_Spillway + inflow_3011[i,] + (Intake9 * alpha_Intake)- Intake11 + (Intake11 * alpha_Intake) > 0){
      Agr10 <- R4_Agr * alpha_Agr + Dam4_Spillway - Intake11 +(Intake11 * alpha_Intake) +  inflow_3011[i,]
    }else{
      Agr10 <- 0
    }
    df_agr[i,10] <- Agr10
    
    ### Node E
    Node_E <- Node_D - Agr9 + (Agr9 * alpha_Agr) - Power3 + (Power3 * alpha_Pow) +
              (R4_Agr * alpha_Agr) + Dam4_Spillway - Intake11 + (Intake11 * alpha_Intake) + 
              inflow_3011[i,] + (Intake9 * alpha_Intake) - Agr10 + (Agr10 * alpha_Agr)
    
    ### Boryeong Dam
    if(Dam3_Stor > dam3_Stor_Max){ #Flood
      Dam3_Spillway <- Dam3_Stor + inflow_320305[i,] - dam3_Stor_Max
      R3_Agr <- Release_Boryeong$A_LV0[i]
      R3_Ind <- Release_Boryeong$I_LV0[i]
      R3_Env <- Release_Boryeong$E_LV0[i]
      Dam3_Stor <- Dam3_Stor + inflow_320305[i,] - (R3_Agr + R3_Env + R3_Ind) - Dam3_Spillway
      df_waterway[i] <- 0
    } else if(Dam3_Stor >= Release_Boryeong$LV1[i]){ #normal(LV0)
      Dam3_Spillway <- 0
      R3_Agr = Release_Boryeong$A_LV0[i]
      R3_Ind = Release_Boryeong$I_LV0[i]
      R3_Env = Release_Boryeong$E_LV0[i]
      Dam3_Stor = Dam3_Stor + inflow_320305[i,] - (R3_Agr + R3_Env + R3_Ind)
      df_waterway[i] <- 0
    } else if(Dam3_Stor >= Release_Boryeong$LV2[i]){ #Interest(LV1)
      Dam3_Spillway <- 0
      R3_Agr = Release_Boryeong$A_LV1[i]
      R3_Ind = Release_Boryeong$I_LV1[i]
      R3_Env = Release_Boryeong$E_LV1[i]
      Dam3_Stor = Dam3_Stor + inflow_320305[i,] - (R3_Agr + R3_Env + R3_Ind) + waterway
      Node_E <- Node_E - waterway
      df_waterway[i] <- waterway
    } else if(Dam3_Stor >= Release_Boryeong$LV3[i]){ #Watch(LV2)
      Dam3_Spillway <- 0
      R3_Agr = Release_Boryeong$A_LV2[i] 
      R3_Ind = Release_Boryeong$I_LV2[i] 
      R3_Env = Release_Boryeong$E_LV2[i]
      Dam3_Stor = Dam3_Stor + inflow_320305[i,] - (R3_Agr + R3_Env + R3_Ind) + waterway
      Node_E <- Node_E - waterway
      df_waterway[i] <- waterway
    } else if(Dam3_Stor >= Release_Boryeong$LV4[i]){ #Alert(LV3)
      Dam3_Spillway <- 0
      R3_Agr = Release_Boryeong$A_LV3[i]
      R3_Ind = Release_Boryeong$I_LV3[i] 
      R3_Env = Release_Boryeong$E_LV3[i] 
      Dam3_Stor = Dam3_Stor + inflow_320305[i,] - (R3_Agr + R3_Env + R3_Ind) + waterway
      Node_E <- Node_E - waterway
      df_waterway[i] <- waterway
    } else if(Dam3_Stor >= dam3_Stor_Min){ #Serious(LV4)
      Dam3_Spillway <- 0
      R3_Agr = Release_Boryeong$A_LV4[i] 
      R3_Ind = Release_Boryeong$I_LV4[i]
      R3_Env = Release_Boryeong$E_LV4[i] 
      Dam3_Stor = Dam3_Stor + inflow_320305[i,] - (R3_Agr + R3_Env + R3_Ind) + waterway
      Node_E <- Node_E - waterway
      df_waterway[i] <- waterway
    } else { #Under LWL
      Dam3_Spillway <- 0
      R3_Agr = 0
      R3_Ind = 0
      R3_Env = 0
      Dam3_Stor = Dam3_Stor + inflow_320305[i,] - (R3_Agr + R3_Env + R3_Ind) + waterway
      Node_E <- Node_E - waterway
      df_waterway[i] <- waterway
    }
    Dam3 <- R3_Agr + R3_Ind + R3_Env
    df_dam[i,3] <- Dam3
    
    ### Agr 11
    if(Node_E >= demand_agr[i,11]){
      Agr11 <- demand_agr[i,11]
    }else if (Node_E > 0){
      Agr11 <- Node_E
    }else{
      Agr11 <- 0
    }
    df_agr[i,11] <- Agr11
    
    ## 논산천 3013
    ### 탑정저수지 Dam 5
    if(Dam5_Stor >= dam5_Stor_Max){ #총 저수량 이상
      Dam5_Spillway <- Dam5_Stor + inflow_3013[i,] - dam5_Stor_Max
      R5_Agr <- demand_dam[i,5]
    }else if(Dam5_Stor >= dam5_Stor_Nor){ #총 저수량 이하 유효저수량 이상
      Dam5_Spillway <- 0
      R5_Agr <- demand_dam[i,5]
    }else if(Dam5_Stor >= dam5_Stor_Min){ #유효저수량 이하 사수량 이상
      Dam5_Spillway <- 0
      R5_Agr <- demand_dam[i,5] * 0.8
    }else{ #사수량 이하
      Dam5_Spillway <- 0
      R5_Agr <- 0
    }
    Dam5_Stor <- Dam5_Stor + inflow_3013[i,] - R5_Agr - Dam5_Spillway
    Dam5 <- R5_Agr
    df_dam[i,5] <- Dam5
   
    ### Agr 12
    if(Dam5 * alpha_Agr + Dam5_Spillway >= demand_agr[i,12]){
      Agr12 <- demand_agr[i,12]
    }else if (Dam5 * alpha_Agr + Dam5_Spillway > 0){
      Agr12 <- Dam5 * alpha_Agr
    }else{
      Agr12 <- 0
    }
    df_agr[i,12] <- Agr12

    
    ## Node F
    Node_F <- Node_E - Agr11 + (Agr11 * alpha_Agr) + R5_Agr * alpha_Agr - Agr12 + (Agr12 *alpha_Agr) +Dam5_Spillway
    
    ### Agr 13
    if(Node_F >= demand_agr[i,13]){
      Agr13 <- demand_agr[i,13]
    }else if(Node_F > 0){
      Agr13 <- Node_F
    }else{
      Agr13 <- 0
    }
    df_agr[i,13] <- Agr13
    
    Outlet1 <- Node_F - Agr13 + (Agr13 * alpha_Agr)
    Outlet2 <- R3_Agr * alpha_Agr + R3_Env * alpha_Env + R3_Ind * alpha_Ind + Dam3_Spillway
    
    # Data restoring
    df_dam1_Stor[i] <- Dam1_Stor
    df_dam2_Stor[i] <- Dam2_Stor
    df_dam3_Stor[i] <- Dam3_Stor
    df_dam4_Stor[i] <- Dam4_Stor
    df_dam5_Stor[i] <- Dam5_Stor
    df_power1_Stor[i] <- Power1_Stor
    df_outlet1[i] <- Outlet1 
    df_outlet2[i] <- Outlet2 
    df_nodeA[i] <- Node_A
    df_nodeB[i] <- Node_B
    df_nodeC[i] <- Node_C
    df_nodeD[i] <- Node_D
    df_nodeE[i] <- Node_E
    df_nodeF[i] <- Node_F
    df_R1[i,] <- c(R1_Agr, R1_Ind, R1_Env)
    df_R2[i,] <- c(R2_Agr, R2_Ind, R2_Env)
    df_R3[i,] <- c(R3_Agr, R3_Ind, R3_Env)
    df_R4[i,] <- c(R4_Agr)
    df_R5[i,] <- c(R5_Agr)
    df_dam1_Spillway[i] <- Dam1_Spillway
    df_dam2_Spillway[i] <- Dam2_Spillway
    df_dam3_Spillway[i] <- Dam3_Spillway
    df_dam4_Spillway[i] <- Dam4_Spillway
    df_dam5_Spillway[i] <- Dam5_Spillway
    
    
    print(paste("[ITER] Day", i, "calculation completed"))

  # for syntax end : day

    # Agent based model ------------------------------------------------------ 
    # update demand after 1 month  
    
    if ((i %in% breaker[["ABM"]]) && (any(breaker$month[breaker$ABM == i] %in% c(5, 6, 7, 8))) && ABM == TRUE) { #i=61 
      decision_start <- i - breaker[which(i == breaker[["ABM"]]),"days"] +1 
      decision_end <- i 
      Shell <- data.frame(matrix(NA, ncol=3, nrow=13)) 
      agent <- Shell 
      agent[c(1:12),1] <- colSums(df_intake[decision_start:decision_end,]/org_demand_intake[decision_start:decision_end,], na.rm=TRUE) 
      agent[c(1,8),3] <- colSums(df_dam[decision_start:decision_end,]/org_demand_dam[decision_start:decision_end,], na.rm=TRUE)[c(1,2)] 
      agent[,2] <- colSums(df_agr[decision_start:decision_end,]/org_demand_agr[decision_start:decision_end,]) 
      agent[c(2,3,10),3] <- colSums(df_power[decision_start:decision_end,]/org_demand_power[decision_start:decision_end,], na.rm=TRUE) 
      
      if (length(unique(na.omit(unlist(agent)))) == 1) { ord <- matrix(NA, nrow = 3, ncol = 2) } else { ord <- order(as.vector(as.matrix(agent)), na.last = NA) } 
      
      top3_idx <- ord[1:3] 
      top3_pos <- arrayInd(top3_idx, dim(agent)) 
      
      ABM_Geum <- function(top3_pos, agent) { #k=3 # 1. top3 위치에서 각 셀까지의 맨하탄 거리 계산 (3개 지점에 대해) 
        
        distance_list <- lapply(1:3, function(k) { #k=2 
          
          if (is.na(top3_pos[k, 1])) 
            return(matrix(NA, nrow=nrow(agent), ncol=ncol(agent))) 
          
          distance_matrix <- matrix(NA, nrow=nrow(agent), ncol=ncol(agent)) 
          row <- top3_pos[k,1] 
          col <- top3_pos[k,2] 
          
          if (is.na(row) & is.na(col)){ invisible(NULL)} 
          else if (row == 1 & col == 3) { # Dam 1 
              distance_matrix[1,1] <- 1 }
          else if(row == 1 & col == 1){ # Intake 1 
            invisible(NULL) }
          else if(row == 2 & col == 1){ # Intake 2 
            invisible(NULL) } else if (row == 3 & col == 2) { # Agr 3 
              distance_matrix[3,1] <- 1 } 
          else if (row == 4 & col == 1) { # Intake 4 
                invisible(NULL) } 
          else if (row == 3 & col == 3){ # Power 1 
            invisible(NULL) } 
          else if (row == 5 & col == 2) { # Agr 5 
            distance_matrix[4,1] <- 1 } 
          else if (row == 6 & col == 1){ # Intake 6 
            invisible(NULL) } 
          else if (row == 7 & col == 2) { # Agr 7 
              distance_matrix[6,1] <- 1 } 
          else if (row == 8 & col == 2){ # Agr 8 
            invisible(NULL) } 
          else if (row == 10 & col == 2){ # Agr 10 
            distance_matrix[11,1] <- 1 } 
          else if (row ==11 & col == 1){ # Intake 11 
            invisible(NULL) } 
          else if(row == 12 & col == 1){ # Intake 12 
            invisible(NULL) }
          else if (row ==12 & col == 2){ # Agr 12 
            invisible(NULL) }
          else{ 
            
            for (y in 1:nrow(agent)) { 
            for (p in 1:ncol(agent)) { 
              distance_matrix[y,p] <- abs(y - row) + abs(p - col) # 맨하탄 거리 
            } } } 
          if (is.na(row)){ distance_matrix }
          else if (row +1 > 13) { distance_matrix }
          else{ distance_matrix[(row+1):13,] <- NA } 
          return(distance_matrix) } ) 
        
        
        # 2. 각 셀에 대해 3개 거리 중 최소값 선택 
        
        min_distance <- matrix(Inf, nrow=nrow(agent), ncol=ncol(agent)) # 초기값 무한대 
        
        for (n in 1:3) { 
          if (!all(is.na(distance_list[[n]]))) { 
            min_distance <- pmin(min_distance, distance_list[[n]], na.rm=TRUE) } } 
        min_distance[is.infinite(min_distance)] <- NA # 계산 불가 영역 처리 # 3. 최소 거리를 기반으로 agent_prob 계산 
        
        agent_prob <- matrix(1, nrow=nrow(agent), ncol=ncol(agent)) # 기본값 1 
        
        for (g in 1:nrow(agent)) { 
          for (t in 1:ncol(agent)) { 
          d <- min_distance[g,t] 
          if(!is.na(d)) { 
            if(d == 1) { 
            agent_prob[g,t] <- sample(c(0.8,0.9,1), size = 1, prob = c(0.6,0.3,0.1)) } 
            else if(d == 2) { agent_prob[g,t] <- sample(c(0.8,0.9, 1), size = 1, prob = c(1/3,1/3,1/3)) } 
            else if(d == 3) { agent_prob[g,t] <- sample(c(0.8,0.9,1), size = 1, prob = c(0.1,0.3,0.6)) } 
            else { agent_prob[g,t] <- 1.0 # 3 이상 거리는 영향 없음 
            } } } } 
        return(agent_prob) } 

    
      agent_prob <- ABM_Geum(top3_pos, agent)
      idx <- which(breaker$ABM == i) 
      
      breaker[idx, "first_row"]  <- top3_pos[1, 1]
      breaker[idx, "first_col"]  <- top3_pos[1, 2]
      breaker[idx, "second_row"] <- top3_pos[2, 1]
      breaker[idx, "second_col"] <- top3_pos[2, 2]
      breaker[idx, "third_row"]  <- top3_pos[3, 1]
      breaker[idx, "third_col"]  <- top3_pos[3, 2]

      update_start <- decision_end + 1 
      update_end <- decision_end +breaker[which(i == breaker[["ABM"]]),"days"] - 1
              
        # demand update  
    demand_intake[update_start:update_end,] <- sweep(demand_intake[update_start:update_end,],2,agent_prob[c(1:12),1],"*")
    demand_agr[update_start:update_end,] <- sweep(demand_agr[update_start:update_end,],2,agent_prob[1:13,2],"*")
    demand_power[update_start:update_end,] <- sweep(demand_power[update_start:update_end,],2,agent_prob[c(2,3,10),3],"*")
    Release_Yongdam[update_start:update_end,] <- Release_Yongdam[update_start:update_end,] * agent_prob[1,3]
    Release_Daecheong[update_start:update_end,] <- Release_Daecheong[update_start:update_end,] * agent_prob[8,3]

    
    print(paste("[ABM] ABM run completed — Day",i))
  } # end of ABM
  } # end of for (Water balance)
  
  
# Result checking -------------------------------------------------------------
 
  df_outlet1[df_outlet1 < 0 ]
  df_outlet2[df_outlet2 < 0 ]
  df_nodeA[df_nodeA < 0 ]
  df_nodeB[df_nodeB < 0 ]
  df_nodeC[df_nodeC < 0 ]
  df_nodeD[df_nodeD < 0 ]
  df_nodeE[df_nodeE < 0]
  df_nodeF[df_nodeF < 0]
   
  
  df_dam1_Spillway[df_dam1_Spillway > 0]
  df_dam2_Spillway[df_dam2_Spillway > 0]
  df_dam3_Spillway[df_dam3_Spillway > 0]
  df_dam4_Spillway[df_dam4_Spillway > 0]
  df_dam5_Spillway[df_dam5_Spillway > 0]
  
# Result ---------------------------------------------------------------------
  
  intake_vector <- c("Intake 1","Intake 2","Intake 3","Intake 4","Intake 5",
                     "Intake 6","Intake 7","Intake 8","Intake 9","Intake 10",
                     "Intake 11","Intake 12")
  table_intake <- data.frame(Stakeholder = intake_vector,
                             용수공급가능일수 =   colSums(df_intake/org_demand_intake, na.rm=TRUE))
  
  gg_intake <- table_intake %>%
    mutate(Stakeholder = fct_relevel(Stakeholder,intake_vector)) %>%
    ggplot(aes(x=Stakeholder, y = 용수공급가능일수)) + 
    geom_bar(stat = 'identity',width = 0.5, fill = "#edae49") +
    geom_hline(yintercept=365*opt_year) + theme_bw() + ylab("Days of Water Supply Availability")+
    ggtitle("취수장의 용수공급가능일수")
  
  agr_vector <- c("Agr 1","Agr 2","Agr 3","Agr 4","Agr 5",
                  "Agr 6","Agr 7","Agr 8","Agr 9","Agr 10",
                  "Agr 11","Agr 12","Agr 13")
  df_agr[,1] <- df_agr[,1] + df_R1[,1]
  df_agr[,10] <- df_agr[,10] + df_dam[,4]
  df_agr[,12] <- df_agr[,12] + df_dam[,5]
  
  table_agr <- data.frame(Stakeholder = agr_vector,
                          용수공급가능일수 =   colSums(df_agr/org_demand_agr, na.rm=TRUE))
  
  gg_agr <- table_agr %>%
    mutate(Stakeholder = fct_relevel(Stakeholder,agr_vector)) %>%
    ggplot(aes(x=Stakeholder, y = 용수공급가능일수)) + 
    geom_bar(stat = 'identity',width = 0.5, fill="#66a182") +
    geom_hline(yintercept=opt_year *length(yday("2025-05-01"):yday("2025-09-30"))) + theme_bw() +
    ylab("Days of Water Supply Availability")+
    ggtitle("농업용수의 용수공급가능일수")
  
  
  dam_vector <- c("Dam 1","Dam 2","Dam 3","Dam 4","Dam 5")
  
  table_dam <- data.frame(Stakeholder = dam_vector,
                          용수공급가능일수 =   colSums(df_dam/org_demand_dam, na.rm=TRUE))
  
  gg_dam <- table_dam %>%
    mutate(Stakeholder = fct_relevel(Stakeholder,dam_vector)) %>%
    ggplot(aes(x=Stakeholder, y = 용수공급가능일수)) + 
    geom_bar(stat = 'identity',width = 0.5, fill = "#B39DDB") +
    geom_hline(yintercept=365*opt_year) + theme_bw() + ylab("Days of Water Supply Availability")+
    ggtitle("다목적댐의 용수공급가능일수")
  
  power_vector <- c("Power 1","Power 2","Power 3")
  table_power <- data.frame(Stakeholder = power_vector,
                            용수공급가능일수 =   colSums(df_power/org_demand_power, na.rm=TRUE))
  gg_power <- table_power %>%
    mutate(Stakeholder = fct_relevel(Stakeholder,power_vector)) %>%
    ggplot(aes(x=Stakeholder, y = 용수공급가능일수)) + 
    geom_bar(stat = 'identity',width = 0.5, fill="#d1495b") +
    geom_hline(yintercept=365*opt_year) + theme_bw() + ylab("Days of Water Supply Availability") +
    ggtitle("발전용수의 용수공급가능일수")
  
  # view(df_intake/org_demand_intake)
  # view(df_agr/org_demand_agr)
  # view(df_power/org_demand_power)
  
  Average_days_intake <-  colSums(df_intake/org_demand_intake) %>% mean()
  Average_days_agr <-  colSums(df_agr/org_demand_agr, na.rm=TRUE) %>% mean()
  Average_days_power <-  colSums(df_power/org_demand_power) %>% mean()
  
  # Profit calculation
  ## Agricultural Water
  밭 <- data.frame(작물 = c("감자", "고구마", "채소류","과실류"),
                  단위가격 = c(1402,3339,mean(c(927,686,7270,808,6916)),mean(c(4876,3704,4487,4270))))
  
  profit_agr <- data.frame(유역 = c("용담댐","용담댐하류","무주남대천","영동천",
                                  "초강","대청댐상류","보청천","대청댐","갑천",
                                  "대청댐하류","미호천","금강공주","논산천",
                                  "금강하구언"),
                           수리답 = c(3002,178,592,1456,2666,405,2923,3037,2480,
                                   919,15759,17365,10765,12583),
                           관개전 = c(1887,293,1399,2812,4411,438,1848,1720,1573,
                                   324,6313,6299,3847,1602)) %>% # unit : ha = 10000 m^3
    mutate(수리답비율 = 수리답 / (수리답 + 관개전),
           관개전비율 = 1 - 수리답비율, 
           공급량 = append(colSums(df_agr),sum(df_R2[,1]), after = 7 ) , 
           수리답공급량 = 공급량 * 수리답비율,
           관개전공급량 = 공급량 * 관개전비율,
           수리답이익 = 수리답공급량 * 2107.8,
           관개전이익 = 관개전공급량 * 11196.4,
           전체이익 = 수리답이익 + 관개전이익) %>%
    select(전체이익) %>% sum()
  
  max_profit_agr <- data.frame(유역 = c("용담댐","용담댐하류","무주남대천","영동천",
                                      "초강","대청댐상류","보청천","대청댐","갑천",
                                      "대청댐하류","미호천","금강공주","논산천",
                                      "금강하구언"),
                               수리답 = c(3002,178,592,1456,2666,405,2923,3037,2480,
                                       919,15759,17365,10765,12583),
                               관개전 = c(1887,293,1399,2812,4411,438,1848,1720,1573,
                                       324,6313,6299,3847,1602)) %>% # unit : ha = 10000 m^3
    mutate(수리답비율 = 수리답 / (수리답 + 관개전),
           관개전비율 = 1 - 수리답비율, 
           공급량 = append(colSums(df_agr),sum(df_R2[,1]), after = 7 ), 
           수리답공급량 = 공급량 * 수리답비율,
           관개전공급량 = 공급량 * 관개전비율,
           수리답이익 = 수리답공급량 * 2107.8,
           관개전이익 = 관개전공급량 * 11196.4,
           전체이익 = 수리답이익 + 관개전이익) %>%
    select(전체이익) %>% sum()
  
  ## Intake Water
  profit_intake <- data.frame(공급량 = colSums(df_intake)) %>%
    mutate(생활용수 = 공급량 * 0.83,
           공업용수 = 공급량 * 0.17,
           생활용수이익 = 생활용수 * mean(c(1539,1898.2,1839.7)),
           공업용수이익 = 공업용수 * 5583) %>% select(생활용수이익, 공업용수이익) %>% sum()
  
  max_profit_intake <- data.frame(공급량 = colSums(org_demand_intake)) %>%
    mutate(생활용수 = 공급량 * 0.83,
           공업용수 = 공급량 * 0.17,
           생활용수이익 = 생활용수 * mean(c(1539,1898.2,1839.7)),
           공업용수이익 = 공업용수 * 5583) %>% select(생활용수이익, 공업용수이익) %>% sum()
  
  
  ## Power water
  profit_power <- data.frame(공급량 = colSums(df_power)) %>%
    mutate(정산단가 = c(277.58,206.59, 206.59),
           발전설비 = c(600000,2121,2310),
           비율 = 발전설비/(공급량/6),
           이익 = 정산단가 * 비율) %>% select(이익) %>% sum()
  
  max_profit_power <- data.frame(공급량 = colSums(org_demand_power)) %>%
    mutate(정산단가 = c(277.58,206.59, 206.59),
           발전설비 = c(600000,2121,2310),
           비율 = 발전설비/(demand_power[1,]/6),
           이익 = 정산단가 * 비율) %>% select(이익) %>% sum()
  
  profit <- sum(c(profit_intake,profit_power, profit_agr))
  
  max_profit <- sum(c(max_profit_agr, max_profit_intake, max_profit_power))
  
  profit_ratio <- profit/max_profit
  
  profit_ratio_power <- profit_power/max_profit_power
  profit_ratio_intake <- profit_intake/max_profit_intake
  profit_ratio_agr <- profit_agr/max_profit_agr
  
  # Gini coefficient calculation
  
  gini_total <- Gini(c(colSums(df_agr/demand_agr, na.rm=TRUE)/(length(yday("2025-05-01"):yday("2025-09-30"))*opt_year), 
                       colSums(df_intake/demand_intake, na.rm=TRUE)/opt_year*365,
                       colSums(df_power/demand_power, na.rm=TRUE)/opt_year*365))
  
  gini_intake <- Gini(colSums(df_intake/demand_intake, na.rm=TRUE))
  
  gini_power <- Gini(colSums(df_power/demand_power, na.rm=TRUE))
  
  gini_agr <- Gini(colSums(df_agr/demand_agr, na.rm=TRUE))
  
  
  #LC <- plot(Lc(c(colSums(df_agr/demand_agr, na.rm=TRUE)/(length(yday("2025-05-01"):yday("2025-09-30"))*2), 
  #                colSums(df_intake/demand_intake, na.rm=TRUE)/730,
  #                colSums(df_power/demand_power, na.rm=TRUE)/730)))
  
  # Result
  
  Result_list <- list(df_intake, df_agr, df_power, demand_intake,demand_agr,demand_power, 
                      org_demand_intake,org_demand_agr,org_demand_power,
                      table_intake,table_agr,table_power, table_dam,
                      Average_days_intake,Average_days_agr,Average_days_power,
                      gg_intake,gg_agr,gg_power, gg_dam,
                      gini_total, gini_intake, gini_power, gini_agr,
                      profit,  profit_ratio,
                      profit_intake, profit_agr, profit_power,
                      profit_ratio_power,profit_ratio_agr,profit_ratio_intake, breaker)
  
  names(Result_list) <- c("df_intake","df_agr","df_power", "demand_intake", "demand_agr","demand_power",
                          "org_demand_intake", "org_demand_agr","org_demand_power",
                          "Table_intake","Table_agr","Table_power","Table_dam",
                          "Average_days_intake","Average_days_agr","Average_days_power",
                          "gg_intake","gg_agr","gg_power","gg_dam",
                          "Gini_coefficient", "Gini_intake", "Gini_power", "Gini_agr",
                          "System_profit",  "Profit_ratio", 
                          "System_profit_intake","System_profit_agr","System_profit_power",
                          "Profit_ratio_power","Profit_ratio_agr","Profit_ratio_intake","breaker")
  return(Result_list) 
}


