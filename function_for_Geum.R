################################################################################
# 작성자: 김연주
# 금강유역 function_for_Geum 파일
# 문의 or 오류 신고 : iamkimyeonju@gmail.com
# 최종 수정일 : 2025년 1월 28일 (README 참고)
################################################################################

setwd("C:/Users/pqh24002/OneDrive - University of Connecticut/University of Seoul/3차년도/Research/코드n자료")

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

Geum_Network <- function(supply_scenario = 1, demand_scenario = 1, drought_scenario = 1, 
                         alpha1 =1, alpha2 = 1,alpha3=1){
  
  #supply_scenario = 1; demand_scenario = 1; drought_scenario = 22; alpha1 =1; alpha2 = 1; alpha3=1
  
  # General Setting
  supply_scenario <- supply_scenario # 공급시나리오 supply_scenario
  demand_scenario <- demand_scenario # 수요시나리오 demand_scenario
  drought_scenario <- drought_scenario # 몇번째 가뭄시나리오를 사용할 것 인가?
  
  opt <- 365*2
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
  
  runoff_array <- array(NA,dim = c(365,137,22),
                        dimnames = list(c(1:365),colnames(read_csv("drought scenario/Sub-basin_runoff_GG_DU3_RP5.csv",show_col_types = FALSE))
                                        ,c(1:22)))
  
  runoff_array[,,1] <- as.matrix(read.csv("drought scenario/Sub-basin_runoff_GG_DU3_RP5.csv",header=TRUE))
  runoff_array[,,2] <- as.matrix(read.csv("drought scenario/Sub-basin_runoff_GG_DU3_RP10.csv",header=TRUE))
  runoff_array[,,3] <- as.matrix(read.csv("drought scenario/Sub-basin_runoff_GG_DU3_RP20.csv",header=TRUE))
  runoff_array[,,4] <- as.matrix(read.csv("drought scenario/Sub-basin_runoff_GG_DU3_RP30.csv",header=TRUE))
  runoff_array[,,5] <- as.matrix(read.csv("drought scenario/Sub-basin_runoff_GG_DU4_RP5.csv",header=TRUE))
  runoff_array[,,6] <- as.matrix(read.csv("drought scenario/Sub-basin_runoff_GG_DU4_RP10.csv",header=TRUE))
  runoff_array[,,7] <- as.matrix(read.csv("drought scenario/Sub-basin_runoff_GG_DU4_RP20.csv",header=TRUE))
  runoff_array[,,8] <- as.matrix(read.csv("drought scenario/Sub-basin_runoff_GG_DU4_RP30.csv",header=TRUE))
  runoff_array[,,9] <- as.matrix(read.csv("drought scenario/Sub-basin_runoff_GG_DU5_RP5.csv",header=TRUE))
  runoff_array[,,10] <- as.matrix(read.csv("drought scenario/Sub-basin_runoff_GG_DU5_RP10.csv",header=TRUE))
  runoff_array[,,11] <- as.matrix(read.csv("drought scenario/Sub-basin_runoff_GG_DU5_RP20.csv",header=TRUE))
  runoff_array[,,12] <- as.matrix(read.csv("drought scenario/Sub-basin_runoff_GG_DU5_RP30.csv",header=TRUE))
  runoff_array[,,13] <- as.matrix(read.csv("drought scenario/Sub-basin_runoff_GG_DU6_RP5.csv",header=TRUE))
  runoff_array[,,14] <- as.matrix(read.csv("drought scenario/Sub-basin_runoff_GG_DU6_RP10.csv",header=TRUE))
  runoff_array[,,15] <- as.matrix(read.csv("drought scenario/Sub-basin_runoff_GG_DU6_RP20.csv",header=TRUE))
  runoff_array[,,16] <- as.matrix(read.csv("drought scenario/Sub-basin_runoff_GG_DU6_RP30.csv",header=TRUE))
  runoff_array[,,17] <- as.matrix(read.csv("drought scenario/Sub-basin_runoff_GG_DU12_RP10.csv",header=TRUE))
  runoff_array[,,18] <- as.matrix(read.csv("drought scenario/Sub-basin_runoff_GG_DU12_RP20.csv",header=TRUE))
  runoff_array[,,19] <- as.matrix(read.csv("drought scenario/Sub-basin_runoff_GG_DU12_RP30.csv",header=TRUE))
  runoff_array[,,20] <- as.matrix(read.csv("drought scenario/Sub-basin_runoff_GG_DU12_RP50.csv",header=TRUE))
  runoff_array[,,21] <- as.matrix(read.csv("drought scenario/Sub-basin_runoff_GG_DU24_RP50.csv",header=TRUE))
  runoff_array[,,22] <- as.matrix(read.csv("drought scenario/Sub-basin_runoff_GG_DU24_RP100.csv",header=TRUE))
  
  runoff_array <- runoff_array * 86400 /1000000 #CMS --> m^3/day --> MCM/day
  
  inflow_3001 <- subset(runoff_array[,,drought_scenario],select= as.character(runoff_code$용담댐)) %>% rowSums() %>% as.data.frame()
  inflow_3001 <- bind_rows(replicate(2,inflow_3001, simplify=FALSE))
  
  inflow_3002 <- subset(runoff_array[,,drought_scenario],select= as.character(runoff_code$용담댐하류)) %>% rowSums() %>% as.data.frame()
  inflow_3002 <- bind_rows(replicate(2,inflow_3002, simplify=FALSE))
  
  inflow_3003 <- subset(runoff_array[,,drought_scenario],select= as.character(runoff_code$무주남대천)) %>% rowSums() %>% as.data.frame()
  inflow_3003 <- bind_rows(replicate(2,inflow_3003, simplify=FALSE))
  
  inflow_3004 <- subset(runoff_array[,,drought_scenario],select= as.character(runoff_code$영동천)) %>% rowSums() %>% as.data.frame()
  inflow_3004 <- bind_rows(replicate(2,inflow_3004, simplify=FALSE))
  
  inflow_3005 <- subset(runoff_array[,,drought_scenario],select= as.character(runoff_code$초강)) %>% rowSums() %>% as.data.frame()
  inflow_3005 <- bind_rows(replicate(2,inflow_3005, simplify=FALSE))
  
  inflow_3006 <- subset(runoff_array[,,drought_scenario],select= as.character(runoff_code$대청댐상류)) %>% rowSums() %>% as.data.frame()
  inflow_3006 <- bind_rows(replicate(2,inflow_3006, simplify=FALSE))
  
  inflow_3007 <- subset(runoff_array[,,drought_scenario],select= as.character(runoff_code$보청천)) %>% rowSums() %>% as.data.frame()
  inflow_3007 <- bind_rows(replicate(2,inflow_3007, simplify=FALSE))
  
  inflow_3008 <- subset(runoff_array[,,drought_scenario],select= as.character(runoff_code$대청댐)) %>% rowSums() %>% as.data.frame()
  inflow_3008 <- bind_rows(replicate(2,inflow_3008, simplify=FALSE))
  
  inflow_3009 <- subset(runoff_array[,,drought_scenario],select= as.character(runoff_code$갑천)) %>% rowSums() %>% as.data.frame()
  inflow_3009 <- bind_rows(replicate(2,inflow_3009, simplify=FALSE))
  
  inflow_3010 <- subset(runoff_array[,,drought_scenario],select= as.character(runoff_code$대청댐하류)) %>% rowSums() %>% as.data.frame()
  inflow_3010 <- bind_rows(replicate(2,inflow_3010, simplify=FALSE))
  
  inflow_3011 <- subset(runoff_array[,,drought_scenario],select= as.character(runoff_code$미호천)) %>% rowSums() %>% as.data.frame()
  inflow_3011 <- bind_rows(replicate(2,inflow_3011, simplify=FALSE))
  
  inflow_3012 <- subset(runoff_array[,,drought_scenario],select= as.character(runoff_code$금강공주)) %>% rowSums() %>% as.data.frame()
  inflow_3012 <- bind_rows(replicate(2,inflow_3012, simplify=FALSE))
  
  inflow_3013 <- subset(runoff_array[,,drought_scenario],select= as.character(runoff_code$논산천)) %>% rowSums() %>% as.data.frame()
  inflow_3013 <- bind_rows(replicate(2,inflow_3013, simplify=FALSE))
  
  inflow_3014 <- subset(runoff_array[,,drought_scenario],select= as.character(runoff_code$금강하구언)) %>% rowSums() %>% as.data.frame()
  inflow_3014 <- bind_rows(replicate(2,inflow_3014, simplify=FALSE))
  
  inflow_301103 <- subset(runoff_array[,,drought_scenario],select= as.character(runoff_code$백곡댐)) %>% rowSums() %>% as.data.frame()
  inflow_301103 <- bind_rows(replicate(2,inflow_301103, simplify=FALSE))
  
  inflow_320305 <- subset(runoff_array[,,drought_scenario],select= as.character(runoff_code$보령댐)) %>% rowSums() %>% as.data.frame()
  inflow_320305 <- bind_rows(replicate(2,inflow_320305, simplify=FALSE))
  
  # 다목적댐
  ## 용담댐
  Dam1_Stor <- 815.0*0.7 # 총 저수용량 * 0.7 (MCM)
  dam1_Stor_Min <- 815.0*0.1 # 총 저수용량 * 0.1 (MCM)
  dam1_Stor_Max <- 815.0* 0.9 # 총 저수용량 * 0.9 (MCM)
  R1_Agr <- 0; R1_Ind <- 0; R1_Env <- 0;
  df_dam1_Stor <- matrix(NA, nrow = opt)
  df_dam1_Spillway <- matrix(NA, nrow = opt)
  df_R1 <- matrix(NA, nrow= opt, ncol=3, dimnames = list(1:opt, c('Agr','Ind','Env')))
  Dam1_Spillway <- 0
  Release_Yongdam <- sweep(read.csv("Yongdam_Release.csv")[2:length(read.csv("Yongdam_Release.csv"))],1,c(31, 30, 31,31, 28, 31, 30, 31, 30, 31, 31, 30),FUN="/")
  Release_Yongdam <- as.data.frame(lapply(Release_Yongdam, rep, c(31, 30, 31,31, 28, 31, 30, 31, 30, 31, 31, 30)))
  Release_Yongdam <- bind_rows(replicate(2,Release_Yongdam, simplify=FALSE))
  Release_Yongdam[,6:length(Release_Yongdam)] <- Release_Yongdam[,6:length(Release_Yongdam)] * supply_scenario
  
  ## 대청댐
  Dam2_Stor <- 1490.0*0.7 # 총 저수용량 * 0.7 (MCM)
  dam2_Stor_Min <- 1490.0*0.1 # 총 저수용량 * 0.1 (MCM)
  dam2_Stor_Max <- 1490.0* 0.9 # 총 저수용량 * 0.9 (MCM)
  R2_Agr = 0; R2_Ind = 0; R2_Env = 0;
  df_dam2_Stor <- matrix(NA,nrow =  opt)
  df_dam2_Spillway <- matrix(NA,nrow =  opt)
  df_R2 <- matrix(NA, nrow= opt, ncol=3, dimnames = list( 1:opt,c('Agr', 'Ind','Env')))
  Dam2_Spillway <- 0
  Release_Daecheong <- sweep(read.csv("Daecheong_Release.csv")[2:length(read.csv("Daecheong_Release.csv"))],1,c(31, 30, 31,31, 28, 31, 30, 31, 30, 31, 31, 30),FUN="/")
  Release_Daecheong <- as.data.frame(lapply(Release_Daecheong, rep, c(31, 30, 31,31, 28, 31, 30, 31, 30, 31, 31, 30)))
  Release_Daecheong <- bind_rows(replicate(2,Release_Daecheong, simplify=FALSE))
  Release_Daecheong[,6:length(Release_Daecheong)] <- Release_Daecheong[,6:length(Release_Daecheong)] * supply_scenario
  
  ## 보령댐
  Dam3_Stor <- 116.9*0.7 # 총 저수용량 * 0.7 (MCM)
  dam3_Stor_Min <- 116.9*0.1 # 총 저수용량 * 0.1 (MCM)
  dam3_Stor_Max <- 116.9* 0.9 # 총 저수용량 * 0.9 (MCM)
  R3_Agr = 0; R3_Ind = 0; R3_Env = 0;
  df_dam3_Stor <- matrix(NA,nrow =opt)
  df_dam3_Spillway <- matrix(NA,nrow =  opt)
  df_R3 <- matrix(NA, nrow=opt, ncol=3, dimnames = list( 1:opt,c('Agr', 'Ind','Env')))
  waterway <- 3.45 # 보령댐 도수로
  df_waterway <- matrix(NA,nrow=opt)
  Dam3_Spillway <- 0
  Release_Boryeong <- sweep(read.csv("Boryeong_Release.csv")[2:length(read.csv("Boryeong_Release.csv"))],1,c(31, 30, 31,31, 28, 31, 30, 31, 30, 31, 31, 30),FUN="/")
  Release_Boryeong <- as.data.frame(lapply(Release_Boryeong, rep,c(31, 30, 31,31, 28, 31, 30, 31, 30, 31, 31, 30)))
  Release_Boryeong <- bind_rows(replicate(2,Release_Boryeong, simplify=FALSE))
  Release_Boryeong[,6:length(Release_Boryeong)] <- Release_Boryeong[,6:length(Release_Boryeong)] * supply_scenario
  
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
  demand_BG <- (40000000/1000000) /opt # 연간공급량 40,000,000
  Dam4_Spillway <- 0
  
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
  Dam5_Spillway <- 0
  
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
  
  ## Power 1 : 무주양수발전소
  Power1_Stor <- as.numeric()
  Power1_Stor_Nor <- as.numeric()
  Power1_Stor_Min <- as.numeric()
  Power1_Stor_Max <- as.numeric()
  df_Power1 <- matrix(NA, nrow=opt , ncol=1)
  df_power1_Stor <- matrix(NA, nrow=opt)
  df_power1_Spillway <- matrix(NA,nrow=opt)
  Power1_Spillway <- 0
  
  # 취수장
  ## Intake
  Intake.name <- list(Intake1 = c("삼락취수장"),
                      Intake2 = c("용담댐"),
                      Intake3 = c("구천취수장", "무풍취수장","설천취수장","무주취수장"),
                      Intake4 = c("영동취수장"),
                      Intake5 = c("모동취수장","궁촌취수장"),
                      Intake6 = c("옥천취수장"),
                      Intake7 = c("교사취수장","원남취수장","청산취수장"),
                      Intake8 = c("안남취수장"),
                      Intake9 = c("대청취수장"),
                      Intake10 = c("중리취수장"),
                      Intake11 = c("국전취수장"),
                      Intake12 = c("삼정취수장"),
                      Intake13 = c("백곡취수장"),
                      Intake14 = c("병천취수장","현도취수장"),
                      Intake15 = c("보령댐"))
  
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
  
  df_intake <- matrix(NA,nrow = opt,ncol = 15)
  demand_intake <- matrix(NA,nrow = opt,ncol = 15)
  days_in_month <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  
  for(i in 1:15){
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
  demand_intake <- rbind(demand_intake,demand_intake) * demand_scenario
  demand_intake[,2] <- rowSums(Release_Yongdam[,5:7]) #용담댐: 취수장 자료가 아닌 운영룰 기반
  demand_intake[,9] <- rowSums(Release_Daecheong[,6:8]) #대청댐: 취수장 자료가 아닌 운영룰 기반
  demand_intake[,15] <- rowSums(Release_Boryeong[,6:8]) #보령댐: 취수장 자료가 아닌 운영룰 기반
  
  # 농업용수
  ## 하천수 허가량 관개기에만 농업용수 사용, 비관개기에는 0
  df_agr <- matrix(NA,nrow = opt,ncol = 14)
  demand_agr <- matrix(NA,nrow = opt,ncol = 14)

  river_agr <- data.frame(유역 = c("용담댐","용담댐하류","무주남대천","영동천",
                                 "초강","대청댐상류","보청천","대청댐","갑천",
                                 "대청댐하류","미호천","금강공주","논산천",
                                 "금강하구언"),
                          이용현황 = c(76106.10,6951.30,25862.00,57247.00,83853.70,
                                   12024.80,77021.60,81823.20,67460.50,23540.70,
                                   379454.50,386994.80,212032.90, 237368.20)/1000) %>% # MCM/year
    mutate(관개기이용 = 이용현황/length(yday("2025-05-01"):yday("2025-09-30"))) #MCM/day (관개기)
  
  for (i in 1:14){ 
    assign(paste0("Agr",i),NA)
    demand_agr[yday("2025-05-01"):yday("2025-09-30"),i] <- river_agr$관개기이용[i]
  }
  demand_agr[1:yday("2025-04-30"),] <- 0 #비관개기 0
  demand_agr[yday("2025-10-01"):365,] <- 0 #비관개기 0
  demand_agr <- rbind(demand_agr[yday("2025-10-01"):365,],demand_agr[1:yday("2025-09-30"),]) # 10월 시작
  demand_agr <- rbind(demand_agr,demand_agr) * demand_scenario

  # Water Balance
  for(i in 1:opt){ #Start from October
    
    ## 3001 용담댐
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
      R1_Agr <- Release_Yongdam$A_LV2[i] * alpha1
      R1_Ind <- Release_Yongdam$I_LV2[i] * alpha1
      R1_Env <- Release_Yongdam$E_LV2[i] * alpha1
    } else if(Dam1_Stor >= Release_Yongdam$LV4[i]){ #Alert(LV3)
      Dam1_Spillway <- 0
      R1_Agr <- Release_Yongdam$A_LV3[i] * alpha2
      R1_Ind <- Release_Yongdam$I_LV3[i] * alpha2
      R1_Env <- Release_Yongdam$E_LV3[i] * alpha2
    } else if(Dam1_Stor >= dam1_Stor_Min){ #Serious(LV4)
      Dam1_Spillway <- 0
      R1_Agr <- Release_Yongdam$A_LV4[i] * alpha3
      R1_Ind <- Release_Yongdam$I_LV4[i] * alpha3
      R1_Env <- Release_Yongdam$E_LV4[i] * alpha3
    } else { #Under LWL
      Dam1_Spillway <- 0
      R1_Agr <- 0
      R1_Ind <- 0
      R1_Env <- 0
    }
    diff1 <- Dam1_Stor
    Dam1_Stor <- Dam1_Stor - (R1_Agr + R1_Ind + R1_Env) + inflow_3001[i,] - Intake1 +
      (Intake1 * alpha_Intake) - Dam1_Spillway
    diff1 <- diff1 - Dam1_Stor  
    Intake2 <- R1_Agr + R1_Ind + R1_Env
    df_intake[i,2] <- Intake2
    
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
    
    if(inflow_3003[i,] >= demand_power[i,1]){
      Power1 <- demand_power[i,1]
    }else if(inflow_3003[i,] > 0){
      Power1 <- inflow_3003[i,]
    }else {
      Power1 <- 0
    }
    df_power[i,1] <- Power1
    
    ### Intake 3
    if(inflow_3003[i,] - Power1 + Power1 *alpha_Pow >= demand_intake[i,3]){
      Intake3 <- demand_intake[i,3]
    }else if (inflow_3003[i,] - Power1 + Power1 *alpha_Pow >0){
      Intake3 <- inflow_3003[i,] - Power1 + Power1 *alpha_Pow
    }else {
      Intake3 <- 0
    }
    df_intake[i,3] <- Intake3
    
    ### Agr 3
    if(inflow_3003[i,] - Power1 + Power1 *alpha_Pow - Intake3 + (Intake3*alpha_Intake) >= demand_agr[i,3]){
      Agr3 <- demand_agr[i,3]
    }else if (inflow_3003[i,] - Power1 + Power1 *alpha_Pow - Intake3 + (Intake3*alpha_Intake) > 0){
      Agr3 <- inflow_3003[i,] - Power1 + Power1 *alpha_Pow - Intake3 + (Intake3*alpha_Intake)
    }else {
      Agr3 <- 0
    }
    df_agr[i,3] <- Agr3
    
    ## 3004 영동천
    ### Node A
    Node_A = R1_Agr * alpha_Agr + R1_Env * alpha_Env + R1_Ind * alpha_Ind + 
      Dam1_Spillway - Agr1 + Agr1*alpha_Agr + inflow_3002[i,] - Agr2 + 
      Agr2 *alpha_Agr +inflow_3003[i,] - Power1 + Power1 *alpha_Pow - Intake3 + 
      (Intake3*alpha_Intake) - Agr3 + (Agr3 * alpha_Agr) + inflow_3004[i,]
    
    ### Power 2
    if( Node_A >= demand_power[i,2]){
      Power2 <- demand_power[i,2]
    }else if (Node_A > 0){
      Power2 <- Node_A
    }else{
      Power2 <- 0
    }
    df_power[i,2] <- Power2
    
    ### Intake 4  
    if(Node_A - Power2 + (Power2 *alpha_Pow) >= demand_intake[i,4]){
      Intake4 <- demand_intake[i,4]
    }else if(Node_A - Power2 + (Power2 *alpha_Pow) > 0){
      Intake4 <- Node_A - Power2 + (Power2 *alpha_Pow)
    }else{
      Intake4 <- 0
    }
    df_intake[i,4] <- Intake4
    
    ### Agr 4
    if(Node_A - Power2 + (Power2 *alpha_Pow) - Intake4 + (Intake4 * alpha_Intake) >= demand_agr[i,4]){
      Agr4 <- demand_agr[i,4]
    }else if(Node_A - Power2 + (Power2 *alpha_Pow) - Intake4 + (Intake4 * alpha_Intake) > 0){
      Agr4 <- Node_A - Power2 + (Power2 *alpha_Pow) - Intake4 + (Intake4 * alpha_Intake)
    }else{
      Agr4 <- 0
    }
    df_agr[i,4] <- Agr4
    
    ## 3005 초강
    ### Intake 5
    if(inflow_3005[i,] >= demand_intake[i,5]){
      Intake5 <- demand_intake[i,5]
    }else if(inflow_3005[i,] > 0 ){
      Intake5 <- inflow_3005[i,]
    }else{
      Intake5 <- 0
    }
    df_intake[i,5] <- Intake5
    
    ### Agr 5
    if(inflow_3005[i,] - Intake5 + Intake5 * alpha_Intake >= demand_agr[i,5]){
      Agr5 <- demand_agr[i,5]
    }else if(inflow_3005[i,] - Intake5 + Intake5 * alpha_Intake > 0){
      Agr5 <- inflow_3005[i,] - Intake5 + Intake5 * alpha_Intake
    }else{
      Agr5 <- 0
    }
    df_agr[i,5] <- Agr5
    
    ### Node B
    Node_B <- Node_A - Power2 + (Power2 * alpha_Pow) - Intake4 + (Intake4 * alpha_Intake) - 
      Agr4 + (Agr4 * alpha_Agr) + inflow_3005[i,] - Intake5 + (Intake5 * alpha_Intake) - 
      Agr5 + (Agr5 * alpha_Agr) + inflow_3006[i,]   
    
    ## 3006 대청댐상류 
    ### Intake 6
    if(Node_B >= demand_intake[i,6]){
      Intake6 <- demand_intake[i,6]
    }else if(Node_B > 0){
      Intake6 <- Node_B
    }else {
      Intake6 <- 0
    }
    df_intake[i,6] <- Intake6
    
    ### Agr 6
    if(Node_B - Intake6 + (Intake6*alpha_Intake) >= demand_agr[i,6]){
      Agr6 <- demand_agr[i,6]
    }else if(Node_B - Intake6 + (Intake6*alpha_Intake) > 0){
      Agr6 <- Node_B - Intake6
    }else{
      Agr6 <- 0
    }
    df_agr[i,6] <- Agr6
    
    ## 3007 보청천
    ## Intake 7
    if(inflow_3007[i,] >= demand_intake[i,7]){
      Intake7 <- demand_intake[i,7]
    }else if (inflow_3007[i,] > 0){
      Intake7 <- inflow_3007[i,]
    }else{
      Intake7 <- 0
    }
    df_intake[i,7] <- Intake7
    
    ### Agr 7
    if(inflow_3007[i,] - Intake7 + (Intake7 * alpha_Intake) >= demand_agr[i,7]){
      Agr7 <- demand_agr[i,7]
    }else if (inflow_3007[i,] - Intake7 + (Intake7 * alpha_Intake) > 0){
      Agr7 <- inflow_3007[i,] - Intake7
    }else{
      Agr7 <- 0
    }
    df_agr[i,7] <- Agr7
    
    ### Node C
    Node_C <- Node_B - Intake6 + (Intake6 * alpha_Intake) - Agr6 + (Agr6 * alpha_Agr) + 
      inflow_3007[i,] - Intake7 + (Intake7 * alpha_Intake) - Agr7 + (Agr7 * alpha_Agr)
    
    ### Intake 8
    if(Node_C >= demand_intake[i,8]){
      Intake8 <- demand_intake[i,8]
    }else if (Node_C > 0){
      Intake8 <- Node_C
    }else{
      Intake8 <- 0
    }
    df_intake[i,8] <- Intake8
    
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
      R2_Agr = Release_Daecheong$A_LV2[i]  * alpha1
      R2_Ind = Release_Daecheong$I_LV2[i]  * alpha1
      R2_Env = Release_Daecheong$E_LV2[i]  * alpha1
    } else if(Dam2_Stor >= Release_Daecheong$LV4[i]){ #Alert(LV3)
      Dam2_Spillway <- 0
      R2_Agr = Release_Daecheong$A_LV3[i]  * alpha2
      R2_Ind = Release_Daecheong$I_LV3[i]  * alpha2
      R2_Env = Release_Daecheong$E_LV3[i]  * alpha2
    } else if(Dam2_Stor >= dam2_Stor_Min){ #Serious(LV4)
      Dam2_Spillway <- 0
      R2_Agr = Release_Daecheong$A_LV4[i]  * alpha3
      R2_Ind = Release_Daecheong$I_LV4[i]  * alpha3
      R2_Env = Release_Daecheong$E_LV4[i]  * alpha3
    } else { #Under LWL
      Dam2_Spillway <- 0
      R2_Agr = 0
      R2_Ind = 0
      R2_Env = 0
    }
    diff2 <- Dam2_Stor
    Dam2_Stor = Dam2_Stor - (R2_Agr + R2_Env + R2_Ind) + inflow_3008[i,] + Node_C - Intake8 + (Intake8 * alpha_Intake) - Dam2_Spillway
    diff2 <- diff2 - Dam2_Stor  
    Intake9 <- R2_Agr + R2_Ind + R2_Env
    df_intake[i,9] <- Intake9
    
    ### Intake 10
    if((R2_Agr * alpha_Agr) + (R2_Ind * alpha_Ind) + (R2_Env * alpha_Env) + Dam2_Spillway >= demand_intake[i,10]){
      Intake10 <- demand_intake[i,10]
    }else if ((R2_Agr * alpha_Agr) + (R2_Ind * alpha_Ind) + (R2_Env * alpha_Env) + Dam2_Spillway > 0){
      Intake10 <- (R2_Agr * alpha_Agr) + (R2_Ind * alpha_Ind) + (R2_Env * alpha_Env) + Dam2_Spillway
    }else{
      Intake10 <- 0
    }
    df_intake[i,10] <- Intake10
    
    ### Intake 11
    if((R2_Agr * alpha_Agr) + (R2_Ind * alpha_Ind) + (R2_Env * alpha_Env) + 
       Dam2_Spillway -Intake10 + (Intake10 * alpha_Intake) >= demand_intake[i,11]){
      Intake11 <- demand_intake[i,11]
    }else if ((R2_Agr * alpha_Agr) + (R2_Ind * alpha_Ind) + (R2_Env * alpha_Env) + 
              Dam2_Spillway -Intake10 + (Intake10 * alpha_Intake) > 0){
      Intake11 <- (R2_Agr * alpha_Agr) + (R2_Ind * alpha_Ind) + (R2_Env * alpha_Env) + 
        Dam2_Spillway -Intake10 + (Intake10 * alpha_Intake)
    }else{
      Intake11 <- 0
    }
    df_intake[i,11] <- Intake11
    
    ### Intake 12
    if((R2_Agr * alpha_Agr) + (R2_Ind * alpha_Ind) + (R2_Env * alpha_Env) + 
       Dam2_Spillway - Intake10 + (Intake10 * alpha_Intake) - Intake11  >= demand_intake[i,12]){
      Intake12 <- demand_intake[i,12]
    }else if((R2_Agr * alpha_Agr) + (R2_Ind * alpha_Ind) + (R2_Env * alpha_Env) + 
             Dam2_Spillway - Intake10 + (Intake10 * alpha_Intake) - Intake11 > 0){
      Intake12 <- (R2_Agr * alpha_Agr) + (R2_Ind * alpha_Ind) + (R2_Env * alpha_Env) + 
        Dam2_Spillway - Intake10 + (Intake10 * alpha_Intake) - Intake11
    }else{
      Intake12 <- 0
    }
    df_intake[i,12] <- Intake12
    
    ### Agr 8
    if((R2_Agr * alpha_Agr) + (R2_Ind * alpha_Ind) + (R2_Env * alpha_Env) + 
       Dam2_Spillway - Intake10 + (Intake10 * alpha_Intake) - Intake11 -Intake12 +
       (Intake12 * alpha_Intake)>= demand_agr[i,8] - R2_Agr){
      Agr8 <- demand_agr[i,8] - R2_Agr
    }else if ((R2_Agr * alpha_Agr) + (R2_Ind * alpha_Ind) + (R2_Env * alpha_Env) + 
              Dam2_Spillway - Intake10 + (Intake10 * alpha_Intake) - Intake11 -Intake12 +
              (Intake12 * alpha_Intake) > 0){
      Agr8 <- (R2_Agr * alpha_Agr) + (R2_Ind * alpha_Ind) + (R2_Env * alpha_Env) + 
        Dam2_Spillway - Intake10 + (Intake10 * alpha_Intake) - Intake11 -Intake12 +
        (Intake12 * alpha_Intake)
    }else{
      Agr8 <- 0
    }
    df_agr[i,8] <- Agr8
    
    ## 3009 갑천
    ### Agr 9
    if(inflow_3009[i,] >= demand_agr[i,9]){
      Agr9 <- demand_agr[i,9]
    }else if(inflow_3009[i,] > 0 ){
      Agr9 <- inflow_3009[i,]
    }else{
      Agr9 <- 0
    }
    df_agr[i,9] <- Agr9
    
    ### Node D
    Node_D <- (R2_Agr * alpha_Agr) + (R2_Ind * alpha_Ind) + (R2_Env * alpha_Env) - 
      Intake10 + (Intake10 * alpha_Intake) - Intake11 - Intake12 + (Intake12 * alpha_Intake) - 
      Agr8 + (Agr8 * alpha_Agr) + inflow_3009[i,] - Agr9 + (Agr9 * alpha_Agr) + inflow_3010[i,]
    
    ## 3010 대청댐 하류
    ### Agr 10
    if(Node_D >= demand_agr[i,10]){
      Agr10 <- demand_agr[i,10]
    }else if (Node_D > 0){
      Agr10 <- Node_D
    }else{
      Agr10 <- 0
    }
    df_agr[i,10] <- Agr10
    
    ### Power 3
    if(Node_D - Agr10 + (Agr10 * alpha_Agr) >= demand_power[i,3]){
      Power3 <- demand_power[i,3]
    }else if (Node_D - Agr10 + (Agr10 * alpha_Agr) > 0){
      Power3 <- Node_D - Agr10 + (Agr10 * alpha_Agr)
    }else{
      Power3 <- 0
    }
    df_power[i,3] <- Power3
    
    ## 301103 백곡댐
    ### Intake 13
    if(inflow_301103[i,] >= demand_intake[i,13]){
      Intake13 <- demand_intake[i,13]
    }else if(inflow_301103[i,] > 0){
      Intake13 <- inflow_301103[i,]
    }else{
      Intake13 <- 0
    }
    df_intake[i,13] <- Intake13
    
    ### 백곡저수지
    if(Dam4_Stor >= dam4_Stor_Max){ #총 저수량 이상
      Dam4_Spillway <- Dam4_Stor + inflow_301103[i,] - Intake13 + (Intake13 * alpha_Intake) - dam4_Stor_Max
      R4_Agr <- demand_BG
    }else if(Dam4_Stor >= dam4_Stor_Nor){ #총 저수량 이하 유효저수량 이상
      Dam4_Spillway <- 0
      R4_Agr <- demand_BG
    }else if(Dam4_Stor >= dam4_Stor_Min){ #유효저수량 이하 사수량 이상
      Dam4_Spillway <- 0
      R4_Agr <- demand_BG * 0.8
    }else{ #사수량 이하
      Dam4_Spillway <- 0
      R4_Agr <- 0
    }
    Dam4_Stor <- Dam4_Stor + inflow_301103[i,] - Intake13 + (Intake13 * alpha_Intake)  - Dam4_Spillway
    
    ## 미호천 3011
    ### Intake 14
    if(R4_Agr * alpha_Agr + Dam4_Spillway + inflow_3011[i,] >= demand_intake[i,14]){
      Intake14 <- demand_intake[i,14]
    }else if(R4_Agr * alpha_Agr + Dam4_Spillway + inflow_3011[i,] > 0){
      Intake14 <- R4_Agr * alpha_Agr + Dam4_Spillway + inflow_3011[i,]
    }else{
      Intake14 <- 0
    }
    df_intake[i,14] <- Intake14
    
    ### Agr 11
    if(R4_Agr * alpha_Agr + Dam4_Spillway - Intake14 +(Intake14 * alpha_Intake) + 
       inflow_3011[i,] + (Intake11 * alpha_Intake) >= demand_agr[i,11]){
      Agr11 <- demand_agr[i,11]
    }else if (R4_Agr * alpha_Agr + Dam4_Spillway - Intake14 +(Intake14 * alpha_Intake) + 
              inflow_3011[i,] + (Intake11 * alpha_Intake) > 0){
      Agr11 <- R4_Agr * alpha_Agr + Dam4_Spillway - Intake14 +(Intake14 * alpha_Intake) + 
        inflow_3011[i,] + (Intake11 * alpha_Intake)
    }else{
      Agr11 <- 0
    }
    df_agr[i,11] <- Agr11
    
    ### Node E
    Node_E <- Node_D - Agr10 + (Agr10 * alpha_Agr) - Power3 + (Power3 * alpha_Pow) +
      (R4_Agr * alpha_Agr) + Dam4_Spillway - Intake14 + (Intake14 * alpha_Intake) + 
      inflow_3011[i,] - Intake13 + (Intake13 * alpha_Intake) + (Intake11 * alpha_Intake) -
      Agr11 + (Agr11 * alpha_Agr)
    
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
      R3_Agr = Release_Boryeong$A_LV2[i]  * alpha1
      R3_Ind = Release_Boryeong$I_LV2[i]  * alpha1
      R3_Env = Release_Boryeong$E_LV2[i]  * alpha1
      Dam3_Stor = Dam3_Stor + inflow_320305[i,] - (R3_Agr + R3_Env + R3_Ind) + waterway
      Node_E <- Node_E - waterway
      df_waterway[i] <- waterway
    } else if(Dam3_Stor >= Release_Boryeong$LV4[i]){ #Alert(LV3)
      Dam3_Spillway <- 0
      R3_Agr = Release_Boryeong$A_LV3[i] * alpha2
      R3_Ind = Release_Boryeong$I_LV3[i] * alpha2
      R3_Env = Release_Boryeong$E_LV3[i] * alpha2
      Dam3_Stor = Dam3_Stor + inflow_320305[i,] - (R3_Agr + R3_Env + R3_Ind) + waterway
      Node_E <- Node_E - waterway
      df_waterway[i] <- waterway
    } else if(Dam3_Stor >= dam3_Stor_Min){ #Serious(LV4)
      Dam3_Spillway <- 0
      R3_Agr = Release_Boryeong$A_LV4[i] * alpha3
      R3_Ind = Release_Boryeong$I_LV4[i] * alpha3
      R3_Env = Release_Boryeong$E_LV4[i] * alpha3
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
    df_intake[i,15] <- R3_Agr + R3_Ind + R3_Env
    
    ### Agr 12
    if(Node_E >= demand_agr[i,12]){
      Agr12 <- demand_agr[i,12]
    }else if (Node_E > 0){
      Agr12 <- Node_E
    }else{
      Agr12 <- 0
    }
    df_agr[i,12] <- Agr12
    
    ## 논산천 3013
    ### 탑정저수지 Agr 13
    if(Dam5_Stor >= dam5_Stor_Max){ #총 저수량 이상
      Dam5_Spillway <- Dam5_Stor + inflow_3013[i,] - dam5_Stor_Max
      R5_Agr <- demand_agr[i,13]
    }else if(Dam5_Stor >= dam5_Stor_Nor){ #총 저수량 이하 유효저수량 이상
      Dam5_Spillway <- 0
      R5_Agr <- demand_agr[i,13]
    }else if(Dam5_Stor >= dam5_Stor_Min){ #유효저수량 이하 사수량 이상
      Dam5_Spillway <- 0
      R5_Agr <- demand_agr[i,13] * 0.8
    }else{ #사수량 이하
      Dam5_Spillway <- 0
      R5_Agr <- 0
    }
    Dam5_Stor <- Dam5_Stor + inflow_3013[i,] - R5_Agr - Dam5_Spillway
    df_agr[i,13] <- R5_Agr
    
    ## Node F
    Node_F <- Node_E - Agr12 + (Agr12 * alpha_Agr) + R5_Agr * alpha_Agr
    
    ### Agr 14
    if(Node_F >= demand_agr[i,14]){
      Agr14 <- demand_agr[i,14]
    }else if(Node_F > 0){
      Agr14 <- Node_F
    }else{
      Agr14 <- 0
    }
    df_agr[i,14] <- Agr14
    
    Outlet1 <- Node_F - Agr14 + (Agr14 * alpha_Agr)
    Outlet2 <- R3_Agr * alpha_Agr + R3_Env * alpha_Env + R3_Ind * alpha_Env + Dam3_Spillway
    
    # Data restoring
    df_dam1_Stor[i] <- Dam1_Stor
    df_dam2_Stor[i] <- Dam2_Stor
    df_dam3_Stor[i] <- Dam3_Stor
    df_dam4_Stor[i] <- Dam4_Stor
    df_dam5_Stor[i] <- Dam5_Stor
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
    
    
    print(paste(i,"번째 계산완료"))
    #print(paste0("Trial # ", i, ", Non-Irrigation Period, Month: ", Month[i]))
    #print(paste0("Dam1_Stor: ", round(dam1_Stor, 2), ", Max: ", Dam1_Stor_Max, ", Min: ", Dam1_Stor_Min))
    #print(paste0("Dam2_Stor: ", round(dam2_Stor, 2), ", Max: ", Dam2_Stor_Max, ", Min: ", Dam2_Stor_Min))
    #print(paste0("Dam3_Stor: ", round(dam3_Stor, 2), ", Max: ", Dam3_Stor_Max, ", Min: ", Dam3_Stor_Min))
    #print(paste0("Dam4_Stor: ", round(dam4_Stor, 2), ", Max: ", Dam4_Stor_Max, ", Min: ", Dam4_Stor_Min))
    #print(paste0("Dam5_Stor: ", round(dam5_Stor, 2), ", Max: ", Dam5_Stor_Max, ", Min: ", Dam5_Stor_Min))
    #print(paste0("Dam4_Inflow: Agr = ", R4_Agr, ", Ind = ", R4_Ind, ", Dom = ", R4_Dom, ", 
    #Env = ", R4_Env, ", Total = ", R4_Agr+R4_Dom+R4_Env+R4_Ind))
    #print(paste0("                                  "))
    
    
    #T_Env1 = T_Env1 + R1_Env_ni; T_Env4 = T_Env4 + R4_Env
    #T_Ind4 = T_Ind4 + R4_Ind; T_Ind5 = T_Ind5 + R5_Ind_ni; T_Ind8 = T_Ind8 + R8_Ind_ni 
    #T_Ind10 = T_Ind10 + R10_Ind_ni; T_Ind11 = T_Ind11 + R11_Ind_ni
    
    
  } # for syntax end
  
  # Result 
  intake_vector <- c("Intake 1","Intake 2","Intake 3","Intake 4","Intake 5",
                     "Intake 6","Intake 7","Intake 8","Intake 9","Intake 10",
                     "Intake 11","Intake 12","Intake 13","Intake 14","Intake 15")
  table_intake <- data.frame(Stakeholder = intake_vector,
                             용수공급가능일수 =   colSums(df_intake/demand_intake))
  
  gg_intake <- table_intake %>%
    mutate(Stakeholder = fct_relevel(Stakeholder,intake_vector)) %>%
    ggplot(aes(x=Stakeholder, y = 용수공급가능일수)) + 
    geom_bar(stat = 'identity',width = 0.5, fill = "#edae49") +
    geom_hline(yintercept=365*2) + theme_bw() + ylab("Days of Water Supply Availability")+
    ggtitle("취수장의 용수공급가능일수")
  
  agr_vector <- c("Agr 1","Agr 2","Agr 3","Agr 4","Agr 5",
                  "Agr 6","Agr 7","Agr 8","Agr 9","Agr 10",
                  "Agr 11","Agr 12","Agr 13","Agr 14")
  df_agr[,1] <- df_agr[,1] + df_R1[,1]
  df_agr[,8] <- df_agr[,8] + df_R2[,1]
  table_agr <- data.frame(Stakeholder = agr_vector,
                          용수공급가능일수 =   colSums(df_agr/demand_agr, na.rm=TRUE))
  gg_agr <- table_agr %>%
    mutate(Stakeholder = fct_relevel(Stakeholder,agr_vector)) %>%
    ggplot(aes(x=Stakeholder, y = 용수공급가능일수)) + 
    geom_bar(stat = 'identity',width = 0.5, fill="#66a182") +
    geom_hline(yintercept=2 *length(yday("2025-05-01"):yday("2025-09-30"))) + theme_bw() +
    ylab("Days of Water Supply Availability")+
    ggtitle("농업용수의 용수공급가능일수")
  
  power_vector <- c("Power 1","Power 2","Power 3")
  table_power <- data.frame(Stakeholder = power_vector,
                            용수공급가능일수 =   colSums(df_power/demand_power, na.rm=TRUE))
  gg_power <- table_power %>%
    mutate(Stakeholder = fct_relevel(Stakeholder,power_vector)) %>%
    ggplot(aes(x=Stakeholder, y = 용수공급가능일수)) + 
    geom_bar(stat = 'identity',width = 0.5, fill="#d1495b") +
    geom_hline(yintercept=365*2) + theme_bw() + ylab("Days of Water Supply Availability") +
    ggtitle("발전용수의 용수공급가능일수")
  
  
  Average_days_intake <-  colSums(df_intake/demand_intake) %>% mean()
  Average_days_agr <-  colSums(df_agr/demand_agr, na.rm=TRUE) %>% mean()
  Average_days_power <-  colSums(df_power/demand_power) %>% mean()
  
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
           공급량 = colSums(df_agr), 
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
           공급량 = colSums(demand_agr), 
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
    
  max_profit_intake <- data.frame(공급량 = colSums(demand_intake)) %>%
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
  
  max_profit_power <- data.frame(공급량 = colSums(demand_power)) %>%
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

  gini_total <- Gini(c(colSums(df_agr/demand_agr, na.rm=TRUE)/(length(yday("2025-05-01"):yday("2025-09-30"))*2), 
                 colSums(df_intake/demand_intake, na.rm=TRUE)/730,
                 colSums(df_power/demand_power, na.rm=TRUE)/730))
  
  gini_intake <- Gini(colSums(df_intake/demand_intake, na.rm=TRUE))
  
  gini_power <- Gini(colSums(df_power/demand_power, na.rm=TRUE))
  
  gini_agr <- Gini(colSums(df_agr/demand_agr, na.rm=TRUE))
  
  
  LC <- plot(Lc(c(colSums(df_agr/demand_agr, na.rm=TRUE)/(length(yday("2025-05-01"):yday("2025-09-30"))*2), 
                  colSums(df_intake/demand_intake, na.rm=TRUE)/730,
                  colSums(df_power/demand_power, na.rm=TRUE)/730)))
  
  # Result
  
  Result_list <- list(df_intake, df_agr, df_power, demand_intake,demand_agr,demand_power, table_intake,table_agr,table_power,
                      Average_days_intake,Average_days_agr,Average_days_power,
                      gg_intake,gg_agr,gg_power, 
                      gini_total, gini_intake, gini_power, gini_agr,
                      profit,  profit_ratio,
                      profit_intake, profit_agr, profit_power,
                      profit_ratio_power,profit_ratio_agr,profit_ratio_intake,LC)
  
  names(Result_list) <- c("df_intake","df_agr","df_power", "demand_intake", "demand_agr","demand_power",
                          "Table_intake","Table_agr","Table_power",
                          "Average_days_intake","Average_days_agr","Average_days_power",
                          "gg_intake","gg_agr","gg_power",
                          "Gini_coefficient", "Gini_intake", "Gini_power", "Gini_agr",
                          "System_profit",  "Profit_ratio", 
                          "System_profit_intake","System_profit_agr","System_profit_power",
                          "Profit_ratio_power","Profit_ratio_agr","Profit_ratio_intake","Lorenz")
  return(Result_list) 
}

# System initial : Stor
# System input : Inflow, 
# System output : Intake, Power, Agr, Outlet
