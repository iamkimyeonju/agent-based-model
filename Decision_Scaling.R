################################################################################
# 작성자: 김연주
# 금강유역 Decision_Scaling 파일
# 문의 or 오류 신고 : iamkimyeonju@gmail.com
# 최종 수정일 : 2025년 1월 28일 (README 참고)
################################################################################

setwd("C:/Users/pqh24002/OneDrive - University of Connecticut/University of Seoul/3차년도/Research/코드n자료")
source("function_for_Geum.R")
options(digits = 10)
options(scipen = 999) 
library(GA)
library(parallel)
library(doParallel)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(mco)
# install.packages('formattable')
library(formattable)
set.seed(22)

################################################################################
# 용수공급가능일수
################################################################################

Shell.dataframe <- data.frame(matrix(NA, ncol= 4, nrow=3))
rownames(Shell.dataframe) <- c("Low","Standard","High")
colnames(Shell.dataframe) <- c("Very Low","Low","Standard","High")

Result_intake <- Shell.dataframe 
Result_agr <- Shell.dataframe 
Result_power <- Shell.dataframe 

for (supply in 1:4){
  for(demand in 1:3){
    if (demand == 1){ i <- 0.9 }else if (demand==2){i <- 1.0} else {i <- 1.1}
    if (supply == 1){ j <- 0.6 }else if (supply==2){j <-0.8} else if(supply ==3) {j <- 1.0} else{j <- 1.2}
    blank <- Geum_Network(i,j,22)
    
    Result_intake[demand,supply] <- blank$Average_days_intake
    Result_agr[demand,supply] <- blank$Average_days_agr
    Result_power[demand,supply] <- blank$Average_days_power
  }
}


ggplot(melt(as.matrix(round(Result_intake,2))),aes(x=X2,y=X1,fill=value)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1)  +
  geom_text(aes(label = value), color = "white", size = 3)+
  coord_fixed() +labs(title="Intake Station",
                      x ="Supply scenario", y = "Demand scenario") +
  theme_bw()


ggplot(melt(as.matrix(round(Result_agr,2))),aes(x=X2,y=X1,fill=value)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1)  +
  geom_text(aes(label = value), color = "white", size = 3)+
  coord_fixed() +labs(title="Agricutural Water",
                      x ="Supply scenario", y = "Demamd scenario") +
  theme_bw()


ggplot(melt(as.matrix(round(Result_power,2))),aes(x=X2,y=X1,fill=value)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1)  +
  geom_text(aes(label = value), color = "white", size = 3)+
  coord_fixed() +labs(title="Power Plant",
                      x ="Supply scenario", y = "Demand scenario") +
  theme_bw()

################################################################################
# 지니계수
################################################################################

Shell.dataframe <- data.frame(matrix(NA, ncol= 4, nrow=3))
rownames(Shell.dataframe) <- c("Low","Standard","High")
colnames(Shell.dataframe) <- c("Very Low","Low","Standard","High")

Result_intake <- Shell.dataframe 
Result_agr <- Shell.dataframe 
Result_power <- Shell.dataframe 
Result_total <- Shell.dataframe

for (supply in 1:4){
  for(demand in 1:3){
    if (demand == 1){ i <- 0.9 }else if (demand==2){i <- 1.0} else {i <- 1.1}
    if (supply == 1){ j <- 0.6 }else if (supply==2){j <-0.8} else if(supply ==3) {j <- 1.0} else{j <- 1.2}
    blank <- Geum_Network(i,j,22)
    Result_total[demand,supply] <- blank$Gini_coefficient
    Result_intake[demand,supply] <- blank$Gini_intake
    Result_agr[demand,supply] <- blank$Gini_agr
    Result_power[demand,supply] <- blank$Gini_power
  }
}

ggplot(melt(as.matrix(round(Result_total,3))),aes(x=X2,y=X1,fill=value)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1)  +
  geom_text(aes(label = value), color = "white", size = 3)+
  coord_fixed() +labs(title="Decision Scaling",
                      x ="Supply scenario", y = "Demand scenario") +
  theme_bw()

ggplot(melt(as.matrix(round(Result_intake,3))),aes(x=X2,y=X1,fill=value)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1)  +
  geom_text(aes(label = value), color = "white", size = 3)+
  coord_fixed() +labs(title="Intake Station",
                      x ="Supply scenario", y = "Demand scenario") +
  theme_bw()


ggplot(melt(as.matrix(round(Result_agr,3))),aes(x=X2,y=X1,fill=value)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1)  +
  geom_text(aes(label = value), color = "white", size = 3)+
  coord_fixed() +labs(title="Agricutural Water",
                      x ="Supply scenario", y = "Demamd scenario") +
  theme_bw()


ggplot(melt(as.matrix(round(Result_power,3))),aes(x=X2,y=X1,fill=value)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1)  +
  geom_text(aes(label = value), color = "white", size = 3)+
  coord_fixed() +
  labs(title="Power Plant",
       x ="Supply scenario", y = "Demand scenario") +
  theme_bw()


##########################################################################
# 시스템 이익
#########################################################################

Shell.dataframe <- data.frame(matrix(NA, ncol= 4, nrow=3))
rownames(Shell.dataframe) <- c("Low","Standard","High")
colnames(Shell.dataframe) <- c("Very Low","Low","Standard","High")

Result_intake <- Shell.dataframe 
Result_agr <- Shell.dataframe 
Result_power <- Shell.dataframe 
Result_total <- Shell.dataframe

for (supply in 1:4){
  for(demand in 1:3){
    if (demand == 1){ i <- 0.9 }else if (demand==2){i <- 1.0} else {i <- 1.1}
    if (supply == 1){ j <- 0.6 }else if (supply==2){j <-0.8} else if(supply ==3) {j <- 1.0} else{j <- 1.2}
    blank <- Geum_Network(i,j,1)
    Result_total[demand,supply] <- blank$System_profit
    Result_intake[demand,supply] <- blank$System_profit_intake
    Result_agr[demand,supply] <- blank$System_profit_agr
    Result_power[demand,supply] <- blank$System_profit_power
  }
}

ggplot(melt(as.matrix(round(Result_intake,0))),aes(x=X2,y=X1,fill=value)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1)  +
  geom_text(aes(label = value), color = "white", size = 3)+
  coord_fixed() +labs(title="Intake Station",
                      x ="Supply scenario", y = "Demand scenario") +
  theme_bw()


ggplot(melt(as.matrix(round(Result_agr,0))),aes(x=X2,y=X1,fill=value)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1)  +
  geom_text(aes(label = value), color = "white", size = 3)+
  coord_fixed() +labs(title="Agricutural Water",
                      x ="Supply scenario", y = "Demamd scenario") +
  theme_bw()


ggplot(melt(as.matrix(round(Result_power,0))),aes(x=X2,y=X1,fill=value)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1)  +
  geom_text(aes(label = value), color = "white", size = 3)+
  coord_fixed() +
  labs(title="Power Plant",
       x ="Supply scenario", y = "Demand scenario") +
  theme_bw()
