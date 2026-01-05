################################################################################
# 작성자: 김연주
# 금강유역 Optimization 파일
# 문의 or 오류 신고 : iamkimyeonju@gmail.com
################################################################################

setwd("G:/My Drive/서울시립대 SN가뭄취약성/3차년도/Research/코드n자료")
source("function_for_Geum.R")
options(scipen = 999) 
options(digits = 10)

#install.packages("GA")
#install.packages("parallel")
#install.packages("doParallel")
#install.packages("reshape2")
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("mco")


library(naniar)
library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(GA)
library(parallel)
library(doParallel)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(mco)
set.seed(22)

################################################################################
# Initial Condition
################################################################################

Initial <- Geum_Network(supply_scenario = 1, demand_scenario = 1, drought_scenario = 22, 
                        alpha1 =1, alpha2 = 1,alpha3=1)
Initial$System_profit
Initial$Gini_coefficient                      
Initial$Average_days_intake
Initial$Average_days_agr
Initial$Average_days_power
Initial$Profit_ratio
Initial$Lorenz
Initial$gg_intake
Initial$gg_agr
Initial$gg_power


sd(c(Initial$Table_agr$용수공급가능일수,Initial$Table_power$용수공급가능일수,
     Initial$Table_intake$용수공급가능일수))


################################################################################
# Single Objective function - Efficiency : Maximize System Profit
################################################################################

Efficiency <- ga(type="real-valued", fitness = function(x) Geum_Network(1,1,22,x[1],x[2],x[3])$Profit_ratio,
         lower= c(0.7,0.7,0.7), upper=c(1.3,1.3,1.3))
summary(Efficiency)
plot(Efficiency,main="Single-Objective Problem: Efficiency")

solution_Eff <- Geum_Network(1,1,22,Efficiency@solution[1,1],Efficiency@solution[1,2],
                             Efficiency@solution[1,3])
solution_Eff$System_profit
solution_Eff$gg_intake
solution_Eff$gg_agr
solution_Eff$gg_power
solution_Eff$Average_days_intake
solution_Eff$Average_days_agr
solution_Eff$Average_days_power
solution_Eff$Table_agr
solution_Eff$Gini_coefficient

sd(c(solution_Eff$Table_agr$용수공급가능일수,solution_Eff$Table_power$용수공급가능일수,
     solution_Eff$Table_intake$용수공급가능일수))


################################################################################
# Single Objective function - Equity : Minimize Gini Coefficient
################################################################################

Equity <- ga(type="real-valued", fitness = function(x) -Geum_Network(1,1,22,x[1],x[2],x[3])$Gini_coefficient,
         lower= c(0.7,0.7,0.7), upper=c(1.3,1.3,1.3))
summary(Equity)

plot(Equity,main="Single-Objective Problem: Equity")
solution_Equ <- Geum_Network(1,1,22,Equity@solution[1,1],Equity@solution[1,2],Equity@solution[1,3])

solution_Equ$gg_intake
solution_Equ$gg_agr
solution_Equ$gg_power
solution_Equ$Average_days_intake
solution_Equ$Average_days_agr
solution_Equ$Average_days_power
solution_Equ$System_profit
solution_Equ$Gini_coefficient
sd(c(solution_Equ$Table_agr$용수공급가능일수,solution_Equ$Table_power$용수공급가능일수,
     solution_Equ$Table_intake$용수공급가능일수))

################################################################################
# Muiti-Objective function - Efficiency & Equity 
################################################################################

fun <- function(x) {
  basin_result <- Geum_Network(1,1,22,x[1],x[2],x[3])
  y <- numeric(2)
  y[1] <- basin_result$Profit_ratio
  y[2] <- -basin_result$Gini_coefficient
  return(y)
}
multi_nsga <- nsga2(fn = fun, # function for minimize
              idim = 3, # Number of decision values: which is x here
              odim = 2, # Number of objective functions: which is f1 and f2 here
              generations = 100,
              popsize = 80,
              lower.bounds = c(0.7,0.7,0.7), #If there are multiple x, it should be vector: c(,)
              upper.bounds = c(1.3,1.3,1.3))

multi_nsga$par
multi_nsga$value[,2] <- -multi_nsga$value[,2]
multi_nsga$pareto.optimal

plot(multi_nsga$value)

min(multi_nsga$value[,1])
plot(multi_nsga, xlab="System Profit", ylab="Gini coefficient",
     main = "Pareto front plot")

matrix_for_plot <- multi_nsga$value
colnames(matrix_for_plot) <- c("X","Y")

ggplot(matrix_for_plot, aes(x=X, y=Y)) +
  geom_point() +
  theme_classic() +
  xlab("시스템 이익") +
  ylab("지니계수") +
  ggtitle("Pareto front plot") +
  geom_line(linetype = "dashed")

plot(multi_nsga, 
     xlim=c(min(multi_nsga$value[,1]),max(multi_nsga$value[,1])), 
     ylim=c(min(multi_nsga$value[,2]),max(multi_nsga$value[,2])))

points(multi_nsga$value[,1],multi_nsga$value[,2], cex=3)


save(Equity, file = "Equity.Rdata") 
save(solution_Equ, file = "solution_Equ.Rdata") 
save(Efficiency, file = "Efficiency.Rdata")
save(solution_Eff, file = "solution_Eff.Rdata") 
save(multi_nsga, file = "multi_nsga.Rdata")
