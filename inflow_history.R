
#setwd("G:/My Drive/University of Seoul/3차년도/Research/코드n자료")

library(naniar)
library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(reshape2)
library(tidyr)
library(reshape)
library(DescTools)
library(ggplot2)




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



runoff_array <- read_csv("data/Netflow_1981-2020.csv") 
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




inflow_all <- data.frame(
  Time = runoff_array$Time,
  inflow_3001,
  inflow_3002,
  inflow_3003,
  inflow_3004,
  inflow_3005,
  inflow_3006,
  inflow_3007,
  inflow_3008,
  inflow_3009,
  inflow_3010,
  inflow_3011,
  inflow_3012,
  inflow_3013,
  inflow_3014,
  inflow_301103,
  inflow_320305
)

colnames(inflow_all) <- c(
  "Time",
  "용담댐", "용담댐하류", "무주남대천", "영동천", "초강", "대청댐상류",
  "보청천", "대청댐", "갑천", "대청댐하류", "미호천", "금강공주",
  "논산천", "금강하구언", "백곡댐", "보령댐"
)

inflow_long <- inflow_all %>%
  pivot_longer(
    cols = -Time,
    names_to = "Location",
    values_to = "Flow"
  )

ggplot(inflow_long, aes(x = Time, y = Flow, color = Location)) +
  geom_line(size = 0.8, alpha = 0.7) +
  labs(
    title = "Time Series of Inflows",
    x = "Time",
    y = "Flow (CMS or MCM/day)"
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.key.height = unit(0.4, "cm")
  )



ggplot(inflow_long, aes(x = Time, y = Flow, color = Location)) +
  geom_line(size = 0.8, alpha = 0.7) +
  labs(
    title = "Time Series of Inflows",
    x = "Time",
    y = "Flow (CMS or MCM/day)"
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.key.height = unit(0.4, "cm")
  )



ggplot(inflow_long, aes(x = Time, y = Flow)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ Location, scales = "free_y", ncol = 3) +
  theme_bw()


inflow_all
which.max(colSums(inflow_all[,2:17]))


monthly_sum <- inflow_long %>%
  mutate(month = floor_date(Time, "month")) %>%   # 날짜를 월 단위로 내림
  group_by(Location, month) %>%
  summarise(monthly_flow = sum(Flow, na.rm = TRUE)) %>%
  ungroup()

total_monthly_sum <- monthly_sum %>%
  group_by(month) %>%
  summarise(total_flow = sum(monthly_flow, na.rm = TRUE))



ggplot(total_monthly_sum, aes(x = month, y = total_flow)) +
  geom_line(color = "steelblue", size = 1) +              # 선 그래프
  geom_point(color = "darkblue", size = 1.5) +           # 포인트 추가 (선택)
  labs(
    title = "Total Monthly Inflow (All subasin)",
    x = "Month",
    y = "Total Flow (MCM/day or CMS)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

threshold_table <- total_monthly_sum %>%
  mutate(month = month(month)) %>%
  group_by(month) %>%
  summarise(threshold = quantile(total_flow, 0.20, na.rm = TRUE))







# df: Time(Date), Flow(numeric)
run_theory_events <- function(df, threshold_table){
  stopifnot(all(c("Time","Flow") %in% names(df)))
  d <- df %>%
    mutate(month = month(Time)) %>%
    left_join(threshold_table, by = "month") %>%
    mutate(deficit = pmax(threshold - Flow, 0),
           drought = deficit > 0)
  
  r <- rle(d$drought)
  ends   <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1
  ev_idx <- which(r$values)  # TRUE run들(가뭄)
  
  if(length(ev_idx) == 0){
    return(list(series = d, events = tibble(), idf = tibble()))
  }
  
  events <- lapply(ev_idx, function(k){
    s <- starts[k]; e <- ends[k]
    tibble(
      start_date = d$Time[s],
      end_date   = d$Time[e],
      L          = e - s + 1,                         # 지속기간
      D          = sum(d$deficit[s:e], na.rm = TRUE), # 총 결핍량
      I          = mean(d$deficit[s:e], na.rm = TRUE) # 강도(평균 결핍)
    )
  }) %>% bind_rows()
  
  # (선택) 경험적 재현기간 T ~ (n+1)/rank  (강도 기준)
  idf <- events %>%
    arrange(desc(I)) %>%
    mutate(rank = row_number(),
           T = (n() + 1) / rank)
  
  list(series = d, events = events, idf = idf)
}

# 사용 예
# out <- run_theory_events(df, threshold_table)
# out$events  # 각 이벤트의 L, D, I
# out$idf     # I–T 표 (IDF 곡선 만들 때 사용)

colnames(total_monthly_sum) <- c("Time", "Flow")

out <- run_theory_events(total_monthly_sum, threshold_table)




plot_idf <- function(idf_tbl){
  if(nrow(idf_tbl) == 0) return(message("No drought events to plot."))
  ggplot(idf_tbl, aes(x = L, y = I)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE, span = 0.8) +
    labs(title = "Drought IDF (Intensity vs Duration)",
         x = "Duration (months)", y = "Intensity (avg deficit)") +
    theme_bw()
}

# 사용
plot_idf(out$idf)


out$events %>%
  arrange(desc(I)) %>%
  head(5) 



ggplot(out$series, aes(x = Time, y = deficit)) +
  geom_line(color = "red") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  labs(title = "Drought Deficit Over Time",
       y = "Deficit (MCM)",
       x = "Time") +
  scale_x_date(
    date_breaks = "1 year",   # 연도 간격
    date_labels = "%Y"        # 연도만 표시
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )+
  theme(
    text = element_text(size = 12,family = "Helvetica"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black")
  )



ggplot(out$series, aes(x = Time, y = deficit)) +
  geom_line(color = "red") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  
  # 🔶 1994–1997 강조 박스
  annotate("rect",
           xmin = as.Date("1994-01-01"),
           xmax = as.Date("1997-12-31"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.5,          # 투명도
           fill = "grey") +
  
  # 🔶 2014–2017 강조 박스
  annotate("rect",
           xmin = as.Date("2014-01-01"),
           xmax = as.Date("2017-12-31"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.5,
           fill = "grey") +
  
  labs(title = "Drought Deficit Over Time",
       y = "Deficit (MCM)",
       x = "Time") +
  
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )

  
