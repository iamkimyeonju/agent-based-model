setwd("G:/My Drive/University of Seoul/Agent-based model")
library(ggplot2)
library(reshape2)
library(dplyr)
library(lubridate)
library(tidyr)
library(writexl)

#install.packages("writexl")

#source("function_for_Geum.R")
source("Agent_based_Geum.R")
source("Agent_based_Geum_diversion_v4.R")

# running ABM ------------------------------------------------------------------

set.seed(1125)

with_ABM <- Agent_Geum_Network(ABM=TRUE,opt_year = 39)
without_ABM <- Agent_Geum_Network(ABM=FALSE, opt_year = 39)

with_ABM_new_diversion <- Agent_Geum_Network_Diversion(ABM=TRUE,opt_year = 39)
without_ABM_new_diversion <- Agent_Geum_Network_Diversion(ABM=FALSE, opt_year = 39)

# plot --------------------------------------------------------------------

without_ABM_new_diversion$gg_agr

# Save csv ---------------------------------------------------------------------

without_ABM_new_diversion$df_agr

without_ABM_new_diversion$demand_agr
df <- data.frame(without_ABM_new_diversion$df_agr)
demand <- data.frame(without_ABM_new_diversion$demand_agr)

colnames(demand) <- paste0("Agr", seq_len(ncol(df)))
colnames(df) <- paste0("Agr", seq_len(ncol(df)))
start_date <- as.Date("1981-10-01")
n_days     <- nrow(df)

df <- df %>%
  mutate(
    Date = seq(start_date, by = "day", length.out = n_days)
  ) %>%
  relocate(Date)

df <- df %>%
  mutate(ym = floor_date(Date, "month")) %>%
  group_by(ym) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  ungroup()

demand <- demand %>%
  mutate(
    Date = seq(start_date, by = "day", length.out = n_days)
  ) %>%
  relocate(Date)

demand <- demand %>%
  mutate(ym = floor_date(Date, "month")) %>%
  group_by(ym) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  ungroup()

write_xlsx(
  list(
    "actual_result" = df,
    "demand" = demand
  ),
  path = "result_csv/Agr_result.xlsx"
)

# Result -----------------------------------------------------------------------

without_ABM$gg_intake
with_ABM$gg_intake


without_ABM$breaker
with_ABM$breaker

without_ABM$gg_agr
with_ABM$gg_agr

without_ABM$gg_dam
with_ABM$gg_dam


with_ABM$gg_power
without_ABM$gg_power

with_ABM$Gini_coefficient
without_ABM$Gini_coefficient

with_ABM$Gini_intake
without_ABM$Gini_intake

with_ABM$Gini_agr
without_ABM$Gini_agr

with_ABM$Gini_power
without_ABM$Gini_power


# Current status of the Geum River Basin

with_ABM$Table_intake[,2] - without_ABM$Table_intake[,2]
with_ABM$Table_agr[,2] - without_ABM$Table_agr[,2]
with_ABM$Table_power[,2] - without_ABM$Table_power[,2]

# with and without for New diversion

with_ABM_new_diversion$Table_intake[,2] - without_ABM_new_diversion$Table_intake[,2]
with_ABM_new_diversion$Table_agr[,2] - without_ABM_new_diversion$Table_agr[,2]
with_ABM_new_diversion$Table_power[,2] - without_ABM_new_diversion$Table_power[,2]

# Gini 
with_ABM_new_diversion$Gini_coefficient
without_ABM_new_diversion$Gini_coefficient

with_ABM_new_diversion$Gini_agr
without_ABM_new_diversion$Gini_agr

with_ABM_new_diversion$Gini_intake
without_ABM_new_diversion$Gini_intake

# each station

with_ABM$Table_intake[,2] - without_ABM$Table_intake[,2]
with_ABM$Table_agr[,2] - without_ABM$Table_agr[,2]

with_ABM$Table_power[,2] - without_ABM$Table_power[,2]
with_ABM$Table_dam[,2] - without_ABM$Table_dam[,2]

with_ABM_new_diversion$Table_intake[,2] - with_ABM$Table_intake[,2]
with_ABM_new_diversion$Table_agr[,2] - with_ABM$Table_agr[,2]

without_ABM_new_diversion$Table_intake[,2] - without_ABM$Table_intake[,2]
without_ABM_new_diversion$Table_agr[,2] - without_ABM$Table_agr[,2]

with_ABM_new_diversion$Table_intake[,2] - without_ABM$Table_intake[,2]
with_ABM_new_diversion$Table_agr[,2]-without_ABM$Table_agr[,2]
with_ABM_new_diversion$Table_power[,2] - without_ABM$Table_power[,2]

# + with ABM이 더 좋은 결과
# - with ABM이 더 나쁜 결과

sum(with_ABM$Table_intake[,2] - without_ABM$Table_intake[,2])
sum(with_ABM$Table_agr[,2] - without_ABM$Table_agr[,2])
sum(with_ABM$Table_power[,2] - without_ABM$Table_power[,2])
sum(with_ABM$Table_dam[,2] - without_ABM$Table_dam[,2])

# 누적 그래프 ------------------------------------------------------------------
## Intake 11
cumline <- data.frame(days = 1:c(39*365), cumsum_withoutABM= cumsum(without_ABM_new_diversion$df_intake[,11]),cumsum_withABM= cumsum(with_ABM_new_diversion$df_intake[,11]))
ggplot(data = cumline, aes(x = days)) +
  geom_line(aes(y = cumsum_withoutABM, color = "Without ABM")) +
  geom_line(aes(y = cumsum_withABM, color = "With ABM")) +
  scale_color_manual(
    values = c("Without ABM" = "blue", "With ABM" = "red"),
    name = "Scenario"  # 범례 제목
  ) +
  labs(
    title = "Cumulative Intake Comparison for Intake 11",
    x = "Days",
    y = "Cumulative Intake (MCM)"
  ) +
  theme_classic()

## Agr 12 receiver
cumline <- data.frame(days = 1:c(39*365), cumsum_withoutABM= cumsum(without_ABM_new_diversion$df_agr[,12]),cumsum_withABM= cumsum(with_ABM_new_diversion$df_agr[,12]))
ggplot(data = cumline, aes(x = days)) +
  geom_line(aes(y = cumsum_withoutABM, color = "Without ABM")) +
  geom_line(aes(y = cumsum_withABM, color = "With ABM")) +
  scale_color_manual(
    values = c("Without ABM" = "blue", "With ABM" = "red"),
    name = "Scenario"  # 범례 제목
  ) +
  labs(
    title = "Cumulative Intake Comparison for Agr 12",
    x = "Days",
    y = "Cumulative Intake (MCM)"
  ) +
  theme_classic()


# Agr 7
cumline <- data.frame(days = 1:c(39*365), cumsum_withoutABM= cumsum(without_ABM_new_diversion$df_agr[,7]),cumsum_withABM= cumsum(with_ABM_new_diversion$df_agr[,7]))
ggplot(data = cumline, aes(x = days)) +
  geom_line(aes(y = cumsum_withoutABM, color = "Without ABM")) +
  geom_line(aes(y = cumsum_withABM, color = "With ABM")) +
  scale_color_manual(
    values = c("Without ABM" = "blue", "With ABM" = "red"),
    name = "Scenario"  # 범례 제목
  ) +
  labs(
    title = "Cumulative Intake Comparison for Agr 7",
    x = "Days",
    y = "Cumulative Intake (MCM)"
  ) +
  theme_classic()


# Agr 10
cumline <- data.frame(days = 1:c(39*365), cumsum_withoutABM= cumsum(without_ABM_new_diversion$df_agr[,10]),cumsum_withABM= cumsum(with_ABM_new_diversion$df_agr[,10]))
ggplot(data = cumline, aes(x = days)) +
  geom_line(aes(y = cumsum_withoutABM, color = "Without ABM")) +
  geom_line(aes(y = cumsum_withABM, color = "With ABM")) +
  scale_color_manual(
    values = c("Without ABM" = "blue", "With ABM" = "red"),
    name = "Scenario"  # 범례 제목
  ) +
  labs(
    title = "Cumulative Intake Comparison for Agr 10",
    x = "Days",
    y = "Cumulative Intake (MCM)"
  ) +
  theme_classic()

## donor
cumline <- data.frame(days = 1:c(39*365), cumsum_withoutABM= cumsum(without_ABM_new_diversion$df_agr[,13]),cumsum_withABM= cumsum(with_ABM_new_diversion$df_agr[,13]))
ggplot(data = cumline, aes(x = days)) +
  geom_line(aes(y = cumsum_withoutABM, color = "Without ABM")) +
  geom_line(aes(y = cumsum_withABM, color = "With ABM")) +
  scale_color_manual(
    values = c("Without ABM" = "blue", "With ABM" = "red"),
    name = "Scenario"  # 범례 제목
  ) +
  labs(
    title = "Cumulative Intake Comparison for Agr 13",
    x = "Days",
    y = "Cumulative Intake (MCM)"
  ) +
  theme_classic()


# colormap ---------------------------------------------------------------------

rows <- c(with_ABM_new_diversion$breaker$first_row, 
          with_ABM_new_diversion$breaker$second_row, 
          with_ABM_new_diversion$breaker$third_row)
cols <- c(with_ABM_new_diversion$breaker$first_col, 
          with_ABM_new_diversion$breaker$second_col,
          with_ABM_new_diversion$breaker$third_col)

valid <- !is.na(rows) & !is.na(cols)
rows <- rows[valid]
cols <- cols[valid]

heat_matrix <- matrix(0, nrow = 13, ncol = 3)

for (i in seq_along(rows)) {
  r <- rows[i]
  c <- cols[i]
  heat_matrix[r, c] <- heat_matrix[r, c] + 1
}

df_heat <- melt(heat_matrix)
colnames(df_heat) <- c("row", "col", "count")

# --- 1) 행/열 라벨 설정 (네가 원하는 이름으로 수정 가능) ---

stakeholder_labels <- c(1:13)


df_heat <- df_heat %>%
  mutate(
    row_f = factor(row,
                   levels = rev(1:13),
                   labels = rev(stakeholder_labels)),
    col_f = factor(col,
                   levels = 1:3,
                   labels = col_labels),
    count_plot = ifelse(count == 0, NA, count)
  )

col_labels <- c("Intake", "Agr", "Power")

ggplot(df_heat, aes(x = as.factor(col), y = row, fill = count)) +
  geom_tile(color = "black") +
  geom_text(aes(label = ifelse(count == 0, "", count)), size = 3) +
  scale_x_discrete(
    labels = col_labels,
    position = "top") +
  scale_y_reverse(
    breaks = 1:13,
    labels = 1:13 ) +
  scale_fill_gradient(
    low  = "white",
    high = "red",
    name = "Number of\nABM triggers") +
  labs(x = NULL, y = NULL) +
  
  theme_minimal(base_size = 12) +
  theme(
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.text.x.top    = element_text( size = 12),
    axis.title.x.top   = element_blank(),
    axis.text.x.bottom  = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    axis.text.y = element_text(size = 10)
  )



# ABM graph ------------------------------------------------------------------------

DF <- data.frame(Intake = 1:12, 
                 with_ABM = with_ABM_new_diversion$Table_intake[,2], 
                 without_ABM =  without_ABM_new_diversion$Table_intake[,2])
DFlong <- DF |> pivot_longer(cols = -Intake,names_to = "ABM")

ggplot(DFlong,aes(x=Intake, y = value,fill= ABM)) + 
  geom_col(position="dodge") +
  theme_bw() +ylab("Days of Water Supply Availability")+
  ggtitle("취수장의 용수공급가능일수") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = NULL) +
  coord_cartesian(ylim = c(1500, NA))



DF <- data.frame(Agr = 1:13, 
                 with_ABM = with_ABM_new_diversion$Table_agr[,2], 
                 without_ABM =  without_ABM_new_diversion$Table_agr[,2])
DFlong <- DF |> pivot_longer(cols = -Agr,names_to = "ABM")

ggplot(DFlong,aes(x=Agr, y = value,fill= ABM)) + 
  geom_col(position="dodge") +
  theme_bw() +ylab("Days of Water Supply Availability")+
  ggtitle("농업용수의 용수공급가능일수")+
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = NULL) +
  coord_cartesian(ylim = c(500, NA))



DF <- data.frame(Power = 1:3, 
                 with_ABM = with_ABM_new_diversion$Table_power[,2], 
                 without_ABM =  without_ABM_new_diversion$Table_power[,2])
DFlong <- DF |> pivot_longer(cols = -Power,names_to = "ABM")

ggplot(DFlong,aes(x=Power, y = value,fill= ABM)) + 
  geom_col(position="dodge") +
  theme_bw() +ylab("Days of Water Supply Availability")+
  ggtitle("발전용수의 용수공급가능일수")

# Time-series ------------------------------------------------------------------


df_ts <- data.frame(
  time = c(1:length(with_ABM$df_intake[,12])),
  with_ABM = cumsum(with_ABM$df_intake[,12]),
  without_ABM = cumsum(without_ABM$df_intake[,12])
)

df_long <- pivot_longer(df_ts, cols = c(with_ABM, without_ABM),
                        names_to = "Series", values_to = "CumulativeSum")

ggplot(df_long, aes(x = time, y = CumulativeSum, color = Series, linetype = Series)) +
  geom_line(size = 1) +
  labs(title = "Intake 12",
       x = "days", y = "Cumulative Sum of 용수사용량 (MCM)") +
  theme_bw()



df_ts <- data.frame(
  time =c(1:length(with_ABM$df_intake[,9])),
  with_ABM = cumsum(with_ABM$df_intake[,9]),
  without_ABM = cumsum(without_ABM$df_intake[,9])
)

df_long <- pivot_longer(df_ts, cols = c(with_ABM, without_ABM),
                        names_to = "Series", values_to = "CumulativeSum")

ggplot(df_long, aes(x = time, y = CumulativeSum, color = Series, linetype = Series)) +
  geom_line(size = 1) +
  labs(title = "Intake 9",
       x = "days", y = "Cumulative Sum of 용수사용량 (MCM)") +
  theme_bw()



df_ts <- data.frame(
  time = c(1:length(with_ABM$df_agr[,9])),
  with_ABM = cumsum(with_ABM$df_agr[,9]),
  without_ABM = cumsum(without_ABM$df_agr[,9])
)

df_long <- pivot_longer(df_ts, cols = c(with_ABM, without_ABM),
                        names_to = "Series", values_to = "CumulativeSum")

ggplot(df_long, aes(x = time, y = CumulativeSum, color = Series, linetype = Series)) +
  geom_line(size = 1) +
  labs(title = "Agr 9",
       x = "days", y = "Cumulative Sum of 용수사용량 (MCM)") +
  theme_bw()


source("inflow_history.R")

# Drought case 1: 1994 - 1997 ------------------------------------------------------------------

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
  
  labs(#title = "Drought Deficit Over Time",
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



# Drought case 2: 2014 - 2017

## ===== 0. 날짜 붙이기 (공통 준비 코드) =====
start_date <- as.Date("1981-10-01")
n_days <- nrow(with_ABM_new_diversion$df_intake)

date_seq <- seq(start_date, by = "day", length.out = n_days)

# 관심 있는 지점들:
#  - Intake 11 : 미호천 취수장 계열
#  - Agr 12    : 새 도수로 receiver
#  - Agr 13    : 새 도수로 donor
ts_df <- data.frame(
  Date = date_seq,
  Intake11_withABM    = with_ABM_new_diversion$df_intake[, 11],
  Intake11_withoutABM = without_ABM_new_diversion$df_intake[, 11],
  Agr10_withABM       = with_ABM_new_diversion$df_agr[, 10],
  Agr10_withoutABM    = without_ABM_new_diversion$df_agr[, 10],
  Agr11_withABM       = with_ABM_new_diversion$df_agr[, 11],
  Agr11_withoutABM    = without_ABM_new_diversion$df_agr[, 11]
)

# long-format으로 변환 (Stakeholder × Scenario)
ts_long <- ts_df |>
  pivot_longer(
    cols = -Date,
    names_to   = c("Stakeholder", "Scenario"),
    names_pattern = "(.*)_(withABM|withoutABM)",
    values_to  = "value"
  ) |>
  mutate(
    Stakeholder = factor(Stakeholder,
                         levels = c("Intake11", "Agr10", "Agr11"),
                         labels = c("Intake 11 (tributary donor)", "Agr 10 (receiver)", "Agr 11 (main donor)")),
    Scenario = factor(Scenario,
                      levels = c("withoutABM", "withABM"),
                      labels = c("Without ABM", "With ABM"))
  )




## ===== 1. Drought Case 1 (1994–1997) =====

drought1_long <- ts_long |>
  filter(
    Date >= as.Date("1994-01-01"),
    Date <= as.Date("1997-12-31")
  )

gg_drought1_ABM <- ggplot(drought1_long,
                          aes(x = Date, y = value, color = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ Stakeholder, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("Without ABM" = "grey40",
                                "With ABM"    = "red")) +
  labs(
    title = "Drought Case 1 (1994–1997): ABM Effects on Water Supply",
    x = "Time",
    y = "Daily Supplied Water (MCM)"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold"),
    legend.position = "top",
    axis.text.x = element_text( vjust = 0.5)
  )

gg_drought1_ABM



## ===== 2. Drought Case 2 (2014–2017) =====

drought2_long <- ts_long |>
  filter(
    Date >= as.Date("2014-01-01"),
    Date <= as.Date("2017-12-31")
  )

gg_drought2_ABM <- ggplot(drought2_long,
                          aes(x = Date, y = value, color = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ Stakeholder, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("Without ABM" = "grey40",
                                "With ABM"    = "red")) +
  labs(
    title = "Drought Case 2 (2014–2017): ABM Effects on Water Supply",
    x = "Time",
    y = "Daily Supplied Water (MCM)"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold"),
    legend.position = "top",
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )

gg_drought2_ABM



# long-format으로 변환 (Stakeholder × Scenario)
ts_long <- ts_df |>
  pivot_longer(
    cols = -Date,
    names_to   = c("Stakeholder", "Scenario"),
    names_pattern = "(.*)_(withABM|withoutABM)",
    values_to  = "value"
  ) |>
  mutate(
    Stakeholder = factor(Stakeholder,
                         levels = c("Intake11", "Agr10", "Agr11"),
                         labels = c("Intake 11 (donor 1)", "Agr 10 (receiver)", "Agr 11 (donor 2)")),
    Scenario = factor(Scenario,
                      levels = c("withoutABM", "withABM"),
                      labels = c("Without ABM", "With ABM"))
  )






# Plot 2 -----------------------------------------------------------------------

# 관심 있는 지점들:
#  - Agr 11 : 미호천 취수장 계열
#  - Agr 12    : 새 도수로 receiver
#  - Agr 13    : 새 도수로 donor
ts_df <- data.frame(
  Date = date_seq,
  Agr12_withABM       = with_ABM_new_diversion$df_agr[, 12],
  Agr12_withoutABM    = without_ABM_new_diversion$df_agr[, 12],
  Agr13_withABM       = with_ABM_new_diversion$df_agr[, 13],
  Agr13_withoutABM    = without_ABM_new_diversion$df_agr[, 13]
)


# long-format으로 변환 (Stakeholder × Scenario)
ts_long <- ts_df |>
  pivot_longer(
    cols = -Date,
    names_to   = c("Stakeholder", "Scenario"),
    names_pattern = "(.*)_(withABM|withoutABM)",
    values_to  = "value"
  ) |>
  mutate(
    Stakeholder = factor(Stakeholder,
                         levels = c("Agr12", "Agr13"),
                         labels = c("Agr 12 (receiver)", "Agr 13 (main donor)")),
    Scenario = factor(Scenario,
                      levels = c("withoutABM", "withABM"),
                      labels = c("Without ABM", "With ABM"))
  )




## ===== 1. Drought Case 1 (1994–1997) =====

drought1_long <- ts_long |>
  filter(
    Date >= as.Date("1994-01-01"),
    Date <= as.Date("1997-12-31")
  )

gg_drought1_ABM <- ggplot(drought1_long,
                          aes(x = Date, y = value, color = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ Stakeholder, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("Without ABM" = "grey40",
                                "With ABM"    = "red")) +
  labs(
    title = "Drought Case 1 (1994–1997): ABM Effects on Water Supply",
    x = "Time",
    y = "Daily Supplied Water (MCM)"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold"),
    legend.position = "top",
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )

gg_drought1_ABM



## ===== 2. Drought Case 2 (2014–2017) =====

drought2_long <- ts_long |>
  filter(
    Date >= as.Date("2014-01-01"),
    Date <= as.Date("2017-12-31")
  )

gg_drought2_ABM <- ggplot(drought2_long,
                          aes(x = Date, y = value, color = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ Stakeholder, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("Without ABM" = "grey40",
                                "With ABM"    = "red")) +
  labs(
    title = "Drought Case 2 (2014–2017): ABM Effects on Water Supply",
    x = "Time",
    y = "Daily Supplied Water (MCM)"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold"),
    legend.position = "top",
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )

gg_drought2_ABM



# Plot 3 -----------------------------------------------------------------------

# 관심 있는 지점들:
#  - Agr 11 : 미호천 취수장 계열
#  - Agr 12    : 새 도수로 receiver
#  - Agr 13    : 새 도수로 donor
ts_df <- data.frame(
  Date = date_seq,
  Intake6_withABM    = with_ABM_new_diversion$df_intake[, 6],
  Intake6_withoutABM = without_ABM_new_diversion$df_intake[, 6],
  Agr7_withABM       = with_ABM_new_diversion$df_agr[, 7],
  Agr7_withoutABM    = without_ABM_new_diversion$df_agr[, 7],
  Intake7_withABM       = with_ABM_new_diversion$df_intake[, 7],
  Intake7_withoutABM    = without_ABM_new_diversion$df_intake[, 7]
)


# long-format으로 변환 (Stakeholder × Scenario)
ts_long <- ts_df |>
  pivot_longer(
    cols = -Date,
    names_to   = c("Stakeholder", "Scenario"),
    names_pattern = "(.*)_(withABM|withoutABM)",
    values_to  = "value"
  ) |>
  mutate(
    Stakeholder = factor(Stakeholder,
                         levels = c("Intake6", "Agr7", "Intake7"),
                         labels = c("Intake 6 (tributary donor)", "Agr 7 (receiver)", "Intake 7 (main donor)")),
    Scenario = factor(Scenario,
                      levels = c("withoutABM", "withABM"),
                      labels = c("Without ABM", "With ABM"))
  )




## ===== 1. Drought Case 1 (1994–1997) =====

drought1_long <- ts_long |>
  filter(
    Date >= as.Date("1994-01-01"),
    Date <= as.Date("1997-12-31")
  )

gg_drought1_ABM <- ggplot(drought1_long,
                          aes(x = Date, y = value, color = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ Stakeholder, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("Without ABM" = "grey40",
                                "With ABM"    = "red")) +
  labs(
    title = "Drought Case 1 (1994–1997): ABM Effects on Water Supply",
    x = "Time",
    y = "Daily Supplied Water (MCM)"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold"),
    legend.position = "top",
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )

gg_drought1_ABM



## ===== 2. Drought Case 2 (2014–2017) =====

drought2_long <- ts_long |>
  filter(
    Date >= as.Date("2014-01-01"),
    Date <= as.Date("2017-12-31")
  )

gg_drought2_ABM <- ggplot(drought2_long,
                          aes(x = Date, y = value, color = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ Stakeholder, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("Without ABM" = "grey40",
                                "With ABM"    = "red")) +
  labs(
    title = "Drought Case 2 (2014–2017): ABM Effects on Water Supply",
    x = "Time",
    y = "Daily Supplied Water (MCM)"
  ) +
  theme_bw() +
  theme(
    #text = element_text(family = "Helvetica"),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold"),
    legend.position = "top",
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )

gg_drought2_ABM



