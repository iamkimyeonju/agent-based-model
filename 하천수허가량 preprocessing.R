Agr_facility <- read.csv("농업기반시설 시설제원.csv") %>%
  mutate(금강 = ifelse(grepl("전북",소재지) | grepl("대전광역시",소재지) |grepl("충청북도",소재지)|grepl("세종",소재지),TRUE,FALSE)) %>%
  filter(금강 == TRUE) %>% 
  select(표준코드, 시설명,소재지) %>%
  filter(시설명 != "양수" & 시설명 != "수장" &시설명 != "수정" & nchar(시설명) !=1)

river_intake <- read.csv("하천수허가량(금강홍수통제소).csv") %>% 
  mutate(소재지 = NA) %>%
  mutate(표준코드 = NA) %>%
  mutate(조인트코드 = NA)

for (k in 1:nrow(Agr_facility)){
  # n <- str_detect(river_intake$시설물명,Agr_facility$시설명[k])
  n <- NA
  n <- grep(Agr_facility$시설명[k],river_intake$시설물명)
  river_intake$소재지[n] <- Agr_facility$소재지[k]
  river_intake$표준코드[n] <- Agr_facility$표준코드[k]
  river_intake$조인트코드[n] <- Agr_facility$시설명[k]
}

river_intake %>%
  group_by(용도) %>%
  summarize(sum=sum(허가량.m..일.),n=n())

grep(Agr_facility$시설명[2],river_intake$시설물명)

Main_Agr <- read.csv("주요농업용저수지상황.csv") %>% 
  select(저수지명) 
Main_Agr <- c(Main_Agr[3:17,])

Agr_reservoir <- read.csv("농업기반시설 시설제원.csv")%>%
  filter(종별 ==1 & 구분 == "주수원"& 시설명 %in% Main_Agr ) 

rbind(Agr_reservoir[str_detect(Agr_reservoir$소재지,"충청북도"),],
      Agr_reservoir[str_detect(Agr_reservoir$소재지,"충청남도"),],
      Agr_reservoir[str_detect(Agr_reservoir$소재지,"전북"),])

river_plot <- read.csv("하천수허가량(금강홍수통제소).csv") %>%
  group_by(용도) %>%
  summarise(사용량 = sum(허가량.m..일.)) %>% 
  mutate(비율 = 사용량/sum(사용량) * 100) %>%
  mutate(용도 = forcats::fct_reorder(용도, 사용량)) %>%
  ggplot(aes(x=용도,y=사용량)) + geom_bar(stat='identity') + theme_minimal() +
  geom_text(aes(label = round(비율, 2)), vjust=-1) +
  labs(title= "금강홍수통제소 하천수허가량 자료 용도별 추이",
       caption = "2024년 5월 자료 기준")+
  theme(axis.title = element_text(size=15),
        title = element_text(size=17))

aaaaa <- read.csv("하천수허가량(금강홍수통제소).csv") %>% 
  filter(용도 == "농업용수") %>% 
  arrange(desc(허가량.m..일.)) %>%
  mutate(누적반영비율 = cumsum(허가량.m..일.)/sum(허가량.m..일.)) 
