서울시립대학교
University of Connecticut
김연주 Yeonju Kim

작성일: 2024년 06월 25일
1차 수정일: 2024년 8월 31일
2차 수정일: 2025년 1월 28일
3차 수정일: 2025년 7월 13일

---

<개요>

1. 코딩 프로그램에 필요한 엑셀파일 설명
2. 코딩 프로그램 설명
3. 업데이트 내용

<1. 코딩 프로그램에 필요한 엑셀파일 설명>

* drought scenario/Sub-basin\_runoff\_GG\_DU\*\_RD\*.csv (Legacy)
  출처: 다른세부에서 제공받은 금강유역 가뭄 유출시나리오
  단위: 10월부터 시작하는 일단위 cms
* Netflow\_1981-202
  출처: KEI (김재영 초빙연구원) 으로부터 받은 SWAT 모의 과거기간 유량자료 (서승범 교수님이 받아서 전달)
  단위: 일단위 cms
* Yongdam\_Releases.csv
  출처: 수자원공사 다목적댐 운영룰
  단위: 10월부터 시작하는 월단위 MCM
* Daecheong\_Releases.csv
  출처: 수자원공사 다목적댐 운영룰
  단위: 10월부터 시작하는 월단위 MCM
* Boryeong\_Releases.csv
  출처: 수자원공사 다목적댐 운영룰
  단위: 10월부터 시작하는 월단위 MCM
* 농업기반시설 시설제원.csv
  출처: 한국농어촌공사\_농업기반시설 시설제원\_저수지 https://www.data.go.kr/data/15044339/fileData.do
  단위: 면적(ha), 빈도(년), 체적(m2), 길이(m), 저수량(천m3), 홍수위(EL.m)
* intake\_filter\_reservoir.csv
  출처: 4세부 제공자료
* 하천수허가량(금강홍수통제수).csv
  출처: 금강홍수통제소 하천수허가량

<2. 코딩 프로그램 설명>
setwd("본 파일위치") <- 설정필요
10년 365일로 모의 (윤년은 반영하지 않음, 1년이 항상 365일이라고 가정)
drought\_scenario - 1 ~ 22로 설정하면 다른세부의 가뭄시나리오 22종으로 모의
Supply ratio - 0.9~1.1로 설정하면 다목적댐의 방류량을 기존 방류량의 0.9 ~ 1.1 비율로 모의
Demand ratio - 0.8 ~ 1.2로 설정하면 다목적댐을 제외한 모든 stakeholder의 demand값을 본 비율을 곱하여 모의

---

## function\_for\_Geum

Initial\_Geum 파일에서 최적화를 위해 금강유역 전체 네트워크 계산과 동시에 최적화 파라미터인 Efficiency (시스템 이익), Equity (지니계수)를 계산하는 함수파일입니다

setwd("본 파일위치") <- 설정필요

---

## Optimization

function\_for\_Geum 파일을 활용하여 최적화를 하는 파일입니다

setwd("본 파일위치") <- 설정필요

Efficiency \& Equity \& 둘을 합한 Multi-Objective problem
총 3번의 최적화를 할때 컴퓨터 사양에 따라 다르지만 Thinkpad Ryzen pro7 기준 약 24시간이 소요됩니다

시간이 오래 걸리기 때문에 제가 계산한 파일 함께 드립니다
본 파일을 실행하고 24시간이 지나면 동일한 값이 도출됩니다

Efficiency : 시스템 이익 최적화 결과

Equity : 지니계수 최적화 결과

solution\_Eff : 최적화를 통해 나온 결과 (Efficiency 파일)를 실제 금강유역에 적용하였을때

solution\_Equ : 최적화를 통해 나온 결과 (Equity 파일)를 실제 금강유역에 적용하였을때

multi\_nsga : multi-objective 최적화 결과



---

Agent\_based\_model: 현재 금강유역을 기준으로 한 물배분 모형과 ABM
Agent\_based\_model\_potential\_diversion: 현재 금강유역 기준 + 본류에서 지류로 물을 가져오는 가상의 도수로를 만성적으로 넣고 ABM
Agent\_based\_model\_diversion: 현재 금강유역에서 + 본류에서 지류로의 도수로 (diversion\_main) + 지류안에서의 도수로 (diversion\_tributary)
- 본 파일에서는 Agent\_based\_model과는 달리 ABM을 적용할때 Demand를 감소시키는 것이 아닌 물을 공급해준다는 로직으로 수정
---

function\_for\_Geum 파일을 활용하여 agent based model을 개발하기 위한 test file



<3. 업데이트 내용>

2차 수정예정 (2025년 2월 5일)
--> 농업용 저수지 (백곡저수지, 탑정저수지)에서 운영룰로 인해 하천유지유량이 마이너스 값을 가질 때가 발생하여 운영룰을 조정
--> 돈 계산의 단위가 확실하지 않아 김연주가 ABM 적용 이전에 다시 공부하는 차원에서 손을 봄

