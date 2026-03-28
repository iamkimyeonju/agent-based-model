# Agent-Based Model for Equity-Oriented Water Reallocation in the Geum River Basin

**Author:** Yeonju Kim

**Affiliations:** University of Connecticut

**Contact:** iamkimyeonju@gmail.com


---

## Overview

This repository contains the R code and input data for an agent-based model (ABM) that simulates water reallocation in the **Geum River Basin (금강유역), South Korea**, under historical drought conditions. The model evaluates trade-offs between **system efficiency** (profit ratio) and **distributional equity** (Gini coefficient) when virtual diversion channels are activated by water-stressed agents.


---

## Repository Structure

```
.
├── ABM.R                                      # Main entry point: run simulations & export results
├── Agent_based_Geum.R                         # Core ABM — base network (no diversions)
├── Agent_based_Geum_diversion_v1.R            # ABM — diversion v1
├── Agent_based_Geum_diversion_v2.R            # ABM — diversion v2
├── Agent_based_Geum_diversion_v3.R            # ABM — diversion v3
├── Agent_based_Geum_diversion_v4.R            # ABM — diversion v4 (current, used in sensitivity analysis)
├── function_for_Geum.R                        # Network solver + optimization objective functions
├── inflow_history.R                           # Historical inflow preprocessing & drought identification
├── sensitivity_analysis.Rmd                   # Sensitivity analysis: behavioral scenarios
├── ABM_result_figures.Rmd                     # Full analysis report (R Markdown)
│
├── data/
│   ├── Netflow_1981-2020.csv                  # SWAT-simulated daily streamflow (CMS)
│   ├── Yongdam_Release.csv                    # Yongdam Dam monthly release rules (MCM)
│   ├── Yongdam_Release.xlsx
│   ├── Daecheong_Release.csv                  # Daecheong Dam monthly release rules (MCM)
│   ├── Daecheong_Release.xlsx
│   ├── Boryeong_Release.csv                   # Boryeong Dam monthly release rules (MCM)
│   ├── Boryeong_Release.xlsx
│   ├── Buan_Release.csv                       # Buan Dam monthly release rules (MCM)
│   ├── Buan_Release.xlsx
│   ├── intake_filter_reservoir.csv            # Water intake node specifications
│   ├── intake_filter_reservoir.xlsx
│   ├── 농업기반시설 시설제원.csv               # Agricultural reservoir specifications
│   ├── 하천수허가량(금강홍수통제소).csv        # River water permit data
│   ├── 하천수허가량(금강홍수통제소).xlsx
│   ├── 용수공급체계(취수장, 정수장, 배수장).xlsx  # Water supply system layout
│   └── drought scenario/                      # 22 sub-basin drought runoff scenarios (legacy)
│
├── result_csv/                                # Simulation output files
└── Figures/                                   # Generated figures
```

---

## Input Data

All input data files are located in the `data/` folder.

| File | Source | Unit | Description |
|------|--------|------|-------------|
| `data/Netflow_1981-2020.csv` | SWAT model via KEI | CMS (daily) | Simulated streamflow for ~100 standard sub-basins, 1981–2020. Converted internally to MCM/day (`× 86400 / 1e6`). Leap day (Feb 29) is removed to match the 365-day/year simulation assumption. |
| `data/Yongdam_Release.csv` | K-water multipurpose dam operation rules | MCM (monthly, starting October) | Monthly release targets for Yongdam Dam, disaggregated to daily values internally. |
| `data/Daecheong_Release.csv` | K-water | MCM (monthly, starting October) | Same structure as Yongdam. |
| `data/Boryeong_Release.csv` | K-water | MCM (monthly, starting October) | Same structure as Yongdam. |
| `data/Buan_Release.csv` | K-water | MCM (monthly, starting October) | Buan Dam monthly release rules. |
| `data/intake_filter_reservoir.csv` | Sub-project 4 data | — | Water intake node metadata (location, capacity). |
| `data/농업기반시설 시설제원.csv` | Korea Rural Community Corporation (한국농어촌공사) via [data.go.kr](https://www.data.go.kr/data/15044339/fileData.do) | ha, yr, m², m, thousand m³, EL.m | Agricultural reservoir specifications: area, return period, volume, length, effective storage, flood level. Used to set Baekgok (백곡) and Tapjeong (탑정) reservoir parameters. |
| `data/하천수허가량(금강홍수통제소).csv` | Geum River Flood Control Office | — | Licensed river water withdrawal permits. |
| `data/drought scenario/Sub-basin_runoff_GG_DU*_RD*.csv` | External sub-project (legacy) | CMS (daily, starting October) | 22 sub-basin drought runoff scenarios. No longer used in the current version. |


---


**Return rate constants (α):**

| Variable | Value | Meaning |
|----------|-------|---------|
| `alpha_Agr` | 0.35 | 35% of agricultural withdrawals return to the stream |
| `alpha_Ind` | 0.65 | 65% of industrial withdrawals return to the stream |
| `alpha_Dom` | 0.65 | 65% of domestic withdrawals return to the stream |
| `alpha_Env` | 1.00 | Environmental flow is fully returned (pass-through) |
| `alpha_Pow` | 1.00 | Hydropower flow is fully returned (pass-through) |
| `alpha_Intake` | 0.65 | Average of industrial and domestic return rates |

**Dam storage parameters:**

| Dam | Total Capacity (MCM) | Initial Storage | Min Storage | Max Storage |
|-----|---------------------|-----------------|-------------|-------------|
| Yongdam (용담댐) | 815.0 | 70% = 570.5 MCM | 10% = 81.5 MCM | 90% = 733.5 MCM |
| Daecheong (대청댐) | 1,490.0 | 70% = 1,043 MCM | 10% = 149.0 MCM | 90% = 1,341 MCM |
| Boryeong (보령댐) | 116.9 | 70% = 81.8 MCM | 10% = 11.7 MCM | 90% = 105.2 MCM |

Boryeong Dam has a fixed waterway transfer capacity of **3.45 MCM/day** connecting it to the main channel.


### `Agent_based_Geum_diversion_v1.R` to `v4.R` — Diversion Network ABM

**Function:** `Agent_Geum_Network_Diversion(supply_scenario, demand_scenario, opt_year, ABM, p_vec)`

Extends the base network by adding **7 virtual diversion channels** that can transfer water from the main stream or intake nodes to water-stressed agricultural stakeholders. The ABM logic is modified: instead of reducing demand, agents receive additional supply through activated diversions.

**Diversion channels:**

| ID | From | To | Type |
|----|------|----|------|
| div1 | Node B (main stream) | Agr5 | Main-to-tributary |
| div2 | Node C (main stream) | Agr7 | Main-to-tributary |
| div3 | Node E (main stream) | Agr11 | Main-to-tributary |
| div4 | Node F (main stream) | Agr13 | Main-to-tributary |
| div5 | Intake 4 | Agr5 | Intake-to-agricultural |
| div6 | Intake 6 | Agr7 | Intake-to-agricultural |
| div7 | Intake 11 | Agr11 | Intake-to-agricultural |

**Additional parameter — `p_vec`:**

A vector of four activation probabilities corresponding to Manhattan distances d = 1, 2, 3, ≥4 between the donor and recipient nodes in the network graph.

| Behavioral Scenario | p(d=1) | p(d=2) | p(d=3) | p(d≥4) | Description |
|--------------------|--------|--------|--------|--------|-------------|
| Pro-social | 0.95 | 0.80 | 0.60 | 0.40 | Agents prioritize equity; willingly donate even to distant users |
| **Bounded Rational** (default) | **0.90** | **0.60** | **0.30** | **0.10** | Agents balance self-interest with network awareness |
| Self-interested | 0.50 | 0.10 | 0.02 | 0.00 | Agents rarely donate; strongly prefer nearby beneficiaries |


**Version history:**

| Version | Key changes |
|---------|-------------|
| v1 | Initial implementation with diversion supply logic |
| v2 | Refined activation trigger conditions |
| v3 | Updated ABM update window timing |
| v4 (current) | Fixed Node_D bug; corrected `update_end` to use next month's days; filtered Feb 29 from inflow data; fixed day-243 annotation; added `demand_scenario` local assignment; added `Dam2_Spillway` to Node_D |

---


### `inflow_history.R` — Historical Inflow Analysis

Preprocesses SWAT streamflow data and identifies historical drought events using **run theory**.

**Sub-basin aggregation:** 16 locations are constructed by summing across standard sub-basin codes (300101–320305):

| Code | Location |
|------|----------|
| 3001 | Yongdam Dam (용담댐) |
| 3002 | Yongdam downstream (용담댐하류) |
| 3003 | Muju Namdaecheon (무주남대천) |
| 3004 | Yeongdongcheon (영동천) |
| 3005 | Chogang (초강) |
| 3006 | Daecheong upstream (대청댐상류) |
| 3007 | Bocheongcheon (보청천) |
| 3008 | Daecheong Dam (대청댐) |
| 3009 | Gapcheon (갑천) |
| 3010 | Daecheong downstream (대청댐하류) |
| 3011 | Mihocheon (미호천, excluding Baekgok sub-basin) |
| 3012 | Geum at Gongju (금강공주) |
| 3013 | Nonsancheon (논산천) |
| 3014 | Geum Estuary Barrage (금강하구언) |
| 301103 | Baekgok Dam (백곡댐) |
| 320305 | Boryeong Dam (보령댐) |



---

### `sensitivity_analysis.Rmd` — Behavioral Sensitivity Analysis

Compares the three behavioral scenarios (Pro-social, Bounded Rational, Self-interested) plus a No-ABM baseline across:

- **Section 4.1** Gini coefficient by sector
- **Section 4.2** Agricultural stakeholder reliability (summed days with demand met)
- **Section 4.3** Diversion activation frequency (proportion of days active)
- **Section 4.4** System profit ratio by sector
- **Section 4.5** Equity–efficiency trade-off scatter
- **Section 4.6** Total water transferred (MCM) per diversion
- **Section 4.7** Pattern consistency: ΔReliability and ΔGini vs No-ABM baseline

**Reliability** for each agricultural stakeholder is computed as:
```r
colSums(r$df_agr / r$org_demand_agr, na.rm = TRUE)
```
A value equal to `opt_year × (days in irrigation season)` indicates full demand satisfaction throughout the simulation.

---

---

## Dependencies

```r
install.packages(c(
  "tidyverse", "dplyr", "lubridate", "zoo",
  "ggplot2", "reshape2", "reshape", "naniar", "DescTools",
  "writexl", "patchwork", "knitr",
  "GA", "mco", "parallel", "doParallel",
  "ggrepel", "scales"
))
```

