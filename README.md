# 🥅 Goalie Intelligence System

**Live Dashboard:** https://nixyyanalytics.github.io/goalie-intelligence-system

An end-to-end NHL goaltender evaluation platform combining regularized regression modeling, multi-season scouting analytics, and granular shot-situation breakdowns.

## Features
- **Ridge Regression Model** — Projects GSAx over next 10 games with confidence bands
- **3-Year Scouting Tiers** — Volatility scoring, directional outlook, regression & rebound risk flags
- **Shot Detail Analytics** — Save % breakdowns by distance, angle, game state, period, and play type
- **Goalie Comparison Tool** — Side-by-side heatmap comparison across all shot situations
- **Interactive Dashboard** — Built in R Shiny and standalone HTML (no install required)

## Tech Stack
`R` `Shiny` `Plotly` `Ridge Regression` `JavaScript` `HTML/CSS`

## Files
| File | Description |
|------|-------------|
| `goalie_dashboard_v2.R` | Full R Shiny app |
| `index.html` | Standalone interactive dashboard (no install needed) |
| `Goalie_Master_Model_Output_Recent_Data.xlsx` | Model outputs, scouting tiers, projections |
| `Goalie_Shot_Detail_Outputs.xlsx` | Shot-situation metrics and predictor importance |

## Running Locally
```r
install.packages(c("shiny","readxl","dplyr","plotly","DT","scales","shinycssloaders"))
shiny::runApp("goalie_dashboard_v2.R")
```
