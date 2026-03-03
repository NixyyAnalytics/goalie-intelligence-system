library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(DT)
library(plotly)
library(scales)
library(shinycssloaders)

# ── Load Data ──────────────────────────────────────────────────────────────────
raw      <- read_excel("Goalie_Master_Model_Output_Recent_Data.xlsx", sheet = "All_Raw_Data")
scout    <- read_excel("Goalie_Master_Model_Output_Recent_Data.xlsx", sheet = "Scout_3yr_Summary")
proj     <- read_excel("Goalie_Master_Model_Output_Recent_Data.xlsx", sheet = "Next10_Projection")
drv_glob <- read_excel("Goalie_Master_Model_Output_Recent_Data.xlsx", sheet = "Drivers_Global")
drv_coh  <- read_excel("Goalie_Master_Model_Output_Recent_Data.xlsx", sheet = "Drivers_Cohort")
shot_raw <- read_excel("Goalie_Shot_Detail_Outputs.xlsx", sheet = "goalie_metrics")
pred_imp <- read_excel("Goalie_Shot_Detail_Outputs.xlsx", sheet = "predictor_importance")

# ── Clean / Prep ───────────────────────────────────────────────────────────────
proj <- proj %>%
  mutate(
    band_lo = as.numeric(gsub("\\[([^,]+),.*",      "\\1", `Band (10g)`)),
    band_hi = as.numeric(gsub(".*,\\s*([^]]+)\\]",  "\\1", `Band (10g)`))
  )

shot <- shot_raw %>%
  mutate(across(-goalie_name, ~suppressWarnings(as.numeric(.)))) %>%
  filter(!is.na(games_played), games_played >= 20) %>%
  arrange(desc(sv_pct_overall))

seasons <- sort(unique(raw$Season))

tier_cols <- c(Elite = "#00C8F0", Strong = "#52E8A0", Average = "#F0C040", Weak = "#F05060")
dir_cols  <- c("Strong Up" = "#52E8A0", "Up" = "#00C8F0",
               "Flat" = "#F0C040", "Down" = "#F08040", "Strong Down" = "#F05060")

# ── CSS ────────────────────────────────────────────────────────────────────────
css <- '
@import url("https://fonts.googleapis.com/css2?family=Barlow+Condensed:wght@300;400;600;700;900&family=Barlow:wght@300;400;500&display=swap");

* { box-sizing: border-box; }
body { background:#060A12; color:#C5D5E8; font-family:"Barlow",sans-serif; margin:0; padding:0; }

.dash-header {
  background: linear-gradient(90deg, #070E1C, #0A1628 60%, #06101E);
  border-bottom: 1px solid #1A3050;
  padding: 0 32px; height: 62px;
  display: flex; align-items: center; gap: 14px;
}
.dash-header h1 {
  font-family:"Barlow Condensed",sans-serif; font-weight:900;
  font-size:22px; letter-spacing:4px; text-transform:uppercase; color:#fff; margin:0;
}
.dash-header .sub { font-size:9px; letter-spacing:3.5px; color:#00C8F0; text-transform:uppercase; margin-top:3px; }

.sidebar-panel { background:#0A1220; border-right:1px solid #1A3050; padding:20px 14px; min-height:calc(100vh - 62px); }
.section-title { font-family:"Barlow Condensed",sans-serif; font-size:9px; letter-spacing:3px; color:#00C8F0;
  text-transform:uppercase; margin:18px 0 8px; padding-bottom:4px; border-bottom:1px solid #1A3050; }
.section-title:first-child { margin-top:0; }

.main-panel { background:#060A12; padding:20px 24px; }

/* Tabs */
.nav-tabs { border-bottom:1px solid #1A3050 !important; }
.nav-tabs > li > a {
  font-family:"Barlow Condensed",sans-serif; font-size:12px; font-weight:600;
  letter-spacing:2px; text-transform:uppercase; color:#566F8A !important;
  background:transparent !important; border:none !important; padding:10px 16px;
}
.nav-tabs > li.active > a, .nav-tabs > li.active > a:hover {
  color:#00C8F0 !important; border-bottom:2px solid #00C8F0 !important; background:transparent !important;
}
.nav-tabs > li > a:hover { color:#C5D5E8 !important; }

/* KPI Cards */
.kpi-row { display:flex; gap:12px; margin-bottom:18px; flex-wrap:wrap; }
.kpi-card {
  flex:1; min-width:130px; background:linear-gradient(145deg,#0D1B2E,#0A1525);
  border:1px solid #1A3050; border-radius:6px; padding:13px 16px; position:relative; overflow:hidden;
}
.kpi-card::before { content:""; position:absolute; top:0; left:0; right:0; height:2px;
  background:linear-gradient(90deg,#00C8F0,transparent); }
.kpi-label { font-size:8.5px; letter-spacing:2.5px; color:#566F8A; text-transform:uppercase; margin-bottom:6px; }
.kpi-value { font-family:"Barlow Condensed",sans-serif; font-size:28px; font-weight:700; color:#fff; line-height:1; }
.kpi-sub   { font-size:10px; color:#566F8A; margin-top:3px; }

/* Plot cards */
.plot-card { background:#0A1220; border:1px solid #1A3050; border-radius:6px; padding:16px; margin-bottom:16px; }
.plot-title { font-family:"Barlow Condensed",sans-serif; font-size:11px; font-weight:600; letter-spacing:2.5px;
  color:#EDF2F8; text-transform:uppercase; margin-bottom:12px; padding-bottom:8px; border-bottom:1px solid #1A3050; }

/* DataTables */
.dataTables_wrapper { background:transparent !important; color:#C5D5E8; font-size:12px; }
table.dataTable { background:transparent !important; border:none !important; }
table.dataTable thead th {
  background:#0A1220 !important; border-bottom:1px solid #1A3050 !important;
  color:#00C8F0 !important; font-family:"Barlow Condensed",sans-serif;
  font-size:10px; letter-spacing:1.5px; text-transform:uppercase;
}
table.dataTable tbody tr { background:#060A12 !important; }
table.dataTable tbody tr:hover { background:#0D1828 !important; }
table.dataTable tbody td { border-bottom:1px solid #0F1D2E !important; color:#C5D5E8; }
.dataTables_filter input, .dataTables_length select {
  background:#0D1828; border:1px solid #1A3050; color:#C5D5E8; border-radius:4px; padding:4px 8px;
}
.dataTables_info, .dataTables_paginate { color:#566F8A !important; }
.paginate_button { color:#566F8A !important; }
.paginate_button.current { background:#1A3050 !important; color:#00C8F0 !important; border:none !important; }

/* Inputs */
.selectize-input { background:#0D1828 !important; border:1px solid #1A3050 !important; color:#C5D5E8 !important; }
.selectize-dropdown { background:#0D1828; border:1px solid #1A3050; color:#C5D5E8; }
.selectize-dropdown .option:hover { background:#1A3050; }
.irs-bar,.irs-bar-edge { background:#00C8F0; border-color:#00C8F0; }
.irs-slider { background:#00C8F0; border-color:#00C8F0; }
.irs-from,.irs-to,.irs-single { background:#00C8F0; }
label { color:#8AA3BF; font-size:11px; letter-spacing:1px; text-transform:uppercase; }

/* Heatmap cells */
.hm-grid { display:flex; gap:4px; flex-wrap:wrap; margin-bottom:8px; }
.hm-cell {
  flex:1; min-width:60px; border-radius:5px; padding:8px 5px;
  text-align:center; font-family:"Barlow Condensed",sans-serif;
}
.hm-label { font-size:8px; letter-spacing:1px; color:rgba(255,255,255,.6); text-transform:uppercase; display:block; margin-bottom:3px; }
.hm-val   { font-size:18px; font-weight:700; color:#fff; display:block; line-height:1; }
.hm-na    { font-size:12px; color:rgba(255,255,255,.3); }
.hm-delta-pos { font-size:9px; color:#52E8A0; display:block; }
.hm-delta-neg { font-size:9px; color:#F05060; display:block; }

::-webkit-scrollbar { width:5px; height:5px; }
::-webkit-scrollbar-track { background:#060A12; }
::-webkit-scrollbar-thumb { background:#1A3050; border-radius:3px; }
'

# ── Heatmap colour helper (server-side, returns hex) ──────────────────────────
sv_color <- function(v) {
  if (is.na(v) || is.null(v)) return("#1A2A3A")
  lo <- 0.76; hi <- 0.975
  t  <- max(0, min(1, (v - lo) / (hi - lo)))
  if (t < 0.5) {
    u  <- t * 2
    r  <- round(200 - u * 150); g <- round(50 + u * 120); b <- round(50 + u * 90)
  } else {
    u  <- (t - 0.5) * 2
    r  <- round(50 - u * 50);  g <- round(170 + u * 62); b <- round(140 + u * 110)
  }
  sprintf("rgb(%d,%d,%d)", r, g, b)
}

# Build one HTML heatmap row
hm_html <- function(labels, values, cmp_values = NULL) {
  cells <- mapply(function(lbl, val, cmp) {
    bg  <- sv_color(val)
    fmt <- if (is.na(val)) '<span class="hm-na">N/A</span>' else sprintf('<span class="hm-val">%.3f</span>', val)
    dlt <- ""
    if (!is.null(cmp) && !is.na(val) && !is.na(cmp)) {
      d   <- val - cmp
      cls <- if (d >= 0) "hm-delta-pos" else "hm-delta-neg"
      dlt <- sprintf('<span class="%s">%+.3f</span>', cls, d)
    }
    sprintf('<div class="hm-cell" style="background:%s"><span class="hm-label">%s</span>%s%s</div>', bg, lbl, fmt, dlt)
  }, labels, values, if (is.null(cmp_values)) rep(list(NULL), length(labels)) else as.list(cmp_values),
  SIMPLIFY = TRUE)
  paste0('<div class="hm-grid">', paste(cells, collapse=""), '</div>')
}

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(tags$style(HTML(css))),

  div(class = "dash-header",
    div(style = "font-size:28px;filter:drop-shadow(0 0 10px #00C8F066)", "🥅"),
    div(
      h1("Goalie Intelligence System"),
      div(class = "sub", "Advanced Goaltending Analytics & Projection Model · 2025–26")
    ),
    div(style = "margin-left:auto;display:flex;gap:8px",
      tags$span(style="background:#00C8F018;border:1px solid #00C8F033;color:#00C8F0;font-family:'Barlow Condensed';font-size:10px;letter-spacing:2px;text-transform:uppercase;padding:4px 10px;border-radius:2px", "Ridge Model"),
      tags$span(style="background:#00C8F018;border:1px solid #00C8F033;color:#00C8F0;font-family:'Barlow Condensed';font-size:10px;letter-spacing:2px;text-transform:uppercase;padding:4px 10px;border-radius:2px", "Shot Detail"),
      tags$span(style="background:#00C8F018;border:1px solid #00C8F033;color:#00C8F0;font-family:'Barlow Condensed';font-size:10px;letter-spacing:2px;text-transform:uppercase;padding:4px 10px;border-radius:2px", "3-Season")
    )
  ),

  fluidRow(
    # ── SIDEBAR ────────────────────────────────────────────────────────────────
    column(2,
      div(class = "sidebar-panel",
        div(class = "section-title", "Scout Filters"),
        selectInput("sel_season", "Season",  choices = c("All", seasons), selected = "2025-26"),
        selectInput("sel_tier",   "3yr Tier", choices = c("All","Elite","Strong","Average","Weak"), selected = "All"),
        selectInput("sel_trend",  "Trend",    choices = c("All","Improving","Flat","Declining"), selected = "All"),
        sliderInput("sel_gp", "Min Games Played", 1, 60, 20, step = 1),

        div(class = "section-title", "Shot Detail"),
        selectInput("shot_goalie",  "Goalie",      choices = sort(unique(shot$goalie_name)), selected = "Ilya Sorokin"),
        selectInput("shot_compare", "Compare To",  choices = c("None", sort(unique(shot$goalie_name))), selected = "None")
      )
    ),

    # ── MAIN ───────────────────────────────────────────────────────────────────
    column(10,
      div(class = "main-panel",

        # KPI row
        uiOutput("kpi_row"),

        tabsetPanel(
          # ── Scout Board ────────────────────────────────────────────────────
          tabPanel("Scout Board",
            br(),
            div(class = "plot-card",
              div(class = "plot-title", "3-Year Scouting Summary"),
              DTOutput("tbl_scout") %>% withSpinner(color="#00C8F0", type=6)
            )
          ),

          # ── Projections ────────────────────────────────────────────────────
          tabPanel("Next-10 Projections",
            br(),
            fluidRow(
              column(8,
                div(class = "plot-card",
                  div(class = "plot-title", "Projected GSAx — Next 10 Games"),
                  plotlyOutput("plt_proj", height = "440px") %>% withSpinner(color="#00C8F0", type=6)
                )
              ),
              column(4,
                div(class = "plot-card",
                  div(class = "plot-title", "Form vs 3-Year Baseline"),
                  plotlyOutput("plt_form", height = "440px") %>% withSpinner(color="#00C8F0", type=6)
                )
              )
            )
          ),

          # ── Shot Detail ────────────────────────────────────────────────────
          tabPanel("Shot Detail",
            br(),
            fluidRow(
              column(6,
                div(class = "plot-card",
                  div(class = "plot-title", "Save % by Shot Distance"),
                  uiOutput("hm_dist")
                ),
                div(class = "plot-card",
                  div(class = "plot-title", "Save % by Shot Angle"),
                  uiOutput("hm_angle")
                ),
                fluidRow(
                  column(6,
                    div(class = "plot-card",
                      div(class = "plot-title", "Side"),
                      uiOutput("hm_side")
                    )
                  ),
                  column(6,
                    div(class = "plot-card",
                      div(class = "plot-title", "Game State"),
                      uiOutput("hm_gamestate")
                    )
                  )
                ),
                div(class = "plot-card",
                  div(class = "plot-title", "Period & Play Type"),
                  uiOutput("hm_period")
                )
              ),
              column(6,
                div(class = "plot-card",
                  div(class = "plot-title", "Overall Sv% Leaderboard — 20+ GP"),
                  DTOutput("tbl_leaderboard") %>% withSpinner(color="#00C8F0", type=6)
                ),
                div(class = "plot-card",
                  div(class = "plot-title", "Shot-Detail Predictor Importance (Ridge β)"),
                  plotlyOutput("plt_pred_imp", height = "380px") %>% withSpinner(color="#00C8F0", type=6)
                )
              )
            )
          ),

          # ── Goalie Spotlight ───────────────────────────────────────────────
          tabPanel("Goalie Spotlight",
            br(),
            fluidRow(
              column(6,
                div(class = "plot-card",
                  div(class = "plot-title", "Season-by-Season GSAx"),
                  plotlyOutput("plt_gsax_trend", height = "300px") %>% withSpinner(color="#00C8F0", type=6)
                )
              ),
              column(6,
                div(class = "plot-card",
                  div(class = "plot-title", "Year-over-Year Model Rank"),
                  plotlyOutput("plt_rank_trend", height = "300px") %>% withSpinner(color="#00C8F0", type=6)
                )
              )
            )
          ),

          # ── Model Drivers ──────────────────────────────────────────────────
          tabPanel("Model Drivers",
            br(),
            fluidRow(
              column(6,
                div(class = "plot-card",
                  div(class = "plot-title", "Global Feature Importance — Current Season Ridge"),
                  plotlyOutput("plt_drivers_global", height = "500px") %>% withSpinner(color="#00C8F0", type=6)
                )
              ),
              column(6,
                div(class = "plot-card",
                  div(class = "plot-title", "Cohort Feature Contributions — Top 10 by GSAx"),
                  plotlyOutput("plt_drivers_cohort", height = "500px") %>% withSpinner(color="#00C8F0", type=6)
                )
              )
            )
          ),

          # ── Raw Data ───────────────────────────────────────────────────────
          tabPanel("Raw Data",
            br(),
            div(class = "plot-card",
              div(class = "plot-title", "All Raw Data"),
              DTOutput("tbl_raw") %>% withSpinner(color="#00C8F0", type=6)
            )
          )
        )
      )
    )
  )
)

# ── SERVER ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Reactives
  scout_filt <- reactive({
    d <- scout
    if (input$sel_tier  != "All") d <- d %>% filter(`Tier Over 3 Yrs`           == input$sel_tier)
    if (input$sel_trend != "All") d <- d %>% filter(`Moving Trend in Performance` == input$sel_trend)
    d
  })

  raw_filt <- reactive({
    d <- raw %>% filter(Games_Played >= input$sel_gp)
    if (input$sel_season != "All") d <- d %>% filter(Season == input$sel_season)
    d
  })

  shot_sel <- reactive({ shot %>% filter(goalie_name == input$shot_goalie) %>% slice(1) })
  shot_cmp <- reactive({
    if (input$shot_compare == "None") return(NULL)
    shot %>% filter(goalie_name == input$shot_compare) %>% slice(1)
  })

  # ── KPI ─────────────────────────────────────────────────────────────────────
  output$kpi_row <- renderUI({
    d      <- raw_filt()
    n_g    <- n_distinct(d$goalie)
    avg_sv <- round(mean(d$`Save%`, na.rm=TRUE), 4)
    avg_gx <- round(mean(d$GSAx,   na.rm=TRUE), 2)
    n_el   <- scout_filt() %>% filter(`Tier Over 3 Yrs` == "Elite") %>% nrow()
    n_risk <- scout %>% filter(Regression_Risk_Flag == "Yes") %>% nrow()
    top_sv <- shot %>% slice(1)

    div(class="kpi-row",
      div(class="kpi-card",
        div(class="kpi-label","Goalies Tracked"),
        div(class="kpi-value",style="color:#00C8F0",n_g),
        div(class="kpi-sub",paste("Season:",if(input$sel_season=="All")"All" else input$sel_season))
      ),
      div(class="kpi-card",
        div(class="kpi-label","Avg Save %"),
        div(class="kpi-value",sprintf("%.3f",avg_sv)),
        div(class="kpi-sub","Filtered cohort")
      ),
      div(class="kpi-card",
        div(class="kpi-label","Avg GSAx"),
        div(class="kpi-value",style="color:#F0C040",sprintf("%+.1f",avg_gx)),
        div(class="kpi-sub","Goals saved above expected")
      ),
      div(class="kpi-card",
        div(class="kpi-label","Elite Tier (3yr)"),
        div(class="kpi-value",style="color:#00C8F0",n_el),
        div(class="kpi-sub","Top-tier goalies")
      ),
      div(class="kpi-card",
        div(class="kpi-label","Regression Flags"),
        div(class="kpi-value",style="color:#F05060",n_risk),
        div(class="kpi-sub","Active this season")
      ),
      div(class="kpi-card",
        div(class="kpi-label","Top Sv% (20+ GP)"),
        div(class="kpi-value",style="color:#9B6BF0",sprintf("%.4f",top_sv$sv_pct_overall)),
        div(class="kpi-sub",top_sv$goalie_name)
      )
    )
  })

  # ── Scout table ──────────────────────────────────────────────────────────────
  output$tbl_scout <- renderDT({
    d <- scout_filt() %>%
      select(Goalie,`3yr Model Rank`,`2025-26 Model Rank`,`2024-25 Model Rank`,
             `Tier Over 3 Yrs`,`Current Season Tier Standing`,
             `Moving Trend in Performance`,`Directional_Outlook`,
             `Regression_Risk_Flag`,`Rebound_Risk_Flag`,`Volatility Over 3 Yrs`)
    datatable(d, rownames=FALSE, escape=FALSE,
              options=list(pageLength=15,scrollX=TRUE,dom="ftip",
                           columnDefs=list(list(className="dt-center",targets="_all")))) %>%
      formatStyle("Tier Over 3 Yrs",
                  color=styleEqual(c("Elite","Strong","Average","Weak"),c("#00C8F0","#52E8A0","#F0C040","#F05060")),
                  fontWeight="bold") %>%
      formatStyle("Moving Trend in Performance",
                  color=styleEqual(c("Improving","Flat","Declining"),c("#52E8A0","#F0C040","#F05060"))) %>%
      formatStyle("Regression_Risk_Flag",
                  color=styleEqual(c("Yes","No"),c("#F05060","#1A3050")),fontWeight=styleEqual("Yes","bold")) %>%
      formatStyle("Rebound_Risk_Flag",
                  color=styleEqual(c("Yes","No"),c("#F0C040","#1A3050")),fontWeight=styleEqual("Yes","bold")) %>%
      formatStyle("3yr Model Rank",
                  background=styleColorBar(range(d$`3yr Model Rank`),"#00C8F033"),
                  backgroundSize="98% 70%",backgroundRepeat="no-repeat",backgroundPosition="center")
  })

  # ── Projection bar ───────────────────────────────────────────────────────────
  output$plt_proj <- renderPlotly({
    d <- proj %>% arrange(desc(`Proj GSAx (Next 10)`))
    d$Goalie <- factor(d$Goalie, levels=d$Goalie)
    col_map  <- dir_cols[d$`Direction (5)`]; col_map[is.na(col_map)] <- "#F0C040"

    plot_ly(d, y=~Goalie, x=~`Proj GSAx (Next 10)`, type="bar", orientation="h",
            marker=list(color=col_map, line=list(color="#00000000")),
            error_x=list(type="data",
                         array=~(band_hi-`Proj GSAx (Next 10)`),
                         arrayminus=~(`Proj GSAx (Next 10)`-band_lo),
                         color="#FFFFFF44",thickness=1.5,width=4),
            text=~sprintf("%s<br>Proj: <b>%+.2f</b><br>Band: %s<br>Dir: %s",
                          Goalie,`Proj GSAx (Next 10)`,`Band (10g)`,`Direction (5)`),
            hoverinfo="text") %>%
      layout(paper_bgcolor="#0A1220",plot_bgcolor="#0A1220",
             font=list(family="Barlow",color="#C5D5E8"),
             xaxis=list(title="Projected GSAx",color="#566F8A",gridcolor="#1A3050",
                        zeroline=TRUE,zerolinecolor="#FFFFFF44"),
             yaxis=list(title="",color="#C5D5E8",tickfont=list(size=11)),
             margin=list(l=10,r=20,t=10,b=40),showlegend=FALSE)
  })

  # ── Form vs baseline scatter ─────────────────────────────────────────────────
  output$plt_form <- renderPlotly({
    d <- proj
    col_map <- tier_cols[d$`Tier Over 3 Yrs`]; col_map[is.na(col_map)] <- "#888888"
    plot_ly(d, x=~`Baseline GSAx/GP (3yr)`, y=~`Current Form GSAx/GP`,
            type="scatter", mode="markers+text", text=~Goalie,
            textposition="top right", textfont=list(size=9,color="#8AA3BF"),
            marker=list(size=10,color=col_map,line=list(color="#FFFFFF44",width=1)),
            hovertemplate=~paste0("<b>",Goalie,"</b><br>Baseline: ",
                                  round(`Baseline GSAx/GP (3yr)`,3),"<br>Form: ",
                                  round(`Current Form GSAx/GP`,3),"<extra></extra>")) %>%
      add_lines(x=c(-.5,.9),y=c(-.5,.9),inherit=FALSE,
                line=list(color="#FFFFFF22",dash="dash",width=1),
                hoverinfo="none",showlegend=FALSE) %>%
      layout(paper_bgcolor="#0A1220",plot_bgcolor="#0A1220",
             font=list(family="Barlow",color="#C5D5E8"),
             xaxis=list(title="3yr Baseline GSAx/GP",color="#566F8A",gridcolor="#1A3050"),
             yaxis=list(title="Current Form GSAx/GP",color="#566F8A",gridcolor="#1A3050"),
             margin=list(l=10,r=10,t=10,b=10),showlegend=FALSE)
  })

  # ── Shot Detail heatmaps ─────────────────────────────────────────────────────
  hm_reactive <- function(labels, keys) {
    reactive({
      g   <- shot_sel()
      cmp <- shot_cmp()
      vals     <- sapply(keys, function(k) { v <- g[[k]]; if(length(v)==0 || is.null(v)) NA else as.numeric(v) })
      cmp_vals <- if (!is.null(cmp)) sapply(keys, function(k) { v <- cmp[[k]]; if(length(v)==0||is.null(v)) NA else as.numeric(v) }) else NULL
      HTML(hm_html(labels, vals, cmp_vals))
    })
  }

  output$hm_dist     <- renderUI(hm_reactive(
    c("0-10ft","10-20ft","20-30ft","30-40ft","40ft+"),
    c("sv_pct_dist_0_10ft","sv_pct_dist_10_20ft","sv_pct_dist_20_30ft","sv_pct_dist_30_40ft","sv_pct_dist_40ft_plus"))())

  output$hm_angle    <- renderUI(hm_reactive(
    c("0-15°","15-30°","30-45°","45-60°","60-90°"),
    c("sv_pct_angle_0_15deg","sv_pct_angle_15_30deg","sv_pct_angle_30_45deg","sv_pct_angle_45_60deg","sv_pct_angle_60_90deg"))())

  output$hm_side     <- renderUI(hm_reactive(
    c("Left","Right","Center"),
    c("sv_pct_side_left","sv_pct_side_right","sv_pct_side_center"))())

  output$hm_gamestate <- renderUI(hm_reactive(
    c("Leading","Tied","Trailing"),
    c("sv_pct_leading","sv_pct_tied","sv_pct_trailing"))())

  output$hm_period   <- renderUI(hm_reactive(
    c("Period 1","Period 2","Period 3","High Speed","Low Speed","Post-FO","Mid Play","Long Play"),
    c("sv_pct_period_1","sv_pct_period_2","sv_pct_period_3",
      "sv_pct_high_speed","sv_pct_low_speed",
      "sv_pct_post_faceoff","sv_pct_mid_play","sv_pct_long_play"))())

  # ── Shot leaderboard ────────────────────────────────────────────────────────
  output$tbl_leaderboard <- renderDT({
    d <- shot %>%
      select(goalie_name, games_played, shots_total, sv_pct_overall,
             sv_pct_even_strength, sv_pct_power_kill, sv_pct_rebound, sv_pct_high_speed) %>%
      mutate(across(where(is.numeric), ~round(., 4))) %>%
      rename(Goalie=goalie_name, GP=games_played, Shots=shots_total,
             `Overall Sv%`=sv_pct_overall, `ES Sv%`=sv_pct_even_strength,
             `PK Sv%`=sv_pct_power_kill, `Rebound Sv%`=sv_pct_rebound,
             `High Speed Sv%`=sv_pct_high_speed)

    datatable(d, rownames=FALSE, escape=FALSE,
              options=list(pageLength=12,scrollX=TRUE,dom="ftip")) %>%
      formatStyle("Overall Sv%",
                  background=styleColorBar(c(.85,.93),"#00C8F033"),
                  backgroundSize="98% 70%",backgroundRepeat="no-repeat",backgroundPosition="center") %>%
      formatStyle("ES Sv%",
                  color=styleInterval(c(.905,.915),c("#F05060","#F0C040","#52E8A0"))) %>%
      formatStyle("Rebound Sv%",
                  color=styleInterval(c(.80,.87),c("#F05060","#F0C040","#52E8A0")))
  })

  # ── Predictor importance ─────────────────────────────────────────────────────
  output$plt_pred_imp <- renderPlotly({
    d <- pred_imp %>%
      arrange(beta) %>%
      mutate(feature_display=factor(raw_feature, levels=raw_feature),
             col=ifelse(beta>=0,"#00C8F0","#F05060"),
             label=gsub("sv_pct_","",raw_feature) %>% gsub("_"," ",.))

    plot_ly(d, y=~label, x=~beta, type="bar", orientation="h",
            marker=list(color=~col,line=list(color="#00000000")),
            hovertemplate=~paste0("<b>",label,"</b><br>β = ",round(beta,4),"<extra></extra>")) %>%
      layout(paper_bgcolor="#0A1220",plot_bgcolor="#0A1220",
             font=list(family="Barlow",color="#C5D5E8",size=10),
             xaxis=list(title="Ridge β",color="#566F8A",gridcolor="#1A3050",
                        zeroline=TRUE,zerolinecolor="#FFFFFF33"),
             yaxis=list(title="",color="#C5D5E8",tickfont=list(size=9)),
             margin=list(l=10,r=10,t=10,b=30),showlegend=FALSE)
  })

  # ── Goalie Spotlight ─────────────────────────────────────────────────────────
  output$plt_gsax_trend <- renderPlotly({
    g <- input$shot_goalie
    d <- raw %>% filter(goalie == g) %>% arrange(Season)
    plot_ly(d, x=~Season, y=~GSAx, type="scatter", mode="lines+markers",
            line=list(color="#00C8F0",width=2.5),
            marker=list(size=8,color="#00C8F0",line=list(color="#fff",width=1.5)),
            fill="tozeroy", fillcolor="#00C8F015",
            hovertemplate=~paste0("<b>",Season,"</b><br>GSAx: <b>",round(GSAx,2),
                                  "</b><br>GP: ",Games_Played,"<extra></extra>")) %>%
      layout(paper_bgcolor="#0A1220",plot_bgcolor="#0A1220",
             font=list(family="Barlow",color="#C5D5E8"),
             xaxis=list(title="",color="#566F8A",gridcolor="#1A3050"),
             yaxis=list(title="GSAx",color="#566F8A",gridcolor="#1A3050",
                        zeroline=TRUE,zerolinecolor="#FFFFFF33"),
             margin=list(l=10,r=10,t=10,b=10))
  })

  output$plt_rank_trend <- renderPlotly({
    g  <- input$shot_goalie
    ds <- scout %>% filter(Goalie == g)
    if (nrow(ds) == 0) return(NULL)
    rd <- data.frame(
      Season = c("2023-24","2024-25","2025-26","3yr Overall"),
      Rank   = c(ds$`2023-24 Model Rank`,ds$`2024-25 Model Rank`,ds$`2025-26 Model Rank`,ds$`3yr Model Rank`)
    )
    plot_ly(rd, x=~Season, y=~Rank, type="scatter", mode="lines+markers",
            line=list(color="#52E8A0",width=2.5),
            marker=list(size=10,color="#52E8A0",line=list(color="#fff",width=1.5)),
            text=~paste0("Rank: #",Rank), hoverinfo="text+x") %>%
      layout(paper_bgcolor="#0A1220",plot_bgcolor="#0A1220",
             font=list(family="Barlow",color="#C5D5E8"),
             xaxis=list(title="",color="#566F8A",gridcolor="#1A3050"),
             yaxis=list(title="Model Rank",color="#566F8A",gridcolor="#1A3050",autorange="reversed"),
             margin=list(l=10,r=10,t=10,b=10))
  })

  # ── Model Drivers ────────────────────────────────────────────────────────────
  drv_chart <- function(data, model_name) {
    d <- data %>%
      filter(model == model_name) %>%
      arrange(beta) %>%
      mutate(feature_display = factor(feature_display, levels=feature_display),
             col = ifelse(beta >= 0, "#00C8F0", "#F05060"))
    plot_ly(d, y=~feature_display, x=~beta, type="bar", orientation="h",
            marker=list(color=~col,line=list(color="#00000000")),
            hovertemplate=~paste0("<b>",feature_display,"</b><br>β = ",round(beta,4),"<extra></extra>")) %>%
      layout(paper_bgcolor="#0A1220",plot_bgcolor="#0A1220",
             font=list(family="Barlow",color="#C5D5E8",size=11),
             xaxis=list(title="Ridge Coefficient (β)",color="#566F8A",gridcolor="#1A3050",
                        zeroline=TRUE,zerolinecolor="#FFFFFF33"),
             yaxis=list(title="",color="#C5D5E8",tickfont=list(size=10)),
             margin=list(l=10,r=10,t=10,b=30),showlegend=FALSE)
  }

  output$plt_drivers_global <- renderPlotly({ drv_chart(drv_glob, "Current Season Ridge (2025-26)") })
  output$plt_drivers_cohort <- renderPlotly({
    d <- drv_coh %>%
      filter(model == "Current Season Ridge (2025-26)") %>%
      arrange(avg_contrib) %>%
      mutate(feature_display=factor(feature_display,levels=feature_display),
             col=ifelse(avg_contrib>=0,"#52E8A0","#F08040"))
    plot_ly(d, y=~feature_display, x=~avg_contrib, type="bar", orientation="h",
            marker=list(color=~col,line=list(color="#00000000")),
            hovertemplate=~paste0("<b>",feature_display,"</b><br>Avg Contrib: ",
                                  round(avg_contrib,4),"<extra></extra>")) %>%
      layout(paper_bgcolor="#0A1220",plot_bgcolor="#0A1220",
             font=list(family="Barlow",color="#C5D5E8",size=11),
             xaxis=list(title="Avg Contribution",color="#566F8A",gridcolor="#1A3050",
                        zeroline=TRUE,zerolinecolor="#FFFFFF33"),
             yaxis=list(title="",color="#C5D5E8",tickfont=list(size=10)),
             margin=list(l=10,r=10,t=10,b=30),showlegend=FALSE)
  })

  # ── Raw Data ─────────────────────────────────────────────────────────────────
  output$tbl_raw <- renderDT({
    d <- raw_filt() %>%
      select(Season,goalie,team,Games_Played,`Save%`,`5on5 Sv%`,`4on5 Sv%`,
             GSAx,`GSAx 5on5`,`GSAx 4on5`,`GSAx HD`,`Total HD Sv%`,
             `Total Rebound%`,pred_gsax_ridge) %>%
      mutate(across(where(is.numeric), ~round(.,4)))
    datatable(d, rownames=FALSE,
              options=list(pageLength=20,scrollX=TRUE,dom="ftip")) %>%
      formatStyle("GSAx", color=styleInterval(0, c("#F05060","#52E8A0"))) %>%
      formatStyle("Save%",
                  background=styleColorBar(c(.88,.93),"#00C8F033"),
                  backgroundSize="98% 70%",backgroundRepeat="no-repeat",backgroundPosition="center")
  })
}

shinyApp(ui, server)
