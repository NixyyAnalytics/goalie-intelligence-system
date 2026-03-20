# ============================================================
# HOCKEY INTELLIGENCE SYSTEM — COMPLETE PIPELINE v3
#   Lines & Pairings + Game-by-Game + Shot Detail
#   - Ridge models: Lines, Pairings (xG% target, 3yr + current)
#   - Game-by-game GSAx with rolling form windows
#   - Shot-level detail from raw shots CSV
#   - Contextual goalie adjustment layer
#   - Exports master Excel workbook
# ============================================================

rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(tidyr)
  library(tibble)
  library(glmnet)
  library(lubridate)
  library(openxlsx)
  library(purrr)
  library(stringr)
})

SEED   <- 123
MIN_GP <- 15
set.seed(SEED)

# ============================================================
# 0) PATHS — EDIT THESE ONLY
# ============================================================

FILE_LINES_2526    <- "D:/Personal Data Projects/CompleteGoalieCode/lines/lines20252026.csv"
FILE_LINES_2425    <- "D:/Personal Data Projects/CompleteGoalieCode/lines/lines20242025.csv"
FILE_LINES_2324    <- "D:/Personal Data Projects/CompleteGoalieCode/lines/lines20232024.csv"
FILE_GBG_2526      <- "D:/Personal Data Projects/GamebyGame20252026/GamebyGame2025.csv"
FILE_SHOTS_2526    <- "D:/Personal Data Projects/ShotData/shots_2025.csv"



GOALIE_MASTER_XLSX <- "D:/Personal Data Projects/CompleteGoalieCode/Goalie_Master_Model_Output.xlsx"
OUT_DIR            <- "D:/Personal Data Projects/CompleteGoalieCode"
LINES_XLSX_OUT     <- file.path(OUT_DIR, "Lines_Master_Model_Output_v3.xlsx")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 1) HELPERS
# ============================================================

safe_div <- function(num, den) {
  ifelse(is.finite(den) & den > 0, num / den, NA_real_)
}

tier_from_rank <- function(rank_vec) {
  n <- length(rank_vec)
  if (n <= 1) return(rep("Average", n))
  pct <- (rank_vec - 1) / (n - 1)
  dplyr::case_when(
    pct <= 0.10 ~ "Elite",
    pct <= 0.30 ~ "Strong",
    pct <= 0.70 ~ "Average",
    TRUE        ~ "Weak"
  )
}

rolling_mean <- function(x, n) {
  if (length(x) < 1) return(rep(NA_real_, length(x)))
  result <- numeric(length(x))
  for (i in seq_along(x)) {
    window <- x[max(1, i - n + 1):i]
    result[i] <- mean(window, na.rm = TRUE)
  }
  result
}

# ============================================================
# 2) LOAD LINES DATA
# ============================================================

load_lines <- function(path, season_label) {
  message("Loading: ", path)
  df <- read_csv(path, show_col_types = FALSE) %>%
    mutate(season_label = season_label) %>%
    filter(situation == "5on5", games_played >= MIN_GP) %>%
    mutate(across(where(is.character), trimws))
  message("  -> ", nrow(df), " rows (", season_label, ")")
  df
}

raw_2526 <- load_lines(FILE_LINES_2526, "2025-26")
raw_2425 <- load_lines(FILE_LINES_2425, "2024-25")
raw_2324 <- load_lines(FILE_LINES_2324, "2023-24")

lines_2526 <- raw_2526 %>% filter(position == "line")
lines_2425 <- raw_2425 %>% filter(position == "line")
lines_2324 <- raw_2324 %>% filter(position == "line")
pairs_2526 <- raw_2526 %>% filter(position == "pairing")
pairs_2425 <- raw_2425 %>% filter(position == "pairing")
pairs_2324 <- raw_2324 %>% filter(position == "pairing")

# ============================================================
# 3) FEATURE ENGINEERING (lines/pairs)
# ============================================================

engineer_features <- function(df) {
  df %>% mutate(
    icetime_60         = icetime / 60,
    xGF_per60          = safe_div(xGoalsFor,                            icetime_60) * 60,
    xGA_per60          = safe_div(xGoalsAgainst,                        icetime_60) * 60,
    GF_per60           = safe_div(goalsFor,                             icetime_60) * 60,
    GA_per60           = safe_div(goalsAgainst,                         icetime_60) * 60,
    HD_xGF_per60       = safe_div(highDangerxGoalsFor,                  icetime_60) * 60,
    HD_xGA_per60       = safe_div(highDangerxGoalsAgainst,              icetime_60) * 60,
    MD_xGF_per60       = safe_div(mediumDangerxGoalsFor,                icetime_60) * 60,
    LD_xGF_per60       = safe_div(lowDangerxGoalsFor,                   icetime_60) * 60,
    shots_for_per60    = safe_div(shotsOnGoalFor,                       icetime_60) * 60,
    shots_ag_per60     = safe_div(shotsOnGoalAgainst,                   icetime_60) * 60,
    corsi_for_per60    = safe_div(shotAttemptsFor,                      icetime_60) * 60,
    corsi_ag_per60     = safe_div(shotAttemptsAgainst,                  icetime_60) * 60,
    takeaways_per60    = safe_div(takeawaysFor,                         icetime_60) * 60,
    giveaways_per60    = safe_div(giveawaysFor,                         icetime_60) * 60,
    hits_per60         = safe_div(hitsFor,                              icetime_60) * 60,
    penalties_per60    = safe_div(penaltiesFor,                         icetime_60) * 60,
    dzGiveaways_per60  = safe_div(dZoneGiveawaysFor,                    icetime_60) * 60,
    rebounds_for_per60 = safe_div(reboundsFor,                          icetime_60) * 60,
    rebounds_ag_per60  = safe_div(reboundsAgainst,                      icetime_60) * 60,
    faceoffs_per60     = safe_div(faceOffsWonFor,                       icetime_60) * 60,
    flurry_xGF_per60   = safe_div(flurryScoreVenueAdjustedxGoalsFor,    icetime_60) * 60,
    flurry_xGA_per60   = safe_div(flurryScoreVenueAdjustedxGoalsAgainst,icetime_60) * 60,
    HD_shot_share_for  = safe_div(highDangerShotsFor,
                                  highDangerShotsFor + mediumDangerShotsFor + lowDangerShotsFor),
    HD_shot_share_ag   = safe_div(highDangerShotsAgainst,
                                  highDangerShotsAgainst + mediumDangerShotsAgainst + lowDangerShotsAgainst),
    xG_net_per60       = xGF_per60 - xGA_per60,
    HD_xG_net_per60    = HD_xGF_per60 - HD_xGA_per60,
    takeaway_ratio     = safe_div(takeawaysFor, takeawaysFor + giveawaysFor),
    rebound_ratio      = safe_div(reboundsFor,  reboundsFor  + reboundsAgainst),
    finishing_delta    = safe_div(goalsFor    - xGoalsFor,    games_played),
    defensive_delta    = safe_div(xGoalsAgainst - goalsAgainst, games_played),
    score_adj_corsi_pct= safe_div(scoreAdjustedShotsAttemptsFor,
                                  scoreAdjustedShotsAttemptsFor + scoreAdjustedShotsAttemptsAgainst)
  )
}

lines_2526 <- engineer_features(lines_2526)
lines_2425 <- engineer_features(lines_2425)
lines_2324 <- engineer_features(lines_2324)
pairs_2526 <- engineer_features(pairs_2526)
pairs_2425 <- engineer_features(pairs_2425)
pairs_2324 <- engineer_features(pairs_2324)

# ============================================================
# 4) PREDICTOR WHITELIST
# ============================================================

LINE_PREDICTORS <- c(
  "xGF_per60","xGA_per60","HD_xGF_per60","HD_xGA_per60","MD_xGF_per60","LD_xGF_per60",
  "shots_for_per60","shots_ag_per60","corsi_for_per60","corsi_ag_per60",
  "flurry_xGF_per60","flurry_xGA_per60","HD_shot_share_for","HD_shot_share_ag",
  "xG_net_per60","HD_xG_net_per60","takeaways_per60","giveaways_per60",
  "takeaway_ratio","dzGiveaways_per60","hits_per60","penalties_per60",
  "rebounds_for_per60","rebounds_ag_per60","rebound_ratio","faceoffs_per60",
  "finishing_delta","defensive_delta","score_adj_corsi_pct"
)
PAIR_PREDICTORS <- LINE_PREDICTORS

# ============================================================
# 5) RIDGE RUNNER
# ============================================================

run_ridge <- function(dat, predictors, target = "xGoalsPercentage",
                      group_label = "Lines", season_label = "2025-26") {
  message("\nRIDGE: ", group_label, " | ", season_label)
  predictors <- intersect(predictors, names(dat))
  predictors <- predictors[sapply(dat[predictors], function(x) {
    x <- x[is.finite(x)]; length(x) >= 2 && sd(x, na.rm = TRUE) > 0
  })]
  dat_clean <- dat %>%
    select(name, team, season_label, games_played, icetime,
           all_of(target), all_of(predictors)) %>%
    filter(complete.cases(.))
  z_map <- tibble(raw = character(), z = character())
  for (v in predictors) {
    zn <- paste0("z_", make.names(v))
    dat_clean[[zn]] <- as.numeric(scale(dat_clean[[v]]))
    z_map <- bind_rows(z_map, tibble(raw = v, z = zn))
  }
  skill_terms <- z_map$z
  Xmm <- model.matrix(~ ., data = dat_clean[, skill_terms, drop = FALSE])
  if ("(Intercept)" %in% colnames(Xmm)) Xmm <- Xmm[, colnames(Xmm) != "(Intercept)", drop = FALSE]
  qr_r <- qr(Xmm)$rank
  if (qr_r < ncol(Xmm)) {
    keep <- qr(Xmm)$pivot[seq_len(qr_r)]
    skill_terms <- colnames(Xmm)[keep]
    z_map <- z_map %>% filter(z %in% skill_terms)
  }
  X <- as.matrix(dat_clean[, skill_terms, drop = FALSE])
  y <- dat_clean[[target]]
  set.seed(SEED)
  cv  <- cv.glmnet(X, y, alpha = 0, nfolds = 10, standardize = FALSE)
  fit <- glmnet(X, y, alpha = 0, lambda = cv$lambda.min, standardize = FALSE)
  dat_clean$pred_xgp_ridge <- as.numeric(predict(fit, newx = X))
  message("  lambda=", round(cv$lambda.min,5), " n=", nrow(dat_clean),
          " R2=", round(cor(y, dat_clean$pred_xgp_ridge)^2, 3))
  invisible(list(season_label=season_label, group_label=group_label,
                 dat=dat_clean, fit=fit, cv=cv,
                 skill_terms=skill_terms, z_map=z_map, target=target))
}

# ============================================================
# 6) RUN SEASON MODELS
# ============================================================

res_lines_2526 <- run_ridge(lines_2526, LINE_PREDICTORS, group_label="Lines",    season_label="2025-26")
res_lines_2425 <- run_ridge(lines_2425, LINE_PREDICTORS, group_label="Lines",    season_label="2024-25")
res_lines_2324 <- run_ridge(lines_2324, LINE_PREDICTORS, group_label="Lines",    season_label="2023-24")
res_pairs_2526 <- run_ridge(pairs_2526, PAIR_PREDICTORS, group_label="Pairings", season_label="2025-26")
res_pairs_2425 <- run_ridge(pairs_2425, PAIR_PREDICTORS, group_label="Pairings", season_label="2024-25")
res_pairs_2324 <- run_ridge(pairs_2324, PAIR_PREDICTORS, group_label="Pairings", season_label="2023-24")

# ============================================================
# 7) 3-YEAR STRUCTURAL RIDGE
# ============================================================

run_structural_3yr <- function(res_a, res_b, res_c, predictors, group_label) {
  all3 <- bind_rows(
    res_a$dat %>% mutate(s = res_a$season_label),
    res_b$dat %>% mutate(s = res_b$season_label),
    res_c$dat %>% mutate(s = res_c$season_label)
  )
  combos_3yr <- all3 %>% distinct(name, s) %>% count(name) %>% filter(n == 3) %>% pull(name)
  message("3yr combos [", group_label, "]: ", length(combos_3yr))
  dat_3yr <- all3 %>% filter(name %in% combos_3yr)
  preds   <- intersect(predictors, names(dat_3yr))
  dat_avg <- dat_3yr %>%
    group_by(name, team) %>%
    summarise(xGoalsPercentage_3yr = mean(xGoalsPercentage, na.rm=TRUE),
              games_played_total   = sum(games_played, na.rm=TRUE),
              across(all_of(preds), ~mean(.x, na.rm=TRUE)), .groups="drop")
  X_raw <- dat_avg %>% select(all_of(preds)) %>% as.matrix()
  X     <- scale(X_raw); colnames(X) <- colnames(X_raw)
  y     <- dat_avg$xGoalsPercentage_3yr
  set.seed(SEED)
  cv  <- cv.glmnet(X, y, alpha=0, standardize=FALSE)
  fit <- glmnet(X, y, alpha=0, lambda=cv$lambda.min, standardize=FALSE)
  dat_avg$pred_3yr_xgp <- as.numeric(predict(fit, newx=X))
  message("  R2=", round(cor(y, dat_avg$pred_3yr_xgp)^2, 3))
  list(dat_avg=dat_avg, fit=fit, cv=cv, X=X, names_vec=dat_avg$name, predictors=preds)
}

struct_lines <- run_structural_3yr(res_lines_2324, res_lines_2425, res_lines_2526, LINE_PREDICTORS, "Lines")
struct_pairs <- run_structural_3yr(res_pairs_2324, res_pairs_2425, res_pairs_2526, PAIR_PREDICTORS, "Pairings")

# ============================================================
# 8) SCOUT SUMMARY TABLE
# ============================================================

build_scout_tbl <- function(struct_res, res_cur, res_prev, res_prev2,
                              id_col="name", group_label="Lines") {
  rank_cur   <- res_cur$dat   %>% transmute(!!id_col:=!!sym(id_col), cur_rank  =rank(-pred_xgp_ridge, ties.method="first"))
  rank_prev  <- res_prev$dat  %>% transmute(!!id_col:=!!sym(id_col), prev_rank =rank(-pred_xgp_ridge, ties.method="first"))
  rank_prev2 <- res_prev2$dat %>% transmute(!!id_col:=!!sym(id_col), prev2_rank=rank(-pred_xgp_ridge, ties.method="first"))
  rank_3yr   <- struct_res$dat_avg %>%
    transmute(!!id_col:=!!sym(id_col), rank_3yr=rank(-pred_3yr_xgp, ties.method="first"), team) %>%
    mutate(`Tier Over 3 Yrs`=tier_from_rank(rank_3yr))
  tbl <- rank_3yr %>%
    left_join(rank_cur, by=id_col) %>% left_join(rank_prev, by=id_col) %>% left_join(rank_prev2, by=id_col) %>%
    rowwise() %>%
    mutate(rank_swing=max(c_across(c(cur_rank,prev_rank,prev2_rank)),na.rm=TRUE)-
                      min(c_across(c(cur_rank,prev_rank,prev2_rank)),na.rm=TRUE),
           Volatility=case_when(rank_swing>=30~"High",rank_swing>=15~"Medium",TRUE~"Low")) %>%
    ungroup() %>%
    mutate(
      `Current Tier`=case_when((cur_rank-1)/(max(cur_rank,na.rm=TRUE)-1)<=.10~"Elite",
                               (cur_rank-1)/(max(cur_rank,na.rm=TRUE)-1)<=.30~"Strong",
                               (cur_rank-1)/(max(cur_rank,na.rm=TRUE)-1)<=.70~"Average",TRUE~"Weak"),
      Trend=case_when((cur_rank-prev2_rank)<=-12~"Improving",(cur_rank-prev2_rank)>=12~"Declining",TRUE~"Flat"),
      Form_Delta=cur_rank-rank_3yr,
      `Regression Risk`=case_when(Form_Delta<=-12&Volatility%in%c("Medium","High")~"Yes",TRUE~"No"),
      `Directional Outlook`=case_when(
        Form_Delta<=-12&Volatility=="Low"~"Surging",Form_Delta>=-11&Form_Delta<=-5~"Improving",
        Form_Delta>=-4&Form_Delta<=4~"Stable",Form_Delta>=5&Form_Delta<=11~"Cooling",
        `Regression Risk`=="Yes"~"Regression Watch",Form_Delta<=-12~"Surging",
        Form_Delta>=12~"Cooling",TRUE~"Stable"),
      Group=group_label
    ) %>%
    select(Name=!!id_col, Team=team, Group, `3yr Rank`=rank_3yr,
           `2025-26 Rank`=cur_rank, `2024-25 Rank`=prev_rank, `2023-24 Rank`=prev2_rank,
           Volatility, `Tier Over 3 Yrs`, `Current Tier`, Trend,
           `Form Delta Rank`=Form_Delta, `Directional Outlook`, `Regression Risk`) %>%
    arrange(`3yr Rank`)
  tbl
}

scout_lines    <- build_scout_tbl(struct_lines, res_lines_2526, res_lines_2425, res_lines_2324, group_label="Lines")
scout_pairs    <- build_scout_tbl(struct_pairs, res_pairs_2526, res_pairs_2425, res_pairs_2324, group_label="Pairings")
scout_combined <- bind_rows(scout_lines, scout_pairs) %>% arrange(`3yr Rank`, Group)

# ============================================================
# 9) PROJECTION (65/35 blend — will be superseded by GBG form in dashboard)
# ============================================================

W_BASE <- 0.65; W_FORM <- 0.35

build_projection <- function(struct_res, res_cur, scout_tbl, group_label) {
  baseline <- struct_res$dat_avg %>% transmute(name, baseline_xgp=xGoalsPercentage_3yr)
  form_tbl <- res_cur$dat %>% transmute(name, form_xgp=xGoalsPercentage)
  proj <- scout_tbl %>% rename(name=Name) %>%
    left_join(baseline, by="name") %>% left_join(form_tbl, by="name") %>%
    mutate(delta_form_vs_base=form_xgp-baseline_xgp,
           blended_xgp=W_BASE*baseline_xgp+W_FORM*form_xgp)
  q <- quantile(proj$delta_form_vs_base, probs=c(.2,.4,.6,.8), na.rm=TRUE, names=FALSE)
  proj %>%
    mutate(Direction=case_when(delta_form_vs_base<=q[1]~"Strong Down",delta_form_vs_base<=q[2]~"Down",
                               delta_form_vs_base<=q[3]~"Flat",delta_form_vs_base<=q[4]~"Up",TRUE~"Strong Up"),
           band_width=case_when(Volatility=="Low"~0.02,Volatility=="Medium"~0.04,TRUE~0.06),
           proj_lo=blended_xgp-band_width, proj_hi=blended_xgp+band_width, Group=group_label) %>%
    transmute(Name=name, Team, Group, `Tier Over 3 Yrs`, `Current Tier`, Volatility,
              `Baseline xG%`=round(baseline_xgp,4), `Current Form xG%`=round(form_xgp,4),
              `Delta Form vs Base`=round(delta_form_vs_base,4), Direction,
              `Proj xG% (Blended)`=round(blended_xgp,4),
              Band=paste0("[",round(proj_lo,4),", ",round(proj_hi,4),"]")) %>%
    arrange(desc(`Proj xG% (Blended)`))
}

proj_lines    <- build_projection(struct_lines, res_lines_2526, scout_lines,  "Lines")
proj_pairs    <- build_projection(struct_pairs, res_pairs_2526, scout_pairs,  "Pairings")
proj_combined <- bind_rows(proj_lines, proj_pairs) %>% arrange(desc(`Proj xG% (Blended)`))

# ============================================================
# 10) GAME-BY-GAME GOALIE DATA
# ============================================================

message("\nBuilding game-by-game GSAx...")

gbg_raw <- read_csv(FILE_GBG_2526, show_col_types=FALSE)
"D:/Personal Data Projects/GamebyGame20252026/GamebyGame2025.csv"

gbg_all  <- gbg_raw %>% filter(position=="G", situation=="all")
gbg_5on5 <- gbg_raw %>% filter(position=="G", situation=="5on5")
gbg_4on5 <- gbg_raw %>% filter(position=="G", situation=="4on5")

gbg_joined <- gbg_all %>%
  left_join(gbg_5on5 %>% select(name, gameId, xGoals_5on5=xGoals, goals_5on5=goals,
                                  ongoal_5on5=ongoal),
            by=c("name","gameId")) %>%
  left_join(gbg_4on5 %>% select(name, gameId, xGoals_4on5=xGoals, goals_4on5=goals),
            by=c("name","gameId")) %>%
  mutate(
    gameDate_fmt = ymd(gameDate),
    gsax_total   = xGoals - goals,
    gsax_5on5    = coalesce(xGoals_5on5 - goals_5on5, 0),
    gsax_4on5    = coalesce(xGoals_4on5 - goals_4on5, 0),
    sv_pct       = ifelse(ongoal > 0, (ongoal - goals) / ongoal, NA_real_),
    icetime_min  = icetime / 60
  ) %>%
  arrange(name, gameDate_fmt) %>%
  group_by(name) %>%
  mutate(
    game_num    = row_number(),
    roll5_gsax  = rolling_mean(gsax_total, 5),
    roll10_gsax = rolling_mean(gsax_total, 10),
    cum_gsax    = cumsum(gsax_total)
  ) %>%
  ungroup() %>%
  select(name, playerTeam, gameDate=gameDate_fmt, gameId, opposingTeam,
         home_or_away, game_num, icetime_min,
         xGoals_against=xGoals, goals_against=goals, shots=ongoal,
         gsax_total, gsax_5on5, gsax_4on5, sv_pct,
         roll5_gsax, roll10_gsax, cum_gsax)

message("  GBG rows: ", nrow(gbg_joined))

# Season summary per goalie (for projection tab)
gbg_season_summary <- gbg_joined %>%
  group_by(name, playerTeam) %>%
  summarise(
    gp              = n(),
    total_gsax      = round(sum(gsax_total, na.rm=TRUE), 2),
    avg_gsax_per_gp = round(mean(gsax_total, na.rm=TRUE), 3),
    form_last10_gsax= round(mean(tail(gsax_total, 10), na.rm=TRUE), 3),
    form_last5_gsax = round(mean(tail(gsax_total, 5), na.rm=TRUE), 3),
    proj_next10_gbg = round(mean(tail(gsax_total, 10), na.rm=TRUE) * 10, 2),
    home_gsax_avg   = round(mean(gsax_total[home_or_away=="HOME"], na.rm=TRUE), 3),
    away_gsax_avg   = round(mean(gsax_total[home_or_away=="AWAY"], na.rm=TRUE), 3),
    avg_sv_pct      = round(mean(sv_pct, na.rm=TRUE), 4),
    .groups = "drop"
  ) %>%
  arrange(desc(proj_next10_gbg))

message("  Season summaries: ", nrow(gbg_season_summary))

# ============================================================
# 11) SHOT DETAIL FROM RAW SHOTS
# ============================================================

message("\nBuilding shot-level detail...")

shots_raw <- read_csv(FILE_SHOTS_2526, show_col_types=FALSE)

# Filter to shots on goal or goals
shots_og <- shots_raw %>%
  filter(shotWasOnGoal == 1 | goal == 1, goalieNameForShot != "")

sv_pct_group <- function(df, group_col, group_val, col_name) {
  sub <- df %>% filter(!!sym(group_col) == group_val)
  if (nrow(sub) < 3) return(tibble(!!col_name := NA_real_))
  tibble(!!col_name := round(1 - sum(sub$goal) / nrow(sub), 4))
}

# Build per-goalie shot breakdowns
shot_detail <- shots_og %>%
  mutate(
    dist_bin  = cut(shotDistance, breaks=c(0,10,20,30,40,Inf),
                    labels=c("0_10","10_20","20_30","30_40","40plus"), right=FALSE),
    angle_bin = cut(abs(shotAngle), breaks=c(0,15,30,45,60,90,Inf),
                    labels=c("0_15","15_30","30_45","45_60","60_90","90plus"), right=FALSE)
  ) %>%
  group_by(name=goalieNameForShot) %>%
  summarise(
    shots_total    = n(),
    goals_against  = sum(goal),
    sv_overall     = round(1 - sum(goal)/n(), 4),
    # Distance
    sv_dist_0_10   = round(1 - sum(goal[dist_bin=="0_10"],  na.rm=TRUE) / max(1,sum(dist_bin=="0_10",  na.rm=TRUE)), 4),
    sv_dist_10_20  = round(1 - sum(goal[dist_bin=="10_20"], na.rm=TRUE) / max(1,sum(dist_bin=="10_20", na.rm=TRUE)), 4),
    sv_dist_20_30  = round(1 - sum(goal[dist_bin=="20_30"], na.rm=TRUE) / max(1,sum(dist_bin=="20_30", na.rm=TRUE)), 4),
    sv_dist_30_40  = round(1 - sum(goal[dist_bin=="30_40"], na.rm=TRUE) / max(1,sum(dist_bin=="30_40", na.rm=TRUE)), 4),
    sv_dist_40plus = round(1 - sum(goal[dist_bin=="40plus"],na.rm=TRUE) / max(1,sum(dist_bin=="40plus",na.rm=TRUE)), 4),
    # Angle
    sv_angle_0_15  = round(1 - sum(goal[angle_bin=="0_15"],  na.rm=TRUE) / max(1,sum(angle_bin=="0_15",  na.rm=TRUE)), 4),
    sv_angle_15_30 = round(1 - sum(goal[angle_bin=="15_30"], na.rm=TRUE) / max(1,sum(angle_bin=="15_30", na.rm=TRUE)), 4),
    sv_angle_30_45 = round(1 - sum(goal[angle_bin=="30_45"], na.rm=TRUE) / max(1,sum(angle_bin=="30_45", na.rm=TRUE)), 4),
    sv_angle_45_60 = round(1 - sum(goal[angle_bin=="45_60"], na.rm=TRUE) / max(1,sum(angle_bin=="45_60", na.rm=TRUE)), 4),
    sv_angle_60_90 = round(1 - sum(goal[angle_bin=="60_90"], na.rm=TRUE) / max(1,sum(angle_bin=="60_90", na.rm=TRUE)), 4),
    # Shot type
    sv_wrist       = round(1 - sum(goal[shotType=="WRIST"],  na.rm=TRUE) / max(1,sum(shotType=="WRIST",  na.rm=TRUE)), 4),
    sv_snap        = round(1 - sum(goal[shotType=="SNAP"],   na.rm=TRUE) / max(1,sum(shotType=="SNAP",   na.rm=TRUE)), 4),
    sv_slap        = round(1 - sum(goal[shotType=="SLAP"],   na.rm=TRUE) / max(1,sum(shotType=="SLAP",   na.rm=TRUE)), 4),
    sv_tip         = round(1 - sum(goal[shotType=="TIP"],    na.rm=TRUE) / max(1,sum(shotType=="TIP",    na.rm=TRUE)), 4),
    sv_backhand    = round(1 - sum(goal[shotType=="BACK"],   na.rm=TRUE) / max(1,sum(shotType=="BACK",   na.rm=TRUE)), 4),
    sv_deflection  = round(1 - sum(goal[shotType=="DEFL"],   na.rm=TRUE) / max(1,sum(shotType=="DEFL",   na.rm=TRUE)), 4),
    # Situation
    sv_rush        = round(1 - sum(goal[shotRush==1],        na.rm=TRUE) / max(1,sum(shotRush==1,        na.rm=TRUE)), 4),
    sv_rebound     = round(1 - sum(goal[shotRebound==1],     na.rm=TRUE) / max(1,sum(shotRebound==1,     na.rm=TRUE)), 4),
    sv_non_rebound = round(1 - sum(goal[shotRebound==0],     na.rm=TRUE) / max(1,sum(shotRebound==0,     na.rm=TRUE)), 4),
    sv_period1     = round(1 - sum(goal[period==1],          na.rm=TRUE) / max(1,sum(period==1,          na.rm=TRUE)), 4),
    sv_period2     = round(1 - sum(goal[period==2],          na.rm=TRUE) / max(1,sum(period==2,          na.rm=TRUE)), 4),
    sv_period3     = round(1 - sum(goal[period==3],          na.rm=TRUE) / max(1,sum(period==3,          na.rm=TRUE)), 4),
    sv_vs_left     = round(1 - sum(goal[shooterLeftRight=="L"],na.rm=TRUE)/max(1,sum(shooterLeftRight=="L",na.rm=TRUE)),4),
    sv_vs_right    = round(1 - sum(goal[shooterLeftRight=="R"],na.rm=TRUE)/max(1,sum(shooterLeftRight=="R",na.rm=TRUE)),4),
    sv_off_wing    = round(1 - sum(goal[offWing==1],         na.rm=TRUE) / max(1,sum(offWing==1,         na.rm=TRUE)), 4),
    sv_on_wing     = round(1 - sum(goal[offWing==0],         na.rm=TRUE) / max(1,sum(offWing==0,         na.rm=TRUE)), 4),
    sv_high_speed  = round(1 - sum(goal[speedFromLastEvent>30], na.rm=TRUE)/max(1,sum(speedFromLastEvent>30,na.rm=TRUE)),4),
    sv_low_speed   = round(1 - sum(goal[speedFromLastEvent<=30],na.rm=TRUE)/max(1,sum(speedFromLastEvent<=30,na.rm=TRUE)),4),
    .groups = "drop"
  ) %>%
  filter(shots_total >= 50)

message("  Shot detail for ", nrow(shot_detail), " goalies")

# ============================================================
# 12) LAYER 3 — CONTEXTUAL GOALIE ADJUSTMENT
# ============================================================

goalie_proj <- tryCatch(
  read_excel(GOALIE_MASTER_XLSX, sheet="Next10_Projection"),
  error=function(e){ message("⚠ Could not load goalie workbook. Skipping Layer 3."); NULL }
)

if (!is.null(goalie_proj)) {
  league_avg_xgp <- mean(c(res_lines_2526$dat$xGoalsPercentage,
                            res_pairs_2526$dat$xGoalsPercentage), na.rm=TRUE)
  ADJ_SCALE <- 5.0

  line_tier_xgp <- res_lines_2526$dat %>%
    mutate(model_rank=rank(-pred_xgp_ridge,ties.method="first"), n=n(),
           pct=(model_rank-1)/(n-1),
           tier=case_when(pct<=.10~"Elite",pct<=.30~"Strong",pct<=.70~"Average",TRUE~"Weak")) %>%
    group_by(tier) %>% summarise(avg_xgp=mean(xGoalsPercentage,na.rm=TRUE),.groups="drop")

  pair_tier_xgp <- res_pairs_2526$dat %>%
    mutate(model_rank=rank(-pred_xgp_ridge,ties.method="first"), n=n(),
           pct=(model_rank-1)/(n-1),
           tier=case_when(pct<=.10~"Elite",pct<=.30~"Strong",pct<=.70~"Average",TRUE~"Weak")) %>%
    group_by(tier) %>% summarise(avg_xgp=mean(xGoalsPercentage,na.rm=TRUE),.groups="drop")

  line_adj <- line_tier_xgp %>%
    mutate(xgp_vs_avg=avg_xgp-league_avg_xgp, line_gsax_adj=-xgp_vs_avg*ADJ_SCALE) %>%
    select(opposing_line_tier=tier, line_gsax_adj)
  pair_adj <- pair_tier_xgp %>%
    mutate(xgp_vs_avg=avg_xgp-league_avg_xgp, pair_gsax_adj=xgp_vs_avg*ADJ_SCALE) %>%
    select(own_pair_tier=tier, pair_gsax_adj)

  goalie_base <- goalie_proj %>%
    transmute(Goalie, baseline_gsax_per_gp=`Baseline GSAx/GP (3yr)`,
              form_gsax_per_gp=`Current Form GSAx/GP`,
              proj_gsax_next10=`Proj GSAx (Next 10)`, goalie_tier=`Tier Over 3 Yrs`)

  tiers <- c("Elite","Strong","Average","Weak")
  contextual_grid <- expand.grid(Goalie=goalie_base$Goalie,
                                  opposing_line_tier=tiers, own_pair_tier=tiers,
                                  stringsAsFactors=FALSE) %>%
    left_join(goalie_base, by="Goalie") %>%
    left_join(line_adj, by="opposing_line_tier") %>%
    left_join(pair_adj, by="own_pair_tier") %>%
    mutate(adjusted_gsax_per_gp=baseline_gsax_per_gp+line_gsax_adj+pair_gsax_adj,
           adjusted_gsax_next10=adjusted_gsax_per_gp*10,
           context_label=paste0("vs ",opposing_line_tier," line | ",own_pair_tier," D pairing")) %>%
    arrange(Goalie, opposing_line_tier, own_pair_tier)

  message("Layer 3: ", nrow(contextual_grid), " combinations")
} else {
  contextual_grid <- tibble()
}

# ============================================================
# 13) DRIVER TABLES
# ============================================================

build_driver_tables <- function(fit, X_mat, name_vec, model_label, z_map, cohort_n=10) {
  b <- as.matrix(coef(fit))
  coef_df <- tibble(feature=rownames(b), beta=as.numeric(b[,1])) %>%
    filter(feature!="(Intercept)") %>%
    mutate(abs_beta=abs(beta), model=model_label) %>%
    arrange(desc(abs_beta)) %>% mutate(global_rank=row_number()) %>%
    left_join(z_map %>% rename(feature=z, feature_display=raw), by="feature") %>%
    mutate(feature_display=coalesce(feature_display,feature))
  X_df <- as_tibble(X_mat); X_df$name <- name_vec
  contrib_long <- X_df %>%
    pivot_longer(-name, names_to="feature", values_to="x_val") %>%
    left_join(coef_df %>% select(feature,beta,feature_display), by="feature") %>%
    mutate(contribution=beta*x_val, abs_contrib=abs(contribution), model=model_label)
  top_cohort <- name_vec[order(-as.numeric(predict(fit,newx=X_mat)))][seq_len(min(cohort_n,length(name_vec)))]
  list(
    drivers_global = coef_df %>% select(model,feature,feature_display,beta,abs_beta,global_rank),
    drivers_cohort = contrib_long %>% filter(name %in% top_cohort) %>%
      group_by(model,feature,feature_display) %>%
      summarise(avg_contrib=mean(contribution,na.rm=TRUE), avg_abs_contrib=mean(abs_contrib,na.rm=TRUE),.groups="drop") %>%
      arrange(desc(avg_abs_contrib)) %>% mutate(cohort_rank=row_number())
  )
}

drv_lines <- build_driver_tables(res_lines_2526$fit,
  as.matrix(res_lines_2526$dat[,res_lines_2526$skill_terms]),
  res_lines_2526$dat$name, "Lines Ridge (2025-26)", res_lines_2526$z_map)
drv_pairs <- build_driver_tables(res_pairs_2526$fit,
  as.matrix(res_pairs_2526$dat[,res_pairs_2526$skill_terms]),
  res_pairs_2526$dat$name, "Pairings Ridge (2025-26)", res_pairs_2526$z_map)
drivers_global_all <- bind_rows(drv_lines$drivers_global, drv_pairs$drivers_global)
drivers_cohort_all <- bind_rows(drv_lines$drivers_cohort, drv_pairs$drivers_cohort)

# ============================================================
# 14) EXPORT MASTER EXCEL WORKBOOK
# ============================================================

all_lines_raw <- bind_rows(
  res_lines_2526$dat %>% mutate(group="Lines",    season="2025-26"),
  res_lines_2425$dat %>% mutate(group="Lines",    season="2024-25"),
  res_lines_2324$dat %>% mutate(group="Lines",    season="2023-24"),
  res_pairs_2526$dat %>% mutate(group="Pairings", season="2025-26"),
  res_pairs_2425$dat %>% mutate(group="Pairings", season="2024-25"),
  res_pairs_2324$dat %>% mutate(group="Pairings", season="2023-24")
)

wb <- createWorkbook()
sheets <- list(
  "All_Raw_Data"        = all_lines_raw,
  "Scout_Lines"         = scout_lines,
  "Scout_Pairings"      = scout_pairs,
  "Scout_Combined"      = scout_combined,
  "Proj_Lines"          = proj_lines,
  "Proj_Pairings"       = proj_pairs,
  "Proj_Combined"       = proj_combined,
  "GBG_Game_Log"        = gbg_joined,
  "GBG_Season_Summary"  = gbg_season_summary,
  "Shot_Detail"         = shot_detail,
  "Drivers_Global"      = drivers_global_all,
  "Drivers_Cohort"      = drivers_cohort_all,
  "Contextual_Goalie"   = contextual_grid
)
for (nm in names(sheets)) {
  addWorksheet(wb, nm)
  writeData(wb, nm, sheets[[nm]])
  setColWidths(wb, nm, cols=1:ncol(sheets[[nm]]), widths="auto")
}
saveWorkbook(wb, LINES_XLSX_OUT, overwrite=TRUE)

cat("\n✅ DONE.\n", LINES_XLSX_OUT, "\n")
cat("Sheets:", paste(names(sheets), collapse=", "), "\n")
cat("GBG games:", nrow(gbg_joined), "| Shot detail goalies:", nrow(shot_detail), "\n")
