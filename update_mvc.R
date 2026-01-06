# ============================================================
# MVC AUTO-UPDATE PIPELINE (GITHUB ACTIONS READY)
# ============================================================

# -------------------------------
# Libraries
# -------------------------------
suppressPackageStartupMessages({
  library(bigballR)
  library(dplyr)
  library(purrr)
  library(janitor)
  library(stringr)
})

# -------------------------------
# Paths (GitHub Actions safe)
# -------------------------------
base_dir  <- getwd()
data_dir  <- file.path(base_dir, "data")
logos_dir <- file.path(base_dir, "logos")

dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

# -------------------------------
# Persistent files
# -------------------------------
game_id_file <- file.path(data_dir, "mvc_game_ids.rds")
pbp_file     <- file.path(data_dir, "pbp_mvc_all.rds")

# ============================================================
# 1️⃣ Load previously stored game_ids
# ============================================================
if (file.exists(game_id_file)) {
  mvc_game_ids_old <- readRDS(game_id_file)
} else {
  mvc_game_ids_old <- character(0)
}

# ============================================================
# 2️⃣ Get all MVC game_ids (2025–26)
# ============================================================
data("teamids", package = "bigballR")

mvc_teams <- teamids %>%
  clean_names() %>%
  dplyr::filter(conference == "MVC", season == "2025-26")

safe_schedule <- safely(function(id) {
  get_team_schedule(team.id = id)
})

mvc_schedules <- mvc_teams$id %>%
  map(safe_schedule) %>%
  map("result") %>%
  compact() %>%
  bind_rows() %>%
  clean_names()

mvc_game_ids_all <- mvc_schedules %>%
  dplyr::filter(!is.na(game_id)) %>%
  distinct(game_id) %>%
  pull(game_id)

# ============================================================
# 3️⃣ Detect new games
# ============================================================
new_game_ids <- setdiff(mvc_game_ids_all, mvc_game_ids_old)

message("🔎 New games detected: ", length(new_game_ids))

# ============================================================
# 4️⃣ Scrape ONLY new games
# ============================================================
if (length(new_game_ids) > 0) {
  pbp_new <- get_play_by_play(game_ids = new_game_ids)
} else {
  pbp_new <- NULL
}

# ============================================================
# 5️⃣ Accumulate play-by-play
# ============================================================
if (file.exists(pbp_file)) {
  pbp_old <- readRDS(pbp_file)
} else {
  pbp_old <- NULL
}

pbp_all <- bind_rows(pbp_old, pbp_new)

saveRDS(pbp_all, pbp_file)
saveRDS(mvc_game_ids_all, game_id_file)

# ============================================================
# 6️⃣ Regenerate player stats (FULL REFRESH)
# ============================================================
player_stats_mvc <- get_player_stats(
  play_by_play_data = pbp_all,
  multi.games = TRUE
) %>%
  clean_names()

# ============================================================
# 7️⃣ Cleaning + advanced features
# ============================================================
player_clean <- player_stats_mvc %>%
  dplyr::filter(gp >= 5, mins >= 50)

player_adv <- player_clean %>%
  mutate(
    mpg = mins / gp,
    ppg = pts / gp,
    rpg = (orb + drb) / gp,
    apg = ast / gp,
    spg = stl / gp,
    bpg = blk / gp,
    tov_pg = tov / gp,

    t2_pct = if_else((fga - tpa) > 0, (fgm - tpm) / (fga - tpa), NA_real_) * 100,
    t3_pct = tp * 100,
    t1_pct = ft * 100,

    pts_per_poss = pts / o_poss,
    ast_tov = if_else(tov > 0, ast / tov, NA_real_),

    shot_usage_pct = 100 * fga / o_poss,

    impact_rating =
      0.40 * ppg +
      25   * pts_per_poss +
      10   * (ts - mean(ts, na.rm = TRUE)) -
      2    * tov_pg
  )

# ============================================================
# 8️⃣ Final MVC table
# ============================================================
format_player_name <- function(x) {
  x %>%
    str_replace_all("\\.", " ") %>%
    str_to_lower() %>%
    str_to_title()
}

mvc_table_fmt <- player_adv %>%
  mutate(
    Player = format_player_name(player),
    Team   = team,
    Logo   = file.path("logos", paste0(team, ".png"))
  ) %>%
  dplyr::select(
    Player, Team, gp, mpg, ppg, rpg, apg, spg, bpg, tov_pg,
    ts, e_fg, t2_pct, t3_pct, t1_pct,
    shot_usage_pct, pts_per_poss, ast_tov, impact_rating,
    Logo
  ) %>%
  arrange(desc(impact_rating))

# ============================================================
# 9️⃣ Save outputs
# ============================================================
saveRDS(mvc_table_fmt, file.path(data_dir, "mvc_table_fmt.rds"))
saveRDS(player_adv,    file.path(data_dir, "player_adv_mvc.rds"))

message("✅ MVC update completed successfully")
