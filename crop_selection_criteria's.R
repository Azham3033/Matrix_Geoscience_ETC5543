# ---- Packages ----
# install.packages(c("tidyverse","kableExtra"))  # uncomment if needed
library(tidyverse)
library(kableExtra)

# ---- 1) Define criteria + weights (edit as needed; weights must sum to 1) ----
criteria <- tribble(
  ~criterion,               ~description,                                                       ~weight,
  "not_in_TESM",            "Absent from Tas Enterprise Suitability Maps",                      0.15,
  "market_value",           "High value / specialty market potential",                          0.25,
  "climate_resilience",     "Tolerance to drought/frost/salinity (Tas conditions)",            0.25,
  "regen_small_scale_fit",  "Suited to regenerative, low-input, small-scale systems",           0.20,
  "novelty_trials",         "Under trial / limited local production (first-mover bonus)",        0.15
)

stopifnot(abs(sum(criteria$weight) - 1) < 1e-9)

# ---- 2) Helper: make a blank scoring row for a new crop ----
blank_row <- function(crop_name) {
  tibble(
    crop = crop_name,
    not_in_TESM = NA_real_,
    market_value = NA_real_,
    climate_resilience = NA_real_,
    regen_small_scale_fit = NA_real_,
    novelty_trials = NA_real_,
    notes = ""
  )
}

# ---- 3) Start your candidate list (0–5 scoring scale; 0=doesn’t fit, 5=excellent) ----
# Replace these example rows with your real candidates & scores.
candidates <- bind_rows(
  tibble(
    crop = "Saffron",
    not_in_TESM = 4, market_value = 5, climate_resilience = 3,
    regen_small_scale_fit = 5, novelty_trials = 3,
    notes = "Tiny footprint; high labour need; drying critical."
  ),
  tibble(
    crop = "Quinoa",
    not_in_TESM = 4, market_value = 3, climate_resilience = 4,
    regen_small_scale_fit = 4, novelty_trials = 2,
    notes = "Cool-tolerant lines exist; watch saponin processing."
  ),
  tibble(
    crop = "Wasabi",
    not_in_TESM = 4, market_value = 5, climate_resilience = 2,
    regen_small_scale_fit = 3, novelty_trials = 3,
    notes = "Very high value but finicky; shade/water quality critical."
  )
)

# ---- 4) Validate input scores (0–5 only) ----
score_cols <- intersect(names(candidates), criteria$criterion)
if (any(!score_cols %in% criteria$criterion)) {
  stop("Candidate columns must match criteria names.")
}
if (any(map_lgl(candidates[score_cols], ~ any(.x < 0 | .x > 5, na.rm = TRUE)))) {
  stop("All scores must be between 0 and 5.")
}

# ---- 5) Compute weighted scores & rank ----
w <- criteria %>% select(criterion, weight)
scored <- candidates %>%
  pivot_longer(all_of(score_cols), names_to = "criterion", values_to = "score") %>%
  left_join(w, by = "criterion") %>%
  mutate(weighted = score * weight) %>%
  group_by(crop) %>%
  summarise(
    total_score = sum(weighted, na.rm = TRUE),                    # 0–5 (because scores are 0–5 and weights sum to 1)
    pct_score   = round(100 * total_score / 5, 1)                 # 0–100%
  ) %>%
  arrange(desc(total_score))

# ---- 6) Join back detail for a presentation table ----
present <- scored %>%
  left_join(candidates, by = "crop") %>%
  relocate(crop, pct_score, all_of(score_cols), notes)

# ---- 7) Show as kableExtra table (with simple color bars for pct_score) ----
present_display <- present %>%
  mutate(
    pct_score_bar = cell_spec(
      sprintf("%s%%", pct_score),
      "html",
      background = scales::col_numeric(
        palette = c("#ffeeee", "#e8f5e9"),  # light red -> light green (kept subtle)
        domain  = c(0, 100)
      )(pct_score),
      bold = TRUE
    )
  ) %>%
  select(crop, `Score (0–100)` = pct_score_bar,
         all_of(score_cols), notes)

kable(present_display, escape = FALSE, align = "l", caption = "Tasmania Novel Crop Shortlist – Weighted Scoring") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped","hover","condensed")) %>%
  add_header_above(c(" " = 2, "Criterion Scores (0–5)" = length(score_cols), " " = 1))

# ---- 8) Export (optional) ----
# write_csv(present, "crop_shortlist_scored.csv")
# write_csv(candidates, "crop_scores_raw.csv")
# write_csv(criteria, "criteria_weights.csv")

# ---- 9) How to add a new crop quickly ----
# new <- blank_row("Sea Buckthorn")
# new$not_in_TESM <- 4
# new$market_value <- 4
# new$climate_resilience <- 5
# new$regen_small_scale_fit <- 4
# new$novelty_trials <- 3
# new$notes <- "Drought/frost hardy; berries need processing."
# candidates <- bind_rows(candidates, new)
# (then re-run steps 4–7)
