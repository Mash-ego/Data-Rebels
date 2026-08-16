# =============================================================================
# diagnostics.R - reads the panel and reproduces the headline findings

# =============================================================================

source("00_config.R")
library(readr); library(tidyr)

panel <- read_csv(file.path(OUT, "feature_panel.csv"), show_col_types = FALSE)
snap <- read_csv(file.path(OUT, "track_b_snapshot.csv"), show_col_types = FALSE)


cmp <- panel %>% filter(fy_complete)

cat("\n=== PANEL SHAPE ===\n")
cat("all entity-years:", nrow(panel), " | complete:", nrow(cmp), " | entities:", n_distinct(panel$entity_id), "\n")
cat("June reporters get 3 complete years, everyone else 2 -- a Dec reporter's", "\nFY2023 is only half inside the data window.\n\n")
print(panel %>%
        distinct(entity_id, entity_name, fye_month, data_confidence) %>%
        left_join(panel %>% count(entity_id, wt = fy_complete, name = "complete_fys"), by = "entity_id"), n = 20)

# 1. multi - year trend
# total includes intercompany sweeps; WALLET is third party flow only. Where the two disagree, the client's treasury behaviour and its real business are moving in different directions -- that divergence is itself the finding.
cat("\n=== 36 MONTH TREND SLOPE (fraction of a typical month, per month) ===\n")
print(panel %>%
        distinct(entity_id, entity_name, sector, trend_slope_total, trend_slope_wallet, recent_vs_prior, data_confidence) %>%
        mutate(across(starts_with("trend_"), ~ round(.x, 4)), recent_vs_prior = round(recent_vs_prior, 3), divergence = round(trend_slope_wallet - trend_slope_total, 4)) %>%
        arrange(trend_slope_wallet), n = 20)

# 2. payroll primacy
# 1.0 = payroll seen in every observed month. Below that, payroll is running through a competitor for the remaining months. Distribution is BINARY: 14 clients at 1.00, 6 at 0-0.33, nothing between.
cat("\n=== PAYROLL PRIMACY, complete years only ===\n")
print(cmp %>%
        select(entity_id, entity_name, fy, payroll_cadence_score) %>%
        pivot_wider(names_from = fy, values_from = payroll_cadence_score, names_prefix = "FY") %>%
        mutate(across(starts_with("FY"), ~ round(.x, 3))) %>%
        arrange(across(last_col())), n = 20)

# 3. wallet level
# wallet excludes intercompany sweeps (approximately 50% of ledger value): a client moving its own money is not spend a competitor could take
cat("\n=== WALLET VALUE, ZAR bn, complete years (sweeps excluded) ===\n")
print(cmp %>%
        select(entity_id, entity_name, fy, tx_value_wallet) %>%
        mutate(tx_value_wallet = round(tx_value_wallet / 1e9, 2)) %>%
        pivot_wider(names_from = fy, values_from = tx_value_wallet, names_prefix = "FY"), n = 20)

#4. growth attribution
# the same headline % means different things: COUNT means transactions left or arrived (business moved); ticket means the same transactions got bigger or smaller (repricing, or the client itself changed size)
cat("\n=== GROWTH DRIVER, latest complete year ===\n")
latest <- cmp %>% group_by(entity_id) %>% slice_max(fy, n = 1) %>% ungroup()
print(latest %>%
        filter(!is.na(tx_value_wallet_yoy)) %>%
        transmute(entity_id, entity_name, fy, wallet_yoy = round(tx_value_wallet_yoy, 3), driver = growth_driver, count_yoy  = round(count_yoy, 3),
                  ticket_yoy = round(ticket_yoy, 3)) %>%
        arrange(wallet_yoy), n = 20)

# 5. at risk screen
# flags are deliberately not combined into one score: should know which thing is wrong because each implies a different conversation and conclusion
#
#   shrinking -> retention. Is it still falling (recent_vs_prior) or has it already stabilised
#   no_primacy -> the operating mandate sits with a competitor. Cross-sell.
#   concentrated -> cross-border flow depends on few corridors. Fragile.
cat("\n=== AT-RISK SCREEN (Track B, common window) ===\n")
print(snap %>%
        transmute(entity_id, entity_name, sector, conf = data_confidence,
                  wallet_bn = round(tb_wallet_ttm / 1e9, 2), wallet_yoy = round(wallet_yoy, 3),
                  recent = round(recent_vs_prior, 3), payroll = round(payroll_cadence, 2),
                  hhi = round(xb_country_hhi, 3), f_shrinking = trend_slope_wallet < -0.002,
                  f_no_primacy = payroll_cadence < 0.50, f_concentrated = xb_country_hhi > 0.20) %>%
        filter(f_shrinking | f_no_primacy | f_concentrated) %>%
        arrange(wallet_yoy), n = 20)

#6. the two commercial groups
cat("\n=== RETENTION: shrinking despite holding the operating mandate ===\n")
print(snap %>% filter(wallet_yoy < -0.02, payroll_cadence >= 0.5) %>%
        transmute(entity_name, wallet_bn = round(tb_wallet_ttm/1e9, 2),
                  wallet_yoy = round(wallet_yoy, 3), recent = round(recent_vs_prior, 3),
                  still_falling = recent_vs_prior < wallet_yoy) %>%
        arrange(wallet_yoy))

cat("\n=== CROSS-SELL: growing, but the operating bank is someone else ===\n")
print(snap %>% filter(payroll_cadence < 0.5) %>%
        transmute(entity_name, conf = data_confidence, wallet_bn = round(tb_wallet_ttm/1e9, 2),
                  wallet_yoy = round(wallet_yoy, 3), payroll = round(payroll_cadence, 2)) %>%
        arrange(desc(wallet_yoy)))

#7. sanity
cat("\n=== SANITY (all TRUE) ===\n")
cat("no NA financial years :", !any(is.na(panel$fy)), "\n")
cat("leg-type shares sum to 1 :", all(abs(rowSums(cmp %>% select(starts_with("share_")), na.rm = TRUE) - 1) < 1e-6), "\n")
cat("wallet + sweeps == total :", isTRUE(all.equal(panel$tx_value_wallet + panel$tx_value_sweeps, panel$tx_value_total)), "\n")
cat("cadence within [0,1] :", all(panel$payroll_cadence_score >= 0 & panel$payroll_cadence_score <= 1), "\n")
cat("all 20 entities present :", n_distinct(panel$entity_id) == 20, "\n")
cat("confidence tiers :", paste(names(table(snap$data_confidence)), table(snap$data_confidence), sep = "=", collapse = "  "), "\n")