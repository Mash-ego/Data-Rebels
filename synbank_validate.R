# project validation
# run after synbank_pipeline.R
# =============================================================================
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(purrr); library(lubridate) })

OUT <- "out"; P <- 0; FL <- 0; W <- 0

# Mirrors 00_config in the pipeline - declared independently so a change to the pipeline's lookup does not silently propagate into its own test
FYE_MONTH_CHK <- c(E01=6, E02=12, E03=12, E04=12, E05=12, E06=12, E07=6, E08=12, E09=6, E10=6, E11=9, E12=8, E13=12, E14=3, E15=3, E16=12,
                   E17=3, E18=6, E19=6, E20=12)
chk  <- function(n, ok, d="") { ok <- isTRUE(ok); if (ok) P<<-P+1 else FL<<-FL+1
cat(sprintf("%-4s %-52s %s\n", if(ok)"PASS" else"FAIL", n, d)) }
warn <- function(n, ok, d="") { if (!isTRUE(ok)) { W<<-W+1
cat(sprintf("%-4s %-52s %s\n", "WARN", n, d)) } }

rd <- function(f) read_csv(file.path(OUT,f), show_col_types=FALSE)

# must use the same col_types as the pipeline with default type guessing, memo and reference parse differently, so distinct() removes a different set of rows and the totals cannot reconcile. That mismatch was a TEST bug, not a pipeline
# bug - it made B1 fail while the pipeline was correct
tx <- read_csv("transactional_banking.csv", col_types = cols(date = col_date("%Y-%m-%d"), memo = col_character(), reference = col_character(), .default = col_guess()), progress = FALSE) %>% distinct()
panel <- rd("feature_panel.csv"); snap <- rd("track_b_snapshot.csv")
ext <- rd("external_panel.csv"); wal <- rd("wallet_by_client.csv")
pil <- rd("wallet_by_pillar.csv"); hmm <- rd("hmm_state_summary.csv")
path <- rd("hmm_state_path.csv"); trn <- rd("hmm_transitions.csv")
fc <- rd("regime_forecast.csv"); clu <- rd("clusters.csv")
per <- rd("periods.csv"); ents <- rd("entities.csv")
sens <- rd("wallet_sensitivity.csv")

cat("\n=== A. data integrity ===\n")
chk("A1 20 entities throughout",
    nrow(ents)==20 && n_distinct(panel$entity_id)==20 && nrow(snap)==20)
# Joins each transaction to the bounds of the year it was LABELLED with. If
# fy_of() and the period bounds disagree by even a day at a year boundary, that
# client's whole wallet ratio is contaminated and nothing else would catch it.
chk("A2 every tx date inside its labelled financial year",
    { fyl <- tx %>% mutate(date = as.Date(date),
                           fye = FYE_MONTH_CHK[entity_id],
                           fy = ifelse(month(date) > fye, year(date)+1L, year(date)))
    b <- fyl %>% inner_join(per %>% mutate(fy_start=as.Date(fy_start),
                                           fy_end=as.Date(fy_end)) %>%
                              select(entity_id, fy, fy_start, fy_end),
                            by=c("entity_id","fy"))
    nrow(b) == nrow(fyl) && all(b$date >= b$fy_start & b$date <= b$fy_end) })
chk("A3 no NA financial years", !any(is.na(panel$fy)))
chk("A4 complete years have coverage 1.0",
    all(abs(panel$coverage[panel$fy_complete] - 1) < 1e-9))
# Parenthesised: without them, + binds tighter than %in% and the test is
# comparing a number to a logical.
chk("A5 periods are exactly one year long",
    all((as.numeric(as.Date(per$fy_end) - as.Date(per$fy_start)) + 1) %in% c(365,366)))
chk("A6 panel grain is one row per entity-year",
    nrow(panel) == nrow(distinct(panel, entity_id, fy)))

cat("\n=== B. conservation ===\n")
truth <- tx %>% group_by(entity_id) %>%
  summarise(v = sum(amount_zar), w = sum(amount_zar[leg_type!="intercompany_sweeps"]),
            .groups="drop")
cmp <- panel %>% group_by(entity_id) %>%
  summarise(v = sum(tx_value_total), w = sum(tx_value_wallet), .groups="drop") %>%
  left_join(truth, by="entity_id")
chk("B1 panel totals reproduce from raw rows",
    isTRUE(all.equal(cmp$v.x, cmp$v.y)) && isTRUE(all.equal(cmp$w.x, cmp$w.y)))
chk("B2 wallet + sweeps == total",
    isTRUE(all.equal(panel$tx_value_wallet + panel$tx_value_sweeps, panel$tx_value_total)))
lc <- grep("^val_", names(panel), value=TRUE)
chk("B3 leg-type values sum to the total",
    isTRUE(all.equal(rowSums(panel[lc]), panel$tx_value_total)))
chk("B4 growth decomposition reconstructs",
    { r <- panel %>% filter(!is.na(tx_value_wallet_yoy))
    max(abs((1+r$count_yoy)*(1+r$ticket_yoy)-1 - r$tx_value_wallet_yoy)) < 1e-9 })

cat("\n=== C. shares and ratios ===\n")
for (p in c("share_","chshare_","corr_share_","inst_share_")) {
  s <- rowSums(panel %>% select(starts_with(p)), na.rm=TRUE); s <- s[s>0]
  chk(paste0("C ", p, "sums to 1"), max(abs(s-1)) < 1e-9)
}
chk("C5 cadence within [0,1]",
    all(panel$payroll_cadence_score >= 0 & panel$payroll_cadence_score <= 1))
chk("C6 HHI within [0,1]",
    all(panel$xb_country_hhi >= 0 & panel$xb_country_hhi <= 1, na.rm=TRUE))

cat("\n=== D. cross pipeline (Track A vs Track B) ===\n")
rec <- panel %>% filter(fye_month==6, fy==2026) %>%
  select(entity_id, A=tx_value_wallet) %>%
  left_join(snap %>% select(entity_id, B=tb_wallet_ttm), by="entity_id")
chk("D1 June reporters agree to the rand across tracks",
    nrow(rec)==6 && isTRUE(all.equal(rec$A, rec$B)))
tiers <- panel %>% distinct(entity_id, A=data_confidence) %>%
  left_join(snap %>% select(entity_id, B=data_confidence), by="entity_id")
chk("D2 confidence tiers agree across tracks", all(tiers$A == tiers$B))

cat("\n=== E. currency ===\n")
u <- ext
chk("E1 revenue within R1bn..R5tn", all(u$revenue_zar > 1e9 & u$revenue_zar < 5e12),
    sprintf("R%.1fbn..R%.0fbn", min(u$revenue_zar)/1e9, max(u$revenue_zar)/1e9))
chk("E2 ZAR reporters carry rate 1.0",
    { z <- u %>% filter(reporting_currency=="ZAR"); all(z$rate_avg==1 & z$rate_close==1) })
f <- u %>% filter(reporting_currency != "ZAR")
# EXPECTED TO WARN. Rows without a sourced average rate retain the period-end
# rate for income-statement items and are flagged APPROX in external_panel.csv.
# Rates are never invented, so the approximation is visible rather than hidden.
warn("E3 all foreign rows use distinct avg/closing rates",
     nrow(f)==0 || all(f$rate_avg != f$rate_close),
     sprintf("%d of %d foreign rows on a single rate -- STATED LIMITATION (fx_basis = APPROX)", sum(f$rate_avg==f$rate_close), nrow(f)))

cat("\n=== F. wallet ===\n")
chk("F1 share within (0,1]", all(wal$share_total > 0 & wal$share_total <= 1+1e-9),
    sprintf("%.3f..%.3f", min(wal$share_total), max(wal$share_total)))
chk("F2 no negative gaps", all(wal$gap_total >= -1e-6))
chk("F3 no gap above R500bn", all(wal$gap_total < 5e11),
    sprintf("max R%.1fbn", max(wal$gap_total)/1e9))
chk("F4 captured never exceeds estimated (per pillar)",
    all(pil$observed <= pil$estimated_wallet + 1e-6))
chk("F5 client total equals sum of pillars",
    { r <- pil %>% group_by(entity_id, fy) %>%
      summarise(o=sum(observed), e=sum(estimated_wallet), .groups="drop") %>%
      left_join(wal %>% select(entity_id, fy, observed_total, estimated_total),
                by=c("entity_id","fy"))
    all(abs(r$o-r$observed_total)<1) && all(abs(r$e-r$estimated_total)<1) })
chk("F6 wallet output is per-year, never aggregated",
    nrow(wal) == nrow(distinct(wal, entity_id, fy)))
# EXPECTED TO WARN. The frontier percentile materially moves the wallet-gap
# ranking. The retention findings are unaffected -- they come from the regime
# model and the common-window comparison, neither of which uses the frontier --
# but the opportunity ranking must be presented as indicative, not robust.
warn("F7 frontier percentile does not drive the ranking",
     max(sens$rank_swing) <= 3,
     sprintf("max rank swing %d places -- STATED LIMITATION: present the wallet ranking as indicative",
             max(sens$rank_swing)))

cat("\n=== G. regime model ===\n")
chk("G1 state path covers every modelled month",
    { c1 <- path %>% count(entity_id, name="n") %>%
      left_join(hmm %>% select(entity_id, n_months), by="entity_id")
    all(c1$n == c1$n_months) })
chk("G2 only the three expected labels",
    all(unique(path$state) %in% c("Declining","Stable","Growing")))
runs <- path %>% group_by(entity_id) %>%
  summarise(mn=min(rle(state)$lengths), md=median(rle(state)$lengths),
            tr=length(rle(state)$lengths)-1, .groups="drop")
chk("G3 no regime shorter than 3 months", all(runs$mn >= 3))
chk("G4 median run at least 6 months", median(runs$md) >= 6)
chk("G5 transition counts match the decoded path",
    all(runs$tr == hmm$n_transitions[match(runs$entity_id, hmm$entity_id)]))
chk("G6 labels absolute, not one-of-each-per-client",
    any((path %>% group_by(entity_id) %>% summarise(k=n_distinct(state)))$k < 3))
lab <- path %>% group_by(entity_id, state) %>% summarise(mz=mean(z), .groups="drop")
chk("G7 Declining states sit below -0.5 z", all(lab$mz[lab$state=="Declining"] < -0.5+1e-9))
chk("G8 Growing states sit above +0.5 z",  all(lab$mz[lab$state=="Growing"]  >  0.5-1e-9))
chk("G9 regimes are persistent (mean self-transition > 0.7)",
    mean(hmm$persistence) > 0.7, sprintf("%.3f", mean(hmm$persistence)))
drift <- path %>% group_by(entity_id) %>% arrange(ym, .by_group=TRUE) %>%
  summarise(d = mean(tail(z,6)) - mean(head(z,6)), .groups="drop") %>%
  left_join(hmm %>% select(entity_id, wallet_yoy), by="entity_id")
chk("G10 drift correlates with YoY (independent measures agree)",
    cor(drift$d, drift$wallet_yoy, method="spearman") > 0.7,
    sprintf("rho %.3f", cor(drift$d, drift$wallet_yoy, method="spearman")))

cat("\n=== H. ensemble ===\n")
chk("H1 agreement equals the largest vote block",
    all(hmm$agreement == pmax(hmm$n_declining, hmm$n_growing, hmm$n_stable)))
chk("H2 votes sum to 3", all(hmm$n_declining + hmm$n_growing + hmm$n_stable == 3))
chk("H3 consensus matches the majority",
    all((hmm$agreement >= 2 & hmm$consensus != "Split") | (hmm$agreement == 1 & hmm$consensus == "Split")))
# EXPECTED TO WARN. A genuine 1-1-1 split is a finding, not a defect: three
# independently built models disagreeing is information. Reported as "Split"
# rather than forced to a direction.
warn("H4 no client left as a three-way Split", sum(hmm$consensus=="Split")==0,
     sprintf("%d client(s) split 1-1-1: %s - report as Split, do not force a direction",
             sum(hmm$consensus=="Split"), paste(hmm$entity_name[hmm$consensus=="Split"], collapse=", ")))

cat("\n=== I. forecast ===\n")
chk("I1 transition rows sum to 1 per from-state",
    { r <- trn %>% group_by(entity_id, from_state) %>%
      summarise(s=sum(prob), .groups="drop"); max(abs(r$s-1)) < 1e-9 })
chk("I2 all transition probabilities positive (Laplace smoothed)", all(trn$prob > 0))
chk("I3 forecast probabilities sum to 1",
    max(abs(fc$p_declining + fc$p_stable + fc$p_growing - 1)) < 1e-9)
chk("I4 probabilities within [0,1]",
    all(c(fc$p_declining, fc$p_stable, fc$p_growing) >= 0 & c(fc$p_declining, fc$p_stable, fc$p_growing) <= 1))
chk("I5 every entity forecast at every horizon", nrow(fc) == n_distinct(fc$entity_id) * 3)

warn("I6 24-month forecasts still client-specific",
     all(fc$informative[fc$horizon_months==24]),
     sprintf("%d of %d converged to stationary -- STATED LIMITATION: use 6m and 12m",
             sum(!fc$informative[fc$horizon_months==24]),
             sum(fc$horizon_months==24)))

cat("\n=== J. clustering ===\n")
chk("J1 all 20 entities have a cluster row", nrow(clu)==20)
chk("J2 only insufficient-data entities unclustered",
    setequal(clu$entity_id[is.na(clu$cluster)], snap$entity_id[!snap$data_sufficient]))
chk("J3 clusters span sectors (not a sector proxy)",
    { x <- clu %>% filter(!is.na(cluster)) %>%
      left_join(ents %>% select(entity_id, sector), by="entity_id") %>%
      count(cluster, sector) %>% count(cluster, name="ns"); max(x$ns) > 1 })

cat("\n=== K. headline claims ===\n")
chk("K1 Sanlam and Anglo are the two triple-confirmed decliners",
    setequal(hmm$entity_id[hmm$agreement==3 & hmm$consensus=="Declining"], c("E08","E03")))
chk("K2 Sanlam still worsening, Anglo easing",
    hmm$trajectory[hmm$entity_id=="E08"]=="still worsening" &&
      hmm$trajectory[hmm$entity_id=="E03"]=="easing")
chk("K3 six clients lack payroll primacy",
    sum(snap$payroll_cadence < 0.5) == 6)
chk("K4 sweeps are roughly half of ledger value",
    abs(sum(snap$tb_sweeps_ttm)/sum(snap$tb_value_ttm) - 0.5) < 0.08,
    sprintf("%.1f%%", 100*sum(snap$tb_sweeps_ttm)/sum(snap$tb_value_ttm)))

cat(sprintf("\n================  %d passed   %d failed   %d warnings  ================\n",
            P, FL, W))
if (FL > 0) {
  cat("check for errors \n")
} else {
  cat("Pipeline validated.\n\n")
  cat("The four expected warnings are STATED LIMITATIONS, documented in\n")
  cat("METHODOLOGY.md section 10 and on the one-page summary:\n")
  cat(" E3  approximate FX on rows without a sourced average rate\n")
  cat(" F7  wallet-gap ranking is sensitive to the frontier percentile\n")
  cat(" H4  one client is a genuine three-way model split\n")
  cat(" I6  24-month forecasts converge to the stationary distribution\n")
  cat("\nAny warning NOT in that list is new -- investigate before presenting.\n")
}