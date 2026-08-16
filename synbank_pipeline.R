# Syn bank - share of wallet intelligence engine
#
# input in the working directory
#   transactional_banking.csv, cross_border_payments.csv, trade_finance.csv
#   afs_financials_ZAR_normalized.csv, fx_rates.csv

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(purrr)
  library(lubridate); library(jsonlite); library(ggplot2)
})

RAW <- "."; OUT <- "out"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(OUT, "figs"), showWarnings = FALSE, recursive = TRUE)
set.seed(42)

t0 <- Sys.time()
cat("Syn bank pipeline \n\n")

# 0. configuration
# =============================================================================
DATA_MIN <- as.Date("2023-07-01")
DATA_MAX <- as.Date("2026-06-30")

# financial year-end month per entity. Year-ends range Mar-Dec; forcing a common calendar produces 6.4% mean absolute error in annual volume, systematically understating growers and overstating decliners
FYE_MONTH <- c(E01=6, E02=12, E03=12, E04=12, E05=12, E06=12, E07=6, E08=12, E09=6, E10=6, E11=9, E12=8, E13=12, E14=3, E15=3, E16=12,
               E17=3, E18=6, E19=6, E20=12)

# Industry peers (competitive context) -- distinct from behavioural clusters.
PEER_GROUP <- c(E09="Retail", E11="Retail", E12="Retail",
  E10="Distribution & pharma", E18="Distribution & pharma", E19="Distribution & pharma",
  E04="SA miners", E05="SA miners", E06="SA miners",
  E01="Global miners", E02="Global miners", E03="Global miners",
  E16="Telecoms", E17="Telecoms", E15="Tech", E14="Tech",
  E08="Financial & property", E07="Financial & property",
  E13="Financial & property", E20="Financial & property")

MIN_ROWS_FOR_TREND <- 5000
MIN_ROWS_TTM <- MIN_ROWS_FOR_TREND / 3
MIN_ROWS_LOW_CONF <- 1500
MIN_ROWS_LOW_TTM <- MIN_ROWS_LOW_CONF / 3

fy_of <- function(d, m) ifelse(month(d) > m, year(d) + 1L, year(d))

hhi_value <- function(cat, val) {
  if (length(val) == 0 || sum(val, na.rm = TRUE) == 0) return(NA_real_)
  p <- tapply(val, cat, sum, na.rm = TRUE); p <- p / sum(p); sum(p^2)
}

slope_norm <- function(y, min_n = 3) {
  y <- y[!is.na(y)]
  if (length(y) < min_n || mean(y) == 0) return(NA_real_)
  yn <- y / mean(y); unname(coef(lm(yn ~ seq_along(yn)))[2])
}

clean_lab <- function(x) gsub("[^a-z0-9]+", "_", tolower(x))

confidence_tier <- function(rows, hi, lo)
  case_when(rows >= hi ~ "high", rows >= lo ~ "low", TRUE ~ "insufficient")

ZERO_FILL <- c("tf_value","tf_count","xb_value","xb_count", "payroll_months_active","tax_months_active",
               "payroll_cadence_score","tax_cadence_score", "val_payroll","val_tax","val_collections",
               "val_supplier_payments","val_intercompany_sweeps", "n_payroll","n_tax","n_collections",
               "n_supplier_payments","n_intercompany_sweeps")

# 1. load and clean
# =============================================================================
cat("[1/9] load + clean\n")
log <- list()

add_fy <- function(df) df %>%
  mutate(fye_month = FYE_MONTH[entity_id], fy = fy_of(date, fye_month), ym = format(date, "%Y-%m"))

ct <- cols(date = col_date("%Y-%m-%d"), memo = col_character(), reference = col_character(), .default = col_guess())

tx <- read_csv(file.path(RAW, "transactional_banking.csv"), col_types = ct, progress = FALSE)
log$tx_rows_in <- nrow(tx)

tx <- tx %>% mutate(currency = toupper(currency))

# transaction_id is NOT unique: ~95k rows share an id with a different entity, date and amount. Deduplicate on the FULL ROW only
log$tx_rows_sharing_an_id <- sum(duplicated(tx$transaction_id) | duplicated(tx$transaction_id, fromLast = TRUE))
n_before <- nrow(tx); tx <- distinct(tx)
log$tx_full_row_duplicates <- n_before - nrow(tx)

tx <- tx %>%
  mutate(amount_signed = if_else(direction == "outbound", -amount_zar, amount_zar)) %>%
  add_fy()
log$tx_rows_out <- nrow(tx)

xb <- read_csv(file.path(RAW, "cross_border_payments.csv"), col_types = ct, progress = FALSE)
log$xb_rows_in <- nrow(xb)
n_before <- nrow(xb); xb <- distinct(xb)
log$xb_full_row_duplicates <- n_before - nrow(xb)
log$xb_country_missing <- sum(is.na(xb$counterparty_country))
xb <- xb %>%
  mutate(counterparty_country = coalesce(counterparty_country, "UNKNOWN"),
         ccy = sub("/.*$", "", currency_pair),
         value_signed = if_else(direction == "outbound", -value_zar, value_zar)) %>%
  add_fy()
log$xb_rows_out <- nrow(xb)

tf <- read_csv(file.path(RAW, "trade_finance.csv"), col_types = ct, progress = FALSE)
log$tf_rows_in <- nrow(tf)
n_before <- nrow(tf); tf <- distinct(tf)
log$tf_full_row_duplicates <- n_before - nrow(tf)
log$tf_country_missing <- sum(is.na(tf$counterparty_country))
tf <- tf %>% mutate(counterparty_country = coalesce(counterparty_country, "UNKNOWN")) %>%
  add_fy()
log$tf_rows_out <- nrow(tf)

# Period completeness by WINDOW CONTAINMENT, not by counting active days: a
# low-volume client legitimately has days with no transactions.
periods <- tx %>% distinct(entity_id, fy) %>%
  mutate(fye_month = FYE_MONTH[entity_id],
         fy_end = ceiling_date(make_date(fy, fye_month, 1), "month") - days(1),
         fy_start = fy_end - years(1) + days(1),
         fy_complete = fy_start >= DATA_MIN & fy_end <= DATA_MAX,
         coverage = as.numeric(pmin(fy_end, DATA_MAX) - pmax(fy_start, DATA_MIN) + 1) / as.numeric(fy_end - fy_start + 1)) %>%
  arrange(entity_id, fy)

ents <- tx %>% distinct(entity_id, entity_name, sector) %>% arrange(entity_id) %>%
  mutate(peer_group = PEER_GROUP[entity_id])

log$n_entities <- nrow(ents)
log$entity_years_complete <- sum(periods$fy_complete)
log$calendar_gaps <- length(setdiff(seq(min(tx$date), max(tx$date), by = "day"), unique(tx$date)))
log$ids_consistent <- setequal(tx$entity_id, xb$entity_id) && setequal(tx$entity_id, tf$entity_id)
log$fy_never_na <- !any(is.na(tx$fy))

write_csv(ents, file.path(OUT, "entities.csv"))
write_csv(periods, file.path(OUT, "periods.csv"))
write_json(log, file.path(OUT, "dq_report.json"), pretty = TRUE, auto_unbox = TRUE)
cat(" rows", log$tx_rows_in, "->", log$tx_rows_out,
    "| complete entity-years", log$entity_years_complete, "of", nrow(periods), "\n")

# 2. feature panel -track A (each client on its own financial year)
# =============================================================================
cat("[2/9] feature panel\n")
K <- c("entity_id", "fy")

share_block <- function(df, cat_col, val_col, prefix) {
  df %>% group_by(entity_id, fy, .cat = clean_lab(.data[[cat_col]])) %>%
    summarise(v = sum(.data[[val_col]]), .groups = "drop") %>%
    group_by(entity_id, fy) %>% mutate(share = v / sum(v)) %>% ungroup() %>%
    select(-v) %>%
    pivot_wider(names_from = .cat, values_from = share, names_prefix = prefix, values_fill = 0)
}

# wallet excludes intercompany sweeps (which is approximately 50% of ledger value): a client moving its own money is not spend a competitor could take
A <- tx %>% group_by(entity_id, fy) %>%
  summarise(tx_value_total = sum(amount_zar),
            tx_value_wallet = sum(amount_zar[leg_type != "intercompany_sweeps"]),
            tx_value_sweeps = sum(amount_zar[leg_type == "intercompany_sweeps"]),
            tx_count = n(),
            tx_count_wallet = sum(leg_type != "intercompany_sweeps"),
            tx_ticket_median = median(amount_zar),
            tx_ticket_p95 = quantile(amount_zar, .95),
            tx_net_flow = sum(amount_signed),
            tx_top1pct_share = sum(amount_zar[amount_zar >= quantile(amount_zar, .99)]) / sum(amount_zar),
            .groups = "drop") %>%
  mutate(sweep_share = tx_value_sweeps / tx_value_total, tx_avg_ticket_wallet = tx_value_wallet / tx_count_wallet)

sufficiency <- tx %>% group_by(entity_id) %>% summarise(rows_total = n(), .groups="drop") %>%
  mutate(data_confidence = confidence_tier(rows_total, MIN_ROWS_FOR_TREND, MIN_ROWS_LOW_CONF), data_sufficient = data_confidence == "high")

# payroll and tax run through the operating bank - their absence is evidence a competitor holds the mandate and cadence denominator is observed months
leg_val <- tx %>% group_by(entity_id, fy, .lt = clean_lab(leg_type)) %>%
  summarise(v = sum(amount_zar), n = n(), .groups = "drop")
B <- leg_val %>% select(-n) %>%
  pivot_wider(names_from=.lt, values_from=v, names_prefix="val_", values_fill=0) %>%
  left_join(leg_val %>% select(-v) %>%
    pivot_wider(names_from=.lt, values_from=n, names_prefix="n_", values_fill=0), by=K) %>%
  left_join(leg_val %>% select(-n) %>% group_by(entity_id, fy) %>%
    mutate(share = v/sum(v)) %>% ungroup() %>% select(-v) %>%
    pivot_wider(names_from=.lt, values_from=share, names_prefix="share_", values_fill=0), by=K) %>%
  left_join(tx %>% filter(leg_type %in% c("payroll","tax")) %>%
    group_by(entity_id, fy, .lt = clean_lab(leg_type)) %>%
    summarise(m = n_distinct(ym), .groups="drop") %>%
    pivot_wider(names_from=.lt, values_from=m, names_glue="{.lt}_months_active", values_fill=0), by=K) %>%
  left_join(tx %>% group_by(entity_id, fy) %>%
    summarise(months_observed = n_distinct(ym), .groups="drop"), by=K) %>%
  mutate(payroll_months_active = coalesce(payroll_months_active, 0),
         tax_months_active = coalesce(tax_months_active, 0),
         payroll_cadence_score = pmin(payroll_months_active/pmax(months_observed,1), 1),
         tax_cadence_score = pmin(tax_months_active/pmax(months_observed,1), 1),
         has_payroll_primacy = payroll_cadence_score >= 0.5,
         payroll_value_share = val_payroll / (val_payroll + val_supplier_payments + val_collections + val_intercompany_sweeps + val_tax))

C <- share_block(tx, "channel", "amount_zar", "chshare_") %>%
  left_join(tx %>% group_by(entity_id, fy) %>%
    summarise(product_breadth = n_distinct(leg_type), channel_breadth = n_distinct(channel), .groups="drop"), by=K)

D <- xb %>% group_by(entity_id, fy) %>%
  summarise(xb_value = sum(value_zar), xb_count = n(),
            xb_countries = n_distinct(counterparty_country),
            xb_ccy_pairs = n_distinct(ccy),
            xb_country_hhi = hhi_value(counterparty_country, value_zar),
            xb_ccy_hhi = hhi_value(ccy, value_zar),
            xb_net_flow = sum(value_signed), .groups="drop") %>%
  left_join(share_block(xb, "corridor_type", "value_zar", "corr_share_"), by=K)

E <- tf %>% group_by(entity_id, fy) %>%
  summarise(tf_value = sum(value_zar), tf_count = n(),
            tf_tenor_wavg = weighted.mean(tenor_days, value_zar),
            tf_countries = n_distinct(counterparty_country),
            tf_commodities = n_distinct(commodity_or_contract_type),
            tf_country_hhi = hhi_value(counterparty_country, value_zar), .groups="drop") %>%
  left_join(share_block(tf, "instrument_type", "value_zar", "inst_share_"), by=K) %>%
  left_join(share_block(tf, "direction", "value_zar", "trade_dir_share_"), by=K) %>%
  left_join(share_block(tf, "status", "value_zar", "tf_status_share_"), by=K)

monthly <- tx %>% group_by(entity_id, fy, ym) %>%
  summarise(v = sum(amount_zar), .groups="drop") %>% arrange(entity_id, fy, ym)

F_blk <- monthly %>% group_by(entity_id, fy) %>%
  summarise(mo_active = n_distinct(ym),
            mo_cv = if_else(n_distinct(ym) >= 6, sd(v)/mean(v), NA_real_),
            mo_trend_slope = slope_norm(v, 6),
            mo_peak_to_median = max(v)/median(v), .groups="drop")

trend_total <- tx %>% group_by(entity_id, ym) %>%
  summarise(v = sum(amount_zar), .groups="drop") %>% arrange(entity_id, ym) %>%
  group_by(entity_id) %>%
  summarise(trend_slope_total = slope_norm(v, 12), months_total = n_distinct(ym), .groups="drop")

trend_wallet <- tx %>% filter(leg_type != "intercompany_sweeps") %>%
  group_by(entity_id, ym) %>% summarise(v = sum(amount_zar), .groups="drop") %>%
  arrange(entity_id, ym) %>% group_by(entity_id) %>%
  summarise(trend_slope_wallet = slope_norm(v, 12), .groups="drop")

# same quarters a year apart, not adjacent halves: consumer clients peak at around 37% which is above average in Q4 and an H1 vs H2 comparison manufactures false declines
recent_shift <- tx %>% filter(leg_type != "intercompany_sweeps") %>%
  mutate(q = as.integer(format(date, "%Y"))*4 + quarter(date)) %>%
  group_by(entity_id, q) %>% summarise(v = sum(amount_zar), .groups="drop") %>%
  group_by(entity_id) %>% arrange(q, .by_group=TRUE) %>%
  summarise(recent_vs_prior = sum(tail(v,2))/sum(head(tail(v,6),2)) - 1, .groups="drop")

G <- tx %>% group_by(entity_id, fy) %>%
  summarise(tx_beneficiaries = n_distinct(beneficiary_name),
            tx_beneficiary_hhi = hhi_value(beneficiary_name, amount_zar), .groups="drop")

panel <- reduce(list(A,B,C,D,E,F_blk,G), left_join, by=K) %>%
  left_join(periods %>% select(entity_id, fy, fye_month, fy_start, fy_end, fy_complete, coverage), by=K) %>%
  left_join(trend_total, by="entity_id") %>%
  left_join(trend_wallet, by="entity_id") %>%
  left_join(recent_shift, by="entity_id") %>%
  left_join(sufficiency, by="entity_id") %>%
  right_join(ents, ., by="entity_id") %>%
  arrange(entity_id, fy)

zf <- intersect(ZERO_FILL, names(panel))
panel <- panel %>% mutate(across(all_of(zf), ~ coalesce(.x, 0))) %>%
  group_by(entity_id) %>%
  mutate(across(c(tx_value_total, tx_value_wallet, tx_count_wallet,
                  tx_avg_ticket_wallet, xb_value, tf_value),
                ~ if_else(fy - lag(fy) == 1, .x/lag(.x) - 1, NA_real_),
                .names = "{.col}_yoy")) %>% ungroup() %>%
  rename(count_yoy = tx_count_wallet_yoy, ticket_yoy = tx_avg_ticket_wallet_yoy) %>%
  mutate(growth_driver = case_when(
    is.na(tx_value_wallet_yoy) ~ NA_character_,
    abs(count_yoy)  > 2*abs(ticket_yoy) ~ "count",
    abs(ticket_yoy) > 2*abs(count_yoy)  ~ "ticket",
    TRUE ~ "mixed"))

stopifnot(isTRUE(all.equal(panel$tx_value_wallet + panel$tx_value_sweeps,
                           panel$tx_value_total)))
write_csv(panel, file.path(OUT, "feature_panel.csv"))
cat(" panel", nrow(panel), "x", ncol(panel), "| complete", sum(panel$fy_complete), "\n")

# 3. Track B - common trailing 12 month window, identical for all clients
# =============================================================================
cat("[3/9] track B snapshot\n")
WIN_END <- DATA_MAX; WIN_START <- WIN_END - years(1) + days(1)
PRIOR_END <- WIN_START - days(1); PRIOR_START <- PRIOR_END - years(1) + days(1)
inw <- function(d) d >= WIN_START & d <= WIN_END
inp <- function(d) d >= PRIOR_START & d <= PRIOR_END

snap <- ents %>%
  left_join(tx %>% filter(inw(date)) %>% group_by(entity_id) %>%
    summarise(tb_value_ttm = sum(amount_zar),
              tb_wallet_ttm = sum(amount_zar[leg_type != "intercompany_sweeps"]),
              tb_sweeps_ttm = sum(amount_zar[leg_type == "intercompany_sweeps"]),
              tb_count_ttm = n(),
              tb_count_wallet = sum(leg_type != "intercompany_sweeps"),
              tb_beneficiaries = n_distinct(beneficiary_name),
              tb_beneficiary_hhi = hhi_value(beneficiary_name, amount_zar),
              product_breadth = n_distinct(leg_type),
              channel_breadth = n_distinct(channel),
              mo_active_ttm = n_distinct(ym), .groups="drop"), by="entity_id") %>%
  left_join(tx %>% filter(inp(date)) %>% group_by(entity_id) %>%
    summarise(tb_value_prior = sum(amount_zar),
              tb_wallet_prior = sum(amount_zar[leg_type != "intercompany_sweeps"]),
              tb_count_prior = sum(leg_type != "intercompany_sweeps"), .groups="drop"),
    by="entity_id") %>%
  left_join(xb %>% filter(inw(date)) %>% group_by(entity_id) %>%
    summarise(xb_value_ttm = sum(value_zar), xb_count_ttm = n(),
              xb_countries = n_distinct(counterparty_country),
              xb_ccy_pairs = n_distinct(ccy),
              xb_country_hhi = hhi_value(counterparty_country, value_zar), .groups="drop"),
    by="entity_id") %>%
  left_join(xb %>% filter(inp(date)) %>% group_by(entity_id) %>%
    summarise(xb_value_prior = sum(value_zar), .groups="drop"), by="entity_id") %>%
  left_join(tf %>% filter(inw(date)) %>% group_by(entity_id) %>%
    summarise(tf_value_ttm = sum(value_zar), tf_count_ttm = n(), tf_tenor_wavg = weighted.mean(tenor_days, value_zar), tf_countries = n_distinct(counterparty_country), .groups="drop"), by="entity_id") %>%
  left_join(tf %>% filter(inp(date)) %>% group_by(entity_id) %>%
    summarise(tf_value_prior = sum(value_zar), .groups="drop"), by="entity_id") %>%
  left_join(tx %>% filter(inw(date), leg_type %in% c("payroll","tax")) %>%
    group_by(entity_id, .lt = clean_lab(leg_type)) %>%
    summarise(mo = n_distinct(ym), v = sum(amount_zar), .groups="drop") %>%
    pivot_wider(names_from=.lt, values_from=c(mo,v), names_glue="{.lt}_{.value}", values_fill=0), by="entity_id") %>%
  left_join(trend_total, by="entity_id") %>%
  left_join(trend_wallet, by="entity_id") %>%
  left_join(recent_shift, by="entity_id") %>%
  mutate(across(c(payroll_mo, tax_mo, payroll_v, tax_v, tb_sweeps_ttm,
                  tf_value_ttm, tf_count_ttm, xb_value_ttm, xb_count_ttm),
                ~ coalesce(.x, 0)),
         payroll_cadence = pmin(payroll_mo/12, 1),
         tax_cadence = pmin(tax_mo/12, 1),
         has_payroll_primacy = payroll_cadence >= 0.5,
         wallet_yoy = tb_wallet_ttm/tb_wallet_prior - 1,
         count_yoy = tb_count_wallet/tb_count_prior - 1,
         ticket_yoy = (tb_wallet_ttm/tb_count_wallet)/(tb_wallet_prior/tb_count_prior) - 1,
         growth_driver = case_when(abs(count_yoy) > 2*abs(ticket_yoy) ~ "count", abs(ticket_yoy) > 2*abs(count_yoy) ~ "ticket", TRUE ~ "mixed"),
         tb_yoy = tb_value_ttm/tb_value_prior - 1,
         xb_yoy = xb_value_ttm/xb_value_prior - 1,
         tf_yoy = tf_value_ttm/tf_value_prior - 1,
         sweep_share = tb_sweeps_ttm/tb_value_ttm,
         data_confidence = if_else(mo_active_ttm < 12, "insufficient", confidence_tier(tb_count_ttm, MIN_ROWS_TTM, MIN_ROWS_LOW_TTM)),
         data_sufficient = data_confidence == "high",
         total_value_ttm = tb_wallet_ttm + xb_value_ttm + tf_value_ttm,
         total_incl_sweeps = tb_value_ttm + xb_value_ttm + tf_value_ttm,
         window_start = WIN_START, window_end = WIN_END) %>%
  arrange(desc(total_value_ttm))

stopifnot(isTRUE(all.equal(snap$tb_wallet_ttm + snap$tb_sweeps_ttm, snap$tb_value_ttm)))
write_csv(snap, file.path(OUT, "track_b_snapshot.csv"))
cat("wallet base R", round(sum(snap$total_value_ttm)/1e9,1), "bn\n", sep="")

# 4. external financials - gate, then FX normalise per IAS 21
# =============================================================================
cat("[4/9] external financials\n")
ed <- read_csv(file.path(RAW, "afs_financials_ZAR_normalized.csv"), show_col_types=FALSE) %>%
  mutate(fye = as.Date(fiscal_year_end), fy = as.integer(format(fye, "%Y")))

quarantine <- tibble(); qtn <- function(r, why)
  if (nrow(r)) quarantine <<- bind_rows(quarantine, r %>% mutate(reason = why))

conf <- ed %>% group_by(entity_id, fye) %>%
  filter(n() > 1, n_distinct(total_revenue) > 1 | n_distinct(total_assets) > 1) %>% ungroup()
if (nrow(conf) > 0) {
  qtn(conf, "DUPLICATE_KEY_CONFLICTING")
  ed <- ed %>% anti_join(conf %>% distinct(entity_id, fye), by=c("entity_id","fye"))
}
ed <- ed %>% distinct(entity_id, fye, .keep_all = TRUE)

# A balance-sheet value matching exactly across two entities is not coincidence at this precision - one company's figures pasted into another's rows
contam <- tibble()
for (cl in intersect(c("total_assets","equity","total_borrowings"), names(ed))) {
  h <- ed %>% filter(!is.na(.data[[cl]]), .data[[cl]] > 0) %>%
    group_by(.v = .data[[cl]]) %>% filter(n_distinct(entity_id) > 1) %>%
    ungroup() %>% select(-.v)
  if (nrow(h)) contam <- bind_rows(contam, h)
}
if (nrow(contam) > 0) {
  contam <- distinct(contam); qtn(contam, "CROSS_ENTITY_CONTAMINATION")
  ed <- ed %>% anti_join(contam %>% distinct(entity_id, fye), by=c("entity_id","fye"))
}

# A line identical across 3+ years is a copy, not a measurement. Null the FIELD, keep the row.
for (cl in intersect(c("inventory","total_borrowings","payables","receivables", "interest_expense"), names(ed))) {
  bad <- ed %>% group_by(entity_id) %>%
    filter(sum(!is.na(.data[[cl]])) >= 3, n_distinct(.data[[cl]], na.rm=TRUE) == 1) %>%
    pull(entity_id) %>% unique()
  if (length(bad)) ed[[cl]][ed$entity_id %in% bad] <- NA_real_
}

# IAS 21: P&L at AVERAGE rate, balance sheet at CLOSING rate. One rate for both embeds the year's FX drift (7.2% for USD/ZAR in 2025) into every mixed ratio
fxr <- if (file.exists(file.path(RAW,"fx_rates.csv")))
  read_csv(file.path(RAW,"fx_rates.csv"), show_col_types=FALSE) %>%
    filter(!is.na(zar_per_unit)) %>% mutate(rate_date = as.Date(rate_date)) else
  tibble(currency=character(), rate_date=as.Date(character()), rate_type=character(), zar_per_unit=numeric())

ed <- ed %>%
  left_join(fxr %>% filter(rate_type=="average") %>%
              select(reporting_currency=currency, fye=rate_date, r_avg=zar_per_unit),
            by=c("reporting_currency","fye")) %>%
  mutate(rate_close = if_else(reporting_currency=="ZAR", 1, fx_rate_to_zar),
         rate_avg = case_when(reporting_currency=="ZAR" ~ 1, !is.na(r_avg) ~ r_avg, TRUE ~ fx_rate_to_zar),
         fx_basis = case_when(reporting_currency=="ZAR" ~ "ZAR", !is.na(r_avg) ~ "IAS21 avg+closing", TRUE ~ "APPROX period-end for flow"),
         fx_drift_pct = round(100*(rate_close/rate_avg - 1), 2))

FLOW <- intersect(c("total_revenue","cogs","interest_expense","ebitda"), names(ed))
STOCK <- intersect(c("inventory","total_borrowings","total_assets","equity",
                     "payables","receivables"), names(ed))
ed <- ed %>% mutate(across(all_of(FLOW), ~ .x*rate_avg,   .names="{.col}_z"), across(all_of(STOCK), ~ .x*rate_close, .names="{.col}_z"))

# cogs > revenue is legitimate in a loss year (Anglo FY2024, ~$3.8bn impairments)
ed <- ed %>% mutate(ok_scale = total_revenue_z > 1e9 & total_revenue_z < 5e12,
  ok_eq = is.na(equity_z) | is.na(total_assets_z) | equity_z <= total_assets_z,
  keep = ok_scale & ok_eq & !is.na(total_revenue_z))
if (any(!ed$keep)) qtn(ed %>% filter(!keep), "FAILS_SCALE_OR_IDENTITY")
ed <- ed %>% filter(keep)

extp <- ed %>% transmute(entity_id, entity_name,
  financial_year = paste0("FY", fy), fy, fiscal_year_end = fye, reporting_currency,
  revenue_zar = total_revenue_z, cost_of_sales_zar = cogs_z,
  inventory_zar = inventory_z, total_assets_zar = total_assets_z,
  equity_zar = equity_z, debt_zar = total_borrowings_z,
  payables_zar = payables_z, receivables_zar = receivables_z,
  interest_expense_zar = interest_expense_z, ebitda_zar = ebitda_z,
  foreign_revenue_pct, rate_avg, rate_close, fx_basis, fx_drift_pct,
  source_quality = if_else(data_quality == "primary_verified", "VERIFIED - primary AFS", "aggregator - unverified"), usable = TRUE)

if (nrow(quarantine) == 0)
  quarantine <- tibble(entity_id=character(), reason=character())
write_csv(extp, file.path(OUT, "external_panel.csv"))
write_csv(quarantine, file.path(OUT, "external_quarantine.csv"))
cat(" ", nrow(extp), "rows |", n_distinct(extp$entity_id), "entities |",
    sum(grepl("VERIFIED", extp$source_quality)), "verified | quarantined",
    nrow(quarantine), "\n")

# 5. wallet sizing - peer frontier benchmarking
# =============================================================================
cat("[5/9] wallet sizing\n")
FRONTIER_Q <- 0.90; TRIM <- 0.10; CAP_MULTIPLE <- 5

sized <- panel %>% filter(fy_complete) %>%
  select(entity_id, entity_name, sector, peer_group, fy, data_confidence, tx_value_wallet, xb_value, tf_value) %>%
  inner_join(extp %>% select(entity_id, fy, source_quality, revenue_zar, cost_of_sales_zar, foreign_revenue_pct), by=c("entity_id","fy")) %>%
  mutate(foreign_revenue_zar = revenue_zar * foreign_revenue_pct/100)

PILLARS <- tribble(~pillar, ~observed, ~driver,
  "transactional","tx_value_wallet","revenue_zar", "trade_finance","tf_value","cost_of_sales_zar", "cross_border","xb_value","foreign_revenue_zar")

size_pillar <- function(p, oc, dc) {
  d <- sized %>% transmute(entity_id, entity_name, sector, peer_group, fy, data_confidence, source_quality, pillar = p,
                           observed = .data[[oc]], driver = .data[[dc]]) %>%
    filter(!is.na(driver), driver > 0, !is.na(observed), observed > 0)
  if (nrow(d) < 3) return(tibble())
  d <- d %>% mutate(intensity = observed/driver)
  # trim before taking the percentile: the ratio is heavy tailed because the driver can be small, so an untrimmed percentile is set by whichever client has the smallest denominator rather than by achievable penetration.
  pool <- d %>% filter(data_confidence=="high") %>% pull(intensity)
  if (length(pool) < 5) pool <- d$intensity
  lo <- quantile(pool, TRIM, na.rm=TRUE); hi <- quantile(pool, 1-TRIM, na.rm=TRUE)
  pt <- pool[pool >= lo & pool <= hi]; if (length(pt) < 3) pt <- pool
  fr <- unname(quantile(pt, FRONTIER_Q, na.rm=TRUE))
  d %>% mutate(frontier_intensity = fr, raw_estimate = driver*fr,
    # FLOOR at observed (a client above the frontier is best-in-class, not
    # "capturing more than its wallet"); CAP at 5x observed (no client is
    # credibly assumed to have more than 5x its current flow available).
    estimated_wallet = pmin(pmax(raw_estimate, observed), observed*CAP_MULTIPLE),
    cap_binding = raw_estimate > observed*CAP_MULTIPLE,
    at_frontier = observed >= raw_estimate,
    share = observed/estimated_wallet, gap = estimated_wallet - observed)
}

wallet <- pmap_dfr(list(PILLARS$pillar, PILLARS$observed, PILLARS$driver), size_pillar)

client_wallet <- wallet %>%
  group_by(entity_id, entity_name, sector, peer_group, fy, data_confidence, source_quality) %>%
  summarise(observed_total = sum(observed), estimated_total = sum(estimated_wallet), gap_total = sum(gap), share_total = sum(observed)/sum(estimated_wallet),
            pillars_sized = n(), .groups="drop") %>% arrange(desc(gap_total))

sens <- map_dfr(c(0.75,0.80,0.90,0.95,1.00), function(q) {
  pmap_dfr(list(PILLARS$pillar, PILLARS$observed, PILLARS$driver), function(p,o,dr) {
    d <- sized %>% transmute(entity_id, data_confidence, observed=.data[[o]], driver=.data[[dr]]) %>%
      filter(!is.na(driver), driver>0, !is.na(observed), observed>0) %>%
      mutate(intensity = observed/driver)
    if (nrow(d) < 3) return(tibble())
    pool <- d %>% filter(data_confidence=="high") %>% pull(intensity)
    if (length(pool) < 5) pool <- d$intensity
    lo <- quantile(pool,TRIM,na.rm=TRUE); hi <- quantile(pool,1-TRIM,na.rm=TRUE)
    pt <- pool[pool>=lo & pool<=hi]; if (length(pt)<3) pt <- pool
    fr <- unname(quantile(pt,q,na.rm=TRUE))
    d %>% mutate(gap = pmin(pmax(driver*fr, observed), observed*CAP_MULTIPLE) - observed)
  }) %>% group_by(entity_id) %>% summarise(gap=sum(gap), .groups="drop") %>%
    mutate(q=q, rank=rank(-gap, ties.method="min"))
})
rank_stab <- sens %>% select(entity_id,q,rank) %>%
  pivot_wider(names_from=q, values_from=rank, names_prefix="q") %>%
  mutate(rank_swing = do.call(pmax, across(starts_with("q"))) - do.call(pmin, across(starts_with("q"))))

write_csv(wallet, file.path(OUT,"wallet_by_pillar.csv"))
write_csv(client_wallet, file.path(OUT,"wallet_by_client.csv"))
write_csv(rank_stab, file.path(OUT,"wallet_sensitivity.csv"))
cat(" ", n_distinct(client_wallet$entity_id), "clients sized | max gap R",
    round(max(client_wallet$gap_total)/1e9,1), "bn | max rank swing ",
    max(rank_stab$rank_swing), "\n", sep="")

# 6. peer clustering - behavioural, not sectoral
# =============================================================================
cat("[6/9] clustering\n")
cdat <- snap %>% filter(data_sufficient)
FEATS <- c("sweep_share","payroll_cadence","xb_country_hhi","tb_beneficiary_hhi",
           "product_breadth","channel_breadth","count_yoy","ticket_yoy","trend_slope_wallet")
X <- cdat %>% select(all_of(FEATS))

sds <- apply(X, 2, sd, na.rm=TRUE)
X <- X %>% select(-all_of(names(sds)[sds == 0 | is.na(sds)]))
X <- X %>% mutate(across(everything(), ~ replace_na(.x, median(.x, na.rm=TRUE))))

# greedy de-correlation: while any pair exceeds 0.8, drop whichever member has the higher mean absolute correlation with everything else. Hand-picking one feature left a 0.95 pair standing
repeat {
  cm <- cor(X, use="pairwise.complete.obs"); diag(cm) <- NA
  if (all(abs(cm) <= 0.8, na.rm=TRUE) || ncol(X) <= 3) break
  idx <- which(abs(cm) == max(abs(cm), na.rm=TRUE), arr.ind=TRUE)[1,]
  mr <- colMeans(abs(cm), na.rm=TRUE)
  X <- X[, -(if (mr[idx[1]] >= mr[idx[2]]) idx[1] else idx[2]), drop=FALSE]
}
Xs <- scale(X)

d_mat <- dist(Xs); ks <- 2:min(5, nrow(Xs)-1)
sil <- map_dbl(ks, ~ mean(cluster::silhouette(kmeans(Xs, .x, nstart=50)$cluster, d_mat)[,3]))
k_best <- ks[which.max(sil)]
set.seed(42)   # re-seed: the silhouette loop above consumed random numbers
km <- kmeans(Xs, k_best, nstart=50)
cdat$cluster <- km$cluster

cluster_profile <- as_tibble(Xs) %>% mutate(cluster = km$cluster) %>%
  group_by(cluster) %>% summarise(across(everything(), mean), n = n(), .groups="drop")

clusters <- cdat %>% select(entity_id, entity_name, cluster) %>%
  group_by(cluster) %>%
  mutate(peer_names = map_chr(entity_name, ~ paste(setdiff(entity_name, .x), collapse=", "))) %>%
  ungroup() %>%
  bind_rows(snap %>% filter(!data_sufficient) %>%
    transmute(entity_id, entity_name, cluster = NA_integer_, peer_names = "not clustered - insufficient activity")) %>%
  left_join(ents %>% select(entity_id, peer_group), by="entity_id") %>%
  arrange(entity_id)

write_csv(clusters, file.path(OUT,"clusters.csv"))
write_csv(cluster_profile, file.path(OUT,"cluster_profile.csv"))
write_csv(tibble(feature = colnames(Xs)), file.path(OUT,"cluster_features.csv"))
cat(" k =", k_best, "on", ncol(Xs), "features (silhouette",
    round(max(sil),3), ")\n")

# 7. hidden markov model (hmm) - regime detection model (Baum-Welch EM + Viterbi, base R)
# =============================================================================
cat("[7/9] regime detection\n")
N_STATES <- 3; MIN_OBS <- 24; MIN_RUN <- 3; N_RESTART <- 5; BAND <- 0.5

dns <- function(x, mu, sd) dnorm(x, mu, max(sd, 1e-6)) + 1e-300

baum_welch <- function(x, K=N_STATES, iters=200, tol=1e-7) {
  Tn <- length(x)
  mu <- unname(quantile(x, seq(.15,.85,length.out=K))); sd <- rep(stats::sd(x), K)
  A <- matrix(.1/(K-1), K, K); diag(A) <- .9; pi <- rep(1/K, K); prev <- -Inf
  for (it in seq_len(iters)) {
    B <- sapply(seq_len(K), function(j) dns(x, mu[j], sd[j]))
    al <- matrix(0,Tn,K); be <- matrix(0,Tn,K); cf <- numeric(Tn)
    # scaled forward-backward: raw probabilities underflow within ~50 steps
    al[1,] <- pi*B[1,]; cf[1] <- 1/sum(al[1,]); al[1,] <- al[1,]*cf[1]
    for (t in 2:Tn) { al[t,] <- (al[t-1,] %*% A)*B[t,] cf[t] <- 1/sum(al[t,]); al[t,] <- al[t,]*cf[t] }
    be[Tn,] <- cf[Tn]
    for (t in (Tn-1):1) be[t,] <- (A %*% (B[t+1,]*be[t+1,]))*cf[t]
    ll <- -sum(log(cf)); g <- al*be; g <- g/rowSums(g)
    xi <- matrix(0,K,K)
    for (t in 1:(Tn-1)) xi <- xi + outer(al[t,], B[t+1,]*be[t+1,])*A
    A <- xi/rowSums(xi); pi <- g[1,]/sum(g[1,])
    for (j in seq_len(K)) { w <- g[,j]; mu[j] <- sum(w*x)/sum(w)
      sd[j] <- max(sqrt(sum(w*(x-mu[j])^2)/sum(w)), 1e-4) }
    if (abs(ll-prev) < tol) break
    prev <- ll
  }
  list(mu=mu, sd=sd, A=A, pi=pi, loglik=ll, iters=it)
}

viterbi <- function(x, mu, sd, A, pi) {
  Tn <- length(x); K <- length(mu)
  lB <- sapply(seq_len(K), function(j) log(dns(x, mu[j], sd[j])))
  lA <- log(A + 1e-300); dd <- matrix(0,Tn,K); ps <- matrix(0L,Tn,K)
  dd[1,] <- log(pi + 1e-300) + lB[1,]
  for (t in 2:Tn) { m <- dd[t-1,] + lA
    ps[t,] <- max.col(t(m), "first"); dd[t,] <- apply(m,2,max) + lB[t,] }
  s <- integer(Tn); s[Tn] <- which.max(dd[Tn,])
  for (t in (Tn-1):1) s[t] <- ps[t+1, s[t+1]]
  s
}

fit_hmm <- function(x, K=N_STATES, restarts=N_RESTART) {
  best <- NULL
  for (r in seq_len(restarts)) {
    xs <- if (r == 1) x else x + rnorm(length(x), 0, stats::sd(x)*.01)
    f <- try(baum_welch(xs, K), silent=TRUE)
    if (inherits(f,"try-error") || !is.finite(f$loglik)) next
    if (is.null(best) || f$loglik > best$loglik) best <- f
  }
  best
}

# Runs shorter than MIN_RUN absorb into a neighbour. The stop condition is
# length > 2, not > 1: with > 1 the loop cascades until the whole series is one
# state.
enforce_runs <- function(st, mr=MIN_RUN) {
  r <- rle(st); g <- 0
  while (any(r$lengths < mr) && length(r$lengths) > 2 && g < 50) {
    i <- which.min(r$lengths); r$values[i] <- r$values[if (i>1) i-1 else 2]
    r <- rle(inverse.rle(r)); g <- g+1
  }
  inverse.rle(r)
}

# it models the deseasonalised level, not the growth rate: growth here is mean-reverting noise (autocorrelation -0.64) with no persistent regimes while the level is highly persistent (+0.95).
mser <- tx %>% filter(leg_type != "intercompany_sweeps") %>%
  group_by(entity_id, ym) %>% summarise(v = sum(amount_zar), .groups="drop") %>%
  arrange(entity_id, ym) %>%
  mutate(mth = as.integer(substr(ym,6,7))) %>%
  group_by(entity_id) %>% mutate(em = mean(v)) %>%
  group_by(entity_id, mth) %>% mutate(si = mean(v)/first(em)) %>% ungroup() %>%
  mutate(v_adj = v/si) %>%
  group_by(entity_id) %>% arrange(ym, .by_group=TRUE) %>%
  mutate(z = as.numeric(scale(log(v_adj)))) %>% ungroup() %>% filter(is.finite(z))

paths <- mser %>% group_by(entity_id) %>% group_modify(~ {
  if (nrow(.x) < MIN_OBS) return(tibble())
  f <- fit_hmm(.x$z); if (is.null(f)) return(tibble())
  st <- enforce_runs(viterbi(.x$z, f$mu, f$sd, f$A, f$pi))
  bind_cols(.x %>% select(ym, v, v_adj, z),
            tibble(state_raw=st, loglik=f$loglik, em_iters=f$iters,
                   persist=mean(diag(f$A))))
}) %>% ungroup()

# labels anchored to ZERO, not to within-client rank. Rank-based labelling gave every client one of each state by construction, which labelled a client shrinking 21% as "Stable".
labelled <- paths %>% group_by(entity_id, state_raw) %>%
  mutate(state_mean = mean(z)) %>% ungroup() %>%
  mutate(state = case_when(state_mean < -BAND ~ "Declining", state_mean >  BAND ~ "Growing", TRUE ~ "Stable"))

state_summary <- labelled %>% group_by(entity_id) %>% arrange(ym, .by_group=TRUE) %>%
  summarise(current_state = last(state),
            months_in_state = { r <- rle(state); tail(r$lengths,1) },
            last_transition = { r <- rle(state)
              if (length(r$lengths) < 2) NA_character_
              else ym[length(ym) - tail(r$lengths,1) + 1] },
            n_transitions = length(rle(state)$lengths) - 1,
            n_months = n(), pct_declining = mean(state=="Declining"),
            pct_stable = mean(state=="Stable"), pct_growing = mean(state=="Growing"),
            z_first6 = mean(head(z,6)), z_last6 = mean(tail(z,6)),
            persistence = first(persist), em_iters = first(em_iters), .groups="drop") %>%
  left_join(snap %>% select(entity_id, entity_name, sector, data_confidence,
                            tb_wallet_ttm, wallet_yoy, trend_slope_wallet, recent_vs_prior, payroll_cadence), by="entity_id")

# ensemble: each model votes on the direction and the verdict is the majority. then counting decline signals alone scored a client growing 19% as "0/3", which reads as the models disagreeing when it means no model flags a problem
DIR_BAND <- 0.02
state_summary <- state_summary %>% mutate(
  vote_hmm = current_state,
  vote_trend = case_when(trend_slope_wallet < -0.002 ~ "Declining", trend_slope_wallet >  0.002 ~ "Growing", TRUE ~ "Stable"),
  vote_yoy = case_when(wallet_yoy < -DIR_BAND ~ "Declining", wallet_yoy >  DIR_BAND ~ "Growing", TRUE ~ "Stable"),
  n_declining = (vote_hmm=="Declining")+(vote_trend=="Declining")+(vote_yoy=="Declining"),
  n_growing = (vote_hmm=="Growing")+(vote_trend=="Growing")+(vote_yoy=="Growing"),
  n_stable = 3 - n_declining - n_growing,
  consensus = case_when(n_declining >= 2 ~ "Declining", n_growing >= 2 ~ "Growing", n_stable >= 2 ~ "Stable", TRUE ~ "Split"),
  agreement = pmax(n_declining, n_growing, n_stable),
  confirmation = paste0(agreement, "/3 ", consensus),
  n_confirm = n_declining,
  trajectory = case_when(n_declining == 0 ~ "n/a", recent_vs_prior < wallet_yoy ~ "still worsening",
                         recent_vs_prior > wallet_yoy + .02 ~ "easing", TRUE ~ "flat"))

# transition matrix keyed by label (latent state numbers are arbitrary per client). Laplace smoothing: with about 35 observations an unseen transition means "not yet observed", not "impossible" and a zero row would collapse A^h
trans <- labelled %>% group_by(entity_id) %>% arrange(ym, .by_group=TRUE) %>%
  mutate(next_state = lead(state)) %>% filter(!is.na(next_state)) %>%
  count(entity_id, from_state = state, to_state = next_state)
trans <- expand_grid(entity_id = unique(trans$entity_id),
                     from_state = c("Declining","Stable","Growing"), to_state   = c("Declining","Stable","Growing")) %>%
  left_join(trans, by=c("entity_id","from_state","to_state")) %>%
  mutate(n = coalesce(n, 0) + 1) %>%
  group_by(entity_id, from_state) %>% mutate(prob = n/sum(n)) %>% ungroup()

write_csv(labelled %>% select(entity_id, ym, v, v_adj, z, state, state_mean), file.path(OUT,"hmm_state_path.csv"))
write_csv(state_summary, file.path(OUT,"hmm_state_summary.csv"))
write_csv(trans, file.path(OUT,"hmm_transitions.csv"))
cat(" median transitions", median(state_summary$n_transitions), "| persistence", round(mean(state_summary$persistence),3), "\n")

# 8. forward simulation - P(state at t+h) = current %*% A^h
# =============================================================================
cat("[8/9] forward simulation\n")
HORIZONS <- c(6,12,24); STATES <- c("Declining","Stable","Growing")
mat_pow <- function(M,n) { R <- diag(nrow(M)); for (i in seq_len(n)) R <- R %*% M; R }

forecast <- map_dfr(unique(trans$entity_id), function(e) {
  tm <- trans %>% filter(entity_id == e)
  A <- matrix(0,3,3, dimnames=list(STATES,STATES))
  for (i in seq_len(nrow(tm))) A[tm$from_state[i], tm$to_state[i]] <- tm$prob[i]
  rs <- rowSums(A); rs[rs==0] <- 1; A <- A/rs
  cur <- state_summary$current_state[state_summary$entity_id == e]
  if (length(cur) != 1 || !cur %in% STATES) return(tibble())
  v0 <- setNames(as.numeric(STATES == cur), STATES)
  map_dfr(HORIZONS, function(h) {
    p <- as.numeric(v0 %*% mat_pow(A,h))
    tibble(entity_id=e, horizon_months=h, current_state=cur, p_declining=p[1], p_stable=p[2], p_growing=p[3])
  })
}) %>%
  left_join(snap %>% select(entity_id, entity_name, sector, data_confidence,
                            tb_wallet_ttm, wallet_yoy), by="entity_id") %>%
  mutate(most_likely = STATES[max.col(cbind(p_declining,p_stable,p_growing),"first")],
         concentration = pmax(p_declining,p_stable,p_growing),
         # below 0.45 the chain has converged to its stationary distribution and the forecast is no longer client-specific
         informative = concentration > 0.45,
         rands_at_risk_bn = round(tb_wallet_ttm * p_declining / 1e9, 2))

write_csv(forecast, file.path(OUT,"regime_forecast.csv"))
cat(" ", n_distinct(forecast$entity_id), "entities x", length(HORIZONS),
    "horizons\n")

# 9. opportunity ranking - named plays, not a blended score
# =============================================================================
cat("[9/9] opportunity ranking\n")
plays <- snap %>%
  select(entity_id, entity_name, sector, data_confidence, wallet_ttm = tb_wallet_ttm,
         wallet_yoy, recent_vs_prior, payroll_cadence, sweep_share, xb_country_hhi,
         growth_driver, count_yoy, ticket_yoy) %>%
  left_join(ents %>% select(entity_id, peer_group), by="entity_id") %>%
  left_join(state_summary %>% select(entity_id, current_state, last_transition, consensus, agreement, confirmation, n_confirm, trajectory), by="entity_id") %>%
  left_join(clusters %>% select(entity_id, cluster, peer_names), by="entity_id") %>%
  left_join(client_wallet %>% group_by(entity_id) %>% slice_max(fy, n=1) %>% ungroup() %>%
              select(entity_id, share_total, gap_total, estimated_total), by="entity_id") %>%
  left_join(forecast %>% filter(horizon_months == 12) %>%
              select(entity_id, p_decline_12m = p_declining), by="entity_id") %>%
  mutate(
    play = case_when(
      data_confidence == "insufficient" ~ "MONITOR - too little activity to assess",
      wallet_yoy < -0.05 & recent_vs_prior < wallet_yoy ~ "DEFEND URGENT - losing share, still accelerating away",
      wallet_yoy < -0.05 ~ "DEFEND - lost share, decline now easing",
      !is.na(p_decline_12m) & p_decline_12m > 0.35 & current_state != "Declining"
        ~ "WATCH - elevated probability of decline within 12 months",
      payroll_cadence < 0.5 & wallet_yoy > 0.05 ~ "WIN THE MANDATE - growing with us, banks elsewhere",
      payroll_cadence < 0.5 ~ "CROSS-SELL - operating mandate sits with a competitor",
      !is.na(share_total) & share_total < 0.5 ~ "GROW - large untapped wallet at an existing relationship",
      xb_country_hhi > 0.20 ~ "DIVERSIFY - cross-border flow concentrated in few corridors",
      TRUE ~ "MAINTAIN - healthy, no immediate action"),
    rands_at_stake = coalesce(gap_total, wallet_ttm * abs(coalesce(wallet_yoy,0))),
    urgency = case_when(grepl("^DEFEND URGENT",play)~1, grepl("^DEFEND",play)~2,
                        grepl("^WATCH",play)~3, grepl("^WIN THE",play)~4,
                        grepl("^GROW",play)~5, grepl("^CROSS-SELL",play)~6,
                        grepl("^DIVERSIFY",play)~7, grepl("^MONITOR",play)~9, TRUE~8)) %>%
  arrange(urgency, desc(rands_at_stake)) %>%
  mutate(evidence = pmap_chr(list(wallet_ttm, wallet_yoy, recent_vs_prior, payroll_cadence, growth_driver, share_total, current_state, confirmation, p_decline_12m),
    function(w,y,r,p,dv,s,st,cf,pd) {
      b <- c(sprintf("wallet R%.2fbn", w/1e9), sprintf("YoY %+.1f%%", 100*y),
             sprintf("recent %+.1f%%", 100*r), sprintf("%s-driven", dv),
             sprintf("payroll cadence %.2f", p))
      if (!is.na(s)) b <- c(b, sprintf("share %.0f%%", 100*s))
      if (!is.na(st)) b <- c(b, sprintf("regime %s", st))
      if (!is.na(pd)) b <- c(b, sprintf("P(decline 12m) %.0f%%", 100*pd))
      if (!is.na(cf)) b <- c(b, cf)
      paste(b, collapse="; ") }))

write_csv(plays, file.path(OUT,"opportunity_ranking.csv"))

heat <- wallet %>% group_by(entity_id) %>% filter(fy == max(fy)) %>% ungroup() %>%
  select(entity_name, pillar, gap) %>% mutate(gap_bn = round(gap/1e9,2)) %>%
  select(-gap) %>% pivot_wider(names_from=pillar, values_from=gap_bn, values_fill=0)
write_csv(heat, file.path(OUT,"heatmap_gaps.csv"))

# summary
# =============================================================================
cat("\n=== complete in", round(as.numeric(difftime(Sys.time(), t0, units="secs"))), "seconds ===\n")
cat("clients :", nrow(ents), "\n")
cat("wallet base TTM : R", round(sum(snap$total_value_ttm)/1e9,1), "bn\n", sep="")
cat("clients sized :", n_distinct(client_wallet$entity_id), "\n")
cat("regime states :", paste(names(table(state_summary$current_state)), table(state_summary$current_state), sep="=", collapse="  "), "\n")
cat("ensemble 3/3 agreement :", sum(state_summary$agreement == 3), "\n")
cat("\noutputs in", OUT, ":\n"); print(list.files(OUT, pattern="\\.csv$|\\.json$"))
