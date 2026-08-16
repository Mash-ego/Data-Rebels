# 09_charts.R
# =============================================================================

source("00_config.R")
library(readr); library(tidyr); library(ggplot2); library(purrr)

FIG <- file.path(OUT, "figs"); dir.create(FIG, showWarnings = FALSE, recursive = TRUE)
COL <- c(Declining = "red2", Stable = "darkgrey", Growing = "springgreen")

rd <- function(f) if (file.exists(file.path(OUT, f)))
  read_csv(file.path(OUT, f), show_col_types = FALSE) else NULL

snap <- rd("track_b_snapshot.csv"); hmm  <- rd("hmm_state_summary.csv")
path <- rd("hmm_state_path.csv");   clu  <- rd("clusters.csv")
tx   <- rd("clean_tx.csv.gz");      pill <- rd("wallet_by_pillar.csv")
stopifnot(!is.null(snap), !is.null(path))

ym_date <- function(x) as.Date(paste0(substr(as.character(x), 1, 7), "-01"))
path <- path %>% mutate(d = ym_date(ym))

thm <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(colour = "grey35", size = 10),
        plot.caption = element_text(colour = "grey50", size = 8, hjust = 0),
        legend.position = "top")

sv <- function(p, file, w = 12, h = 7) {
  ggsave(file.path(FIG, file), p, width = w, height = h, dpi = 200, bg = "white")
  cat("  saved", file, "\n")
}

cat("\n=== BUILDING DECK GRAPHICS ===\n")

# 01  SMALL MULTIPLES: every client's 3-year history with regime bands the single most informative image in the pack - the whole book at once, and the two red-tailed clients are visible without reading a number. free_y is essential: a R16bn client and a R0.01bn client cannot share a scale
# -----------------------------------------------------------------------------
bands <- path %>% group_by(entity_id) %>% arrange(d, .by_group = TRUE) %>%
  group_modify(~ {
    r <- rle(.x$state); e <- cumsum(r$lengths); s <- e - r$lengths + 1
    tibble(state = r$values, x1 = .x$d[s],
           x2 = c(.x$d[e[-length(e)] + 1], max(.x$d) + 31))
  }) %>% ungroup() %>%
  left_join(distinct(snap, entity_id, entity_name), by = "entity_id")

pth <- path %>% left_join(distinct(snap, entity_id, entity_name), by = "entity_id")

p1 <- ggplot() +
  geom_rect(data = bands, aes(xmin = x1, xmax = x2, ymin = -Inf, ymax = Inf,
                              fill = state), alpha = .25) +
  geom_line(data = pth, aes(d, v / 1e9), linewidth = .5, colour = "midnightblue") +
  facet_wrap(~ entity_name, scales = "free_y", ncol = 5) +
  scale_fill_manual(values = COL, name = "HMM regime") +
  labs(title = "Every client, three years, regime-decoded",
       subtitle = "Monthly third-party wallet flow. Background = hidden Markov state.",
       x = NULL, y = "ZAR bn",
       caption = "Free y-scale per client: levels differ by three orders of magnitude.") +
  thm + theme(strip.text = element_text(size = 8, face = "bold"), axis.text = element_text(size = 6))
sv(p1, "01_regime_small_multiples.png", 15, 9)


# 02  REGIME TIMELINE (Gantt). Shows WHEN the book moved, not just who moved. clients ordered by how much of the window they spent declining
# -----------------------------------------------------------------------------
ord <- hmm %>% arrange(pct_declining, desc(pct_growing)) %>% pull(entity_name)
p2 <- bands %>% mutate(entity_name = factor(entity_name, levels = ord)) %>%
  ggplot(aes(y = entity_name)) +
  geom_rect(aes(xmin = x1, xmax = x2, ymin = as.numeric(entity_name) - .4,
                ymax = as.numeric(entity_name) + .4, fill = state)) +
  scale_fill_manual(values = COL, name = NULL) +
  labs(title = "When the book moved",
       subtitle = "Decoded regime by month. Ordered by time spent in decline.",
       x = NULL, y = NULL,
       caption = "Transitions cluster in mid-2025 -- worth asking what changed then.") +
  thm
sv(p2, "02_regime_timeline.png", 12, 7)

# 03  INDEXED QUARTERLY WALLET. Indexing to 100 removes the size problem, so a R16bn client and a R0.2bn client are directly comparable on one axis
# -----------------------------------------------------------------------------
if (!is.null(tx)) {
  q <- tx %>% filter(leg_type != "intercompany_sweeps") %>%
    mutate(qq = paste0(format(date, "%Y"), "Q", quarters(date))) %>%
    group_by(entity_id, qq) %>% summarise(v = sum(amount_zar), .groups = "drop") %>%
    group_by(entity_id) %>% arrange(qq, .by_group = TRUE) %>%
    mutate(idx = 100 * v / first(v)) %>% ungroup() %>%
    left_join(distinct(snap, entity_id, entity_name), by = "entity_id") %>%
    left_join(hmm %>% select(entity_id, current_state), by = "entity_id")

  hl <- c("Sanlam","Anglo American","Bid Corporation","OUTsurance Group","MTN Group")
  p3 <- ggplot(q, aes(qq, idx, group = entity_name)) +
    geom_line(colour = "grey85", linewidth = .5) +
    geom_line(data = q %>% filter(entity_name %in% hl),
              aes(colour = entity_name), linewidth = 1.1) +
    geom_hline(yintercept = 100, linetype = 2, colour = "grey50") +
    labs(title = "Three years, indexed to 100",
         subtitle = "Quarterly wallet flow, each client indexed to its own 2023Q3.",
         x = NULL, y = "Index (2023Q3 = 100)", colour = NULL,
         caption = "Grey = the other 15 clients. Indexing removes size so all 20 compare directly.") +
    thm + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  sv(p3, "03_indexed_quarterly.png", 12, 7)


  # 04  SEASONALITY. The visual proof behind the corrected quarterly comparison: consumer clients peak ~37% above their own average in Q4, which is why and adjacent-half comparison read Bid Corporation at -23% when it is +18%.
  # ---------------------------------------------------------------------------
  ssn <- tx %>% filter(leg_type != "intercompany_sweeps") %>%
    mutate(qtr = paste0("Q", quarters(date))) %>%
    group_by(entity_id, qtr) %>% summarise(v = sum(amount_zar), .groups = "drop") %>%
    group_by(entity_id) %>% mutate(rel = v / mean(v)) %>% ungroup() %>%
    left_join(distinct(snap, entity_id, entity_name, sector), by = "entity_id")

  p4 <- ggplot(ssn, aes(qtr, rel, group = entity_name, colour = sector)) +
    geom_hline(yintercept = 1, linetype = 2, colour = "grey60") +
    geom_line(alpha = .8, linewidth = .8) + geom_point(size = 1.6) +
    facet_wrap(~ sector, nrow = 2) +
    labs(title = "Why the quarterly comparison had to be like-for-like",
         subtitle = "Value by calendar quarter, relative to each client's own average.",
         x = NULL, y = "Relative to own mean",
         caption = "Consumer clients peak ~37% above average in Q4. Comparing H1 against the preceding H2 subtracts a Christmas peak from a January trough.") +
    thm + theme(legend.position = "none")
  sv(p4, "04_seasonality.png", 12, 6)


  # 05  LEG-TYPE MIX. Shows sweeps as ~50% of ledger value, which is the whole justification for the wallet definition
  # ---------------------------------------------------------------------------
  mix <- tx %>% group_by(leg_type) %>%
    summarise(value = sum(amount_zar), rows = n(), .groups = "drop") %>%
    mutate(pct_value = value / sum(value), pct_rows = rows / sum(rows)) %>%
    select(leg_type, `share of value` = pct_value, `share of rows` = pct_rows) %>%
    pivot_longer(-leg_type)
  p5 <- ggplot(mix, aes(reorder(leg_type, value <- value), value, fill = name)) +
    geom_col(position = "dodge") + coord_flip() +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c("share of value" = "lightblue",
                                 "share of rows" = "#BDC3C7"), name = NULL) +
    labs(title = "Half the ledger is the client moving its own money",
         subtitle = "Intercompany sweeps are 49.6% of value but not contestable spend.",
         x = NULL, y = NULL,
         caption = "Payroll is 0.6% of rows and 0.04% of value: its PRESENCE is the signal, not its size.") +
    thm
  sv(p5, "05_leg_type_mix.png", 10, 5)
}


# 06  GROWTH DECOMPOSITION. Four quadrants, four different conversations: transactions leaving is a competitor taking business; tickets shrinking is the client itself changing. Same headline %, opposite recommendation
# -----------------------------------------------------------------------------
p6 <- snap %>%
  left_join(hmm %>% select(entity_id, current_state), by = "entity_id") %>%
  ggplot(aes(count_yoy, ticket_yoy, size = tb_wallet_ttm, colour = current_state)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey60") +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey60") +
  geom_point(alpha = .85) +
  geom_text(aes(label = entity_name), size = 2.9, vjust = -1.4,
            show.legend = FALSE, colour = "grey30") +
  scale_colour_manual(values = COL, name = NULL) +
  scale_size_continuous(guide = "none", range = c(3, 13)) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Is the money moving, or is the client changing?",
       subtitle = "Wallet change decomposed: transactions (x) against ticket size (y).",
       x = "Transaction count, YoY", y = "Average ticket, YoY",
       caption = "Sanlam: -21% count, +0.4% ticket -- flow redirected, not a shrinking client.") +
  thm
sv(p6, "06_growth_decomposition.png", 12, 7)


# 07  PRIMACY vs MOMENTUM. The two commercial groups, in one image.
# -----------------------------------------------------------------------------
p7 <- snap %>%
  left_join(hmm %>% select(entity_id, current_state), by = "entity_id") %>%
  ggplot(aes(wallet_yoy, payroll_cadence, size = tb_wallet_ttm, colour = current_state)) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = .5, ymax = Inf, alpha = .06, fill = "#C0392B") +
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = .5, alpha = .06, fill = "lightgreen") +
  annotate("text", x = -.15, y = 1.06, label = "RETENTION", size = 3.4,
           colour = "#C0392B", fontface = "bold") +
  annotate("text", x = .15, y = .04, label = "CROSS-SELL", size = 3.4,
           colour = "lightgreen", fontface = "bold") +
  geom_point(alpha = .85) +
  geom_text(aes(label = entity_name), size = 2.9, vjust = -1.4,
            show.legend = FALSE, colour = "grey30") +
  scale_colour_manual(values = COL, name = NULL) +
  scale_size_continuous(guide = "none", range = c(3, 13)) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Two books, two conversations",
       subtitle = "Momentum against operating-mandate primacy.",
       x = "Wallet YoY", y = "Payroll cadence",
       caption = "Top-left: shrinking while we hold the mandate. Bottom-right: growing while someone else does.") +
  thm
sv(p7, "07_primacy_momentum.png", 12, 7)


# 08  SWEEP SHARE. Bimodal, and stable year to year -- a treasury archetype, not noise.
# -----------------------------------------------------------------------------
p8 <- snap %>%
  ggplot(aes(reorder(entity_name, sweep_share), sweep_share, fill = sector)) +
  geom_col() + coord_flip() +
  geom_hline(yintercept = c(.36, .54), linetype = 2, colour = "grey40") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Two treasury archetypes",
       subtitle = "Share of transactional value that is the client sweeping its own accounts.",
       x = NULL, y = "Sweep share", fill = NULL,
       caption = "The gap between the dashed lines is empty: clients cluster at 0.11-0.36 or 0.54-0.59. A high share means Syn Bank holds the header account.") +
  thm
sv(p8, "08_sweep_share.png", 11, 7)


# 09  CROSS-MODEL CONFIRMATION. Rigor made visible.
# -----------------------------------------------------------------------------
p9 <- hmm %>%
  select(entity_name, HMM = sig_hmm, `36-month trend` = sig_trend,
         `Year-on-year` = sig_yoy, n_confirm) %>%
  pivot_longer(-c(entity_name, n_confirm)) %>%
  mutate(entity_name = reorder(entity_name, n_confirm)) %>%
  ggplot(aes(name, entity_name, fill = value)) +
  geom_tile(colour = "white", linewidth = .8) +
  scale_fill_manual(values = c(`TRUE` = "#C0392B", `FALSE` = "#ECF0F1"),
                    labels = c("no signal", "flags decline"), name = NULL) +
  labs(title = "Three models, independently built",
       subtitle = "Which methods flag each client as declining.",
       x = NULL, y = NULL,
       caption = "Sanlam and Anglo are flagged by all three. Disagreements are informative, not errors.") +
  thm
sv(p9, "09_cross_model.png", 9, 7)


# 10  CLUSTERS on the two most separating features.
# -----------------------------------------------------------------------------
if (!is.null(clu)) {
  p10 <- snap %>% left_join(clu %>% select(entity_id, cluster), by = "entity_id") %>%
    filter(!is.na(cluster)) %>%
    ggplot(aes(trend_slope_wallet, payroll_cadence,
               colour = factor(cluster), size = tb_wallet_ttm)) +
    geom_point(alpha = .85) +
    geom_text(aes(label = entity_name), size = 2.9, vjust = -1.5,
              show.legend = FALSE, colour = "grey30") +
    scale_size_continuous(guide = "none", range = c(3, 13)) +
    labs(title = "Behavioural peer groups",
         subtitle = "k-means on five de-correlated features; two most separating shown.",
         x = "36-month trend slope", y = "Payroll cadence", colour = "cluster",
         caption = "Clusters span sectors: this is behaviour, not industry relabelled.") +
    thm
  sv(p10, "10_clusters.png", 12, 7)
}

# -----------------------------------------------------------------------------
# 11  OPPORTUNITY HEATMAP -- only once wallet sizing has run.
# -----------------------------------------------------------------------------
if (!is.null(pill)) {
  d <- pill %>% group_by(entity_id) %>% filter(fy == max(fy)) %>% ungroup()
  p11 <- ggplot(d, aes(pillar, reorder(entity_name, gap), fill = gap / 1e9)) +
    geom_tile(colour = "white", linewidth = .6) +
    geom_text(aes(label = round(gap / 1e9, 2)), size = 3, colour = "grey15") +
    scale_fill_gradient(low = "#EAF2F8", high = "#C0392B", name = "Gap (ZAR bn)") +
    labs(title = "Where the unclaimed wallet sits",
         subtitle = "Estimated wallet minus captured flow, by client and pillar.",
         x = NULL, y = NULL) + thm
  sv(p11, "11_opportunity_heatmap.png", 10, 8)
} else {
  cat("  SKIPPED 11_opportunity_heatmap -- run 05_wallet.R first\n")
}

cat("\n", length(list.files(FIG)), "figures in", FIG, "\n")
cat("Numbered in deck order. 01 and 02 carry the regime story on their own.\n")
