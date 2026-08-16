# =============================================================================
# 09_charts.R -- DECK GRAPHICS
# Run AFTER synbank_pipeline.R. Saves PNGs to out/figs/, numbered in deck order.
# =============================================================================
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(purrr); library(ggplot2) })

OUT <- "out"; FIG <- file.path(OUT,"figs")
dir.create(FIG, showWarnings = FALSE, recursive = TRUE)
COL <- c(Declining="red2", Stable="darkgrey", Growing="springgreen")

rd <- function(f) if (file.exists(file.path(OUT,f)))
  read_csv(file.path(OUT,f), show_col_types=FALSE) else NULL

snap <- rd("track_b_snapshot.csv"); hmm <- rd("hmm_state_summary.csv")
path <- rd("hmm_state_path.csv"); clu <- rd("clusters.csv")
pill <- rd("wallet_by_pillar.csv"); fc <- rd("regime_forecast.csv")
ents <- rd("entities.csv")
stopifnot(!is.null(snap), !is.null(path))

# raw ledger, deduped the same way the pipeline does (charts 3-5 need it)
tx <- read_csv("transactional_banking.csv",
               col_types = cols(date=col_date("%Y-%m-%d"), memo=col_character(),
                                reference=col_character(), .default=col_guess()),
               progress = FALSE) %>% distinct()

ym_date <- function(x) as.Date(paste0(substr(as.character(x),1,7), "-01"))
path <- path %>% mutate(d = ym_date(ym))

thm <- theme_minimal(base_size=12) +
  theme(plot.title=element_text(face="bold", size=14),
        plot.subtitle=element_text(colour="grey35", size=10),
        plot.caption=element_text(colour="grey50", size=8, hjust=0),
        legend.position="top")
sv <- function(p, f, w=12, h=7) {
  ggsave(file.path(FIG,f), p, width=w, height=h, dpi=200, bg="white")
  cat("  ", f, "\n") }

#01 small multiples: whole book, three years, regime-decoded 
bands <- path %>% group_by(entity_id) %>% arrange(d, .by_group=TRUE) %>%
  group_modify(~ { r <- rle(.x$state); e <- cumsum(r$lengths); s <- e - r$lengths + 1
  tibble(state=r$values, x1=.x$d[s],
         x2=c(.x$d[e[-length(e)]+1], max(.x$d)+31)) }) %>% ungroup() %>%
  left_join(distinct(snap, entity_id, entity_name), by="entity_id")
pth <- path %>% left_join(distinct(snap, entity_id, entity_name), by="entity_id")

sv(ggplot() +
     geom_rect(data=bands, aes(xmin=x1, xmax=x2, ymin=-Inf, ymax=Inf, fill=state), alpha=.25) +
     geom_line(data=pth, aes(d, v/1e9), linewidth=.5, colour="#2C3E50") +
     facet_wrap(~entity_name, scales="free_y", ncol=5) +
     scale_fill_manual(values=COL, name="Regime") +
     labs(title="Every client, three years, regime-decoded",
          subtitle="Monthly third-party wallet flow; background = hidden Markov state",
          x=NULL, y="ZAR bn",
          caption="Free y-scale: client levels differ by three orders of magnitude.") +
     thm + theme(strip.text=element_text(size=8, face="bold"),
                 axis.text=element_text(size=6)),
   "01_regime_small_multiples.png", 15, 9)

# 02 regime timeline: when the book moved
ord <- hmm %>% arrange(pct_declining, desc(pct_growing)) %>% pull(entity_name)
sv(bands %>% mutate(entity_name=factor(entity_name, levels=ord)) %>%
     ggplot(aes(y=entity_name)) +
     geom_rect(aes(xmin=x1, xmax=x2, ymin=as.numeric(entity_name)-.4,
                   ymax=as.numeric(entity_name)+.4, fill=state)) +
     scale_fill_manual(values=COL, name=NULL) +
     labs(title="When the book moved", subtitle="Decoded regime by month",
          x=NULL, y=NULL,
          caption="Ordered by time spent in decline. Transitions cluster in mid-2025.") + thm,
   "02_regime_timeline.png", 12, 7)

#03 indexed quarterly: size removed
q <- tx %>% filter(leg_type != "intercompany_sweeps") %>%
  mutate(qq = paste0(format(date,"%Y"), "Q", quarters(date))) %>%
  group_by(entity_id, qq) %>% summarise(v=sum(amount_zar), .groups="drop") %>%
  group_by(entity_id) %>% arrange(qq, .by_group=TRUE) %>%
  mutate(idx = 100*v/first(v)) %>% ungroup() %>%
  left_join(distinct(snap, entity_id, entity_name), by="entity_id")
hl <- c("Sanlam","Anglo American","Bid Corporation","OUTsurance Group","MTN Group")
sv(ggplot(q, aes(qq, idx, group=entity_name)) +
     geom_line(colour="grey85", linewidth=.5) +
     geom_line(data=q %>% filter(entity_name %in% hl), aes(colour=entity_name), linewidth=1.1) +
     geom_hline(yintercept=100, linetype=2, colour="grey50") +
     labs(title="Three years, indexed to 100", subtitle="Quarterly wallet flow, each client rebased to its own 2023Q3",
          x=NULL, y="Index", colour=NULL,
          caption="Grey = the other 15 clients. Indexing removes size so all 20 compare directly.") +
     thm + theme(axis.text.x=element_text(angle=45, hjust=1)),
   "03_indexed_quarterly.png", 12, 7)

#04 seasonality: why the quarterly comparison had to be like-for-like
sv(tx %>% filter(leg_type != "intercompany_sweeps") %>%
     mutate(qtr = paste0("Q", quarters(date))) %>%
     group_by(entity_id, qtr) %>% summarise(v=sum(amount_zar), .groups="drop") %>%
     group_by(entity_id) %>% mutate(rel = v/mean(v)) %>% ungroup() %>%
     left_join(distinct(snap, entity_id, entity_name, sector), by="entity_id") %>%
     ggplot(aes(qtr, rel, group=entity_name, colour=sector)) +
     geom_hline(yintercept=1, linetype=2, colour="grey60") +
     geom_line(alpha=.8, linewidth=.8) + geom_point(size=1.6) +
     facet_wrap(~sector, nrow=2) +
     labs(title="Why the quarterly comparison had to be like-for-like",
          subtitle="Value by calendar quarter, relative to each client's own average",
          x=NULL, y="Relative to own mean",
          caption="Consumer clients peak ~37% above average in Q4. Comparing H1 to the preceding H2 subtracts a Christmas peak from a January trough.") +
     thm + theme(legend.position="none"),
   "04_seasonality.png", 12, 6)

#05 leg-type mix: why sweeps are excluded
mix <- tx %>% group_by(leg_type) %>%
  summarise(value=sum(amount_zar), rows=n(), .groups="drop") %>%
  mutate(`share of value`=value/sum(value), `share of rows`=rows/sum(rows)) %>%
  select(leg_type, `share of value`, `share of rows`) %>% pivot_longer(-leg_type)
sv(ggplot(mix, aes(reorder(leg_type, value), value, fill=name)) +
     geom_col(position="dodge") + coord_flip() +
     scale_y_continuous(labels=scales::percent) +
     scale_fill_manual(values=c("share of value"="#2980B9","share of rows"="#BDC3C7"), name=NULL) +
     labs(title="Half the ledger is the client moving its own money",
          subtitle="Intercompany sweeps: ~50% of value, but not contestable spend",
          x=NULL, y=NULL,
          caption="Payroll is 0.6% of rows and 0.04% of value: its PRESENCE is the signal, not its size.") + thm,
   "05_leg_type_mix.png", 10, 5)

#06 growth decomposition: money moving vs client changing
sv(snap %>% left_join(hmm %>% select(entity_id, current_state), by="entity_id") %>%
     ggplot(aes(count_yoy, ticket_yoy, size=tb_wallet_ttm, colour=current_state)) +
     geom_hline(yintercept=0, linetype=2, colour="grey60") +
     geom_vline(xintercept=0, linetype=2, colour="grey60") +
     geom_point(alpha=.85) +
     geom_text(aes(label=entity_name), size=2.9, vjust=-1.4, show.legend=FALSE, colour="grey30") +
     scale_colour_manual(values=COL, name=NULL) +
     scale_size_continuous(guide="none", range=c(3,13)) +
     scale_x_continuous(labels=scales::percent) + scale_y_continuous(labels=scales::percent) +
     labs(title="Is the money moving, or is the client changing?",
          subtitle="Wallet change decomposed: transaction count against ticket size",
          x="Transaction count, YoY", y="Average ticket, YoY",
          caption="Sanlam: -21% count, +0.4% ticket -- flow redirected, not a shrinking client.") + thm,
   "06_growth_decomposition.png", 12, 7)

#07 primacy vs momentum: the two books
sv(snap %>% left_join(hmm %>% select(entity_id, current_state), by="entity_id") %>%
     ggplot(aes(wallet_yoy, payroll_cadence, size=tb_wallet_ttm, colour=current_state)) +
     annotate("rect", xmin=-Inf, xmax=0, ymin=.5, ymax=Inf, alpha=.06, fill="#C0392B") +
     annotate("rect", xmin=0, xmax=Inf, ymin=-Inf, ymax=.5, alpha=.06, fill="#27AE60") +
     annotate("text", x=-.15, y=1.06, label="RETENTION", size=3.4, colour="#C0392B", fontface="bold") +
     annotate("text", x=.15, y=.04, label="CROSS-SELL", size=3.4, colour="#27AE60", fontface="bold") +
     geom_point(alpha=.85) +
     geom_text(aes(label=entity_name), size=2.9, vjust=-1.4, show.legend=FALSE, colour="grey30") +
     scale_colour_manual(values=COL, name=NULL) +
     scale_size_continuous(guide="none", range=c(3,13)) +
     scale_x_continuous(labels=scales::percent) +
     labs(title="Two books, two conversations",
          subtitle="Momentum against operating-mandate primacy",
          x="Wallet YoY", y="Payroll cadence",
          caption="Top-left: shrinking while we hold the mandate. Bottom-right: growing while someone else does.") + thm,
   "07_primacy_momentum.png", 12, 7)

# 08 sweep share: two treasury archetypes
sv(snap %>% ggplot(aes(reorder(entity_name, sweep_share), sweep_share, fill=sector)) +
     geom_col() + coord_flip() +
     geom_hline(yintercept=c(.36,.54), linetype=2, colour="grey40") +
     scale_y_continuous(labels=scales::percent) +
     labs(title="Two treasury archetypes",
          subtitle="Share of transactional value that is the client sweeping its own accounts",
          x=NULL, y="Sweep share", fill=NULL,
          caption="The gap between the dashed lines is empty. A high share means Syn Bank holds the header account.") + thm,
   "08_sweep_share.png", 11, 7)

#09 ENSEMBLE AGREEMENT (new): three models, one verdict

if (all(c("vote_hmm","vote_trend","vote_yoy") %in% names(hmm))) {
  sv(hmm %>% select(entity_name, agreement, consensus, HMM=vote_hmm, `36m trend`=vote_trend, `Year-on-year`=vote_yoy) %>%
       pivot_longer(c(HMM, `36m trend`, `Year-on-year`), names_to="model", values_to="vote") %>%
       mutate(entity_name = reorder(entity_name, agreement)) %>%
       ggplot(aes(model, entity_name, fill=vote)) +
       geom_tile(colour="white", linewidth=.8) +
       geom_text(aes(label=substr(vote,1,1)), size=3, colour="white", fontface="bold") +
       scale_fill_manual(values=COL, name=NULL) +
       labs(title="Three models, one verdict",
            subtitle="Each method votes on direction; the ensemble takes the majority",
            x=NULL, y=NULL,
            caption="D = Declining, S = Stable, G = Growing. A 1-1-1 split is labelled Split rather than forced to a winner.") + thm,
     "09_ensemble_agreement.png", 9, 7)
}

# 10 FORECAST: probability of decline by horizon

if (!is.null(fc)) {
  fcw <- fc %>% select(entity_name, horizon_months, p_declining, tb_wallet_ttm) %>%
    mutate(horizon = factor(paste0(horizon_months, "m"), levels=c("6m","12m","24m")))
  ordf <- fc %>% filter(horizon_months==12) %>% arrange(p_declining) %>% pull(entity_name)
  sv(fcw %>% mutate(entity_name=factor(entity_name, levels=ordf)) %>%
       ggplot(aes(p_declining, entity_name, fill=horizon)) +
       geom_col(position="dodge", width=.75) +
       geom_vline(xintercept=.35, linetype=2, colour="#C0392B") +
       scale_x_continuous(labels=scales::percent) +
       scale_fill_manual(values=c("6m"="#F5B7B1","12m"="#E6736A","24m"="#C0392B"), name="Horizon") +
       labs(title="Probability of being in decline",
            subtitle="Forward simulation from the fitted transition matrix: P(state) = current x A^h",
            x="P(Declining)", y=NULL,
            caption="Assumes transition behaviour holds. Read the ordering as the finding; 24m approaches the stationary distribution.") + thm,
     "10_forecast_decline_risk.png", 11, 8)
  
  sv(fc %>% filter(horizon_months==12) %>%
       ggplot(aes(p_declining, tb_wallet_ttm/1e9, colour=current_state)) +
       geom_vline(xintercept=.35, linetype=2, colour="grey60") +
       geom_point(size=4, alpha=.85) +
       geom_text(aes(label=entity_name), size=2.9, vjust=-1.3, show.legend=FALSE, colour="grey30") +
       scale_colour_manual(values=COL, name=NULL) +
       scale_x_continuous(labels=scales::percent) +
       labs(title="Where the rands at risk actually are",
            subtitle="12-month decline probability against wallet size",
            x="P(Declining) at 12 months", y="Wallet TTM (ZAR bn)",
            caption="Top-right is the priority quadrant: large wallet, high probability of decline.") + thm,
     "11_rands_at_risk.png", 11, 7)
}

#12 PEER GROUP vs BEHAVIOURAL CLUSTER
# Industry peers answer "who competes alike"; clusters answer "who banks alike".
# Where they diverge is the interesting part.
if (!is.null(clu) && "peer_group" %in% names(clu)) {
  sv(clu %>% filter(!is.na(cluster)) %>%
       count(peer_group, cluster) %>%
       ggplot(aes(factor(cluster), peer_group, fill=n)) +
       geom_tile(colour="white", linewidth=.8) +
       geom_text(aes(label=n), size=4) +
       scale_fill_gradient(low="#EAF2F8", high="#2980B9", guide="none") +
       labs(title="Industry peers vs behavioural clusters",
            subtitle="Where a row spreads across columns, clients in the same industry bank differently",
            x="Behavioural cluster", y=NULL,
            caption="Clustering is not a sector relabelling: it groups by how clients use the bank, not what they sell.") + thm,
     "12_peer_vs_cluster.png", 9, 6)
}

# 13 opportunity heatmap
if (!is.null(pill)) {
  sv(pill %>% group_by(entity_id) %>% filter(fy==max(fy)) %>% ungroup() %>%
       ggplot(aes(pillar, reorder(entity_name, gap), fill=gap/1e9)) +
       geom_tile(colour="white", linewidth=.6) +
       geom_text(aes(label=round(gap/1e9,2)), size=3, colour="grey15") +
       scale_fill_gradient(low="#EAF2F8", high="#C0392B", name="Gap (ZAR bn)") +
       labs(title="Where the unclaimed wallet sits",
            subtitle="Estimated wallet minus captured flow, by client and pillar",
            x=NULL, y=NULL,
            caption="Estimates are floored at observed and capped at 5x, so no client shows a gap it could not credibly close.") + thm,
     "13_opportunity_heatmap.png", 10, 8)
}
