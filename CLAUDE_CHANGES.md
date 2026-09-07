<span style="font-size: 1.6em; font-weight: bold;">
SHLectures — Claude Changes
</span>

## Session 1 Viridis Meridian | 2026-07-09 11:05 JST

SHL-01 — lec_slides/2026/01.qmd:554, 665, 801, 928 | dea | Discovered: 2026-07-09 | Verified: yes

Issue
:   Four chunks (PPPIncomePlot, PPPIncomePlotLessOilRich, GDPIncomePlot, GDPIncomePlotBetterAnnotation) carried fig-width: 12 and fig-height: 7. Each chunk assigns p and prints nothing, so the options had no effect on any output.

Why
:   Figure options act on the chunk that emits the plot. These chunks emit none — p is printed later, in a separate bare r chunk inside the columns block. The options were inert from the day they were written.

Before
:   ```{r GDPIncomePlot, cache = T}
:   #| fig-width: 12
:   #| fig-height: 7
:   (blank)
:   library(WDI)

After
:   ```{r GDPIncomePlot, cache = T}
:   library(WDI)

Outcome
:   Confirmed removed — grep for fig-width: 12 returns nothing. Invalidates the knitr cache for all four chunks (knitr hashes every chunk option except include), so the next render re-runs four WDI network fetches. User accepted that cost in favour of clean source.

SHL-02 — lec_slides/2026/01.qmd:638, 778, 906, 1030 | frg | Discovered: 2026-07-09 | Verified: no — did not fix the reported symptom

Issue
:   The four bare r chunks that print p had no figure options, falling back to the revealjs default of 9x5 in. Intent was to size them to the slide so the reveal.js overflow scrollbar would disappear.

Why
:   Reasoned that a 480px figure plus margins overflowed the slide box. Set 11 x 5.5 in (1056 x 528 px) to fit inside the 1120px-wide 70% column on a 1600x900 slide. Dropped an earlier proposal of out-width: 100%, which would scale the figure up to the column width and grow its height proportionally.

Before
:   ```{r}
:   p

After
:   ```{r}
:   #| fig-width: 11
:   #| fig-height: 5.5
:   p

Outcome
:   Failed. User reports the scrollbar persists and that shrinking fig-height scales the figure and the scrollbar down together. Overflow is therefore not driven by the figure's intrinsic height. The explicit sizing is still correct and worth keeping, but it is not the fix. See CLAUDE_StandingIssues.md, issue 1.

# Sandbox

<!-- Raw per-edit notes. Promoted to canonical section at orderly sign-off. Append-only. -->
* lec_slides/2026/sky-add.scss:9-23 | (no scss:rules section) -> added .reveal div.cell-output-display figure img{display:block} + .quarto-figure-center img{margin-left:auto;margin-right:auto} | quarto renders plot img as display:inline; inline baseline strut adds 3px; div.cell-output-display has overflow-x:auto so overflow-y computes to auto -> vertical scrollbar. Verified post-render: over 3px->0px on all 4 slides, imgDisplay=block, imgW 964->977, centred photo gaps 33/33px still centred.
* style/quarto-fixes.scss:1 | (new file) | canonical shared source for quarto revealjs CSS fixes. Rule 1: .reveal div.cell-output-display figure img{display:block} kills the inline baseline strut that makes div.cell-output-display (overflow-x:auto => overflow-y computes to auto) render a phantom vertical scrollbar. Rule 2: restores margin auto centring for .quarto-figure-center, which display:block would left-align.
* lec_slides/2026/sky-add.scss:9 | (no scss:rules) -> /*-- scss:rules --*/ @import "../../style/quarto-fixes"; | single source of truth
* lec_slides/2024/sky-add.scss:9 | same, ../../style/quarto-fixes
* lec_slides/2026/RP/sky-add.scss:9 | same, ../../../style/quarto-fixes
* lec_slides/2024/RP/sky-add.scss:9 | same, ../../../style/quarto-fixes
* lec_slides/2026/01.qmd:542-560 | tpo | <summary>Generate...<details>* bullets</details></summary> -> <details open><summary>Generate...</summary> ::: {.nonincremental} bullets ::: </details> | tags were inverted: <details> nested inside <summary> has no <summary> child, so it rendered collapsed (prompt hidden) while incremental:true still made its 5 <li> fragments -> 5 key presses revealing nothing. .nonincremental could not be applied before because the list sat inside a raw-HTML block. Old block commented out, not deleted.
* lec_slides/2026/01.qmd:542,660,669 | spl | added .fragment fragment-index=1 (prompt div), =2 (70% column), =3 (30% column .nonincremental) | user-requested overlay order: prompt, figure, right column. Verified post-render: data-fragment-index 1/2/3 each present once, li class="fragment" count 0, screenshots at step 0 and step 3 correct, no scrollbar.
* lec_slides/2026/01.qmd:544 | com | <details open> -> <details> | user chose collapsed-by-click over expanded-on-appear. Verified post-render: 0 occurrences of "<details open>", figure reflows upward into the freed space, no scrollbar, fragment order 1/2/3 unchanged. Note: expanding the widget on stage pushes figure+column down (normal flow), it does not overlay.
* lec_slides/2026/01.qmd:808-819, 1066-1075 | spl | slides 23 and 25 right column: bullet was a <li class="fragment"> but the blockquote below it was not a list, so incremental:true never fragmented it and it was visible on slide entry -- the quote appeared BEFORE its own label. Wrapped label in ::: {.nonincremental .fragment fragment-index=1} and quote in ::: {.fragment fragment-index=2}. Verified post-render: 2 fragments per slide, li-as-fragment count 0; runtime state 0 presses=both hidden, 1 press=label only, 2 presses=both. Reveal renormalises data-fragment-index 1,2 -> 0,1.
* lec_slides/2026/01.qmd:713, 857, 984 | bug1 | clean_data <- clean_data[clean_data$pop > 5000000, ] -> country-level filter on INITIAL-YEAR population > 2e6 (pop_obs / first_year / init_pop / big_countries / %in%) | the old line filtered country-YEARS, not countries: any country below the cutoff early in the sample lost those years. Runtime-verified against WDI: Singapore 34y 1990-2023 -> 14y 2010-2023 (-20y, first exceeds 5M in 2010); Ireland 34y -> 4y 2020-2023 (-30y, first exceeds 5M in 2020); Japan unaffected (never below). Slide 22 chunk has no pop filter, which is why slides 22 and 23 disagreed. Threshold chosen from data: initial-year pop Luxembourg 381,850 < 2,000,000 < Singapore 3,047,132; 3M would clear Singapore by only 47,132 and 3.1M drops it. Latent NA bug on the old line (NA pop -> NA logical -> all-NA rows injected) does not fire: 0 NA pop rows in the current vintage. Old lines commented out.
* lec_slides/2026/01.qmd:732, 881, 1012 | com | removed "Norway" from oil_list / oil_rich / oil_list | user: Norway did not start out resource-reliant. Caveat recorded in code: the sample starts 1990 and Norwegian petroleum dates from 1971, so the distinction is not observable in this window.
* lec_slides/2026/01.qmd:803, 944 | com | subtitle "Pop > 5M, Non-Oil" -> "initial pop > 2M, Non-Oil" | captions had tracked the old row-wise 5M cutoff and would have misstated the sample.
* lec_slides/2026/01.qmd:1128-1260 | new | added slide 26 "GDP vs GNI". Chunk GDPvsGNI (cache = T) builds two plots sharing the slide filter (initial-year pop > 2M, non-oil): p_gni = GNI per capita line plot, ever-top-30 by GNI, Japan red, linear y; p_sc = GDP vs GNI scatter for 2023 on log-log axes with a dashed 45-degree GDP=GNI line, points grouped by wedge = 100*(gdp/gni-1), labels only for |wedge| > 15% plus Japan. Layout 35%/35%/30% columns, right column .nonincremental with a JA note on GDP vs GNI and Ireland IP/HQ relocation. Data facts embedded: Ireland 2015 GDP pc PPP +23.5% YoY (largest in series), 2023 wedge +37.0%, 2007-09 crisis fall -12.1%. Norway has GDP but no GNI for 2023 so it is absent from the scatter (documented in the chunk).
* lec_slides/2026/01.qmd:1216, 1250 | com | left panel labelled all ~50 countries (illegible at 35% width) -> top 20 of final year + Japan; scatter subtitle was one line and clipped at the panel edge ("...plus Ja") -> split to two lines via sep="\n" and plot.subtitle size 7.
* lec_slides/2026/01.qmd:1236-1258 | spl | scatter y axis: GDP per capita (log) -> wedge = 100*(gdp/gni - 1) in %. geom_abline(45 deg) -> geom_hline(0). x stays log (incomes span $1,559-$129,555); y cannot be logged since deviations go negative. Rationale: on log-log a 37% wedge is a small vertical offset and Ireland read as unremarkable; with the deviation on y, Ireland (+37%) and Singapore (+22%) separate cleanly from a rich-country cluster sitting at ~0. Verified by screenshot after re-render.
* lec_slides/2026/01.qmd:1213-1235 | spl | scatter annotation rule extended. Kept clean_data column `income` from WDI extra=TRUE (the World Bank income CLASSIFICATION, renamed inc_grp to avoid colliding with the earlier chunks where `income` means GDP pc). lab_data now = abs(wedge)>15 OR (inc_grp in Low/Lower-middle income AND abs(wedge)>10) OR Japan. First pass used one-sided wedge>10; user corrected to abs, which adds Togo (-12.0%, remittances) alongside Guinea (+11.8%) and Lebanon (+10.5%). 10 labels total. Verified by rendering and screenshot.
* lec_slides/2026/01.qmd:1240 | com | OPEN cosmetic inconsistency: colour groups still split at +/-15% while labels split at +/-10% for poorer countries, so Guinea/Lebanon/Togo are annotated but drawn grey under the legend key "within 15%".
* lec_slides/2026/01.qmd:1240-1259 | spl | scatter colour groups now follow the label rule: added group "low income, |dev| > 10%" (#e69f00) so Guinea, Lebanon and Togo are visible instead of grey70 background noise. Hoisted low_grp above its first use — the edit had placed the definition 10 lines AFTER d23$grp used it, which would have errored at render. Renamed legend key "within 15%" -> "other": the old text was clipped at the panel edge ("within threshol").
* .scratch/escape_income.qmd (WDI fetch) | bug | c("income"=NY.GDP.PCAP.KD)+extra=TRUE -> alias "gdppc", drop group col, rename to income | extra=TRUE adds a CHARACTER income-GROUP column named "income" that clobbers the numeric indicator alias; income becomes "Low income" and numeric ops error. Verified: cached rds had a single character "income" col; after fix the numeric series returns. Same pattern latent in 01.qmd:~875.
* .scratch/ProduFunction.qmd (slides 4-5) | bug | ::: {.fragment fragment-index=N} -> ::: {.fragment .nonincremental fragment-index=N} | incremental:true auto-fragments the dt/dd/li inside with NO index, revealed after the indexed fragments, scrambling order. Verified 0 stray dt/dd/li fragments after fix; index->text sequence correct.
* .scratch/ProduFunction.qmd:12 | rul | incremental: true -> # incremental: true (commented) | user asked to not set global incremental. Output-neutral: every list is explicitly wrapped (::: {.incremental} on slides 2-3; .fragment + explicit fragment-index on pages 4-5). Re-rendered exit 0; page-4 indices remain 1-11, 0 stray fragments. .nonincremental on pages 4-5 now redundant but kept (harmless).
* .scratch/ProduFunction.qmd | new | merged in escape_income.qmd as section 1 (# 所得の成長, 4 slides) above the production-function slides (# 生産関数). Added {=html} tinytable-nowrap style block, execute warning/message:false, per-slide fig-width/height. R chunks (EscapeIncomePlot, EscapeeTable, FastGrowers) verbatim from escape_income.qmd; read cached wdi_pcap.rds from .scratch. Rendered exit 0, all 3 income slides PNG-verified no overflow.
* .scratch/ProduFunction.qmd (低所得->中所得 slide) | spl | wrapped EscapeIncomePlot + success text in ::::: {.columns} 70/30; fig 13x5.8 -> 9x5.4; scale_x breaks seq(1960,2020,20) + expand mult right 0.25->0.12 (was spanning to ~2040); nudge_x 5->2; removed labs subtitle (clipped in narrow panel) -> folded "Red = Japan" into title.
* .scratch/ProduFunction.qmd (EscapeeTable chunk) | new | added Group B sustained growers with 1990 start via rbind onto escapees(~1960); setdiff drops China (dup); start/latest columns carry base year; theme_html class table-hover -> table-borderless (no horizontal row lines per user); tt width 0.6 -> 0.8.
* .scratch/ProduFunction.qmd (FastGrowers chunk) | spl | year filter >=2000 -> >=1960 (both panels start 1960); scale_y dollar_format() -> label_dollar(accuracy=1) on log+nat (drops .00 on log axis); slide header 2000-2023 -> 1960-2023.
* .scratch/ProduFunction.qmd (section dividers + factors slide + style block) | new | # headers gained {.center .sectiontitle background-image=... background-opacity=0.4}; K/H/L/M ## header gained {.factors}; CSS: .sectiontitle h1 centered+text-shadow, .factors .columns align-items flex-start (top-align) + 0.4em vertical gaps.
* .scratch/ProduFunction.qmd (EscapeeTable + new GrowerTable chunks) | new | split the single 移行国 table into TWO stacked borderless tables: escapees(~1960, to Dominican) then growers(1990). Shared mk_disp()/style_tab() helpers; theme_html class table-borderless (no hor lines); CSS .reveal .tinytable margin .1rem for minimal inter-table gap.
* .scratch/ProduFunction.qmd (page-11 factors, 4 blocks + CSS + header) | spl | each factor = fragment DIV letter above a .columns [def | bullets] row; .factors .columns align-items flex-start (top-align, def level with bullets), .factors p margin .15em, header gained {.smaller}. Reveal index order preserved 1..11. NB: use fragment DIV not inline [\$K\$]{.fragment} span; and verify with >=60s print budget (MathJax timing).
* .scratch/ProduFunction.qmd (factors CSS + slides 11/12) | spl | factor col widths set via CSS .factors .column:first/last-child flex-basis 48%/52% !important (not per-div edits, so page5 70/30 untouched); removed font-size cap (full size); moved $M$ block to the former F slide -> p11 K,H,L / p12 M,F, indices renumbered 1..3 (M) and 4..5 (F).
* .scratch/ProduFunction.qmd (page5 + page6 tables + page7) | spl | page5: 2-col wrap, scale_x breaks seq(1960,2020,20) expand .08, subtitle->title. page6: style_tab tt(width=c(.30,.15,.17,.14,.14)) identical both tables (aligned cols, wider col1); mk_disp puts start/end YEAR in headers, cells = "$amt" only. page7: year>=1960, label_dollar(accuracy=1) x2, header 1960-2023.
* .scratch/ProduFunction.qmd (YAML date + EscapeeTable width) | tpo/spl | date literal "2026年8月4日(火)" -> "Invalid Date"; fix = ISO date + date-format "YYYY年M月D日(ddd)" + lang:ja. Table width vector tightened to c(.22,.11,.13,.10,.11).
* lec_slides/2026/01.qmd:1387 | new | added p_gni2 after p_sc: d23$ratio=gdp/gni; ggplot y=ratio, geom_hline yint 1, labs y="GDP / GNI", dropped scale_y % formatter; p_sc unchanged | Tag: com — user wanted a GDP/GNI (not GDP/GNI-1) variant, keep current
* lec_slides/2026/ai_agent_pitfall.qmd | new | reveal.js deck (8 slides, sky+hiragino) on the aggregation-error episode, grounded in Faia Eq(19) p.12 | Tag: com
* lec_slides/2026/01.qmd:1390 | (none) → `lab_data$ratio <- lab_data$gdp / lab_data$gni` | lab_data copied at L1346 before d23$ratio added at L1389; geom_text_repel base aes y=ratio failed (tpo)
