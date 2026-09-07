<span style="font-size: 1.6em; font-weight: bold;">
SHLectures — Claude Task Record
</span>

<!-- Working memory: folder structure, file dependency, chunk maps, where to pick up.
     Not for bug fixes — those go in CLAUDE_CHANGES.md and CLAUDE_StandingIssues.md. -->

## Project layout

* Project root: `/mnt/c/seiro/docs/external/seishin/`
* Slides: `lec_slides/<year>/` — 2019, 2020, 2021, 2022, 2024, 2026
* Shared styling: `style/` — `hiragino.scss`, `toc-slide.html`, `toc-add.html`
* Lua filters: `filters/ListArrow.lua`; extension `_extensions/`
* Log files live at project root (not in an `analysis/` subdir — this is a lecture project)

## Files reviewed

### lec_slides/2026/01.qmd — 開発経済論: はじめに (86.6 KB, ~2300 lines)

Format
:   revealjs, standalone, embed-resources, `width: 1600`, `height: 900`, `incremental: true`
:   theme `[sky, sky-add.scss]`, css `../../style/hiragino.scss`
:   `execute: echo: false, freeze: auto`
:   Line endings are CRLF. Encoding UTF-8 (Japanese body text).

Cache
:   `01_cache/` holds knitr caches for the `cache = T` chunks. Invalidated 2026-07-09 for the four WDI chunks.
:   Rendering re-fetches from the World Bank WDI API — network dependency, not offline-reproducible.

Section map (slide headers, all `## AI利用の考え方と方法`)
:   325, 337 — 自衛
:   356, 387, 406, 440, 468 — 伸ばす
:   493 — 利用例: 文章の要約・翻訳
:   518 — 利用例: 制度の図示
:   537, 659, 797, 925 — 利用例: 計算・演算・プログラミングの委託 vibe coding
:   1050 — クラウドやプログラムの利用
:   1430 — 例1: 中国の塾授業料上限規制
:   1556 — 例2: ボックス禁止政策 Ban The Box
:   2225 — Child in the pond question

The four vibe-coding slides — shared structure
:   Each slide = one named builder chunk (`cache = T`) that assigns `p` and prints nothing,
    followed by a `::::: {.columns}` block whose 70% column holds a bare `{r}` chunk printing `p`,
    and whose 30% column holds Japanese commentary.

| Slide | Builder chunk | Builder line | Printer line |
|---|---|---|---|
| 537 | `PPPIncomePlot` | 554 | 638 |
| 659 | `PPPIncomePlotLessOilRich` | 665 | 778 |
| 797 | `GDPIncomePlot` | 801 | 906 |
| 925 | `GDPIncomePlotBetterAnnotation` | 928 | 1030 |

:   All four line numbers are post-edit (2026-07-09). Each builder pulls WDI indicators
    `NY.GDP.PCAP.PP.KD` or `NY.GDP.PCAP.KD` plus `SP.POP.TOTL`, filters pop > 5M and an oil-exporter
    list, takes "ever top 20/30", forces Japan red, and labels with `geom_text_repel`.

## Knitr / quarto facts worth not re-deriving

* knitr hashes chunk **code and options**; every option except `include` invalidates the cache.
  Changing a single space in the code also invalidates it. Source: <https://yihui.org/knitr/demo/cache/>
* Figure options only act on the chunk that *emits* the plot. On a build-then-print split,
  they must go on the printer.
* `out-width: "100%"` on a 1600px slide scales a figure *up* to the column width and grows its
  height proportionally — it is not a way to shrink a figure.

## Shared SCSS fixes

* Canonical: `style/quarto-fixes.scss`. Imported by all four themes via a `scss:rules` section:
  `@import "../../style/quarto-fixes";` (2024/, 2026/) and `"../../../style/quarto-fixes"` (*/RP/).
* Sass **can** `@import` across directories from a theme file — contrary to the note in `01.qmd`'s
  YAML, which is about `theme:` *list entries*, not `@import`. Verified by render + browser probe.
* Quarto has **no user-level config directory** (rejected upstream for reproducibility). Cross-repo
  reuse means copying `quarto-fixes.scss` into the new repo, or publishing a format extension.

## Verifying CSS in an embed-resources deck — do NOT grep

* `embed-resources: true` inlines the compiled theme as a **base64 `data:text/css` link**.
  `grep` for a selector in the `.html` returns 0 hits **even when the rule is present and applied.**
* This produced two false conclusions this session ("rules layer silently dropped", "@import fails").
* Always confirm a CSS rule via `getComputedStyle` in a headless-Chrome probe, never by grepping.

## Headless-Chrome DOM probing (reusable technique)

* `chrome.exe` is a **GUI-subsystem launcher stub**: it returns immediately and has no stdout.
  `--dump-dom` therefore writes nothing, whether launched from WSL or via `cmd.exe`.
  `--screenshot=<file>` works, but **asynchronously** — the batch file returns before the PNG lands.
* Chrome 149: `--headless=old` was removed (gone since 132) and exits silently. Use `--headless=new`.
* Working recipe: inject a `<script>` into a **copy** of the rendered HTML that paints its findings
  into a fixed-position `<pre>`, then `--screenshot` it and Read the PNG. Always pass a private
  `--user-data-dir`, else Chrome delegates to the running profile and does nothing.
* Scratch scripts kept in `.claude/.scratch/`: `inject_probe.py`, `inject_fixtest.py`,
  `inject_verify.py`, `run_*.bat`. `inject_fixtest.py` A/B-tests candidate CSS rules by swapping
  a `<style>` element and re-measuring — use it before committing any CSS fix.

## Population filter in the four WDI chunks (do not re-break)

* `PPPIncomePlot` (slide 22) has **no** population filter — by design, it is the "before" plot.
* Slides 23/24/25 filter on **initial-year population > 2,000,000**, decided once per country,
  then all years of that country are kept. The old `clean_data[clean_data$pop > 5e6, ]` filtered
  country-*years* and truncated Singapore (-20y) and Ireland (-30y).
* Threshold is data-derived: initial-year pop Luxembourg 381,850 < **2M** < Singapore 3,047,132.
  3M clears Singapore by only 47,132; 3.1M drops it. Do not raise it without re-checking.
* Norway is no longer in the oil lists (user's call). Slide 22 never excluded it.
* `.claude/.scratch/check_pop_filter.R` and `pick_threshold.R` reproduce both findings.
* Editing any of these three chunks invalidates its knitr cache and triggers a WDI refetch.

### Where to pick up

* 2026-09-05 (Vectorized Fox): `lec_slides/2026/01.qmd` — added `p_gni2` (= `p_sc` variant, y =
  GDP/GNI ratio not ratio-1; hline 0→1, label "GDP/GNI", %-format dropped) at end of the first
  GDP/GNI chunk; `p_sc` kept. Object is defined but NOT displayed — add `p_gni2` to a chunk to
  show it. Also created `lec_slides/2026/ai_agent_pitfall.qmd` (reveal.js, JP, on the
  markdown-aggregation error episode; rendered clean). Neither 01.html rebuilt yet.
* **RESOLVED:** the vertical scrollbar on slides 537 / 659 / 797 / 925. Root cause was *not* figure
  height. Quarto renders the plot `<img>` as `display: inline`; the inline baseline strut adds 3px;
  `div.cell-output-display` carries `overflow-x: auto`, and per CSS spec `overflow-y: visible` then
  computes to `auto` — so the container scrolls on 3px. Overflow was 3px at both 4.5in and 5.5in,
  which is why resizing never worked. Fixed in `sky-add.scss` (new `scss:rules` section).
  Verified post-render: over 0px on all four, bar absent from screenshot, image gained 13px width.
* **Open:** `style/toc-add.html:3` throws `TypeError: cloneNode of null` — `#toc-nav` is absent, so
  `toc_slide.remove()` never runs and the generated TOC slide stays in the deck. Reveal still inits.
* **Not re-rendered:** the 2024 decks and the other RP decks. Their `sky-add.scss` now imports the
  fix, but the committed `.html` files still carry the old CSS until each is rendered again.
* **Side effect this session:** `2026/RP/RP02.html` was re-rendered as the import test, so it now
  differs from its previous build (fix applied; also emits pre-existing warnings about a missing
  `../../seiro.css` and `GrootbergGiraffeHead.jpg` — not introduced by this change, but unverified).
* **Inconsistency, unresolved:** slide 537's printer chunk is `fig-height: 4.5` (line 640) while
  659 / 797 / 925 are `5.5`. Left as-is; the 4.5 was an unlogged 12:24 experiment, now moot.
* **Untouched by choice:** the `geom_text_repel` label crowding (`nudge_x = 5`, `direction = "y"`,
  `max.overlaps = Inf`) on slide 797 — the overlap is the pedagogical point of that slide.
* **Corrections to earlier notes:** `xref.sqlite` **does** exist at the project root (198 MB,
  765,882 refs across all lecture years). `CLAUDE_CC.md` now exists. There is still no `CLAUDE.md`.

### Where to pick up (Session 3 Copper Vireo, 2026-08-04)

* Two scratch decks/docs ready in `.claude/.scratch/`, NOT yet moved into the 2026 deck:
  * `escape_income.qmd` (html) -- escape-from-low-income table + escape line figure + two-panel fast-growers (log|natural). Data cached in `wdi_pcap.rds`/`.csv`; `escapees.csv` written.
  * `ProduFunction.qmd` (revealjs) -- production-function lecture. To render outside scratch, move it into `lec_slides/2026/` (assets already resolve there) OR keep the copied `.scratch/_extensions/tarleb/parse-latex`.
* Reusable scratch scripts: `growth_scan.R` (2000-2023 CAGR ranking), plus existing `lowinc_*.R`, `pick_threshold.R`.
* Open cosmetic: ProduFunction slide 2 col2 inline fragment overlaps in print-pdf only (see StandingIssues).
* To verify escape/growth numbers offline, load `.scratch/wdi_pcap.rds` (all-country panel) -- no API needed.
* **DONE 2026-08-04 07:40:** removed global `incremental: true` from `.scratch/ProduFunction.qmd` (commented at L12) per user; re-rendered (exit 0). Page-4 (`貧しい国における投入要素`) reveal order verified clean 1-11 in fresh HTML (K,外国,H,科学知識,学歴,L,大国,小国,M,関税,サプライ) -- the user-confirmed target. `.nonincremental` on pages 4-5 now redundant but kept. Still in `.scratch/`, not yet moved to `lec_slides/2026/`.
* **DONE 2026-08-04 07:48:** merged `escape_income.qmd` into `.scratch/ProduFunction.qmd` as a 2-section deck -- `# 所得の成長` (4 income slides) then `# 生産関数` (existing 4). R chunks read cached `wdi_pcap.rds` from `.scratch` (no WDI refetch). Rendered exit 0; all income slides PNG-verified no overflow. `escape_income.qmd` still exists standalone. Deck title unchanged ("国レヴェルの生産関数") -- may want to broaden to cover growth. Still not moved to `lec_slides/2026/`.

### Where to pick up (2026-08-04 ~10:25 JST)

* `.scratch/ProduFunction.qmd` is a complete 2-section reveal deck, rendered clean (12 slides), title now "低所得国の成長について":
  * `# 低所得国の成長実績` (centered title over dimmed GrootbergGiraffe2.jpg) -- 低所得→中所得以上 (2-col: escape line plot 1960-2023, x-ticks stop 2020, + success stat) ; 移行国 (two stacked BORDERLESS tables sharing a `tt(width=c(...))` vector so columns align: escapees ~1960 down to Dominican, growers 1990; year in header, cells value-only) ; 持続的高成長国 1960-2023 (2-panel log|natural, log y `label_dollar(accuracy=1)`)
  * `# 生産関数による理解` (centered over BirdNest.jpg) -- 企業 ; 集計 ; 貧しい国 K/H/L ; 貧しい国 M/F. Each factor: letter above a top-aligned `[def | bullets]` row; left col width via CSS `.factors .column:first/last-child` (currently 40/59).
  * Title-slide date: `date: 2026-08-04` + `date-format: "MMMM D, YYYY"` -> "August 4, 2026".
* Still in `.scratch/`, NOT moved to `lec_slides/2026/`. Reads cached `wdi_pcap.rds` (no WDI refetch). `pdf_to_png.R` = reusable pdftools PDF->PNG verifier.

## Quarto / reveal.js facts worth not re-deriving (2026-08-04)

* **Print-pdf verifying a MathJax deck**: chrome `--virtual-time-budget` must be >= 60000 (use 90000); below that MathJax has not typeset and every `$...$` renders BLANK in the PNG -> false "math missing" alarm. Math is in the HTML as raw \(...\) and typesets fine live.
* **Fresh chrome profile**: always pass a NEW `--user-data-dir`; a stale/locked one silently yields a ~1.5KB 1-page PDF stub.
* **Inline math in a fragment SPAN** `[$K$]{.fragment}` is fragile (once blanked all slide math); prefer a fragment DIV `::: {.fragment}` / `$K$` / `:::`.
* **Quarto title-slide date**: a non-ISO literal ("2026年8月4日(火)") -> "Invalid Date" (Day.js). Use ISO `date:` + `date-format:`; `lang: ja` + JP tokens still failed here; plain `"MMMM D, YYYY"` works.
* **Align columns across two tinytables**: give both the SAME `tt(width = c(...))` per-column vector.
* **.factors column widths** set via CSS `.column:first/last-child { flex-basis/width !important }` so page-5's own 59/40 columns stay untouched.
