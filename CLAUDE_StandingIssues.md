<span style="font-size: 1.6em; font-weight: bold;">
SHLectures — Standing Issues
</span>

# Session 1 Viridis Meridian | 2026-07-09

## lec_slides/2026/01.qmd

### <span style="color: gray;">~~1. Vertical scrollbar on the four vibe-coding slides — root cause unknown~~</span>

Resolved
:   Session 2 Cobalt Tern, 2026-07-09. Quarto renders the plot img as display:inline; the inline baseline strut adds 3px; div.cell-output-display carries overflow-x:auto, so overflow-y computes to auto per CSS spec and the container scrolls on those 3px. Overflow measured 3px at both 4.5in and 5.5in, which is why resizing could never work. Fixed by a new scss:rules section in sky-add.scss setting display:block on cell-output figure images. Verified after a real render: overflow 0px on all four slides, bar absent from screenshot, image reclaimed 13px of width. The Implication and Next checks below were wrong — the section never overflowed (scrollH = clientH = 900, overflow-y visible).

Location
:   Slides at 537, 659, 797, 925. Printer chunks at 638, 778, 906, 1030.

Symptom
:   reveal.js renders a vertical overflow scrollbar on each of the four GDP/PPP per-capita slides.

Ruled out
:   Figure intrinsic height. Setting fig-width: 11 / fig-height: 5.5 on the printing chunks did not remove the bar. User reports that shrinking the figure scales the figure and the scrollbar down together, so the overflow does not scale with the figure.

Implication
:   The scrollbar lives inside the reveal transform-scaled slide, and the overflowing content is something other than the plot. A constant-height contributor (heading block, columns padding, or a CSS rule fixing the section height) is the likely driver.

Next checks
:   1. Decisive isolation test — comment out the printer chunk on one slide and re-render. If the bar survives with no figure present, the figure is irrelevant and the cause is the heading/columns block.
:   2. Inspect the rendered 01.html in Chrome devtools: find which element scrolls, and read its computed height and overflow.
:   3. Grep style/hiragino.scss and sky-add.scss for any height or overflow rule on .reveal .slides section.
:   4. Check whether incremental: true adds hidden fragment height on these slides.

Do not
:   Apply overflow: hidden to .reveal .slides section. It removes the bar but silently clips the plot caption.

Notes
:   Slide geometry is width 1600, height 900 (set in YAML), not reveal's default 1050x700. An earlier diagnosis in this session assumed the default and was wrong.

# Sandbox

<!-- Raw notes on new issues as they surface. Promoted to canonical sections at orderly sign-off. Append-only. -->
* lec_slides/2026/01.qmd:537,659,797,925 | vertical bar: every CSS scrollbar source ruled out statically (no .scrollable on these sections, no runtime classList.add, viewport overflow:hidden) | premise-unverified — confirm with user what the bar is and which slides show it before further work
* lec_slides/2026/01.qmd:640 | slide 537 printer is fig-height 4.5 while 779/907/1031 are 5.5 — unlogged 12:24 edit, inconsistent | com
* style/toc-add.html:3 | Uncaught TypeError: querySelector("#toc-nav") returns null -> cloneNode throws -> toc_slide.remove() never executes, TOC slide stays in deck | bug (latent, reveal still inits)
* lec_slides/2026/01.qmd:1240 | scatter colour groups cut at +/-15% but labels cut at +/-10% for low/lower-middle income, so Guinea, Lebanon and Togo are annotated in grey under the legend key "within 15%" | cosmetic
* lec_slides/2026/01.qmd:1240 | RESOLVED 2026-07-09: colour groups now follow the label rule; low/lower-middle income with |dev|>10% get their own colour (#e69f00) and legend entry. Legend key shortened to "other" (was clipped).

## Session 3 Copper Vireo | 2026-08-04

### ProduFunction.qmd slide 2 (Kigyo no seisan kansu) col2 span overlaps in print-pdf
The inline [$K,H,L,M$ wo tsukatte...]{.fragment} span overlaps other text in print-pdf/PDF export (all fragments painted at once). Renders fine in live step-through. Cosmetic / print-only; convert to a block fragment if PDF export matters.

### Read tool cannot rasterize PDFs in this WSL
poppler-utils (pdftoppm) not installed, so Read errors on .pdf. Use headless-Chrome PNG screenshots, or apt-get install poppler-utils. Environment/tooling.

### api.worldbank.org intermittent 502
The WB API (used by the WDI R package) returned 502 for ~11 min this session; WDI then fails wholesale. Retry / poll; not a code bug. External.

Correction (2026-08-04, from salvaged Aug-03 recheck): this is DEGRADATION/overload, not an outage. A curl to api.worldbank.org returned HTTP 200 but took 59.4s. WDI's R client uses a 60s per-page timeout, so pages crossing that line error as timeouts and the gateway sheds others as 502. Levers: raise WDI(..., timeout=) above 60s, or just retry; fall back to the Data360 MCP (separate, healthy endpoint). Scratch: .scratch/drc_gdp_gni.R fetches per-indicator so one flaky page does not abort the whole batch.

### Verify-tooling: print-pdf false alarms (2026-08-04, com)
When PNG-verifying ProduFunction via headless-Chrome print-pdf: (a) `--virtual-time-budget` must be >= 60000 (use 90000) or MathJax has not typeset and all `$...$` render BLANK -> false "math missing"; (b) always pass a FRESH `--user-data-dir` or a stale lock yields a ~1.5KB 1-page PDF stub. Both are tooling artifacts, not deck bugs.
* 2026-09-08 07:35 JST | api.worldbank.org 502 (new variant): editing GDPvsGNI2 invalidated its long-stale knitr cache, exposing that raw_data can come back WITHOUT an "income" column when 502s hit the extra=TRUE metadata pages -- clean_data <- raw_data[..., c(...,"income")] then throws "undefined columns selected". Same root cause as the existing "api.worldbank.org intermittent 502" issue above (external, not a code bug), new failure shape (missing column vs wholesale failure). Direct curl confirms fast, consistent 502 (not just slow timeouts) as of this timestamp -- genuine outage, not degradation. No code fix applied; render deferred until the API recovers.
* lec_slides/2026/01.qmd:1873-1880ish (both GDPvsGNI and GDPvsGNI2 chunks) | slide overflow 943 vs clientH 900 (43px), unrelated to fig content -- offsetTop/offsetHeight probe shows the trailing `::: {.fragment fragment-index=3}` bullet ("アイルランド、シンガポール、アンゴラ、DRC: 外国人(企業)の国外への送金") ends at offsetTop(847)+offsetHeight(96)=943, exact match to scrollH. The columns/image block itself reports offsetHeight=58 scrollH=0 regardless of fig-height (tested 4.8 and 4.3, zero effect on total scrollH) -- likely auto-stretch:true (in this deck YAML) recalculating image size dynamically, ignoring chunk fig-height. Not caused by the 2026-09-08 p_traj edit: the trailing fragment and its position are pre-existing, untouched content; fig-height changes provably did not move scrollH. Unconfirmed whether this predates 2026-09-08 entirely (no baseline screenshot of the old p_sc version exists to compare) -- flagged, not fixed.

# Sandbox
* lec_slides/2026/ZhaiAICogAbility_ja.qmd + AICriticalThinking_ja.qmd | both decks rendered and overflow-clean but NOT adversarially verified (claim-verifier pass deferred to next session by user, as a deliberate baseline-vs-verified comparison) | open
* AICriticalThinking_ja.qmd | 8 studies (Fisher, Yatani, Darvishi, Fan, Deng, Pardos, Karatas, Heung) cited only via Vendrell & Johnston's one-clause summaries; labelled 原典未確認 on the slides but originals still unread | open
* AICriticalThinking_ja.qmd | Stadler et al. (2024) n=91 randomisation and Kosmyna et al. (2025) n=54/18 confirmed from publication record via WebSearch only; full texts unread | open
* ~~lec_slides/2026 decks not adversarially verified~~ RESOLVED 2026-09-10: two claim-verifier agents run in parallel, both returned FAIL, corrections applied to *_verified.qmd
* ~~Stadler et al. (2024) cited without reading the full text~~ RESOLVED 2026-09-10: user challenged it, PDF downloaded to lec_slides/papers/AICogAbility/Stadler2024_CognitiveEase.pdf and read; task, scoring scheme and limitations now on the slides from the source
* AICriticalThinking_ja_verified.qmd | 8 studies (Fisher, Yatani, Darvishi, Fan, Deng, Pardos, Karatas, Heung) still known only via Vendrell & Johnston's one-clause summaries; labelled 原典未確認 on the slides but originals unread | open
* AICriticalThinking_ja_verified.qmd | Kosmyna et al. (2025) design and n confirmed from the publication record only; full text unread | open
* ~~AIOverconfidence_ja.qmd | states the peer-review prior as universal~~ RESOLVED 2026-09-14 Session 7 Slate Pangolin: new slide 「ただし層の厚さは雑誌ごとに違う」 added after 査読を通るとは何か — no 共著者 layer (sole author), MDPI Societies, submitted 2024-10-14 → published 2025-01-03 (~11 weeks vs ~200d median), correction later issued. Also added the Correction notice (Societies 15(9):252, doi 10.3390/soc15090252) to the 盲従も誤り slide
* ~~8 studies known only via Vendrell & Johnston's one-clause summaries~~ PARTLY RESOLVED 2026-09-11: the two that mattered (Deng meta-analysis, Heung & Chiu meta-analysis) obtained and read; both were far stronger than V&J's gloss implied and overturned the deck's premise. Still unread: Fisher (arXiv 2410.06415), Yatani (arXiv 2409.09*), Darvishi (Computers & Education 210:104967), Fan (BJET 56(2):489-530), Pardos & Bhandari, Karatas (Educ Inf Technol) -- none now load-bearing in any deck
* ~~Kosmyna full text unread~~ RESOLVED 2026-09-11: arXiv 2506.08872 downloaded and read; randomisation confirmed, and the 83.3% quoting-failure result recovered (no summary anywhere reported it)
* ~~AIOverconfidence_ja.qmd | states the peer-review prior as universal~~ RESOLVED 2026-09-14 Session 7 Slate Pangolin: new slide 「ただし層の厚さは雑誌ごとに違う」 added after 査読を通るとは何か — no 共著者 layer (sole author), MDPI Societies, submitted 2024-10-14 → published 2025-01-03 (~11 weeks vs ~200d median), correction later issued. Also added the Correction notice (Societies 15(9):252, doi 10.3390/soc15090252) to the 盲従も誤り slide
* ~~ZhaiAICogAbility_ja_verified.qmd + AICriticalThinking_ja_verified.qmd | both still frame the benefit side as thin~~ RESOLVED 2026-09-14 Session 7 Slate Pangolin. Two parts, and the entry was WRONG about one of them:
  * ZhaiAICogAbility_ja_verified.qmd never framed the benefit side as thin at all — grep over all 349 lines returns ZERO hits for Deng/Heung/Kosmyna/Stadler, and its 解釈と評価 + まとめ (253-349) charge only Zhai's own quality. No change made or needed.
  * AICriticalThinking_ja_verified.qmd did, in 8 places (337, 340, 343-344, 372, 376, 385, 403, plus the Kosmyna 未読 line at 260). All corrected; 4 new/rewritten slides carry Deng's design ladder instead.
* ~~all three decks re-rendered but NOT overflow-probed — running Chrome blocks headless~~ WITHDRAWN 2026-09-14, my diagnosis was wrong. Chrome was never blocked. Two separate mistakes made it look that way: (a) `--screenshot` lands ASYNCHRONOUSLY (already recorded at CLAUDE_TaskRecord.md:84-86) and I stat'd the PNG before Chrome had written it; (b) my probe injector replaced the FIRST `</body>`, which occurs inside reveal's bundled JS, terminating that script and rendering the rest of the page as literal text — all three PNGs came out byte-identical at 114,589 as a result. Fixed by inserting at `rindex($t,"</body>")`. All three decks then probed 0 overflow. The "既存のブラウザ セッションで開いています" message from `--version` is unrelated to `--screenshot` runs.
* PROBE-GOTCHA 2026-09-14 | injecting a probe `<script>` into a rendered reveal deck MUST target the LAST `</body>`. reveal's bundled JS contains the literal string `</body>`; a first-match insertion breaks the enclosing script silently — the page still screenshots, so it looks like a working probe returning a blank/garbled report. Symptom: all decks produce identically-sized PNGs | com
* Deng et al. (2025) | the meta-analysis codes randomisation, sample size and SQAC quality score for all 62 studies but never moderates on any of them (Table 8 p.13 covers only stage/subject/setting/duration/mode), so "does g+=0.712 survive among the 39 randomised / 16 well-controlled / 8 with N>200" is unanswerable from the paper though the data exist. Chase only if the user wants the primary studies opened | open, not started
* ~~Still unread: Fisher, Yatani, Darvishi, Fan, Pardos & Bhandari, Karatas~~ RESOLVED 2026-09-14 Session 8 Ivory Marmot: all six obtained and read in full (user supplied Darvishi, Fan, Karatas; the rest fetched from arXiv/PLOS). Extracts with page markers at .claude/.scratch/{fisher,yatani,pardos,darvishi,fan,karatas}.txt; summary at lec_slides/papers/AICogAbility/SixStudies_VJ_gloss_check.md. The 原典未確認 label no longer applies to any study in the set.
* Vendrell & Johnston (2026) gloss reliability | 4 of 6 glosses checked this session misrepresent their source: Yatani 2024 is a conceptual framework paper with no study, sample or statistic but is glossed as an empirical over-reliance finding; Karatas 2024 is a 13-student qualitative interview study glossed as evidence of language development; Pardos & Bhandari 2024 is a randomised trial whose AI condition BEAT the control while human-tutor help did not, glossed under creativity/problem-solving with no statistic; Fan 2024 is mixed (AI group had the largest essay improvement, p-adj <= .037 vs all three other groups, but knowledge gain and transfer were null, transfer eta2 = 0.000) and is glossed as pure harm. Darvishi and Fisher survive intact (Fisher's gloss is accurate; only V&J's reference-list entry mangles its title). Treat any remaining V&J-sourced claim as unverified until the original is read | com
* ~~Kosmyna critique dismissed as "査読前のプレプリント"~~ RESOLVED 2026-09-14 Session 8 Ivory Marmot: user forbade dismissing work for being un-peer-reviewed. Stankovic et al. (arXiv:2601.00856, Dec 2025) downloaded, read, and given its own slide. Its substantive charges: the authors' own effect size implies N=159 but they had 54 (18 by session 4); some figures rest on 2-4 essays; FDR correction described only as applied "when multiple comparisons were involved"; F-statistics omitted; 55 completed but 54 analysed with no exclusion reason given; and critically the matching procedure behind the 83.3% correct-quoting figure is never described. PDF at lec_slides/papers/AICogAbility/Stankovic2025_CommentOnKosmyna.pdf, extract at .claude/.scratch/kosmyna_critique.txt
* ~~Kosmyna session-4 direction contradiction~~ WITHDRAWN 2026-09-14 Session 8 Ivory Marmot: checked against the original PDF (kosmyna.txt p.46-47 results, p.150 discussion) and the charge does NOT reproduce - both places give 7/9 correct for the self-directed-first group and 1/9 for the AI-first group. Stankovic et al. may have read an earlier revision. Original entry: | Stankovic et al. report that Kosmyna p.37 gives HIGHER quoting correctness for the LLM-to-Brain-only group while the discussion states the opposite. Deck B's closing recommendation (自分で書いてから、AIに当てる) rests on the session-4 result, so the deck's final advice is built on a passage its own source contradicts internally. Deck now flags this on the 批判 slide. NOT independently verified against the Kosmyna PDF itself - the 216-page original has not been re-opened for this passage | open
* Deck authoring: bold discipline | 2026-09-14 Session 8 Ivory Marmot - Deck B had accumulated 114 bold spans over 19 slides (~6/slide), which the user flagged as meaningless emphasis, naming the confidence interval [-0.14, 1.02] and its explanatory sentence. Rule going forward for authored slides: ONE bold span per slide, on the single claim the slide turns on. Do not bold labels (測定/処置/課題/標本), ordinary terms (対照群/実験室/効果量), whole sentences, or numbers already sitting alone in a table cell. Same principle as the chat-reply rule in feedback_response_style.md, applied to qmd decks | com
* Deck overflow: check .smaller BEFORE trimming content | 2026-09-14 Session 8 Ivory Marmot - spent three render/probe cycles cutting lines from 著者自身の警告 (949/900) with no effect, because the slide simply lacked the {.smaller} class. Adding it dropped the slide to 618. Routine when a slide overflows: (1) does it have .smaller? add it; (2) is there a displayed MathJax fraction? a $\dfrac{}{}$ costs roughly 60-70px versus the inline ÷ form; (3) only then cut content | com
* Translation register for lecture decks | 2026-09-14 Session 8 Ivory Marmot - user has now rejected three opaque Japanese renderings in a row (自己効力感, 情意, and earlier 品質点 as non-essential). Rule: when a paper's construct name has no everyday Japanese equivalent, put the construct's OWN DEFINITION on the slide in plain words rather than coining or importing a technical compound. Check the source's definitional sentence first (Deng defines all five outcome variables on p.10) | com
* Citation linking rule | 2026-09-14 Session 8 Ivory Marmot - user: "always embed a link to first citation". Applies to every authored deck/document, not just Deck B. First mention of a work carries a markdown link to its DOI or arXiv abstract page; later mentions need none. Verify the DOI before using it (Crossref API: curl -s https://api.crossref.org/works/<doi>) - never construct one from volume/article number. Reusable script: .claude/.scratch/deckB_link_citations.py | com
* Quarto revealjs: raw html style block is dropped | 2026-09-14 Session 8 Ivory Marmot - a ```{=html} <style>...</style> ``` block placed in the body before the first ## was silently discarded; grep of the rendered HTML found zero occurrences. Do not use it for deck CSS. Working routes: a .css file listed in the YAML css: field, or (preferred for tables) tinytable tt(width = c(...)) | com
* Quarto revealjs: code chunk breaks .nonincremental | 2026-09-14 Session 8 Ivory Marmot - with incremental: true globally and {.nonincremental} on a slide, inserting an R chunk into that slide made EVERY list on it render blank at fragment 0 (content present in the qmd, invisible in the screenshot). Symptom looks like lost content, is actually fragment state. Fix: set incremental: false in the YAML when all slides are nonincremental anyway | com
* tinytable: tt(align=) is a no-op in HTML | 2026-09-14 Session 8 Ivory Marmot - tt(x, align = "lrdr") produced no alignment whatsoever in revealjs/HTML output (rendered <td> carried no class, no text-align). Use style_tt(tab, j = , align = ) as a separate call. Verified by minimal test: save_tt(tt(d, align="lrd")) = 0 text-align hits vs save_tt(style_tt(tt(d), j=2:3, align="rd")) = 6. Also: align "d" (decimal) needs NUMERIC columns - character strings have no decimal point for it to find - and integers need "r", not "d" | com
* Concurrent editing clobbers work | 2026-09-15 Session 8 Ivory Marmot - user's unsaved editor buffer was saved AFTER my edits and silently reverted two full rounds of changes to AICriticalThinking_ja_verified.qmd. This has now happened three times this session (19:39, 21:23, 03:51). Practice going forward: (a) always re-read the qmd immediately before editing, never trust the in-context copy; (b) copy the file to .claude/.scratch/<name>_before_<round>.qmd.bak before each edit round so a clobber is one cp away from recovery; (c) verify a redo by COUNTING markers (grep -c) rather than eyeballing, since a partial revert looks like a complete one | com
* ~~Abbas et al. (2024) unobtainable - Springer blocks every PDF route~~ RESOLVED 2026-09-15 Session 8 Ivory Marmot: user placed Abbas.pdf in lec_slides/papers/AICogAbility/. Read in full, extract at .claude/.scratch/abbas.txt, row added to Deck B's 参照した論文 table with its design (3-wave time-lagged survey, N=494, no randomisation) taken from the paper itself
* ~~Deck B 参照した論文 slide at 898/900~~ RESOLVED 2026-09-15: the Abbas critique moved to the 根拠の弱い論文 table, slide now 691/900. Original entry: | 2026-09-15 Session 8 Ivory Marmot - after the Abbas row and the three-bullet Abbas critique, this slide has 2px of headroom at .mid (0.76em). It renders correctly now but ANY addition will overflow it. Options when that happens: drop the slide to .mid2 (0.70em, would bring it to about 827), or move the Abbas critique onto its own slide | com
* Deck B opening slides are a live changelog | 2026-09-15 Session 8 Ivory Marmot - slides 2 and 3 record how the deck's content changed under criticism, including errors I made and had to retract. They must be UPDATED, not left to rot, whenever a further substantive correction lands. They are also the only slides whose subject is the deck's own construction, so a future edit that silently drops a row erases the record of a mistake | com
* ~~Reveal fragment stepping via URL hash does not work here~~ SOLVED 2026-09-15: do not use the URL hash. Inject a script that calls Reveal.slide(h,v,f) then Reveal.next() in a loop and logs the visible fragments - working example at .claude/.scratch/frag_probe.js, driven against a minimal reproduction .claude/.scratch/frag_test.qmd. Original entry: | 2026-09-15 Session 8 Ivory Marmot - #/h/v/f is ignored in this deck's build (fragmentInURL not enabled), so headless screenshots always capture step 0 and byte-identical PNGs across different f values. To screenshot a late fragment state, inject a script that calls Reveal.slide(h, v, f) or steps Reveal.next() before capture, the same way .claude/.scratch/overflow_probe_inject.js walks slides | com
* Stale ../../style/ and filters/ paths after the assets move | 2026-09-15 Session 8 Ivory Marmot - the user moved style/ and filters/ from the seishin root into assets/. Fixed only what the live deck needs: lec_slides/2026/sky-add.scss, _quarto.yml, AICriticalThinking_ja_verified.qmd. STILL STALE and will fail on re-render: lec_slides/2024/sky-add.scss, lec_slides/{2024,2026}/RP/sky-add.scss, assets/style/sky-add.scss, lec_slides/2026/01.qmd, lec_slides/2026/ai_agent_pitfall.qmd, and the five decks now in lec_slides/papers/AICogAbility. Their existing .html are standalone and still display | open
* quarto column CSS beats custom classes | 2026-09-15 Session 8 Ivory Marmot - ".reveal .columns > .column:last-child > :not(ul, ol) { margin-left: 0.5rem }" is more specific than a plain ".reveal .myclass" rule, so any custom left-margin silently becomes 8px inside a ::: {.columns} block. Add !important, or raise specificity. Symptom: the class works on a normal slide and stops working when the content is moved into columns | com

## 2026-09-15 | _extensions/ is unreachable after the assets/ move

`_extensions/` now exists only at `assets/_extensions/` (holding `tarleb/parse-latex` and
`imagify`). Quarto finds an extension by starting at the input file's folder and walking up
to the project root; `assets/` is not on that path, so the lookup never reaches it.

Affected, none of which currently render:

* all 14 `lec_slides/2026/RP/RP*.qmd` — each declares `filters: - parse-latex`
* `lec_slides/2026/01.qmd` — same filter
* `lec_slides/2026/RP/test.qmd` — declares `imagify`

Error: `Error running filter C:\...\lec_slides\2026\RP\parse-latex`, then `FATAL QUARTO ERROR`.

A WSL symlink at the project root will not fix it — the Windows `quarto.exe` cannot follow
one. `_extensions/` has to sit physically at the project root, or be duplicated there.

Not fixed: moving the user's folder back is the user's call. Both renders that needed it
(`RP01.qmd`, `01.qmd`) were verified by copying `assets/_extensions` to the root, rendering,
then deleting the copy — so the scss/css path edits are proven independently of this issue.

ADDENDUM 2026-09-15: a full grep of every `filters:` block widens the affected list. Also
broken by the same `_extensions/` lookup: `lec_slides/2026/02.qmd` and `lec_slides/2026/test.qmd`
(parse-latex), `lec_slides/2026/03.qmd` and `lec_slides/2026/DataVis.qmd` (nutshell). Total
affected: 14 RP decks + RP/test.qmd + 01, 02, 03, test, DataVis = 20 files.

`filters/` itself is NOT affected. Its only two references — `_quarto.yml` line 2 and
`01.qmd` line 39 — are explicit paths to `assets/filters/ListArrow.lua`, which exists, and
both files render. Bare names in a `filters:` list are extensions, not files in `filters/`.

RESOLVED 2026-09-15 20:xx JST: user moved `_extensions/` back to the project root. All 20
files re-rendered. 18 produce "Output created". The two that do not fail on the LaTeX
toolchain, not on any path:

* `lec_slides/2026/test.qmd` and `lec_slides/2026/RP/test.qmd` — `fmtutil [ERROR]` for every
  engine (xetex, luatex, luahbtex, pdftex), then `Error: LaTeX failed to compile
  tikz*.tex` / `[ERROR] Imagify: LaTeX compilation failed`. The tinytex/TeX Live format files
  are not built on this box. Separate issue, nothing to do with the assets/ move.

## 2026-09-15 | still-stale paths in lec_slides/2026/RP/, pre-existing, NOT from the assets/ move

All 14 RP decks now render, but with warnings that predate the move and that I have not
touched, because fixing them was not part of the approved task:

* `logo: GrootbergGiraffeHead.jpg` (bare) in all 14 — the image sits in `lec_slides/2026/`
  and `lec_slides/2024/`, not in `RP/`, so every render warns "Could not fetch resource".
  Would become `../GrootbergGiraffeHead.jpg`.
* `css: ../../seiro.css` in RP02-RP06 — two levels up from `RP/` is `lec_slides/`, where no
  such file has ever existed. The file is now at `assets/style/seiro.css`, three levels up.
  Would become `../../../assets/style/seiro.css`.
* `toc-bg-image: "../../GrootbergGiraffe3.jpg"` and `bibliography: ../../seiro.bib` — same
  two-level mistake; both files sit at the project root, three levels up.

These are cosmetic (warnings, not errors) and the decks build without them.

RESOLVED 2026-09-15: the RP path warnings above are fixed at the user's instruction ("yes,
clean them"). All 14 RP decks render with zero warnings. The two `test.qmd` LaTeX failures
remain open.
