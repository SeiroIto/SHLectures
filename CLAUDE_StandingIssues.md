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
