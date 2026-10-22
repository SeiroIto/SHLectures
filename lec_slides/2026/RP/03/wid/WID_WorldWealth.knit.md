---
title: "World wealth inequality, WID"
format:
  html:
    embed-resources: true
    lightbox: true
    code-overflow: wrap
    code-fold: true
execute:
  warning: false
---

## Introduction

* This page builds a world distribution of net personal wealth from the World
  Inequality Database (WID) and plots it.
  * Every country's 100 wealth percentiles (1980 onwards) are pooled, each weighted
    by the country's adults, in US$ at market exchange rates.
* Outputs:
  * a Japan 2022 example of the raw and converted rows (@tbl-japan, @fig-japan)
  * world wealth density, 1980-2020 every 10 years (@fig-world-dist)
  * share of world wealth held by the richest 1% of world adults (@fig-world-top1)
  * top 1% share of national wealth by country, grouped by the 1987 World Bank
    income group (@fig-country-top1)
  * WID metadata: variables, sources and methods (@tbl-meta)
* Data are read from saved files in `data/` (WID bulk extract, World Bank country
  list, income classification, oil rents); nothing is downloaded at render.
  The download script is shown but not run (`eval: false`).


::: {.cell}

```{.r .cell-code  code-summary="Code: load packages, table style"}
options(repos = c(CRAN = "https://cran.ism.ac.jp/"))  # Japan mirror (ISM, Tokyo)
pkgs <- c("data.table", "qs2", "ggplot2", "ggrepel", "scales", "jsonlite", "readxl", "tinytable", "patchwork")
new <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(new) > 0) install.packages(new)
library(data.table)
library(ggplot2)
library(tinytable)
# Tables: striped rows, faint light-blue header
ttx <- function(x, width) style_tt(theme_tt(tt(x, width = width), theme = "striped"), i = 0, background = "#EAF2FB")
```
:::



::: {.cell}

```{.r .cell-code  code-summary="Code: download WID data (record, not run)"}
# Not run at render (kept as a record): streams the WID bulk zip and writes
# data/wid_extract.csv and data/wid_meta.csv (about 6 minutes)
# run from a WSL shell in this folder:  python3 -I wid_extract.py data
writeLines(readLines("wid_extract.py"))
```
:::



::: {.cell}

```{.r .cell-code  code-summary="Code: build world distribution, top 1% shares, income groups"}
d    <- fread("data/wid_extract.csv", fill = TRUE)       # aggregate rows lack data_quality
meta <- as.data.table(read.csv("data/wid_meta.csv", encoding = "UTF-8"))  # fread stops at quoted line breaks
# Keep countries: drop WID regional aggregates (O*, Q*, X*, except Oman OM and Qatar QA),
# the World (WO), former states (CS, DD, SU, YU, AN) and sub-country units (ZZ Zanzibar, XI Channel Islands)
agg  <- c("WO", "ZZ", "CS", "DD", "SU", "YU", "AN", "XI")
cc   <- setdiff(unique(d$country), agg)
cc   <- cc[!grepl("^[OQX]", cc) | cc %in% c("OM", "QA")]
d    <- d[country %in% c(cc, "WO")]

pop <- d[variable == "npopuli992", .(country, year, pop = value)]
# 1% groups: average wealth (constant local currency) -> US$ at the market rate of the
# country's latest wealth year (xlcusxi999 is a yearly series)
w <- d[variable == "ahwealj992" & country != "WO",
       .(country, year, p = as.integer(sub("^p([0-9]+)p.*", "\\1", percentile)), lcu = value)]
fx <- d[variable == "xlcusxi999", .(country, fx_year = year, fx = value)]
fx <- fx[w[, .(fx_year = max(year)), by = country], on = .(country, fx_year)]
w <- w[, if (.N == 100) .SD, by = .(country, year)]         # complete 100-group years only
w <- w[year >= 1980]                                        # all 215 countries from 1980; 48 before
w <- merge(w, pop, by = c("country", "year"))
w <- merge(w, fx, by = "country")
w[, `:=`(usd = lcu / fx, wt = pop / 100)]                   # each group = 1% of adults

# World distribution: pool all country groups of a year, sort by wealth, weight by adults
setorder(w, year, usd)
w[, cum := cumsum(wt) / sum(wt), by = year]
grid <- seq(0.005, 0.995, by = 0.01)
wq <- w[, .(pct = grid * 100, usd = approx(cum, usd, xout = grid, ties = "ordered", rule = 2)$y), by = year]
wq[, decade := paste0(10 * (year %/% 10), "s")]

# World top 1% share: fraction of each group above the 99th world percentile
top1 <- w[, {
  W <- sum(wt); f <- pmin(1, pmax(0, (cum - 0.99) / (wt / W)))
  .(share = sum(usd * wt * f) / sum(usd * wt), countries = uniqueN(country), adults = W)
}, by = year]
top1 <- merge(top1, pop[country == "WO", .(year, world = pop)], by = "year")
top1[, coverage := adults / world]

# Country top 1% shares
s <- d[variable == "shwealj992" & percentile == "p99p100" & country != "WO" & year >= 1980,
       .(country, year, share = value)]
# ISO2 -> ISO3 and region from the World Bank country list (copied from ../gdpgini/data/country.json)
wb  <- as.data.table(jsonlite::fromJSON("data/wb_country.json", flatten = TRUE)[[2]])[
  , .(iso2 = iso2Code, iso3 = id, region = trimws(region.value))]
wb  <- rbind(wb, data.table(iso2 = c("KS", "TW", "GG", "JE", "AI", "MS", "BQ"),   # not in WB list
                            iso3 = c("XKX", "TWN", "GGY", "JEY", "AIA", "MSR", "BES"),  # WID KS = Kosovo
                            region = NA_character_))
s[wb, `:=`(iso3 = i.iso3, region = i.region), on = .(country = iso2)]

# Income group: World Bank analytical classification for calendar year 1987 (FY89), the earliest
og  <- as.data.table(readxl::read_excel("data/wb_OGHIST.xlsx", sheet = "Country Analytical History",
                                        skip = 11, col_names = FALSE))[, 1:3]
setnames(og, c("iso3", "name", "g1987"))
inc <- og[iso3 %in% s$iso3, .(iso3, g1987)]
inc[, inc1987 := fcase(g1987 == "H", "High income",
                       g1987 %in% c("UM", "LM"), "Middle income",
                       default = "Not classified")]
s <- merge(s, inc[, .(iso3, g1987, inc1987)], by = "iso3", all.x = TRUE)
s[is.na(inc1987), inc1987 := "Not classified"]
# Low income split into Sub-Saharan Africa and the rest (too many lines for one panel)
s[g1987 == "L", inc1987 := fifelse(region == "Sub-Saharan Africa", "Low income: Sub-Saharan Africa",
                                   "Low income: other")]
# City states and oil producers (mean oil rents >= 10% of GDP, WB NY.GDP.PETR.RT.ZS) form own groups
city <- c("SGP", "HKG", "MAC", "LUX", "MLT", "MCO", "SMR", "LIE", "AND",
          "ABW", "BHS", "BMU", "GRL", "IMN")                      # + small island economies
oil  <- as.data.table(jsonlite::fromJSON("data/wb_oil.json", flatten = TRUE)[[2]])[!is.na(value)]
oilp <- oil[, .(r = mean(value)), by = countryiso3code][r >= 10, countryiso3code]
s[iso3 %in% oilp, inc1987 := "Oil producers"]
s[iso3 %in% city, inc1987 := "City states"]
s[, inc1987 := factor(inc1987, levels = c("High income", "Middle income", "Low income: Sub-Saharan Africa",
                                          "Low income: other", "Oil producers", "City states", "Not classified"))]
# WID repeats the last estimate in years without new data: a value equal to the previous year's -> NA
setorder(s, country, year)
s[, share := fifelse(share == shift(share), NA_real_, share), by = country]

qs2::qs_save(list(w = w, wq = wq, top1 = top1, s = s, inc = inc), "data/wid_world.qs2")
fwrite(wq, "data/wid_world_quantiles.csv")
fwrite(top1, "data/wid_world_top1.csv")
fwrite(s, "data/wid_country_top1.csv")

top1[, .(years = paste(range(year), collapse = "-"), countries = paste(range(countries), collapse = "-"),
         coverage = paste(round(100 * range(coverage), 1), collapse = "-"))]
```

::: {.cell-output .cell-output-stdout}

```
       years countries coverage
      <char>    <char>   <char>
1: 1980-2024   215-215  100-100
```


:::

```{.r .cell-code  code-summary="Code: build world distribution, top 1% shares, income groups"}
s[, .(countries = uniqueN(country), years = paste(range(year), collapse = "-")), keyby = inc1987]
```

::: {.cell-output .cell-output-stdout}

```
Key: <inc1987>
                          inc1987 countries     years
                           <fctr>     <int>    <char>
1:                    High income        22 1980-2024
2:                  Middle income        62 1980-2024
3: Low income: Sub-Saharan Africa        30 1980-2024
4:              Low income: other        17 1980-2024
5:                  Oil producers        24 1980-2024
6:                    City states        14 1980-2024
7:                 Not classified        46 1980-2024
```


:::

```{.r .cell-code  code-summary="Code: build world distribution, top 1% shares, income groups"}
s[is.na(iso3), unique(country)]
```

::: {.cell-output .cell-output-stdout}

```
character(0)
```


:::
:::


How the world distribution is built:

* For country $c$, year $y$, group $p = 1, \dots, 100$: average wealth $w_{cyp}$ (US$) and adult population $z_{cy}$ of the same year
* World distribution in year $y$: all $100 \times C$ values $w_{cyp}$, each weighted by $z_{cy}/100$
* 2022: 21,500 values = 100 × 215 countries


::: {#tbl-japan .cell tbl-cap='Japan 2022, every 5th group: raw WID rows of `d` (percentile, value in JPY) and the rows of `w` built from them'}

```{.r .cell-code  code-summary="Code: Japan example of d and w"}
jp_d <- d[country == "JP" & year == 2022 & variable == "ahwealj992", .(percentile, value)]
jp_d <- jp_d[order(as.integer(sub("^p([0-9]+)p.*", "\\1", percentile)))][seq(5, 100, 5)]
jp_w <- w[country == "JP" & year == 2022][order(p)][seq(5, 100, 5), .(fx = round(fx, 1), usd, pop_z = pop)]
jp <- cbind(jp_d, jp_w)
for (v in c("value", "usd", "pop_z")) set(jp, j = v, value = format(round(jp[[v]]), big.mark = ","))
ttx(jp, 0.7)
```

::: {.cell-output-display}

```{=html}

    <script src="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.js"></script>

    <script>
      // Create table-specific functions using external factory
      const tableFns_y4ze9s70ubkxz9pv4fs3 = TinyTable.createTableFunctions("tinytable_y4ze9s70ubkxz9pv4fs3");
      // tinytable span after
      window.addEventListener('load', function () {
          var cellsToStyle = [
            // tinytable style arrays after
          { positions: [ { i: '20', j: 1 }, { i: '20', j: 2 }, { i: '20', j: 3 }, { i: '20', j: 4 }, { i: '20', j: 5 } ], css_id: 'tinytable_css_0t3g39zjvp2m9jm3mw7m',}, 
          { positions: [ { i: '1', j: 1 }, { i: '3', j: 1 }, { i: '5', j: 1 }, { i: '7', j: 1 }, { i: '9', j: 1 }, { i: '11', j: 1 }, { i: '13', j: 1 }, { i: '15', j: 1 }, { i: '17', j: 1 }, { i: '19', j: 1 }, { i: '1', j: 2 }, { i: '3', j: 2 }, { i: '5', j: 2 }, { i: '7', j: 2 }, { i: '9', j: 2 }, { i: '11', j: 2 }, { i: '13', j: 2 }, { i: '15', j: 2 }, { i: '17', j: 2 }, { i: '19', j: 2 }, { i: '1', j: 3 }, { i: '3', j: 3 }, { i: '5', j: 3 }, { i: '7', j: 3 }, { i: '9', j: 3 }, { i: '11', j: 3 }, { i: '13', j: 3 }, { i: '15', j: 3 }, { i: '17', j: 3 }, { i: '19', j: 3 }, { i: '1', j: 4 }, { i: '3', j: 4 }, { i: '5', j: 4 }, { i: '7', j: 4 }, { i: '9', j: 4 }, { i: '11', j: 4 }, { i: '13', j: 4 }, { i: '15', j: 4 }, { i: '17', j: 4 }, { i: '19', j: 4 }, { i: '1', j: 5 }, { i: '3', j: 5 }, { i: '5', j: 5 }, { i: '7', j: 5 }, { i: '9', j: 5 }, { i: '11', j: 5 }, { i: '13', j: 5 }, { i: '15', j: 5 }, { i: '17', j: 5 }, { i: '19', j: 5 } ], css_id: 'tinytable_css_ex7gdhq1nkhv8tyct4zb',}, 
          { positions: [ { i: '0', j: 1 }, { i: '0', j: 2 }, { i: '0', j: 3 }, { i: '0', j: 4 }, { i: '0', j: 5 } ], css_id: 'tinytable_css_83onz4wv0bo6aagfqbqe',}, 
          ];

          // Loop over the arrays to style the cells
          cellsToStyle.forEach(function (group) {
              group.positions.forEach(function (cell) {
                  tableFns_y4ze9s70ubkxz9pv4fs3.styleCell(cell.i, cell.j, group.css_id);
              });
          });
      });
    </script>

    <link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.css">
    <style>
    /* tinytable css entries after */
    #tinytable_y4ze9s70ubkxz9pv4fs3 td.tinytable_css_0t3g39zjvp2m9jm3mw7m, #tinytable_y4ze9s70ubkxz9pv4fs3 th.tinytable_css_0t3g39zjvp2m9jm3mw7m {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: var(--tt-line-color); --line-color-left: var(--tt-line-color); --line-color-right: var(--tt-line-color); --line-color-top: var(--tt-line-color); --line-width-bottom: 0.08em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --line-type-bottom: solid; --line-type-left: solid; --line-type-right: solid; --line-type-top: solid; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%;  }
    #tinytable_y4ze9s70ubkxz9pv4fs3 td.tinytable_css_ex7gdhq1nkhv8tyct4zb, #tinytable_y4ze9s70ubkxz9pv4fs3 th.tinytable_css_ex7gdhq1nkhv8tyct4zb { background-color: #ededed }
    #tinytable_y4ze9s70ubkxz9pv4fs3 td.tinytable_css_83onz4wv0bo6aagfqbqe, #tinytable_y4ze9s70ubkxz9pv4fs3 th.tinytable_css_83onz4wv0bo6aagfqbqe {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: var(--tt-line-color); --line-color-left: var(--tt-line-color); --line-color-right: var(--tt-line-color); --line-color-top: var(--tt-line-color); --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.08em; --line-type-bottom: solid; --line-type-left: solid; --line-type-right: solid; --line-type-top: solid; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; background-color: #EAF2FB }
    </style>
    <div class="container">
      <table class="tinytable" id="tinytable_y4ze9s70ubkxz9pv4fs3" style="table-layout: fixed; width: 70% !important; margin-left: auto; margin-right: auto;" data-quarto-disable-processing='true'>
        
        <thead>
              <tr>
                <th scope="col" data-row="0" data-col="1">percentile</th>
                <th scope="col" data-row="0" data-col="2">value</th>
                <th scope="col" data-row="0" data-col="3">fx</th>
                <th scope="col" data-row="0" data-col="4">usd</th>
                <th scope="col" data-row="0" data-col="5">pop_z</th>
              </tr>
        </thead>
        
        <tbody>
                <tr>
                  <td data-row="1" data-col="1">p4p5</td>
                  <td data-row="1" data-col="2">-2,435,154</td>
                  <td data-row="1" data-col="3">151.4</td>
                  <td data-row="1" data-col="4">-16,088</td>
                  <td data-row="1" data-col="5">104,459,406</td>
                </tr>
                <tr>
                  <td data-row="2" data-col="1">p9p10</td>
                  <td data-row="2" data-col="2">-312,937</td>
                  <td data-row="2" data-col="3">151.4</td>
                  <td data-row="2" data-col="4">-2,067</td>
                  <td data-row="2" data-col="5">104,459,406</td>
                </tr>
                <tr>
                  <td data-row="3" data-col="1">p14p15</td>
                  <td data-row="3" data-col="2">810,492</td>
                  <td data-row="3" data-col="3">151.4</td>
                  <td data-row="3" data-col="4">5,355</td>
                  <td data-row="3" data-col="5">104,459,406</td>
                </tr>
                <tr>
                  <td data-row="4" data-col="1">p19p20</td>
                  <td data-row="4" data-col="2">1,553,475</td>
                  <td data-row="4" data-col="3">151.4</td>
                  <td data-row="4" data-col="4">10,263</td>
                  <td data-row="4" data-col="5">104,459,406</td>
                </tr>
                <tr>
                  <td data-row="5" data-col="1">p24p25</td>
                  <td data-row="5" data-col="2">2,486,024</td>
                  <td data-row="5" data-col="3">151.4</td>
                  <td data-row="5" data-col="4">16,424</td>
                  <td data-row="5" data-col="5">104,459,406</td>
                </tr>
                <tr>
                  <td data-row="6" data-col="1">p29p30</td>
                  <td data-row="6" data-col="2">3,755,272</td>
                  <td data-row="6" data-col="3">151.4</td>
                  <td data-row="6" data-col="4">24,809</td>
                  <td data-row="6" data-col="5">104,459,406</td>
                </tr>
                <tr>
                  <td data-row="7" data-col="1">p34p35</td>
                  <td data-row="7" data-col="2">5,215,971</td>
                  <td data-row="7" data-col="3">151.4</td>
                  <td data-row="7" data-col="4">34,459</td>
                  <td data-row="7" data-col="5">104,459,406</td>
                </tr>
                <tr>
                  <td data-row="8" data-col="1">p39p40</td>
                  <td data-row="8" data-col="2">6,857,521</td>
                  <td data-row="8" data-col="3">151.4</td>
                  <td data-row="8" data-col="4">45,304</td>
                  <td data-row="8" data-col="5">104,459,406</td>
                </tr>
                <tr>
                  <td data-row="9" data-col="1">p44p45</td>
                  <td data-row="9" data-col="2">8,682,504</td>
                  <td data-row="9" data-col="3">151.4</td>
                  <td data-row="9" data-col="4">57,361</td>
                  <td data-row="9" data-col="5">104,459,406</td>
                </tr>
                <tr>
                  <td data-row="10" data-col="1">p49p50</td>
                  <td data-row="10" data-col="2">10,922,870</td>
                  <td data-row="10" data-col="3">151.4</td>
                  <td data-row="10" data-col="4">72,162</td>
                  <td data-row="10" data-col="5">104,459,406</td>
                </tr>
                <tr>
                  <td data-row="11" data-col="1">p54p55</td>
                  <td data-row="11" data-col="2">13,231,501</td>
                  <td data-row="11" data-col="3">151.4</td>
                  <td data-row="11" data-col="4">87,414</td>
                  <td data-row="11" data-col="5">104,459,406</td>
                </tr>
                <tr>
                  <td data-row="12" data-col="1">p59p60</td>
                  <td data-row="12" data-col="2">15,895,260</td>
                  <td data-row="12" data-col="3">151.4</td>
                  <td data-row="12" data-col="4">105,012</td>
                  <td data-row="12" data-col="5">104,459,406</td>
                </tr>
                <tr>
                  <td data-row="13" data-col="1">p64p65</td>
                  <td data-row="13" data-col="2">19,283,330</td>
                  <td data-row="13" data-col="3">151.4</td>
                  <td data-row="13" data-col="4">127,395</td>
                  <td data-row="13" data-col="5">104,459,406</td>
                </tr>
                <tr>
                  <td data-row="14" data-col="1">p69p70</td>
                  <td data-row="14" data-col="2">23,491,400</td>
                  <td data-row="14" data-col="3">151.4</td>
                  <td data-row="14" data-col="4">155,196</td>
                  <td data-row="14" data-col="5">104,459,406</td>
                </tr>
                <tr>
                  <td data-row="15" data-col="1">p74p75</td>
                  <td data-row="15" data-col="2">28,782,326</td>
                  <td data-row="15" data-col="3">151.4</td>
                  <td data-row="15" data-col="4">190,150</td>
                  <td data-row="15" data-col="5">104,459,406</td>
                </tr>
                <tr>
                  <td data-row="16" data-col="1">p79p80</td>
                  <td data-row="16" data-col="2">36,020,710</td>
                  <td data-row="16" data-col="3">151.4</td>
                  <td data-row="16" data-col="4">237,970</td>
                  <td data-row="16" data-col="5">104,459,406</td>
                </tr>
                <tr>
                  <td data-row="17" data-col="1">p84p85</td>
                  <td data-row="17" data-col="2">46,384,731</td>
                  <td data-row="17" data-col="3">151.4</td>
                  <td data-row="17" data-col="4">306,440</td>
                  <td data-row="17" data-col="5">104,459,406</td>
                </tr>
                <tr>
                  <td data-row="18" data-col="1">p89p90</td>
                  <td data-row="18" data-col="2">64,033,742</td>
                  <td data-row="18" data-col="3">151.4</td>
                  <td data-row="18" data-col="4">423,038</td>
                  <td data-row="18" data-col="5">104,459,406</td>
                </tr>
                <tr>
                  <td data-row="19" data-col="1">p94p95</td>
                  <td data-row="19" data-col="2">100,572,760</td>
                  <td data-row="19" data-col="3">151.4</td>
                  <td data-row="19" data-col="4">664,433</td>
                  <td data-row="19" data-col="5">104,459,406</td>
                </tr>
                <tr>
                  <td data-row="20" data-col="1">p99p100</td>
                  <td data-row="20" data-col="2">755,586,966</td>
                  <td data-row="20" data-col="3">151.4</td>
                  <td data-row="20" data-col="4">4,991,778</td>
                  <td data-row="20" data-col="5">104,459,406</td>
                </tr>
        </tbody>
      </table>
    </div>
<!-- hack to avoid NA insertion in last line -->
```

:::
:::


* `fx`: market exchange rate, JPY per US$ (`xlcusxi999`), in Japan's latest wealth year; `usd = value / fx`
* `pop_z`: adults (over age 20) in Japan in 2022; each of the 100 groups holds `pop_z / 100` adults


::: {.cell}

```{.r .cell-code  code-summary="Code: plot Japan 2022 density"}
jp_all <- w[country == "JP" & year == 2022]
lab_usd <- scales::label_dollar(scale_cut = scales::cut_short_scale())
p_jl <- ggplot(jp_all[usd > 0], aes(usd)) +
  geom_density(fill = "#56B4E9", colour = "#0072B2", alpha = 0.4, bw = 0.15) +
  scale_x_log10(breaks = c(1e3, 3e3, 1e4, 3e4, 1e5, 3e5, 1e6, 3e6, 1e7, 3e7), labels = lab_usd,
                limits = c(1e3, 3e7)) +
  labs(x = "Net personal wealth per adult (US$, log scale)", y = "Density") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
p_jn <- ggplot(jp_all, aes(usd)) +
  geom_density(fill = "#E69F00", colour = "#D55E00", alpha = 0.4) +
  geom_vline(xintercept = 0, colour = "grey40", linetype = "dashed") +
  scale_x_continuous(breaks = seq(-5e5, 7e6, 5e5), labels = lab_usd, limits = c(-5e5, 7e6)) +  # top percentile ~ $5.0M
  labs(x = "Net personal wealth per adult (US$, natural scale)", y = "Density") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
patchwork::wrap_plots(p_jl, p_jn, ncol = 2)
```

::: {.cell-output-display}
![Japan 2022: density of net personal wealth across Japan's 100 wealth percentiles (US$). Left: log scale (percentiles with zero or negative wealth not shown). Right: natural scale, negatives included.](WID_WorldWealth_files/figure-html/fig-japan-1.png){#fig-japan width=1056}
:::
:::


## Figures


::: {.cell .column-margin}

```{.r .cell-code  code-summary="Code: plot world wealth density, natural scale"}
# Placed before @fig-world-dist so the margin figure sits beside it
wn <- w[year %% 10 == 0]
p_nat <- ggplot(wn, aes(usd, weight = wt, colour = factor(year))) +
  geom_density(linewidth = 0.6, n = 2^14) +
  coord_cartesian(xlim = c(1e3, 3e6)) +                      # zoom only: density uses all values
  scale_x_continuous(breaks = c(1e3, 5e5, 1e6, 1.5e6, 2e6, 2.5e6, 3e6),
                     labels = scales::label_dollar(scale_cut = scales::cut_short_scale())) +
  scale_colour_viridis_d(name = NULL, end = 0.9) +
  labs(x = "Net personal wealth per adult (US$)", y = "Density") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))
p_nat
```

::: {.cell-output-display}
![As @fig-world-dist, natural scale; x axis shown from $1K to $3M, density computed on all values (negatives included).](WID_WorldWealth_files/figure-html/fig-world-dist-nat-1.png){#fig-world-dist-nat width=384}
:::
:::



::: {.cell}

```{.r .cell-code  code-summary="Code: plot world wealth density"}
w[, decade := paste0(10 * (year %/% 10), "s")]
wd <- w[year %% 10 == 0]
# Weighted mean and std of wealth across world adults (natural scale, negatives included), in the legend
sdy <- wd[, {m <- sum(usd * wt) / sum(wt); .(m = m, std = sqrt(sum(wt * (usd - m)^2) / sum(wt)))}, by = year]
sdy[, lab := sprintf("%d  (mean %s, std %s, CV %.2f)", year, format(round(m), big.mark = ","),
                     format(round(std), big.mark = ","), std / m)]           # CV = std / mean
wd[sdy, ylab := i.lab, on = "year"]
p_dist <- ggplot(wd[usd > 0], aes(usd, weight = wt, colour = ylab)) +
  geom_density(linewidth = 0.8, bw = 0.12, key_glyph = "path") +
  scale_x_log10(breaks = c(10, 30, 100, 300, 1e3, 3e3, 1e4, 3e4, 1e5, 3e5, 1e6, 3e6, 1e7),
                labels = scales::label_dollar(scale_cut = scales::cut_short_scale())) +
  scale_colour_viridis_d(name = "Year (US$)", end = 0.9) +
  expand_limits(y = 0.9) +                                   # headroom so the inside legend clears the peaks
  labs(x = "Net personal wealth per adult, by country wealth percentile (US$)", y = "Density (share of world adults)") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "inside", legend.position.inside = c(0.99, 0.99),
        legend.justification = c(1, 1), legend.background = element_rect(fill = "white", colour = NA),
        axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("data/wid_world_dist.png", p_dist, width = 9, height = 5.5, dpi = 200, bg = "white")
p_dist
wd[, .(adults_le0 = sprintf("%.1f%%", 100 * sum(wt[usd <= 0]) / sum(wt))), keyby = year]
```

::: {.cell-output .cell-output-stdout}

```
Key: <year>
    year adults_le0
   <int>     <char>
1:  1980       6.2%
2:  1990       6.7%
3:  2000       8.0%
4:  2010       8.5%
5:  2020       8.1%
```


:::

::: {.cell-output-display}
![Wealth percentiles of the world wealth distribution, 1980-2020 every 10 years: the 100 wealth percentiles of every country pooled, each weighted by its adults; net personal wealth in US$ at market exchange rates, log scale. Percentiles with zero or negative wealth are not drawn (their share of world adults is printed below).](WID_WorldWealth_files/figure-html/fig-world-dist-1.png){#fig-world-dist width=864}
:::
:::




::: {.cell}

```{.r .cell-code  code-summary="Code: plot world top 1% share"}
p_top1 <- ggplot(top1, aes(year, share)) +
  geom_line(colour = "#0072B2") + geom_point(colour = "#0072B2", size = 1.2) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = NULL, y = "Top 1% share of world wealth") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())
ggsave("data/wid_world_top1.png", p_top1, width = 8, height = 4.5, dpi = 200, bg = "white")
p_top1
```

::: {.cell-output-display}
![Share of world net personal wealth held by the richest 1% of world adults, from the pooled wealth percentiles of all countries (wealth treated as equal within each country percentile).](WID_WorldWealth_files/figure-html/fig-world-top1-1.png){#fig-world-top1 width=50%}
:::
:::



::: {.cell}

```{.r .cell-code  code-summary="Code: plot country top 1% shares"}
shp <- c(16, 17, 15, 18, 1, 2, 0, 5, 6, 4, 3, 8)
setorder(s, inc1987, iso3, year)
s[, cid := match(country, unique(country)), by = inc1987]
s[, n := uniqueN(country), by = inc1987]
s[, col := (cid - 1) / pmax(n - 1, 1)]                     # 0-1 within group -> full viridis range
s[, sid := factor((cid - 1) %% length(shp) + 1)]
# Label year: countries spread across 1982-2022 in group order, so labels do not pile up
s[, lab_year := 1982L + as.integer(round(40 * ((cid - 1) %% 9) / 8))]
s[, lab := fifelse(year == lab_year & !is.na(share), iso3, "")]   # empty labels: repel from all points
p_cty <- ggplot(s, aes(year, share, colour = col, shape = sid, group = country)) +
  geom_line(linewidth = 0.35, alpha = 0.8) +
  geom_point(data = s[!is.na(share)], size = 0.9) +
  ggrepel::geom_text_repel(aes(label = lab), size = 2.6, max.overlaps = Inf, box.padding = 0.25,
                           point.padding = 0.1, min.segment.length = 0.2, segment.size = 0.2,
                           max.time = 5, seed = 1, show.legend = FALSE) +
  facet_wrap(~inc1987, ncol = 2, scales = "free_y") +
  scale_colour_viridis_c(end = 0.9, guide = "none") +
  scale_shape_manual(values = shp, guide = "none") +
  scale_x_continuous(breaks = seq(1980, 2025, 5)) +
  scale_y_continuous(labels = scales::label_percent(), breaks = seq(0, 1, 0.05)) +
  labs(x = NULL, y = "Top 1% share of national wealth") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("data/wid_country_top1.png", p_cty, width = 12, height = 15, dpi = 200, bg = "white")
p_cty
```

::: {.cell-output-display}
![Top 1% wealth share by country, grouped by World Bank income classification for 1987 (earliest available), low income split by region, oil producers and city states separate; a value equal to the previous year's is treated as missing (WID carries estimates forward); y axis per panel; viridis colour and shape = country within group; ISO3 labels placed at staggered years along each line; points every 5 years.](WID_WorldWealth_files/figure-html/fig-country-top1-1.png){#fig-country-top1 width=1152}
:::
:::




* High income in 1987 (22), ISO3 = country: AUS = Australia, AUT = Austria, BEL = Belgium, CAN = Canada, CHE = Switzerland, DEU = Germany, DNK = Denmark, ESP = Spain, FIN = Finland, FRA = France, GBR = the United Kingdom, IRL = Ireland, ISL = Iceland, ISR = Israel, ITA = Italy, JPN = Japan, NLD = the Netherlands, NOR = Norway, NZL = New Zealand, SWE = Sweden, TWN = Taiwan, USA = the USA
* Not classified in 1987 (46): Albania, Anguilla, Armenia, Belarus, Bonaire, Sint Eustatius and Saba, Bosnia and Herzegovina, Bulgaria, Cabo Verde, Croatia, Cuba, Curacao, Djibouti, Eritrea, Estonia, Georgia, Guernsey, Jersey, Kosovo, Kyrgyzstan, Latvia, Lithuania, Micronesia, Moldova, Mongolia, Montenegro, Montserrat, Nauru, North Korea, North Macedonia, Palau, Palestine, Serbia, Sint Maarten (Dutch part), Slovakia, Slovenia, Tajikistan, the British Virgin Islands, the Cayman Islands, the Czech Republic, the French Polynesia, the Marshall Islands, the Russian Federation, the Turks and Caicos Islands, Tuvalu, Ukraine, Uzbekistan

## Metadata notes


::: {#tbl-meta .cell tbl-cap='WID variables used (from the WID metadata files)'}

```{.r .cell-code  code-summary="Code: metadata table"}
mt <- meta[country %in% cc]
m <- mt[, {
  u <- unique(unit[unit != ""])
  .(Name = shortname[1], Type = shorttype[1], Population = shortpop[1], Age = shortage[1],
    Unit = if (length(u) > 3) sprintf("local currency (%d codes)", length(u)) else paste(u, collapse = ", "),
    Countries = uniqueN(country), Quality = round(mean(as.numeric(avg_quality), na.rm = TRUE), 1))
}, by = .(Variable = variable)]
m[, Quality := fifelse(is.nan(Quality), "", as.character(Quality))]
ttx(m, 0.95)
```

::: {.cell-output-display}

```{=html}

    <script src="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.js"></script>

    <script>
      // Create table-specific functions using external factory
      const tableFns_fxlkf32uyj5wzvhhr83q = TinyTable.createTableFunctions("tinytable_fxlkf32uyj5wzvhhr83q");
      // tinytable span after
      window.addEventListener('load', function () {
          var cellsToStyle = [
            // tinytable style arrays after
          { positions: [ { i: '5', j: 1 }, { i: '5', j: 2 }, { i: '5', j: 3 }, { i: '5', j: 4 }, { i: '5', j: 5 }, { i: '5', j: 6 }, { i: '5', j: 7 }, { i: '5', j: 8 } ], css_id: 'tinytable_css_n6akt7gffev87tbqu006',}, 
          { positions: [ { i: '1', j: 1 }, { i: '3', j: 1 }, { i: '1', j: 2 }, { i: '3', j: 2 }, { i: '1', j: 3 }, { i: '3', j: 3 }, { i: '1', j: 4 }, { i: '3', j: 4 }, { i: '1', j: 5 }, { i: '3', j: 5 }, { i: '1', j: 6 }, { i: '3', j: 6 }, { i: '1', j: 7 }, { i: '3', j: 7 }, { i: '1', j: 8 }, { i: '3', j: 8 } ], css_id: 'tinytable_css_9md2mh9qy3wqrjovdcf1',}, 
          { positions: [ { i: '0', j: 1 }, { i: '0', j: 2 }, { i: '0', j: 3 }, { i: '0', j: 4 }, { i: '0', j: 5 }, { i: '0', j: 6 }, { i: '0', j: 7 }, { i: '0', j: 8 } ], css_id: 'tinytable_css_lwqg9t2u8jbs7onw7caz',}, 
          ];

          // Loop over the arrays to style the cells
          cellsToStyle.forEach(function (group) {
              group.positions.forEach(function (cell) {
                  tableFns_fxlkf32uyj5wzvhhr83q.styleCell(cell.i, cell.j, group.css_id);
              });
          });
      });
    </script>

    <link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.css">
    <style>
    /* tinytable css entries after */
    #tinytable_fxlkf32uyj5wzvhhr83q td.tinytable_css_n6akt7gffev87tbqu006, #tinytable_fxlkf32uyj5wzvhhr83q th.tinytable_css_n6akt7gffev87tbqu006 {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: var(--tt-line-color); --line-color-left: var(--tt-line-color); --line-color-right: var(--tt-line-color); --line-color-top: var(--tt-line-color); --line-width-bottom: 0.08em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --line-type-bottom: solid; --line-type-left: solid; --line-type-right: solid; --line-type-top: solid; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; background-color: #ededed }
    #tinytable_fxlkf32uyj5wzvhhr83q td.tinytable_css_9md2mh9qy3wqrjovdcf1, #tinytable_fxlkf32uyj5wzvhhr83q th.tinytable_css_9md2mh9qy3wqrjovdcf1 { background-color: #ededed }
    #tinytable_fxlkf32uyj5wzvhhr83q td.tinytable_css_lwqg9t2u8jbs7onw7caz, #tinytable_fxlkf32uyj5wzvhhr83q th.tinytable_css_lwqg9t2u8jbs7onw7caz {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: var(--tt-line-color); --line-color-left: var(--tt-line-color); --line-color-right: var(--tt-line-color); --line-color-top: var(--tt-line-color); --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.08em; --line-type-bottom: solid; --line-type-left: solid; --line-type-right: solid; --line-type-top: solid; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; background-color: #EAF2FB }
    </style>
    <div class="container">
      <table class="tinytable" id="tinytable_fxlkf32uyj5wzvhhr83q" style="table-layout: fixed; width: 95% !important; margin-left: auto; margin-right: auto;" data-quarto-disable-processing='true'>
        
        <thead>
              <tr>
                <th scope="col" data-row="0" data-col="1">Variable</th>
                <th scope="col" data-row="0" data-col="2">Name</th>
                <th scope="col" data-row="0" data-col="3">Type</th>
                <th scope="col" data-row="0" data-col="4">Population</th>
                <th scope="col" data-row="0" data-col="5">Age</th>
                <th scope="col" data-row="0" data-col="6">Unit</th>
                <th scope="col" data-row="0" data-col="7">Countries</th>
                <th scope="col" data-row="0" data-col="8">Quality</th>
              </tr>
        </thead>
        
        <tbody>
                <tr>
                  <td data-row="1" data-col="1">ahwealj992</td>
                  <td data-row="1" data-col="2">Net personal wealth</td>
                  <td data-row="1" data-col="3">Average</td>
                  <td data-row="1" data-col="4">equal-split adults</td>
                  <td data-row="1" data-col="5">Adults</td>
                  <td data-row="1" data-col="6">local currency (151 codes)</td>
                  <td data-row="1" data-col="7">216</td>
                  <td data-row="1" data-col="8">0.2</td>
                </tr>
                <tr>
                  <td data-row="2" data-col="1">anninci992</td>
                  <td data-row="2" data-col="2">National income</td>
                  <td data-row="2" data-col="3">Average</td>
                  <td data-row="2" data-col="4">individuals</td>
                  <td data-row="2" data-col="5">Adults</td>
                  <td data-row="2" data-col="6">local currency (151 codes)</td>
                  <td data-row="2" data-col="7">223</td>
                  <td data-row="2" data-col="8"></td>
                </tr>
                <tr>
                  <td data-row="3" data-col="1">npopuli992</td>
                  <td data-row="3" data-col="2">Population</td>
                  <td data-row="3" data-col="3">Population</td>
                  <td data-row="3" data-col="4">individuals</td>
                  <td data-row="3" data-col="5">Adults</td>
                  <td data-row="3" data-col="6">population</td>
                  <td data-row="3" data-col="7">232</td>
                  <td data-row="3" data-col="8"></td>
                </tr>
                <tr>
                  <td data-row="4" data-col="1">shwealj992</td>
                  <td data-row="4" data-col="2">Net personal wealth</td>
                  <td data-row="4" data-col="3">Share</td>
                  <td data-row="4" data-col="4">equal-split adults</td>
                  <td data-row="4" data-col="5">Adults</td>
                  <td data-row="4" data-col="6">share</td>
                  <td data-row="4" data-col="7">216</td>
                  <td data-row="4" data-col="8">0.2</td>
                </tr>
                <tr>
                  <td data-row="5" data-col="1">xlcusxi999</td>
                  <td data-row="5" data-col="2">Market exchange rate, LCU per USD</td>
                  <td data-row="5" data-col="3">Exchange rates</td>
                  <td data-row="5" data-col="4">individuals</td>
                  <td data-row="5" data-col="5">All Ages</td>
                  <td data-row="5" data-col="6">local currency per foreign currency</td>
                  <td data-row="5" data-col="7">223</td>
                  <td data-row="5" data-col="8"></td>
                </tr>
        </tbody>
      </table>
    </div>
<!-- hack to avoid NA insertion in last line -->
```

:::
:::


* Source: WID.world bulk download `wid_all_data.zip` (`WID_data_XX.csv`, `WID_metadata_XX.csv`)
* Variable code = series + population letter + age code, e.g. `ahweal` + `j` + `992`
* Units (WID `unit` field):
  * `ahwealj992` (average net personal wealth), `anninci992` (average national income): the country's own currency, e.g. JPY for Japan, USD for the United States; converted here at `xlcusxi999` of the country's latest wealth year
  * `xlcusxi999`: local currency per US$
  * `npopuli992`: persons
  * `shwealj992`: share of national net personal wealth, 0 to 1
* Population (`shortpop`):
  * equal-split adults (`j`): individuals, with resources split equally within couples
  * individuals (`i`): no sharing within couples
* Age: `992` = individuals over age 20; `999` = all ages
* Net personal wealth: non-financial and financial assets of households minus their debts
* Quality: mean of WID's `avg_quality` score; empty where WID gives none


::: {.cell}

```{.r .cell-code  code-summary="Code: WID source and method texts"}
# WID source and method notes, top 1% wealth share: the 5 most common texts
# Drop WID link markup: [URL][URL_LINK]..[/URL_LINK][URL_TEXT]text[/URL_TEXT][/URL] -> text
unlink <- function(x) trimws(gsub("\\[/?URL[A-Z_]*\\]", " ", gsub("\\[URL_LINK\\].*?\\[/URL_LINK\\]", "", x, perl = TRUE)))
ttx(mt[variable == "shwealj992", .N, by = .(Source = substr(unlink(source), 1, 200))][order(-N)][1:5], c(0.7, 0.1))
```

::: {.cell-output-display}

```{=html}

    <script src="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.js"></script>

    <script>
      // Create table-specific functions using external factory
      const tableFns_sbxejksvyv6sncjif4mc = TinyTable.createTableFunctions("tinytable_sbxejksvyv6sncjif4mc");
      // tinytable span after
      window.addEventListener('load', function () {
          var cellsToStyle = [
            // tinytable style arrays after
          { positions: [ { i: '5', j: 2 } ], css_id: 'tinytable_css_wd4i3m5i12yk521oqzts',}, 
          { positions: [ { i: '2', j: 2 }, { i: '4', j: 2 } ], css_id: 'tinytable_css_vsyjm513mazw2bwoteap',}, 
          { positions: [ { i: '1', j: 2 }, { i: '3', j: 2 } ], css_id: 'tinytable_css_s7jg2e3tphd279ib9a5v',}, 
          { positions: [ { i: '0', j: 2 } ], css_id: 'tinytable_css_i7z8vto2dtyijrjz7d1b',}, 
          { positions: [ { i: '5', j: 1 } ], css_id: 'tinytable_css_bc9rd9v1ltzty6av4j3g',}, 
          { positions: [ { i: '2', j: 1 }, { i: '4', j: 1 } ], css_id: 'tinytable_css_ikrx6e5na11omrr8hkt1',}, 
          { positions: [ { i: '1', j: 1 }, { i: '3', j: 1 } ], css_id: 'tinytable_css_7rd0iynu8kcz6ds3d4bg',}, 
          { positions: [ { i: '0', j: 1 } ], css_id: 'tinytable_css_u9kq7z4uaz5hssatk0qs',}, 
          ];

          // Loop over the arrays to style the cells
          cellsToStyle.forEach(function (group) {
              group.positions.forEach(function (cell) {
                  tableFns_sbxejksvyv6sncjif4mc.styleCell(cell.i, cell.j, group.css_id);
              });
          });
      });
    </script>

    <link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.css">
    <style>
    /* tinytable css entries after */
    #tinytable_sbxejksvyv6sncjif4mc td.tinytable_css_wd4i3m5i12yk521oqzts, #tinytable_sbxejksvyv6sncjif4mc th.tinytable_css_wd4i3m5i12yk521oqzts {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: var(--tt-line-color); --line-color-left: var(--tt-line-color); --line-color-right: var(--tt-line-color); --line-color-top: var(--tt-line-color); --line-width-bottom: 0.08em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --line-type-bottom: solid; --line-type-left: solid; --line-type-right: solid; --line-type-top: solid; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; background-color: #ededed; width: 12.5%; }
    #tinytable_sbxejksvyv6sncjif4mc td.tinytable_css_vsyjm513mazw2bwoteap, #tinytable_sbxejksvyv6sncjif4mc th.tinytable_css_vsyjm513mazw2bwoteap { width: 12.5%; }
    #tinytable_sbxejksvyv6sncjif4mc td.tinytable_css_s7jg2e3tphd279ib9a5v, #tinytable_sbxejksvyv6sncjif4mc th.tinytable_css_s7jg2e3tphd279ib9a5v { background-color: #ededed; width: 12.5%; }
    #tinytable_sbxejksvyv6sncjif4mc td.tinytable_css_i7z8vto2dtyijrjz7d1b, #tinytable_sbxejksvyv6sncjif4mc th.tinytable_css_i7z8vto2dtyijrjz7d1b {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: var(--tt-line-color); --line-color-left: var(--tt-line-color); --line-color-right: var(--tt-line-color); --line-color-top: var(--tt-line-color); --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.08em; --line-type-bottom: solid; --line-type-left: solid; --line-type-right: solid; --line-type-top: solid; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; background-color: #EAF2FB; width: 12.5%; }
    #tinytable_sbxejksvyv6sncjif4mc td.tinytable_css_bc9rd9v1ltzty6av4j3g, #tinytable_sbxejksvyv6sncjif4mc th.tinytable_css_bc9rd9v1ltzty6av4j3g {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: var(--tt-line-color); --line-color-left: var(--tt-line-color); --line-color-right: var(--tt-line-color); --line-color-top: var(--tt-line-color); --line-width-bottom: 0.08em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --line-type-bottom: solid; --line-type-left: solid; --line-type-right: solid; --line-type-top: solid; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; background-color: #ededed; width: 87.5%; }
    #tinytable_sbxejksvyv6sncjif4mc td.tinytable_css_ikrx6e5na11omrr8hkt1, #tinytable_sbxejksvyv6sncjif4mc th.tinytable_css_ikrx6e5na11omrr8hkt1 { width: 87.5%; }
    #tinytable_sbxejksvyv6sncjif4mc td.tinytable_css_7rd0iynu8kcz6ds3d4bg, #tinytable_sbxejksvyv6sncjif4mc th.tinytable_css_7rd0iynu8kcz6ds3d4bg { background-color: #ededed; width: 87.5%; }
    #tinytable_sbxejksvyv6sncjif4mc td.tinytable_css_u9kq7z4uaz5hssatk0qs, #tinytable_sbxejksvyv6sncjif4mc th.tinytable_css_u9kq7z4uaz5hssatk0qs {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: var(--tt-line-color); --line-color-left: var(--tt-line-color); --line-color-right: var(--tt-line-color); --line-color-top: var(--tt-line-color); --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.08em; --line-type-bottom: solid; --line-type-left: solid; --line-type-right: solid; --line-type-top: solid; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; background-color: #EAF2FB; width: 87.5%; }
    </style>
    <div class="container">
      <table class="tinytable" id="tinytable_sbxejksvyv6sncjif4mc" style="table-layout: fixed; width: 80%; margin-left: auto; margin-right: auto;" data-quarto-disable-processing='true'>
        
        <thead>
              <tr>
                <th scope="col" data-row="0" data-col="1">Source</th>
                <th scope="col" data-row="0" data-col="2">N</th>
              </tr>
        </thead>
        
        <tbody>
                <tr>
                  <td data-row="1" data-col="1">Bajard, F., Chancel, L., Moshrif, R., Piketty, T. (2021). “Global Wealth Inequality on WID.world: Estimates and Imputations”  Technote for the update:   Bajard, F., Bauluz, L., Brassac, P., Chancel, L</td>
                  <td data-row="1" data-col="2">179</td>
                </tr>
                <tr>
                  <td data-row="2" data-col="1">Blanchet, T., Martinez-Toledano, C. (2021), Distributional Financial Accounts in Europe;   Technote for the update:   Bajard, F., Bauluz, L., Brassac, P., Chancel, L., Martinez-Toledano, C., Piketty,</td>
                  <td data-row="2" data-col="2">21</td>
                </tr>
                <tr>
                  <td data-row="3" data-col="1">Dell, Fabien, Thomas Piketty, and Emmanuel Saez (2005). Income and Wealth Concen- tration in Switzerland Over the 20th Century. CEPR Discussion Papers 5090;     Foellmi, Reto and Isabel Z. Martínez (</td>
                  <td data-row="3" data-col="2">1</td>
                </tr>
                <tr>
                  <td data-row="4" data-col="1">Piketty, Thomas; Yang, Li and Zucman, Gabriel (2016). Capital Accumulation, Private Property and Rising Inequality in China, 1978-2015;     Updated by Sehyun, H., Zhexun, M. (2022), "2022 Regional DIN</td>
                  <td data-row="4" data-col="2">1</td>
                </tr>
                <tr>
                  <td data-row="5" data-col="1">Albers, Thilo, Charlotte Bartels, and Moritz Schularick (2020). “The Distribution of Wealth in Germany, 1895-2018”. In: ECONtribute Discussion Papers;     Blanchet, T., Martinez-Toledano, C. (2021), D</td>
                  <td data-row="5" data-col="2">1</td>
                </tr>
        </tbody>
      </table>
    </div>
<!-- hack to avoid NA insertion in last line -->
```

:::

```{.r .cell-code  code-summary="Code: WID source and method texts"}
ttx(mt[variable == "shwealj992" & method != "", .N, by = .(Method = substr(method, 1, 200))][order(-N)][1:5], c(0.7, 0.1))
```

::: {.cell-output-display}

```{=html}

    <script src="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.js"></script>

    <script>
      // Create table-specific functions using external factory
      const tableFns_rdxn7gqn91dj329ztdjd = TinyTable.createTableFunctions("tinytable_rdxn7gqn91dj329ztdjd");
      // tinytable span after
      window.addEventListener('load', function () {
          var cellsToStyle = [
            // tinytable style arrays after
          { positions: [ { i: '5', j: 2 } ], css_id: 'tinytable_css_a86tljc6dj1mzwu0ez4p',}, 
          { positions: [ { i: '2', j: 2 }, { i: '4', j: 2 } ], css_id: 'tinytable_css_559you2e0plz6q5yrx8r',}, 
          { positions: [ { i: '1', j: 2 }, { i: '3', j: 2 } ], css_id: 'tinytable_css_tj8j4ghu8z4i27s66eyx',}, 
          { positions: [ { i: '0', j: 2 } ], css_id: 'tinytable_css_u7rg7oung2fq0mmmw7of',}, 
          { positions: [ { i: '5', j: 1 } ], css_id: 'tinytable_css_7s47kz207x1kjs0is7be',}, 
          { positions: [ { i: '2', j: 1 }, { i: '4', j: 1 } ], css_id: 'tinytable_css_s00h56usn3fvop3ufsvn',}, 
          { positions: [ { i: '1', j: 1 }, { i: '3', j: 1 } ], css_id: 'tinytable_css_0k1rmhilynx030phhhhy',}, 
          { positions: [ { i: '0', j: 1 } ], css_id: 'tinytable_css_1w6q8js14icjlvwr6fmg',}, 
          ];

          // Loop over the arrays to style the cells
          cellsToStyle.forEach(function (group) {
              group.positions.forEach(function (cell) {
                  tableFns_rdxn7gqn91dj329ztdjd.styleCell(cell.i, cell.j, group.css_id);
              });
          });
      });
    </script>

    <link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/vincentarelbundock/tinytable@main/inst/tinytable.css">
    <style>
    /* tinytable css entries after */
    #tinytable_rdxn7gqn91dj329ztdjd td.tinytable_css_a86tljc6dj1mzwu0ez4p, #tinytable_rdxn7gqn91dj329ztdjd th.tinytable_css_a86tljc6dj1mzwu0ez4p {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: var(--tt-line-color); --line-color-left: var(--tt-line-color); --line-color-right: var(--tt-line-color); --line-color-top: var(--tt-line-color); --line-width-bottom: 0.08em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --line-type-bottom: solid; --line-type-left: solid; --line-type-right: solid; --line-type-top: solid; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; background-color: #ededed; width: 12.5%; }
    #tinytable_rdxn7gqn91dj329ztdjd td.tinytable_css_559you2e0plz6q5yrx8r, #tinytable_rdxn7gqn91dj329ztdjd th.tinytable_css_559you2e0plz6q5yrx8r { width: 12.5%; }
    #tinytable_rdxn7gqn91dj329ztdjd td.tinytable_css_tj8j4ghu8z4i27s66eyx, #tinytable_rdxn7gqn91dj329ztdjd th.tinytable_css_tj8j4ghu8z4i27s66eyx { background-color: #ededed; width: 12.5%; }
    #tinytable_rdxn7gqn91dj329ztdjd td.tinytable_css_u7rg7oung2fq0mmmw7of, #tinytable_rdxn7gqn91dj329ztdjd th.tinytable_css_u7rg7oung2fq0mmmw7of {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: var(--tt-line-color); --line-color-left: var(--tt-line-color); --line-color-right: var(--tt-line-color); --line-color-top: var(--tt-line-color); --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.08em; --line-type-bottom: solid; --line-type-left: solid; --line-type-right: solid; --line-type-top: solid; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; background-color: #EAF2FB; width: 12.5%; }
    #tinytable_rdxn7gqn91dj329ztdjd td.tinytable_css_7s47kz207x1kjs0is7be, #tinytable_rdxn7gqn91dj329ztdjd th.tinytable_css_7s47kz207x1kjs0is7be {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 0; --line-color-bottom: var(--tt-line-color); --line-color-left: var(--tt-line-color); --line-color-right: var(--tt-line-color); --line-color-top: var(--tt-line-color); --line-width-bottom: 0.08em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.1em; --line-type-bottom: solid; --line-type-left: solid; --line-type-right: solid; --line-type-top: solid; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; background-color: #ededed; width: 87.5%; }
    #tinytable_rdxn7gqn91dj329ztdjd td.tinytable_css_s00h56usn3fvop3ufsvn, #tinytable_rdxn7gqn91dj329ztdjd th.tinytable_css_s00h56usn3fvop3ufsvn { width: 87.5%; }
    #tinytable_rdxn7gqn91dj329ztdjd td.tinytable_css_0k1rmhilynx030phhhhy, #tinytable_rdxn7gqn91dj329ztdjd th.tinytable_css_0k1rmhilynx030phhhhy { background-color: #ededed; width: 87.5%; }
    #tinytable_rdxn7gqn91dj329ztdjd td.tinytable_css_1w6q8js14icjlvwr6fmg, #tinytable_rdxn7gqn91dj329ztdjd th.tinytable_css_1w6q8js14icjlvwr6fmg {  position: relative; --border-bottom: 1; --border-left: 0; --border-right: 0; --border-top: 1; --line-color-bottom: var(--tt-line-color); --line-color-left: var(--tt-line-color); --line-color-right: var(--tt-line-color); --line-color-top: var(--tt-line-color); --line-width-bottom: 0.05em; --line-width-left: 0.1em; --line-width-right: 0.1em; --line-width-top: 0.08em; --line-type-bottom: solid; --line-type-left: solid; --line-type-right: solid; --line-type-top: solid; --trim-bottom-left: 0%; --trim-bottom-right: 0%; --trim-left-bottom: 0%; --trim-left-top: 0%; --trim-right-bottom: 0%; --trim-right-top: 0%; --trim-top-left: 0%; --trim-top-right: 0%; ; background-color: #EAF2FB; width: 87.5%; }
    </style>
    <div class="container">
      <table class="tinytable" id="tinytable_rdxn7gqn91dj329ztdjd" style="table-layout: fixed; width: 80%; margin-left: auto; margin-right: auto;" data-quarto-disable-processing='true'>
        
        <thead>
              <tr>
                <th scope="col" data-row="0" data-col="1">Method</th>
                <th scope="col" data-row="0" data-col="2">N</th>
              </tr>
        </thead>
        
        <tbody>
                <tr>
                  <td data-row="1" data-col="1">Before 1980, series is constructed based on the trend observed in the fiscal income data available (see sources).</td>
                  <td data-row="1" data-col="2">32</td>
                </tr>
                <tr>
                  <td data-row="2" data-col="1">In 1961 and 1971 data covers rural areas only. Net wealth is reported since 1981. In 1971, net wealth is estimated using the liabilies at the bracket level (assuming individuals remain in the same bra</td>
                  <td data-row="2" data-col="2">1</td>
                </tr>
                <tr>
                  <td data-row="3" data-col="1">The inheritance tax data for 2008-2009 was not used here because of inconsistency of figures in the data. The amount of net personal wealth estimated here is much less than the net household wealth in</td>
                  <td data-row="3" data-col="2">1</td>
                </tr>
                <tr>
                  <td data-row="4" data-col="1">Figures correspond to individual adult series (not splitting among household members). The series are estimated using the mixed method on survey and tax data combined (see full study). Before 1980, se</td>
                  <td data-row="4" data-col="2">1</td>
                </tr>
                <tr>
                  <td data-row="5" data-col="1">NA</td>
                  <td data-row="5" data-col="2">NA</td>
                </tr>
        </tbody>
      </table>
    </div>
<!-- hack to avoid NA insertion in last line -->
```

:::
:::


Saved in `data/`:

* `wid_extract.csv`, `wid_meta.csv`: rows kept from the WID bulk zip
* `wid_world.qs2`: all derived tables
* `wid_world_quantiles.csv`, `wid_world_top1.csv`, `wid_country_top1.csv`
* `wid_world_dist.png`, `wid_world_top1.png`, `wid_country_top1.png`

