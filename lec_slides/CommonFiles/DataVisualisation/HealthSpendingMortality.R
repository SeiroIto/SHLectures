#データのダウンロード

<<Download life expectancy data with API 1>>=
library(WDI)
WDIsearch("expectancy")[, "name"]
@
<<Download life expectancy data with API 2>>=
WDIsearch("expectancy.*birth")
@
<<Download life expectancy data with API 3>>=
WDIsearch("capita.*health.*expen|health.*expen.*capita")[, "name"]
@
<<Download life expectancy data with API 4>>=
WDIsearch("^.ealth.*expenditure.*capita.*PPP.*curr")
@
<<Download life expectancy data with API 5, eval = F>>=
#library(wbackslashtats)
#HEdat0 <- wb(indicator=
#  wbackslashearch("^.ealth.*expenditure.*capita.*PPP")[, "indicatorID"])
HENames <- WDIsearch("^.ealth.*expenditure.*capita.*PPP.*curr|expectancy.*birth")
#HEdat1 <- WDI(indicator=HENames[1, "indicator"])
HEdat2 <- WDI(indicator=HENames[-1, "indicator"])
@
<<change HEdat2 to data.table>>=
library(data.table)
HEd2 <- data.table(HEdat2)
saveRDS(HEd2, paste0(path, "/DataVisualisation/LifeExpectancy.rds"))
@

#データの読み込みと整形、接合

<<read health spending data prn, echo = F>>=
# read tab separated text
#HEdat1 <- fread(paste0(path, "/DataVisualisation/",
#  "DomestiHealthExpenditurePerCapitaCurrentPPP.prn"), header = T)
<<read health spending data>>=
# read comma separated value text
HEdat1 <- fread(paste0(path, "/DataVisualisation/",
  "SH.XPD.CHEX.PP.CD.csv"), skip = 4, header = T)
@
<<def grepout>>=
grepout <- function(str, x)
  # returns element of match (not numbers)
  # str: character strings to find
  # x: character vector
  # perl: if F, do not use perl regular expressions
  x[grep(str, x, perl = T)]
@
<<example of grepout>>=
grepout("^[12]", colnames(HEdat1))
@
<<rename HEdat1>>=
setnames(HEdat1, grepout("^[12]", colnames(HEdat1)),
  paste0("val.", grepout("^[12]", colnames(HEdat1))))
@
<<rename HEdat2>>=
setnames(HEdat1, c("Country Name", "Country Code", 
  "Indicator Name", "Indicator Code"),
  c("country", "iso3c", "name", "indicator"))
colnames(HEdat1)
@
<<reshape HEdat1>>=
HEdat1L <- reshape(HEdat1, direction = "long", idvar = c("country", "name"), 
  varying = grepout("^val", colnames(HEdat1)))
@
<<rename val to indicator in HEdat1>>=
setnames(HEdat1L, "val", as.character(HEdat1[1, "indicator"]))
HEdat1L
@
<<drop unnecessary columns from HEdat1L>>=
HEdat1L[, grepout("name|ind|Co|^V6", colnames(HEdat1L)) := NULL ]
setnames(HEdat1L, "time", "year")
HEdat1L
@
<<read HEd2 and merge with HEdat1L>>=
HEd2 <- readRDS(paste0(path, "/DataVisualisation/LifeExpectancy.rds"))
HEd <- merge(HEdat1L, HEd2, by= c("country", "year"))
@
<<rename HEd column names>>=
setnames(HEd, c("SH.XPD.CHEX.PP.CD", "SP.DYN.LE00.MA.IN", 
  "SP.DYN.LE00.IN", "SP.DYN.LE00.FE.IN"), 
  c("Expend", "LifeyearsMale", "LifeyearsAll", "LifeyearsFemale"))
@
<<dimension of HEd>>=
dim(HEd)
@
<<summary of HEd>>=
summary(HEd)
@
<<turn country from a character variable to a factor variable>>=
HEd[, country := factor(country)]
@
<<change other characters to factors>>=
HEd[, c("iso2c", "iso3c") := lapply(list(iso2c, iso3c), factor)]
summary(HEd)
@
<<drop noncountry entries>>=
HEd <- HEd[!grepl(noncountries, country), ]
@
<<save merged data>>=
saveRDS(HEd, paste0(path, "/DataVisualisation/MergedData.rds"))
@

#描画

<<read merged data>>=
library(data.table)
HEd <- readRDS(paste0(path, "/DataVisualisation/MergedData.rds"))
summary(HEd[year == 2000, Expend])
@
<<fig 2000 health spending and lifeyears, results = "hide", warning = F>>=
library(ggplot2)
p <- ggplot(data = HEd[year == 2000, ], 
  aes(x = Expend, y = LifeyearsAll)) +
  geom_point()
ggsave(
  paste0(path, "/DataVisualisation/SpendingLifeyears2000.png")
  , p,
  width = 10, height = 10, units = "cm",
  dpi = 300
 )
<<echo = F, results = "hide", warning = F>>=
setEPS()
postscript(file =  
  paste0(path, "/DataVisualisation/SpendingLifeyears2000.eps")
  , horizontal = F)
print(p)
dev.off()
@
<<fig 2000 health spending and log lifeyears, results = "hide", warning = F>>=
library(ggplot2)
p <- p + scale_x_log10() + theme(axis.title = element_text(size = 8))
ggsave(
  paste0(path, "/DataVisualisation/SpendingLifeyears2000Log.png")
  , p,
  width = 10, height = 10, units = "cm",
  dpi = 300
 )
<<echo = F, results = "hide", warning = F>>=
setEPS()
postscript(file =  
  paste0(path, "/DataVisualisation/SpendingLifeyears2000Log.eps")
  , horizontal = F)
print(p)
dev.off()
@
<<fig 2000 health spending and log lifeyears and loess, message = F, results = "hide", warning = F>>=
library(ggplot2)
p <- p + geom_smooth()
ggsave(
  paste0(path, "/DataVisualisation/SpendingLifeyears2000LogLoess.png")
  , p,
  width = 10, height = 10, units = "cm",
  dpi = 300
 )
<<echo = F, results = "hide", message = F, warning = F>>=
setEPS()
postscript(file =  
  paste0(path, "/DataVisualisation/SpendingLifeyears2000LogLoess.eps")
  , horizontal = F)
print(p)
dev.off()
@
<<fig 2000 2016 lifeyears, results = "hide", warning = F>>=
library(ggplot2)
HEd0016 <- HEd[year == 2000| year == 2016, .(country, year, Expend,
  LifeyearsMale, LifeyearsFemale, LifeyearsAll)]
setnames(HEd0016, grepout("Li", colnames(HEd0016)),
  gsub("years", "years.", grepout("Li", colnames(HEd0016))))
HEd0016
HEd0016L <- reshape(HEd0016, direction = "long", idvar = c("country", "year"), 
  varying = grepout("Li", colnames(HEd0016)))
setnames(HEd0016L, "time", "category")
setkey(HEd0016L, country, category, year)
HEd0016L
HEd0016W <- reshape(HEd0016L, direction = "wide", 
  idvar = c("country", "category"), timevar = "year",
  v.names = c("Expend", "Lifeyears"))
HEd0016W
p <- ggplot(data = HEd0016W, 
  aes(x = Lifeyears.2000, y = Lifeyears.2016)) +
  geom_point(size = .5)
p <- p + theme(
  axis.title = element_text(size = 8),
  strip.text.x = element_text(color = "blue", size = 8, 
    margin = margin(0, .5, 0, .5, "cm"))
  )
p <- p + geom_abline(intercept = 0, slope = 1, colour = "red")
p <- p + facet_wrap( ~ category)
ggsave(
  paste0(path, "/DataVisualisation/SpendingLifeyears0008LogLoess.png")
  , p,
  width = 15, height = 6, units = "cm",
  dpi = 300
 )
@
<<countries at Female lifeyears between 55 and 65>>=
unique(countries5565 <- HEd0016[year == 2000 & 
  Lifeyears.Female >= 55 & Lifeyears.Female <= 65, country])
@
<<display countries 5565>>=
unique(HEd0016W[country %in% countries5565 & 
  Lifeyears.2016-Lifeyears.2000>=
    mean(Lifeyears.2016-Lifeyears.2000, na.rm = T), country])
unique(HEd0016W[country %in% countries5565 & 
  Lifeyears.2016-Lifeyears.2000<=
    mean(Lifeyears.2016-Lifeyears.2000, na.rm = T), country])
@
<<fig health expenditure and lifeyears 2000 2016, results = "hide", warning = F>>=
HEd0016L[, Expend := Expend[!is.na(Expend)][1], by = .(country, year)]
HEd0016L[, year := factor(year)]
p <- ggplot(data = HEd0016L, 
  aes(x = Expend, y = Lifeyears, group = country)) +
  scale_x_log10()+
  geom_point(size = .5, aes(shape = year, colour = year)) + 
  scale_colour_manual(values = c("red", "darkblue"))+
  scale_shape_manual(values = c(16, 15))+
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 8),
    strip.text.x = element_text(color = "blue", size = 8, 
      margin = margin(0, .5, 0, .5, "cm"))
    ) + 
  facet_wrap( ~ category)
ggsave(
  paste0(path, "/DataVisualisation/SpendingLifeyears0008Log.png")
  , p,
  width = 15, height = 10, units = "cm",
  dpi = 300
 )
@
<<fig health expenditure and lifeyears 2000 2016 arrow, results = "hide", warning = F>>=
p <- p+ geom_line(arrow = arrow(angle = 20, ends = "last", 
    length = unit(.15, "cm"), type = "closed"), alpha = .2, colour = "blue")
ggsave(
  paste0(path, "/DataVisualisation/SpendingLifeyears0008LogArrow.png")
  , p,
  width = 15, height = 10, units = "cm",
  dpi = 300
 )
@
