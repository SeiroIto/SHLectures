tog <- textConnection("
year “s–¯ Ž©–¯ Œö–¾ ‹¤ŽY —§–¯ ˆÛV ƒlƒbƒg –³Š‘®
2021 31 33 23 19 15 1 1 4
2017 -15 8 0 1 8 0 0 -1
")
tog <- data.table(read.table(tog, header = T))
pts <- colnames(tog)[-1]
setnames(tog, pts, paste0("value.", pts))
togL <- reshape(tog, direction = "long", idvar = "year",
  varying = grepout("va", colnames(tog)))
setnames(togL, "time", "parties")
togL[year == 2017, value := value*(-1)]
togL[, value := cumsum(value), by = parties]
togL[, year := factor(year)]
togL[, parties := factor(parties, levels = pts)]
library(ggplot2)
g <- ggplot(togL, aes(x = parties, y = value, fill = year)) +
  geom_col(position = "dodge") + scale_fill_viridis_d(direction = -1) +
  guides(fill = guide_legend(title = "‘I‹“”N", title.position = "left", nrow = 1)) +
  ylab("‹cÈ”")+
  xlab("­“}")+
  theme(
   axis.title = element_text(size=8), #change legend title font size
   axis.text = element_text(size=6), #change axis text font size
   legend.key.size = unit(.25, 'cm'), #change legend key size
   legend.title = element_text(size=8), #change legend title font size
   legend.text = element_text(size=6),
  legend.position = "bottom")
cairo_pdf(
  paste0("c:/seiro/docs/external/seishin/lec_slides/2021/DataVisualisation/Togikaisen20172021.pdf")
  , width=10/2.54, height=10/2.54, 
  family = "Meiryo")
print(g)
whatever <- dev.off()
