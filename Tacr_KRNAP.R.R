
KRNAP <- read.table("C:/Users/Jan Tumajer/Desktop/ifer/TACR_smrk/KRNAP.csv",header=TRUE, sep=";", na.strings="NA", dec=",", strip.white=TRUE)
names(KRNAP)

### Vypocet prirustu DBH a H mezi lety 2003 a 2015
KRNAP_dbh <- subset(KRNAP, subset=((X15_DBH>0) & (X03_DBH>0) & (X15_DBH>X03_DBH) & (X15_DBH-X03_DBH<200)), select=c(IDPlots, IDTrees, X03_DBH,X15_DBH))
KRNAP_h <- subset(KRNAP, subset=((X15_H>0) & (X03_H>0) & (X15_H>X03_H) & (X15_H-X03_H<8)), select=c(IDPlots, IDTrees, X03_H,X15_H))

KRNAP_dbh$X03_15_DBH <- with(KRNAP_dbh, X15_DBH - X03_DBH)
KRNAP_h$X03_15_H <- with(KRNAP_h, X15_H - X03_H)

KRNAP <- merge(KRNAP, KRNAP_dbh[,c("IDPlots", "IDTrees", "X03_15_DBH")], by=c("IDPlots", "IDTrees"), all=TRUE)
KRNAP <- merge(KRNAP, KRNAP_h[,c("IDPlots", "IDTrees", "X03_15_H")], by=c("IDPlots", "IDTrees"), all=TRUE)

### 

scatterplot(X03_15_H~X03_15_DBH, reg.line=FALSE, smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), data=KRNAP)
scatterplot(X16_defoliace_perc~X03_15_DBH, reg.line=FALSE, smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), data=KRNAP)
scatterplot(X16_defoliace_ht_perc~X03_15_DBH, reg.line=FALSE, smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), data=KRNAP)
scatterplot(X16_berevne_zmeny_perc~X03_15_DBH, reg.line=FALSE, smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), data=KRNAP)
scatterplot(X16_suchv~X03_15_DBH, reg.line=FALSE, smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), data=KRNAP)
scatterplot(X16_defoliace_ht_perc~X03_15_DBH, reg.line=FALSE, smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), data=KRNAP)

rcorr.adjust(KRNAP[,c("X03_15_DBH","X03_15_H","X16_berevne_zmeny_perc","X16_defoliace_ht_perc","X16_defoliace_perc","X16_suchv")], use="pairwise.complete")

###

summary(lm(X03_15_DBH~X16_defoliace_perc + X16_suchv, data=KRNAP))

