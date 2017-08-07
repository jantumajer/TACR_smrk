library(relaimpo) ### Testovani vyznamnosti jednotlivym prediktoru v modelu
#####################

Dataset <- readXL("C:/honzaT/TACR_smrk/CZT/TreeBase_complete9.xls", rownames=FALSE, header=TRUE, na="", sheet="TreeBase_complete9", stringsAsFactors=TRUE)

SLT <- readXL("C:/honzaT/TACR_smrk/CZT/CT_SLT_20160912.xlsx", rownames=FALSE, header=TRUE, na="N/A", sheet="CT SLT", stringsAsFactors=TRUE) # SLT pro jednotlive plochy
SLT.rajon <- (readXL("C:/honzaT/TACR_smrk/CZT/CT_SLT_20160912.xlsx", rownames=FALSE, header=TRUE, na="N/A", sheet="SLT_Rajon", stringsAsFactors=TRUE))[,c(3,4)] # Kategorie (kapacita SK+ nasycenost SK) podle projektu rajonizace

merge.SLT <- merge(SLT, SLT.rajon, by.x="SLTcombined", by.y="SLT", all.x=TRUE)
merge <- merge(Dataset, merge.SLT[,c("IDPLOTS","Rajonizace")], by="IDPLOTS", all.x=TRUE)

### Pouze stromy s BAI > 0
CBAI_0 <- subset(merge, subset=CBAI>0)
CBAI_0 <- subset(CBAI_0, subset=BREAK2==100)
CBAI_0 <- subset(CBAI_0, subset=DEADTREE2==100)


Hist(CBAI_0$CBAI, scale="frequency", breaks="Sturges", col="darkgray")

### 

### Konverze veku a nadmorske vysky na faktory
# Nadmorska vyska sloucena do 4 stupnu (stupne 4 a 5 spojeny)

CBAI_0 <- within(CBAI_0, {AGE20YEARS <- factor(AGE20YEARS, labels=c('1','2','3','4','5','6','7'))
			  ALTITUDE_200M <- factor(ALTITUDE_200M, labels=c('do400m','400-600m','600-800m','nad800m','nad800m'))
			  ALTITUDECLASS <- factor(ALTITUDECLASS, labels=c('do400m','400-700m','nad700m'))
			  Rajonizace <- factor(Rajonizace, labels=c('1-nizka','2','3',"4-vysoka"))
			  ALTITUDE_200M <- droplevels(ALTITUDE_200M)})



LM.1 <- lm(CBAI ~ 1 + AGE20YEARS + DBH2_CM + ALTITUDECLASS : SEAS_SRA + SEAS_SRA + ALTITUDECLASS : SEAS_T + SEAS_T + BAL1 + Rajonizace : N_S2000, data=CBAI_0)

summary(LM.1)
vif(LM.1)
calc.relimp(LM.1, rela=T)

par(mfrow=c(2,2))
plot(residuals(LM.1) ~ CBAI_0$CBAI, xlim=c(0,100), ylim=c(-60,60)); abline(0,0, col="red")
plot(predict(LM.1) ~ CBAI_0$CBAI, xlim=c(0,100), ylim=c(0,100)); abline(0,1, col="red")
Hist(residuals(LM.1), scale="frequency", breaks=100, col="darkgray", xlim=c(-50,50))

### Srovnani modelu s interakcemi typu : a *
LM.1.interakce <- lm(CBAI ~ 1 + AGE20YEARS + DBH2_CM + ALTITUDECLASS * SEAS_SRA + ALTITUDECLASS * SEAS_T + BAL1 + Rajonizace + N_S1975, data=CBAI_0)
plot(predict(LM.1) ~ predict(LM.1.interakce), xlim=c(0,70), ylim=c(0,70)); abline(0,1,col="red")
summary(LM.1.interakce)
vif(LM.1.interakce)

plot(CBAI_0$CBAI ~ CBAI_0$N_S2000)






########################################
#### Mean paper

mean_paper <- readXL("C:/honzaT/aceczechfor/Mean_paper/NEW_Database_byZones_2.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Database_byZones_2", stringsAsFactors=TRUE)

LMa <- lm(TRW_A ~ CO2 + T_35_A + VS_PREC_A + NDEP_A * ZONE, data=mean_paper)
summary(LMa)
calc.relimp(LMa, rela=T)

LMb <- lm(TRW_B ~ CO2 + T_35_B + VS_PREC_B + NDEP_B * ZONE, data=mean_paper)
summary(LMb)
calc.relimp(LMb, rela=T)
