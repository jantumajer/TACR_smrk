library(relaimpo) ### Testovani vyznamnosti jednotlivym prediktoru v modelu
#####################

			################################################################
			############# CzechTerra #######################################
			################################################################

	
Dataset <- readXL("C:/honzaT/TACR_smrk/CZT/TreeBase_complete9.xls", rownames=FALSE, header=TRUE, na="", sheet="TreeBase_complete9", stringsAsFactors=TRUE)

SLT <- readXL("C:/honzaT/TACR_smrk/CZT/CT_SLT_20160912.xlsx", rownames=FALSE, header=TRUE, na="N/A", sheet="CT SLT", stringsAsFactors=TRUE) # SLT pro jednotlive plochy
SLT.rajon <- (readXL("C:/honzaT/TACR_smrk/CZT/CT_SLT_20160912.xlsx", rownames=FALSE, header=TRUE, na="N/A", sheet="SLT_Rajon", stringsAsFactors=TRUE))[,c(3,4)] # Kategorie (kapacita SK+ nasycenost SK) podle projektu rajonizace

merge.SLT <- merge(SLT, SLT.rajon, by.x="SLTcombined", by.y="SLT", all.x=TRUE)
merge <- merge(Dataset, merge.SLT[,c("IDPLOTS","Rajonizace")], by="IDPLOTS", all.x=TRUE)

### Pouze stromy s BAI > 0
######################################
CBAI_0 <- subset(merge, subset=CBAI>0)
CBAI_0 <- subset(CBAI_0, subset=BREAK2==100)
CBAI_0 <- subset(CBAI_0, subset=DEADTREE2==100)


Hist(CBAI_0$CBAI, scale="frequency", breaks="Sturges", col="darkgray")

### Prejmenovani promennych
########################################
names(CBAI_0)[c(198,146,60,100,99,12)] <- c("BAL","BAI","Ndep","SRAZKY","TEPLOTA","DBH")


### Konverze veku a nadmorske vysky na faktory
###############################################################
CBAI_0 <- within(CBAI_0, {AGE20YEARS <- factor(AGE20YEARS, labels=c('a1','a2','a3','a4','a5','a6','a7'))
			  ALTITUDECLASS <- factor(ALTITUDECLASS, labels=c('do400m','400-700m','nad700m'))
			  RAJONIZACE <- factor(Rajonizace, labels=c('1','2','3',"4"))
			  })

###

LM.czt <- lm(BAI ~ 1 + AGE20YEARS + DBH + ALTITUDECLASS : SRAZKY + ALTITUDECLASS : TEPLOTA + BAL + RAJONIZACE : Ndep, data=CBAI_0)

summary(LM.czt)
vif(LM.czt)
calc.relimp(LM.czt, rela=T)

par(mfrow=c(2,2))
plot(residuals(LM.czt) ~ CBAI_0$BAI, xlim=c(0,100), ylim=c(-60,60)); abline(0,0, col="red")
plot(predict(LM.czt) ~ CBAI_0$BAI, xlim=c(0,100), ylim=c(0,100)); abline(0,1, col="red")
Hist(residuals(LM.czt), scale="frequency", breaks=100, col="darkgray", xlim=c(-50,50))

### Srovnani modelu s interakcemi typu : a *
LM.czt.interakce <- lm(BAI ~ 1 + AGE20YEARS + DBH + ALTITUDECLASS * SRAZKY + ALTITUDECLASS * TEPLOTA + BAL + Rajonizace + Ndep, data=CBAI_0)
plot(predict(LM.czt) ~ predict(LM.czt.interakce), xlim=c(0,70), ylim=c(0,70)); abline(0,1,col="red")
summary(LM.czt.interakce)
vif(LM.czt.interakce)



			################################################################
			############# KRNAP ############################################
			################################################################


KRNAP <- readXL("C:/honzaT/TACR_smrk/KRNAP_monbase/KRNAP.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Trees", stringsAsFactors=TRUE)


### Vypocet prirustu DBH a H mezi lety 2003 a 2015
##################################################
KRNAP_dbh <- subset(KRNAP, subset=((X15_DBH>0) & (X03_DBH>0) & (X15_DBH>X03_DBH) & (X15_DBH-X03_DBH<200)), select=c(IDPlots, IDTrees, X03_DBH,X15_DBH))
KRNAP_h <- subset(KRNAP, subset=((X15_H>0) & (X03_H>0) & (X15_H>X03_H) & (X15_H-X03_H<8)), select=c(IDPlots, IDTrees, X03_H,X15_H))

KRNAP_dbh$X03_15_DBH <- with(KRNAP_dbh, (X15_DBH - X03_DBH)/100 ) # Prevedeno na cm
KRNAP_h$X03_15_H <- with(KRNAP_h, X15_H - X03_H)

### Vypocet BAI/rok mezi lety 2003 a 2015
##################################################
KRNAP_dbh$BAI_03_15 <- with(KRNAP_dbh, ( (pi*(X15_DBH/2)^2) - (pi*(X03_DBH/2)^2) )/(12*100))  # Prevedeno na cm2/rok

KRNAP <- merge(KRNAP, KRNAP_dbh[,c("IDPlots", "IDTrees", "X03_15_DBH", "BAI_03_15")], by=c("IDPlots", "IDTrees"), all=TRUE)
KRNAP <- merge(KRNAP, KRNAP_h[,c("IDPlots", "IDTrees", "X03_15_H")], by=c("IDPlots", "IDTrees"), all=TRUE)

### Prevod DBH v roce 2015 z mm na cm
##################################################
KRNAP$X15_DBH_cm <- KRNAP$X15_DBH / 10

### Prevod Rajonizace ze spojite promenne na faktor
##################################################
KRNAP <- within(KRNAP, {RAJONIZACE <- factor(RAJONIZACE, labels=c('1','2','3','4'))})

### Funkce pro vypocet BAL (suma vycetni zakladny vsech vetsich stromu) - zkontrolovat
##################################################
output <- data.frame(IDPlots=NA, IDTrees=NA, BAL15=NA)

for (i in unique(KRNAP$IDPlots)) {
	KRNAP.sub <- subset(KRNAP, subset=(IDPlots==i)) # Vyberu jenom stromy z dane plochy
	
	for (k in c(1:nrow(KRNAP.sub))) {
		threshold <- KRNAP.sub[k,"X15_DBH"] # Hodnota DBH daneho stromu ...

		if (is.na(threshold)==F) {
		KRNAP.sub2 <- subset(KRNAP.sub, subset=(X15_DBH > threshold & !is.na(X15_DBH))) # ... se pouzije jako threshold a vyberou se vetsi stromy.
		KRNAP.sub2$BA_15 <- 0.000001 * pi*((KRNAP.sub2$X15_DBH/2)^2) # U vyssich stromu se DBH prepocte na BA / rovnou to prevadim na m^2 
		output <- rbind(output, data.frame(IDPlots = KRNAP.sub[k,"IDPlots"], IDTrees = KRNAP.sub[k,"IDTrees"], BAL15 = sum(KRNAP.sub2$BA_15))) }

		else { output <- rbind(output, data.frame(IDPlots = KRNAP.sub[k,"IDPlots"], IDTrees = KRNAP.sub[k,"IDTrees"], BAL15 = NA)) }
				   }
			}

KRNAP <- merge(KRNAP, output, by=c("IDPlots", "IDTrees"), all.x=TRUE)

### Prejmenovani promennych
########################################
names(KRNAP)[c(331,334,328,329,327,333)] <- c("BAI","BAL","Ndep","SRAZKY","TEPLOTA","DBH")

###
KRNAP.an <- subset(KRNAP, subset=(!is.na(BAI)))

LM.krnap <- lm(BAI ~ 1 + AGE20YEARS + DBH + ALTITUDECLASS : SRAZKY + ALTITUDECLASS : TEPLOTA + BAL + RAJONIZACE : Ndep, data=KRNAP.an)
summary(LM.krnap)
calc.relimp(LM.krnap, rela=T)

par(mfrow=c(2,2))
plot(residuals(LM.krnap) ~ KRNAP.an$BAI, xlim=c(0,100), ylim=c(-60,60)); abline(0,0, col="red")
plot(predict(LM.krnap) ~ KRNAP.an$BAI, xlim=c(0,100), ylim=c(0,100)); abline(0,1, col="red")
Hist(residuals(LM.krnap), scale="frequency", breaks=100, col="darkgray", xlim=c(-50,50))

### Srovnani modelu CZT a KRNAP

par(mfrow=c(2,2))
plot(predict(LM.krnap) ~ predict(LM.czt, newdata=KRNAP.an), xlim=c(0,80), ylim=c(0,80)); abline(0,1, col="red")
plot(KRNAP.an$BAI ~ predict(LM.czt, newdata=KRNAP.an), xlim=c(0,80), ylim=c(0,80)); abline(0,1, col="red")
plot(KRNAP.an$BAI ~ predict(LM.krnap), xlim=c(0,80), ylim=c(0,80)); abline(0,1, col="red")

cor(na.omit(predict(LM.krnap)) , na.omit(predict(LM.czt, newdata=KRNAP.an)))
cor(predict(LM.krnap) , KRNAP.an$BAI, use="complete")
cor(predict(LM.czt, newdata=KRNAP.an) , KRNAP.an$BAI, use="complete")




			################################################################
			############# Jablunkov ########################################
			################################################################



LPB_Old.10 <- readXL("C:/honzaT/TACR_smrk/LASPROBES/LASPROBES_Old.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Trees_2011", stringsAsFactors=TRUE)
LPB_Old.12 <- readXL("C:/honzaT/TACR_smrk/LASPROBES/LASPROBES_Old.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Trees_2012", stringsAsFactors=TRUE)
LPB_Old.13 <- readXL("C:/honzaT/TACR_smrk/LASPROBES/LASPROBES_Old.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Trees_2013", stringsAsFactors=TRUE)

LPB_Old.A <- merge(LPB_Old.10, LPB_Old.12, by=c("IDPlots", "ID"), all=T, suffixes=c(".10", ".12"))
LPB_Old <- merge(LPB_Old.A, LPB_Old.13, by=c("IDPlots", "ID"), all=T) # Kompletni datovy soubor starych ploch - setreni za roky 2010, 2012 a 2013

#############

LPB_Young.10 <- readXL("C:/honzaT/TACR_smrk/LASPROBES/LASPROBES_Young.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Trees", stringsAsFactors=TRUE)
LPB_Young.13 <- readXL("C:/honzaT/TACR_smrk/LASPROBES/LASPROBES_Young.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Trees_2013", stringsAsFactors=TRUE)

LPB_Young <- merge(LPB_Young.10, LPB_Young.13, by=c("IDPlots", "ID"), all=T, suffixes=c(".10", ".13")) # Kompletni datovy soubor mladych ploch - setreni za roky 2010 a 2013

#############

vyber.promennych.Old <- c("IDPlots", "ID", "DBH_mm.10", "H_m.10", "DBH_mm.12", "H_m.12", "DBH_mm.13", "H_m.13", "Drevina.10", "Vek.10", "Vek.12", "Vek.13") 
vyber.promennych.Young <- c("IDPlots", "ID", "DBH_mm.10", "H_m.10", "DBH_mm.13", "H_m.13", "Drevina.10", "Vek.10", "Vek.13")

LPB_Old.2 <- LPB_Old[vyber.promennych.Old]
LPB_Young.2 <- LPB_Young[vyber.promennych.Young] # Pro dalsi operace pracuji jenom se zjednodusenymi datovymi soubory, ktere obsahuji jenom potrebne promenne

LPB <- merge(LPB_Old.2, LPB_Young.2, all=T)
nrow(LPB)==nrow(LPB_Old.2) + nrow(LPB_Young.2) # Kontrola, zda se pripojily vsechny stromy

#############
## Pripojeni dat na urovni plochy (nadmorska vyska, klima, depozice)

LPB.plot.level <- read.table("C:/honzaT/TACR_smrk/LASPROBES/LASPROBES_plots.csv", header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
LPB <- merge(LPB, LPB.plot.level, by.x="IDPlots", by.y="ID", all.x=T)

#############

LPB.an <- subset(LPB, subset= (!is.na(DBH_mm.10) & !is.na(DBH_mm.13) & Drevina.10==1 & (DBH_mm.10 < DBH_mm.13) )) # Do analyzy vstoupi pouze smrky, ktere maji zmerene DBH v letech 2013 a 2011
	### !!! Temer 300 stromu (1417-1162) bylo vyfiltrovano na kriteriu, ze musi mit kladny prirust


### Vypocet BAI/rok mezi lety 2003 a 2015
LPB.an$BAI_10.13 <- with(LPB.an, ( (pi*(DBH_mm.13/2)^2) - (pi*(DBH_mm.10/2)^2) )/3)

### Vypocet 20letych vekovych trid v roce 2013
LPB.an$AGE20YEARS <- with(LPB.an, (paste("a", ceiling(Vek.13/20), sep="")))

### Funkce pro vypocet BAL (suma vycetni zakladny vsech vetsich stromu) - zkontrolovat - prevzato z KRNAP

output <- data.frame(IDPlots=NA, ID=NA, BAL.13=NA)

for (i in unique(LPB.an$IDPlots)) {
	LPB.sub <- subset(LPB.an, subset=(IDPlots==i)) # Vyberu jenom stromy z dane plochy
	
	for (k in c(1:nrow(LPB.sub))) {
		threshold <- LPB.sub[k,"DBH_mm.13"] # Hodnota DBH daneho stromu ...

		if (is.na(threshold)==F) {
		LPB.sub2 <- subset(LPB.sub, subset=(DBH_mm.13 > threshold & !is.na(DBH_mm.13))) # ... se pouzije jako threshold a vyberou se vetsi stromy.
		LPB.sub2$BA_13 <- 0.000001 * pi*((LPB.sub2$DBH_mm.13/2)^2) # U vyssich stromu se DBH prepocte na BA / rovnou to prevadim na m^2 
		output <- rbind(output, data.frame(IDPlots = LPB.sub[k,"IDPlots"], ID = LPB.sub[k,"ID"], BAL.13 = sum(LPB.sub2$BA_13))) }

		else { output <- rbind(output, data.frame(IDPlots = LPB.sub[k,"IDPlots"], ID = LPB.sub[k,"ID"], BAL.13 = NA)) }
				   }
			}

LPB.an <- merge(LPB.an, output, by=c("IDPlots", "ID"), all.x=TRUE)

### Prejmenovani promennych
########################################
names(LPB.an)[c(22,24,17,18,15,5)] <- c("BAI","BAL","Ndep","SRAZKY","TEPLOTA","DBH")

### Prevod Rajonizace ze spojite promenne na faktor, prevody jednotek BAI (mm2->cm2) a DBH (mm->cm)
##################################################
LPB.an <- within(LPB.an, {RAJONIZACE <- factor(RAJONIZACE, labels=c('3','4'))})
LPB.an$DBH <- LPB.an$DBH / 10
LPB.an$BAI <- LPB.an$BAI / 100

###
LPB.an <- subset(LPB.an, subset=!(ALTITUDECLASS=="do400m")) # Vsechny 3 plochy pod 400m maji stejnou hodnotu klimatickych parametru - singularita v modelu
LM.lasprobes <- lm(BAI ~ 1 + AGE20YEARS + DBH + ALTITUDECLASS : SRAZKY + ALTITUDECLASS : TEPLOTA + BAL + RAJONIZACE : Ndep, data=LPB.an)
summary(LM.lasprobes)


par(mfrow=c(2,2))
plot(residuals(LM.lasprobes) ~ LPB.an$BAI, xlim=c(0,100), ylim=c(-60,60)); abline(0,0, col="red")
plot(predict(LM.lasprobes) ~ LPB.an$BAI, xlim=c(0,100), ylim=c(0,100)); abline(0,1, col="red")
Hist(residuals(LM.lasprobes), scale="frequency", breaks=100, col="darkgray", xlim=c(-50,50))


### Srovnani modelu CZT, LasProBes a KRNAP
CZT.estimate <- data.frame(summary(LM.czt)$coef[,c("Estimate", "Pr(>|t|)")]); CZT.estimate$Atr <- rownames(CZT.estimate)
LPB.estimate <- data.frame(summary(LM.lasprobes)$coef[,c("Estimate", "Pr(>|t|)")]); LPB.estimate$Atr <- rownames(LPB.estimate)
KRNAP.estimate <- data.frame(summary(LM.krnap)$coef[,c("Estimate", "Pr(>|t|)")]); KRNAP.estimate$Atr <- rownames(KRNAP.estimate)
srovnani <- merge(CZT.estimate, KRNAP.estimate, by="Atr", all=T); srovnani <- merge(srovnani, LPB.estimate, by="Atr", all=T); colnames(srovnani) <- c("Parametr", "CZT", "CZT.p", "KRNAP", "KRNAP.p", "LASPROBES", "LASPROBES.p")

par(mfrow=c(2,2))
plot(predict(LM.lasprobes) ~ predict(LM.czt, newdata=LPB.an), xlim=c(0,80), ylim=c(0,80)); abline(0,1, col="red")
plot(LPB.an$BAI ~ predict(LM.czt, newdata=LPB.an), xlim=c(0,80), ylim=c(0,80)); abline(0,1, col="red")
plot(LPB.an$BAI ~ predict(LM.lasprobes), xlim=c(0,80), ylim=c(0,80)); abline(0,1, col="red")

cor(na.omit(predict(LM.lasprobes)) , na.omit(predict(LM.czt, newdata=LPB.an)))
cor(predict(LM.lasprobes) , LPB.an$BAI, use="complete")
cor(predict(LM.czt, newdata=LPB.an) , LPB.an$BAI, use="complete")





########################################
#### Mean paper

mean_paper <- readXL("C:/honzaT/aceczechfor/Mean_paper/NEW_Database_byZones_2.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Database_byZones_2", stringsAsFactors=TRUE)

LMa <- lm(TRW_A ~ CO2 + T_35_A + VS_PREC_A + NDEP_A * ZONE, data=mean_paper)
summary(LMa)
calc.relimp(LMa, rela=T)

LMb <- lm(TRW_B ~ CO2 + T_35_B + VS_PREC_B + NDEP_B * ZONE, data=mean_paper)
summary(LMb)
calc.relimp(LMb, rela=T)
