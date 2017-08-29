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

Souradnice <- read.table("C:/honzaT/TACR_smrk/CZT/CZT_souradnice.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE) # Souradnice ploch v S-JTSK
merge <- merge(merge, Souradnice[,c("IDPLOTS", "X", "Y")], by="IDPLOTS", all.x=TRUE)

### Pouze stromy s BAI > 0
######################################
CBAI_0 <- subset(merge, subset=CBAI>0)
CBAI_0 <- subset(CBAI_0, subset=BREAK2==100)
CBAI_0 <- subset(CBAI_0, subset=DEADTREE2==100)


Hist(CBAI_0$CBAI, scale="frequency", breaks="Sturges", col="darkgray")

### Prejmenovani promennych
########################################
names(CBAI_0)[c(26,198,146,60,100,99,12)] <- c("ALTITUDE","BAL","BAI","Ndep","SRAZKY","TEPLOTA","DBH")


### Konverze veku a nadmorske vysky na faktory
###############################################################
CBAI_0 <- within(CBAI_0, {AGE20YEARS <- factor(AGE20YEARS, labels=c('a1','a2','a3','a4','a5','a6','a7'))
			  ALTITUDECLASS <- factor(ALTITUDECLASS, labels=c('do400m','400-700m','nad700m'))
			  RAJONIZACE <- factor(Rajonizace, labels=c('1','2','3',"4"))
			  })

##### Test modelu pro stromy rostouci rychleji nez 40 cm2/rok
# CBAI_0 <- subset(CBAI_0, subset=(subset=(BAI>40)))


LM.czt <- lm(BAI ~ 1 + AGE20YEARS + DBH + ALTITUDECLASS : SRAZKY + ALTITUDECLASS : TEPLOTA + BAL + RAJONIZACE : Ndep, data=CBAI_0)

summary(LM.czt)
vif(LM.czt)
calc.relimp(LM.czt, rela=T)

PLOT.MODEL(LM.czt, CBAI_0)
plot(residuals(LM.czt) ~ CBAI_0$CLAY); abline(0,0, col="red")


### Srovnani modelu s interakcemi typu : a *
LM.czt.interakce <- lm(BAI ~ 1 + AGE20YEARS + DBH + ALTITUDECLASS * SRAZKY + ALTITUDECLASS * TEPLOTA + BAL + Rajonizace + Ndep, data=CBAI_0)
plot(predict(LM.czt) ~ predict(LM.czt.interakce), xlim=c(0,70), ylim=c(0,70)); abline(0,1,col="red")
summary(LM.czt.interakce)
vif(LM.czt.interakce)

names(CBAI_0)

			################################################################
			############# KRNAP ############################################
			################################################################


KRNAP <- readXL("C:/honzaT/TACR_smrk/KRNAP_monbase/KRNAP.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Trees", stringsAsFactors=TRUE)


### Vypocet prirustu DBH a H mezi lety 2003 a 2015
##################################################
KRNAP_dbh <- subset(KRNAP, subset=((X15_DBH>0) & (X03_DBH>0) & (X15_DBH>X03_DBH) & (X15_DBH-X03_DBH<200)), select=c(IDPlots, IDTrees, X03_DBH,X15_DBH))
KRNAP_h <- subset(KRNAP, subset=((X15_H>0) & (X03_H>0) & (X15_H>X03_H) & (X15_H-X03_H<8)), select=c(IDPlots, IDTrees, X03_H,X15_H))

KRNAP_dbh$X03_15_DBH <- with(KRNAP_dbh, (X15_DBH - X03_DBH)/10 ) # Prevedeno na cm
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
names(KRNAP)[c(333,336,328,329,327,335)] <- c("BAI","BAL","Ndep","SRAZKY","TEPLOTA","DBH")

KRNAP.an <- subset(KRNAP, subset=(!is.na(BAI) & X15_zlom==0 & X15_souse==0)) # Vyberu jenom stromy, ktere maji vypocteny prirust a nejsou souse nebo recentni zlom

LM.krnap <- lm(BAI ~ 1 + AGE20YEARS + DBH + ALTITUDECLASS : SRAZKY + ALTITUDECLASS : TEPLOTA + BAL + RAJONIZACE : Ndep, data=KRNAP.an)
summary(LM.krnap)
calc.relimp(LM.krnap, rela=T)

PLOT.MODEL(LM.krnap, KRNAP.an)


### Srovnani modelu CZT a KRNAP

par(mfrow=c(2,2))
plot(predict(LM.krnap) ~ predict(LM.czt, newdata=KRNAP.an), xlim=c(0,80), ylim=c(0,80)); abline(0,1, col="red")
plot(KRNAP.an$BAI ~ predict(LM.czt, newdata=KRNAP.an), xlim=c(0,80), ylim=c(0,80), main="BAI", xlab="CZT model", ylab="KRNAP model"); abline(0,1, col="red")
plot(KRNAP.an$BAI ~ predict(LM.krnap), xlim=c(0,80), ylim=c(0,80)); abline(0,1, col="red")

cor(na.omit(predict(LM.krnap)) , na.omit(predict(LM.czt, newdata=KRNAP.an)))
cor(predict(LM.krnap) , KRNAP.an$BAI, use="complete")^2
cor(predict(LM.czt, newdata=KRNAP.an) , KRNAP.an$BAI, use="complete")^2




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

LPB_Young <- subset(LPB_Young, subset=(Zlom.13==100 & Souse.13==100))
LPB_Old <- subset(LPB_Old, subset=(Zlom.13==100 & Souse.13==100))

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
names(LPB.an)[c(22,24,17,18,15,16,5)] <- c("BAI","BAL","Ndep","SRAZKY","TEPLOTA","ALTITUDE","DBH")

### Prevod Rajonizace ze spojite promenne na faktor, prevody jednotek BAI (mm2->cm2) a DBH (mm->cm)
##################################################
LPB.an <- within(LPB.an, {RAJONIZACE <- factor(RAJONIZACE, labels=c('3','4'))})
LPB.an$DBH <- LPB.an$DBH / 10
LPB.an$BAI <- LPB.an$BAI / 100

###
LPB.an <- subset(LPB.an, subset=!(ALTITUDECLASS=="do400m")) # Vsechny 3 plochy pod 400m maji stejnou hodnotu klimatickych parametru - singularita v modelu
LM.lasprobes <- lm(BAI ~ 1 + AGE20YEARS + DBH + ALTITUDECLASS : SRAZKY + ALTITUDECLASS : TEPLOTA + BAL + RAJONIZACE : Ndep, data=LPB.an)
summary(LM.lasprobes)

PLOT.MODEL(LM.lasprobes, LPB.an)


### Srovnani modelu CZT, LasProBes a KRNAP
CZT.estimate <- data.frame(summary(LM.czt)$coef[,c("Estimate", "Pr(>|t|)")]); CZT.estimate$Atr <- rownames(CZT.estimate)
LPB.estimate <- data.frame(summary(LM.lasprobes)$coef[,c("Estimate", "Pr(>|t|)")]); LPB.estimate$Atr <- rownames(LPB.estimate)
KRNAP.estimate <- data.frame(summary(LM.krnap)$coef[,c("Estimate", "Pr(>|t|)")]); KRNAP.estimate$Atr <- rownames(KRNAP.estimate)
srovnani <- merge(CZT.estimate, KRNAP.estimate, by="Atr", all=T); srovnani <- merge(srovnani, LPB.estimate, by="Atr", all=T); colnames(srovnani) <- c("Parametr", "CZT", "CZT.p", "KRNAP", "KRNAP.p", "LASPROBES", "LASPROBES.p")

par(mfrow=c(2,2))
plot(predict(LM.lasprobes) ~ predict(LM.czt, newdata=LPB.an), xlim=c(0,80), ylim=c(0,80), main="BAI", xlab="CZT model", ylab="LasProBes model"); abline(0,1, col="red")
plot(LPB.an$BAI ~ predict(LM.czt, newdata=LPB.an), xlim=c(0,80), ylim=c(0,80)); abline(0,1, col="red")
plot(LPB.an$BAI ~ predict(LM.lasprobes), xlim=c(0,80), ylim=c(0,80)); abline(0,1, col="red")

cor(na.omit(predict(LM.lasprobes)) , na.omit(predict(LM.czt, newdata=LPB.an)))
cor(predict(LM.lasprobes) , LPB.an$BAI, use="complete")^2
cor(predict(LM.czt, newdata=LPB.an) , LPB.an$BAI, use="complete")^2


########################################################################
########################### Porostni uroven ############################
########################################################################

CZT.plot <- aggregate(CBAI_0, by=list(IDPlots = CBAI_0$IDPLOTS), FUN="mean", na.rm=TRUE) # Prumery za kvantitativni promenne
KRNAP.plot <- aggregate(KRNAP.an, by=list(IDPlots = KRNAP.an$IDPlots), FUN="mean", na.rm=TRUE)
LPB.plot <- aggregate(LPB.an, by=list(IDPlots = LPB.an$IDPlots), FUN="mean", na.rm=TRUE)

nahrada <- c("IDPlots", "RAJONIZACE", "ALTITUDECLASS", "AGE20YEARS"); promenne <- c("IDPlots", "BAI", "BAL", "SRAZKY", "TEPLOTA", "Ndep", "DBH")

CZT.plot1 <- merge(CZT.plot[promenne], aggregate(CBAI_0[c("IDPLOTS", "RAJONIZACE", "ALTITUDECLASS", "AGE20YEARS")], by=list(CBAI_0$IDPLOTS), FUN=MODE), by.x="IDPlots", by.y="IDPLOTS", all.x=T) # NA hodnoty u faktoru nahrazuji nejcastejsimi hodnotami na dane plose
KRNAP.plot1 <- merge(KRNAP.plot[promenne], aggregate(KRNAP.an[nahrada], by=list(KRNAP.an$IDPlots), FUN=MODE), by="IDPlots", all.x=T)
LPB.plot1 <- merge(LPB.plot[promenne], aggregate(LPB.an[nahrada], by=list(LPB.an$IDPlots), FUN=MODE), by="IDPlots", all.x=T)

vahy.CZT <- aggregate(CBAI_0["IDPLOTS"], by=list(IDPlots = CBAI_0$IDPLOTS), FUN="length"); colnames(vahy.CZT) <- c("IDPlots", "N") # Vahy v modelu - pocet stromu na jednotlivych plochach
vahy.KRNAP <- aggregate(KRNAP.an["IDPlots"], by=list(IDPlots = KRNAP.an$IDPlots), FUN="length"); colnames(vahy.KRNAP) <- c("IDPlots", "N")
vahy.LPB <- aggregate(LPB.an["IDPlots"], by=list(IDPlots = LPB.an$IDPlots), FUN="length"); colnames(vahy.LPB) <- c("IDPlots", "N")

CZT.plot2 <- merge(CZT.plot1, vahy.CZT, by="IDPlots", all.x=T)
KRNAP.plot2 <- merge(KRNAP.plot1, vahy.KRNAP, by="IDPlots", all.x=T)
LPB.plot2 <- merge(LPB.plot1, vahy.LPB, by="IDPlots", all.x=T)


LM.czt.plot <- lm(BAI ~ 1 + AGE20YEARS + DBH + ALTITUDECLASS : SRAZKY + ALTITUDECLASS : TEPLOTA + BAL + RAJONIZACE : Ndep, weights=N, data=CZT.plot2)
summary(LM.czt.plot)
PLOT.MODEL(LM.czt.plot, CZT.plot1)

LM.krnap.plot <- lm(BAI ~ 1 + AGE20YEARS + DBH + ALTITUDECLASS : SRAZKY + ALTITUDECLASS : TEPLOTA + BAL + RAJONIZACE : Ndep, weights=N, data=KRNAP.plot2)
summary(LM.krnap.plot)
PLOT.MODEL(LM.krnap.plot, KRNAP.plot1)

LM.lpb.plot <- lm(BAI ~ 1 + AGE20YEARS + DBH + ALTITUDECLASS : SRAZKY + ALTITUDECLASS : TEPLOTA + BAL + RAJONIZACE : Ndep, weights=N, data=LPB.plot2)
summary(LM.lpb.plot)
PLOT.MODEL(LM.lpb.plot, LPB.plot1)

### Srovnani modelu CZT a KRNAP

cor(na.omit(predict(LM.krnap.plot)) , na.omit(predict(LM.czt.plot, newdata=KRNAP.plot2)))
cor(predict(LM.krnap.plot) , KRNAP.plot2$BAI, use="complete")^2
cor(predict(LM.czt.plot, newdata=KRNAP.plot2) , KRNAP.plot2$BAI, use="complete")^2

par(mfrow=c(2,2))
plot(predict(LM.krnap.plot) ~ predict(LM.czt.plot, newdata=KRNAP.plot1), xlim=c(0,50), ylim=c(0,50), main="BAI", xlab="CZT model", ylab="KRNAP model"); abline(0,1, col="red")
plot(KRNAP.plot1$BAI ~ predict(LM.czt.plot, newdata=KRNAP.plot1), xlim=c(0,50), ylim=c(0,50)); abline(0,1, col="red")
plot(KRNAP.plot1$BAI ~ predict(LM.krnap.plot), xlim=c(0,50), ylim=c(0,50)); abline(0,1, col="red")

### Srovnani modelu CZT a LasProBes

cor(na.omit(predict(LM.lpb.plot)) , na.omit(predict(LM.czt.plot, newdata=LPB.plot2)))
cor(predict(LM.lpb.plot) , LPB.plot2$BAI, use="complete")^2
cor(predict(LM.czt.plot, newdata=LPB.plot2) , LPB.plot2$BAI, use="complete")^2

par(mfrow=c(2,2))
plot(predict(LM.lpb.plot) ~ predict(LM.czt.plot, newdata=LPB.plot1), xlim=c(0,30), ylim=c(0,30), main="BAI", xlab="CZT model", ylab="LasProBes model"); abline(0,1, col="red")
plot(LPB.plot1$BAI ~ predict(LM.czt.plot, newdata=LPB.plot1), xlim=c(0,50), ylim=c(0,50)); abline(0,1, col="red")
plot(LPB.plot1$BAI ~ predict(LM.lpb.plot), xlim=c(0,50), ylim=c(0,50)); abline(0,1, col="red")

###############################################################################################################################################################################
### Funkce
###############################################################################################################################################################################


################################################################################
### Vypocet celkove throughfall depozice podle Oulehle et al. (2016) a Kopacek et al. (2012)
################################################################################

LPB.an <- DEPOZICE(LPB.an, prepsat=T)
KRNAP.an <- DEPOZICE(KRNAP.an, prepsat=T)
CBAI_0$SRAZKY <- CBAI_0$ANN_SRA; CBAI_0$TEPLOTA <- CBAI_0$ANN_T; CBAI_0 <- DEPOZICE(CBAI_0, prepsat=T)



DEPOZICE <- function(oblast, prepsat=F) {
oblast[,"c.so4"] <- NA; oblast[,"c.no3"] <- NA; oblast[,"c.nh4"] <- NA
print("Krok 1 - vypocet bulk koncentraci iontu ve srazkove vode")

for (i in c(1:nrow(oblast))) {
	if (oblast[i,"ALTITUDE"]>0) {oblast[i,"c.so4"] <- 10*exp(4.321 + 1.093e-4*oblast[i,"ALTITUDE"] - 3.24e-4*oblast[i,"SRAZKY"] + 9.541e-7*oblast[i,"Y"] + 1.607e-6*oblast[i,"X"])
				    oblast[i,"c.no3"] <- 10*exp(1.123 + 5.156e-3*oblast[i,"c.so4"] - 2.643e-7*oblast[i,"Y"])
				    oblast[i,"c.nh4"] <- 10*exp(0.316 -6.733e-5*oblast[i,"SRAZKY"] + 7.505e-3*oblast[i,"c.so4"] - 4.950e-7*oblast[i,"Y"] - 6.026e-7*oblast[i,"X"])}

		}

oblast[,"bulk.sdep"] <- NA; oblast[,"bulk.no3dep"] <- NA; oblast[,"bulk.nh4dep"] <- NA
print("Krok 2 - vypocet bulk depozic")

for (i in c(1:nrow(oblast))) {
	if (oblast[i,"ALTITUDE"]>0) {oblast[i,"bulk.sdep"] <- oblast[i,"c.so4"] * oblast[i,"SRAZKY"] / 100000
				    oblast[i,"bulk.no3dep"] <- oblast[i,"c.no3"] * oblast[i,"SRAZKY"] / 100000
				    oblast[i,"bulk.nh4dep"] <- oblast[i,"c.nh4"] * oblast[i,"SRAZKY"] / 100000}

		}

oblast[,"thf.no3dep"] <- NA; oblast[,"thf.nh4dep"] <- NA
print("Krok 3 - prepocet depozic z bulk na throughfall")

for (i in c(1:nrow(oblast))) {
	if (oblast[i,"ALTITUDE"]>0) {oblast[i,"thf.no3dep"] <- oblast[i,"bulk.no3dep"] * 1.69 # 1.69 = pomer througfal koncentarce a koncentrace ve srazkach pro NO3
				    oblast[i,"thf.nh4dep"] <- oblast[i,"bulk.nh4dep"] * 1.45 * 0.78} # 1.45 = pomer througfal koncentarce a koncentrace ve srazkach pro NH4 | 0.78 = korekce pro mikrobialni transformaci NH4 v korune
		}

print("Krok 4 - celkova throughfall depozice N")
for (i in c(1:nrow(oblast))) {
	oblast[i,"Ndep2"] <- oblast[i,"thf.no3dep"] + oblast[i,"thf.nh4dep"] }

if (prepsat==T) {oblast[,"Ndep"] <- oblast[,"Ndep2"]}

plot(oblast[,"Ndep2"] ~ oblast[,"Ndep"])
return(oblast)
}


##################################################################
### Funkce pro ziskani nejcastejsi hodnoty faktoru pri agregaci
# Nutne pouzit, protoze na nekterych plochach CZT a LPB jsou ruzne stare stromy
# Prevzato od uzivatele Jay ze StackOverflow
##################################################################

MODE <- function(x) {
	ux <- unique(x)
	ux[which.max(tabulate(match(x, ux)))]
	}

###################################################################
### Vykresleni grafu
###################################################################

PLOT.MODEL <- function(model, data) {
	par(mfrow=c(2,2))
	plot(residuals(model) ~ data$BAI, xlab="pozorovany BAI", ylab="residual", xlim=c(0,100), ylim=c(-40,40)); abline(0,0, col="red")
	plot(predict(model) ~ data$BAI, xlab="pozorovany BAI", ylab="modelovany BAI", xlim=c(0,100), ylim=c(0,100)); abline(0,1, col="red")
	Hist(residuals(model), scale="frequency", breaks=50, col="darkgray", xlim=c(-50,50), xlab="residual")
	}


