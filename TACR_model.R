library(Rcmdr)

####################### Nacteni dat ###################################
### Pouze testovaci (nerealna) data

stromy <- readXL("C:/honzaT/TACR_smrk/testovaci.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Trees", stringsAsFactors=TRUE)
plochy <- readXL("C:/honzaT/TACR_smrk/testovaci.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Plots", stringsAsFactors=TRUE)

############ Testovani ###################
# 1. faze: propojeni <- DO.ALL(plochy, stromy)

# 3. faze: propojeni.agregovane <- AGGREG(propojeni)

# 2+4. faze:	   MODEL(propojeni, Level="Tree")
		MODEL(propojeni.agregovane, Level="Plot")

########################################################################
################### Faze 1 : priprava ##################################
########################################################################

################### Vypocet BAL #############################
# vstupni dataset na urovni stromu

BAL.calc <- function(Dataset) {

output <- data.frame(IDPlots=NA, IDTrees=NA, BAL=NA)

for (i in unique(Dataset$IDPlots)) {
	Dataset.plocha <- subset(Dataset, subset=(IDPlots==i)) # Vyberu jenom stromy z dane plochy
	
	for (k in c(1:nrow(Dataset.plocha))) {
		threshold <- Dataset.plocha[k,"DBH"] # Hodnota DBH daneho stromu ...

		if (is.na(threshold)==F) {
		Dataset.plocha.vetsi <- subset(Dataset.plocha, subset=(DBH > threshold & !is.na(DBH))) # ... se pouzije jako threshold a vyberou se vetsi stromy.
		Dataset.plocha.vetsi$BA <- 0.0001 * pi*((Dataset.plocha.vetsi$DBH/2)^2) # U vyssich stromu se DBH prepocte na BA / rovnou to prevadim na m^2 
		output <- rbind(output, data.frame(IDPlots = Dataset.plocha[k,"IDPlots"], IDTrees = Dataset.plocha[k,"IDTrees"], BAL = sum(Dataset.plocha.vetsi$BA))) }

		else { output <- rbind(output, data.frame(IDPlots = Dataset.plocha[k,"IDPlots"], IDTrees = Dataset.plocha[k,"IDTrees"], BAL = NA)) }
				   }
			}

Dataset <- merge(Dataset, output, by=c("IDPlots", "IDTrees"), all.x=TRUE)

return(Dataset)

}

##################################################################

################### Prevod nadmorske vysky na kategorie ##########################
# vstupni dataset na urovni ploch

ALT.class <- function(Dataset) {

for (i in c(1:nrow(Dataset))) {
	if (Dataset[i,"ALTITUDE"]<400) {Dataset[i,"ALTITUDECLASS"] <- "do400m"}
	if (Dataset[i,"ALTITUDE"]>400 && Dataset[i,"ALTITUDE"]<700) {Dataset[i,"ALTITUDECLASS"] <- "400_700m"}
	if (Dataset[i,"ALTITUDE"]>700) {Dataset[i,"ALTITUDECLASS"] <- "nad700m"}
	}

return(Dataset)

}

###################################################################################

################### Prevod veku na kategorie ##########################
# vstupni dataset muze byt na urovni ploch (asi castejsi pripad) nebo stromu - bude specifikovano vstupnim parametrem

AGE.class <- function(Dataset) {

for (i in c(1:nrow(Dataset))) {
	if (Dataset[i,"AGE"]<21) {Dataset[i,"AGECLASS"] <- "do20let"}
	if (Dataset[i,"AGE"]>=21 && Dataset[i,"AGE"]<41) {Dataset[i,"AGECLASS"] <- "21_40let"}
	if (Dataset[i,"AGE"]>=41 && Dataset[i,"AGE"]<61) {Dataset[i,"AGECLASS"] <- "41_60let"}
	if (Dataset[i,"AGE"]>=61 && Dataset[i,"AGE"]<81) {Dataset[i,"AGECLASS"] <- "61_80let"}
	if (Dataset[i,"AGE"]>=81 && Dataset[i,"AGE"]<101) {Dataset[i,"AGECLASS"] <- "81_100let"}
	if (Dataset[i,"AGE"]>=101 && Dataset[i,"AGE"]<121) {Dataset[i,"AGECLASS"] <- "101_120let"}
	if (Dataset[i,"AGE"]>=121) {Dataset[i,"AGECLASS"] <- "nad120let"}

	}

return(Dataset)
}

########################################################################

################### Prevod SLT na pufracni kapacitu pud Rajonizace ##########################
# Vstupni data na urovni plochy

PUFR.class <- function(Dataset) {

	# Seznam SLT a jejich prirazeni k jednotlivym kategoriim pufrace
	.raj1 <- c("0Z", "1Z", "2Z", "3Z", "4Z", "5Z", "6Z", "7Z", "8Z", "9Z", "0Y", "2Y", "3Y", "4Y", "5Y", "6Y", "7Y", "8Y", "9Y", "0M", "1M", "2M", "3M", "4M", "5M", "6M", "7M", "8M", "0K", "9K", "0Q", "1Q", "2Q", "3Q", "4Q", "5Q", "6Q", "7Q", "8Q", "0T", "1T", "2T", "3T", "5T", "6T", "7T", "8T", "3R", "5R", "7R", "8R", "9R")
	.raj2 <- c("1K", "2K", "3K", "4K", "5K", "6K", "7K", "8K", "0N", "1N", "2N", "3N", "4N", "5N", "6N", "7N", "8N", "1I", "2I", "3I", "4I", "5I", "6I", "1S", "7S", "8S", "0O", "7O", "0P", "1P", "2P", "3P", "4P", "5P", "6P", "7P", "6G", "7G", "8G", "0R", "4R", "6R")
	.raj3 <- c("2S", "3S", "4S", "5S", "6S", "3F", "4F", "5F", "6F", "7F", "8F", "0C", "6B", "7B", "3H", "4H", "5H", "6H", "6A", "7A", "8A", "3L", "5L", "3U", "5U", "3V", "6V", "7V", "8V", "1O", "2O", "3O", "4O", "5O", "6O", "0G", "4G", "5G")
	.raj4 <- c("0X", "1C", "2C", "3C", "4C", "5C", "1B", "2B", "3B", "4B", "5B", "1H", "2H", "1D", "2D", "3D", "4D", "5D", "6D", "1A", "2A", "3A", "4A", "5A", "1J", "2J", "3J", "5J", "1L", "2L", "1U", "1V", "4V", "5V", "1G", "3G", "1X", "2X", "3X", "4X", "1W", "2W", "3W", "4W", "5W")

for (i in c(1:nrow(Dataset))) {
	if (Dataset[i,"SLT"] %in% .raj1) {Dataset[i,"PUFR"] <- "pufrace1"}	
	if (Dataset[i,"SLT"] %in% .raj2) {Dataset[i,"PUFR"] <- "pufrace2"}	
	if (Dataset[i,"SLT"] %in% .raj3) {Dataset[i,"PUFR"] <- "pufrace3"}	
	if (Dataset[i,"SLT"] %in% .raj4) {Dataset[i,"PUFR"] <- "pufrace4"}		
		
	}
return(Dataset)
}

##############################################################################################

################### Depozicni model ##########################
# Vstupni data na urovni plochy


DEP.model <- function(Dataset) {

pomocne <- Dataset
pomocne[,"c.so4"] <- NA; pomocne[,"c.no3"] <- NA; pomocne[,"c.nh4"] <- NA
print("Krok 1 - vypocet bulk koncentraci iontu ve srazkove vode")

for (i in c(1:nrow(pomocne))) {
	if (pomocne[i,"ALTITUDE"]>0) {pomocne[i,"c.so4"] <- 10*exp(4.321 + 1.093e-4*pomocne[i,"ALTITUDE"] - 3.24e-4*pomocne[i,"PRECIPITATION"] + 9.541e-7*pomocne[i,"Y"] + 1.607e-6*pomocne[i,"X"])
				    pomocne[i,"c.no3"] <- 10*exp(1.123 + 5.156e-3*pomocne[i,"c.so4"] - 2.643e-7*pomocne[i,"Y"])
				    pomocne[i,"c.nh4"] <- 10*exp(0.316 -6.733e-5*pomocne[i,"PRECIPITATION"] + 7.505e-3*pomocne[i,"c.so4"] - 4.950e-7*pomocne[i,"Y"] - 6.026e-7*pomocne[i,"X"])}

		}

print("Mezikrok 1.5 - vypocet troughfall koncentraci iontu SO4 ve srazkove vode")
for (i in c(1:nrow(pomocne))) {
	pomocne[i,"c.so4thf"] <- NA
	if (pomocne[i,"ALTITUDE"]>0) {pomocne[i,"c.so4thf"] <- 10*exp(1.557 + 1.165 * log(pomocne[i,"c.so4"]) - 3.57e-4*pomocne[i,"PRECIPITATION"] + 8.941e-7*pomocne[i,"X"])
		}	
	
pomocne[,"bulk.sdep"] <- NA; pomocne[,"bulk.no3dep"] <- NA; pomocne[,"bulk.nh4dep"] <- NA
print("Krok 2 - vypocet bulk depozic")

for (i in c(1:nrow(pomocne))) {
	if (pomocne[i,"ALTITUDE"]>0) {pomocne[i,"bulk.sdep"] <- pomocne[i,"c.so4"] * pomocne[i,"PRECIPITATION"] / 100000
				    pomocne[i,"bulk.no3dep"] <- pomocne[i,"c.no3"] * pomocne[i,"PRECIPITATION"] / 100000
				    pomocne[i,"bulk.nh4dep"] <- pomocne[i,"c.nh4"] * pomocne[i,"PRECIPITATION"] / 100000}

		}

pomocne[,"thf.no3dep"] <- NA; pomocne[,"thf.nh4dep"] <- NA
print("Krok 3 - prepocet depozic z bulk na throughfall")

for (i in c(1:nrow(pomocne))) {
	if (pomocne[i,"ALTITUDE"]>0) {pomocne[i,"thf.no3dep"] <- pomocne[i,"bulk.no3dep"] * 1.69 # 1.69 = pomer througfall koncentarce a koncentrace ve srazkach pro NO3
				    pomocne[i,"thf.nh4dep"] <- pomocne[i,"bulk.nh4dep"] * 1.45 * 0.78} # 1.45 = pomer througfall koncentarce a koncentrace ve srazkach pro NH4 | 0.78 = korekce pro mikrobialni transformaci NH4 v korune
		}

print("Mezikrok 3.5 - troughfall koncentrace siry na througfall depozici")
pomocne[i,"thf.so4dep"] <- NA
	
for (i in c(1:nrow(pomocne))) {
	if (pomocne[i,"ALTITUDE"]>0) {pomocne[i,"thf.so4dep"] <- pomocne[i,"c.so4thf"] * pomocne[i,"PRECIPITATION"] / 100000
				    }
	
print("Krok 4 - celkova throughfall depozice N")
for (i in c(1:nrow(pomocne))) {
	pomocne[i,"Ndep"] <- pomocne[i,"thf.no3dep"] + pomocne[i,"thf.nh4dep"] }

Dataset[,"NDEP"] <- pomocne[,"Ndep"]
Dataset[,"SDEP"] <- pomocne[i,"thf.so4dep"] 

return(Dataset)
}


##################### Uzivatelska funkce - pripravi vse #############################
# Vstupem databaze za plochy i stromy

DO.ALL <- function(Plots, Trees) {

	Trees <- BAL.calc(Trees)

	Plots <- AGE.class(Plots)
	Plots <- ALT.class(Plots)
	Plots <- PUFR.class(Plots)
	Plots <- DEP.model(Plots)

data.merge <- merge(Trees, Plots, by="IDPlots", all.x=T)

return(data.merge)

}



########################################################################
############## Faze 2 + 4 : model stromove/porostni urovne #############
########################################################################

MODEL <- function(Dataset, Level) {

# Jine nodnoty koeficientu pro stromovou a porostni uroven:

if (Level=="Tree") {
	.dbh.param <- 0.911777 # Parametry s unikatni hodnotou
	.bal.param <- -1.558535

	.temp.param <- data.frame(CODE=c("do400m", "400_700m", "nad700m"), VALUE=c(-1.145069, -0.799968, -0.470491)) # Tabulky s parametry se specifickymi hodnotami
	.prec.param <- data.frame(CODE=c("do400m", "400_700m", "nad700m"), VALUE=c(0.015303, 0.010009, 0.007738))
	.ndep.param <- data.frame(CODE=c("pufrace1", "pufrace2", "pufrace3", "pufrace4"), VALUE=c(-6.281425, -6.013290, -5.647832, -5.552308))
	.intercept.param <- data.frame(CODE=c("do20let", "21_40let", "41_60let", "61_80let", "81_100let", "101_120let", "nad120let"), VALUE=c(5.24, 2.86, -1.28, -4.60, -7.58, -9.36, -10.89)) }

if (Level=="Plot") {
	.dbh.param <- 0.827501 # Parametry s unikatni hodnotou
	.bal.param <- -3.456227

	.temp.param <- data.frame(CODE=c("do400m", "400_700m", "nad700m"), VALUE=c(-1.113455, -0.716397, -0.360121)) # Tabulky s parametry se specifickymi hodnotami
	.prec.param <- data.frame(CODE=c("do400m", "400_700m", "nad700m"), VALUE=c(0.014531, 0.008695, 0.006223))
	.ndep.param <- data.frame(CODE=c("pufrace1", "pufrace2", "pufrace3", "pufrace4"), VALUE=c(-4.957312, -4.761930, -4.390128, -4.416950))
	.intercept.param <- data.frame(CODE=c("do20let", "21_40let", "41_60let", "61_80let", "81_100let", "101_120let", "nad120let"), VALUE=c(6.06, 4.38, 1.42, -0.52, -2.66, -3.21, -5.47)) }

for (i in c(1:nrow(Dataset))) {
	
	Dataset[i, "MODEL"] <- .intercept.param[.intercept.param$CODE==Dataset[i,"AGECLASS"], "VALUE"] + 
					+ .prec.param[.prec.param$CODE==Dataset[i,"ALTITUDECLASS"], "VALUE"] * Dataset[i, "PRECIPITATION"] + 
					+ .temp.param[.temp.param$CODE==Dataset[i,"ALTITUDECLASS"], "VALUE"] * Dataset[i, "TEMPERATURE"]+ 
					+ .ndep.param[.ndep.param$CODE==Dataset[i,"PUFR"], "VALUE"] * Dataset[i, "NDEP"]+ 
					+ .dbh.param * Dataset[i, "DBH"]+
					+ .bal.param * Dataset[i, "BAL"]
	
	}

return(Dataset)

}


########################################################################
################ Faze 3 : agregace na porostni uroven ##################
########################################################################

###################### Skryta funkce - nepristupna uzivateli ######################
.MODE <- function(x) {
	ux <- unique(x)
	ux[which.max(tabulate(match(x, ux)))]
	}

######################################################################################

###################### Funkce pro agregaci ###########################################

AGGREG <- function(Dataset) {

means <- aggregate(Dataset[c("DBH", "BAL", "TEMPERATURE", "PRECIPITATION", "NDEP", "X", "Y")], by=list(IDPlots = Dataset$IDPlots), FUN="mean", na.rm=TRUE)
counts <- aggregate(Dataset[c("IDPlots")], by=list(IDPlots = Dataset$IDPlots), FUN="length"); colnames(counts) <- c("IDPlots", "WEIGHT")
frequency <- aggregate(Dataset[c("IDPlots", "AGECLASS", "ALTITUDECLASS", "PUFR")], by=list(Dataset$IDPlots), FUN=.MODE)

Dataset.agregace <- merge(means, counts, by="IDPlots"); Dataset.agregace <- merge(Dataset.agregace, frequency, by="IDPlots")

return(Dataset.agregace)

}
