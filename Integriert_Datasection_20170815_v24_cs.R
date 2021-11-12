###################################################################################################
###################################################################################################
### Research project: KOMIED
### Projekt-Nr. 2094-2012
###################################################################################################
###################################################################################################
### Projektleitung: Astrid Cullmann (DIW Berlin)
### Datengrundlage: AFiD-Panel Energieunternehmen & Energiebetriebe, JAB, URS (Organschaft)
### 
### Dateiname des Programmcodes: Integriert_Deskriptives_20170814_v24_cs.R
### erstellt: 14.08.2017
### von: Caroline Stiel
### E-Mail: cstiel@diw.de
### Tel. 030 89789 514
###
####################################################################################################
###
### Paper: Produktivitätsmessung bei integrierten Unternehmen
###        --------------------------------------------------
###
###
### Aufbau der Analyse in R (Integrierte Unternehmen)
### -------------------------------------------------
### Kapitel 1: Aggregierung mittels Organschaftsverhältnis
### Kapitel 2: Rekommunalisierungstrend (Wochenbericht)
### Kapitel 3: Produktitivätsmessung bei integrierten Unternehmen (Sample construction)  
### Kapitel 4: Produktitivätsmessung bei integrierten Unternehmen (Data Deskriptives) -SKRIPT-
### Kapitel 5: Produktitivätsmessung bei integrierten Unternehmen (Strukturelle Schätzung) 
###
###
####################################################################################################
### Grundriss: Deskriptive Statistiken für die Data Section im Paper
####################################################################################################
###
### Version 24 : - Verwendung von Datensatz V3
###              - kleinere Korrekturen
#############################  Start Analyse #######################################################


###################################################################################################
##				0) 	Preparation                                              ##
###################################################################################################


# Clean memory 
rm(list=ls())


########################################
##   0.1  Working Directory FDZ       ##
########################################

# Arbeitsdateien:
#Pfad1<-"Q:\\AFS\\55_FDZ\\Forschungsprojekte\\2012-2094 DIW - Richter (4)\\Arbeitsdateien\\"

# Originaldaten:
#Pfad2<-"Q:\\AFS\\55_FDZ\\Forschungsprojekte\\2012-2094 DIW - Richter (4)\\Daten\\" 

#  Outputdateien (im jeweiligen Datumsordner):
#Pfad3<-"Q:\\AFS\\55_FDZ\\Forschungsprojekte\\2012-2094 DIW - Richter (4)\\KDFV\\2017_08_09_cs\\"

# R-Packages:
#.libPaths("Q:\\AFS\\55_FDZ\\Forschungsprojekte\\2012-2094 DIW - Richter (4)\\Arbeitsdateien\\R_Packages_neu")


######################################
##   0.2   Working Directory DIW    ##
######################################


# Anpassen der Working Directory an den DIW Rechner
# Ich benutze dieselben Pfadbezeichnungen wie im FDZ.
# In "Pfad1" liegen diese Datei sowie die Dateien mit den selbstgeschriebenen Funktionen.
# In "Pfad2" liegen die Daten.
# In "Pfad3" speichere ich die Ergebnisse ab. In diesem Fall die Log-Datei und die .csv, .pdf
# In ".lib.path" liegen die benötigten Packages.


getwd()    
Pfad1 <- ("V:\\projects\\current\\efficiency\\02_KOMIED\\04_paper_current\\19_integrierte_unternehmen_cs\\02_R_code\\04_Produktivitaet_Data_Section\\Arbeitsdaten\\")
Pfad2 <- ("V:\\projects\\current\\efficiency\\02_KOMIED\\04_paper_current\\19_integrierte_unternehmen_cs\\02_R_code\\04_Produktivitaet_Data_Section\\Daten\\")
Pfad3 <- ("V:\\projects\\current\\efficiency\\02_KOMIED\\04_paper_current\\19_integrierte_unternehmen_cs\\02_R_code\\04_Produktivitaet_Data_Section\\localoutput\\")
.libPaths("V:\\projects\\current\\efficiency\\02_KOMIED\\04_paper_current\\19_integrierte_unternehmen_cs\\02_R_code\\00_packages")


######################################
##   0.3   Log-file & Time Stamp    ##
######################################


setwd(Pfad3)

# Erfasse einmal zu Beginn und am Ende die Zeit, um überprüfen zu können, wielange der Code
# beim FDZ läuft.
date()


#########################################
##   0.4       Packages	     	       ##
#########################################


# Abgleich mit dem Ordner .../efficiency/02_KOMIED/03_data/06_PackagesForFDZ zeigt, ob das
# benötigte Package bereits auf dem FDZ-Rechner installiert ist.

# Install packages
# ----------------
# 1) .zip-File des Paketes runterladen und in die oben angegebene .libPaths directory legen (DIW)
#    bzw. per Email ans FDZ schicken.
# 2) Installieren über install.packages-Befehl, z.B.:
# install.packages(paste("",.libPaths()[1],"\\lmtest_0.9-34.zip",sep=""),repos=NULL)


# load packages
# -------------
library(reshape)
library(foreign)
library(data.table)
library(gdata)
library(plm)
library(car)
library(ggplot2)
library(readstata13)
library(zoo)


############################################
##	0.5	Daten einlesen		  ##
############################################


# Arbeitsdatensatz einlesen (DIW & FDZ)
# -------------------------------------
data <- read.dta(file.path(Pfad1,"data_public_single_final_cs_v3.dta"))


# Originaldatensatz einlesen (DIW)
# --------------------------------
data_original <- read.dta13(file.path(Pfad2,"/na_2094-2012_Energie_JAB_Panel_DSF_erw_full_v01_2003-2014.dta"))

# Originaldatensatz einlesen (FDZ)
# --------------------------------
# data_original <- read.dta13(file.path(Pfad2,"/na2094-2012_Energie_JAB_Panel_erw_full_mit_Organ_II_2003-2014.dta"))  


class(data)
dim(data)


#--- Tell R to use fixed notation instead of exponential notation, i.e. display 500000 instead of 
# 5*e+05 (especially in plots)
options(scipen=999)




##########################################################
## 	0.6	Selbstgeschriebene Funktionen  	        ##         
##########################################################

# Der folgende Abschnitt enthält selbstgeschriebene Funktionen. Die jeweiligen Ziele der Funktion 
# werden im Kasten darueber erklaert.

####################################################################################################
# Funktion dstat                                                                                   #
# --------------                                                                                   #
# Deskriptive Statistik. Liefert 1 bzw 5, 25, 50,75 und 95 bzw 99% Quantile, den Ratio aus         #
# Q75/Q25-Quantil, den Ratio aus Q95/Q5-Quantil, den Median, Mittelwert, Varianz, STABW, Summe     #
# aller, Zahl der Nullen, Punkte .und NA's je Variable. Max und                                    #
# zweitgrößter Wert sind für das FDZ zur Dominanzprüfung - löschen vom FDZ vor Weitergabe          #
#                                                                                                  #
# Inputs: X - Matrix mit Variablen, die der deskriptiven Analyse unterzogen werden sollen          #
# Inputs: d - Anzahl der Nachkommastellen, auf die gerundet wird  (default=0)                      #
####################################################################################################

dstat <- function(X,d){
  X <- as.matrix(X)
  mat <- matrix(NA, ncol=17, nrow=ncol(X))
  colnames(mat) <- c("Q1","Q5","Q25","Med","Mean","Q75","Q95","Q99","Var", "SD"
                     , "Sum","nichtNAs", "Nullen", "Anzahl_'.'","NAs","Max","ZweitgroessterWert")
  rownames(mat) <- colnames(X)
  mat[,1] <- round(apply(X, 2, quantile, probs=0.01, na.rm=T),digits=d)
  mat[,2] <- round(apply(X, 2, quantile, probs=0.05, na.rm=T),digits=d)
  mat[,3] <- round(apply(X, 2, quantile, probs=0.25, na.rm=T),digits=d)
  mat[,4] <- round(apply(X, 2, median, na.rm=T),digits=d)
  mat[,5] <- round(apply(X, 2, mean, na.rm=T),digits=d)
  mat[,6] <- round(apply(X, 2, quantile, probs=0.75, na.rm=T),digits=d)
  mat[,7] <- round(apply(X, 2, quantile, probs=0.95, na.rm=T),digits=d)
  mat[,8] <- round(apply(X, 2, quantile, probs=0.99, na.rm=T),digits=d)
  mat[,9] <- round(apply(X, 2, var,na.rm=T),digits=d)
  mat[,10] <- round(apply(X, 2, sd, na.rm=T), digits=d)
  mat[,11] <- round(apply(X, 2, sum, na.rm=T), digits=d)
  mat[,12] <- round(apply(X, 2, nobs),digits=d)
  for (i in 1:ncol(X)) {mat[i,13] <- length(which(X[,i]==0))[1]}
  for (i in 1:ncol(X)) {mat[i,14] <- length(which(X[,i]=="."))[1]}
  for (i in 1:ncol(X)) {mat[i,15] <- length(which(is.na(X[,i])))[1]}
  mat[,16] <- round(apply(X, 2, max, na.rm=T), digits=d)
  for (i in 1:ncol(X)) {mat[i,17] <- round(sort(X[,i],decreasing=T)[2],digits=d)}
  return(mat)
}




###################################################################################################
##					  Start							 ##
###################################################################################################

###################################################################################################
##            1) Replizierung des Samples, das für die Schätzung verwendet wird                  ##
###################################################################################################


# Erstelle die neue Output-Variable 'value_added3' (mixed utilities, liberalised, power plants)
# ---------------------------------------------------------------------------------------------
# revenues1 (nicht preisbereinigt) - intermediates (preisbereinigt mit Vorleistungsgüterindex)
data$value_added3 <- data$revenues1 - data$intermediates
cor(data$value_added2,data$value_added3,use="complete.obs")

# Erstelle die Output-Variable 'value_added_wa' für die pure Water utilities
# --------------------------------------------------------------------------
# Note: Heißt in der Schätzung auch value_added3

# Preisbereinige die Revenues dafür mit dem PPI für Wasser
# Quelle: Destatis. Erzeugerpreisindizes gewerblicher Produkte. GP 2009, Basisjahr=2010.
# 5-Steller, GP09-36001  Wasser
defl_wa <- cbind("Jahr"=seq(2003,2014),"defl_wa"=c(0.886,0.912,0.934,0.946,0.958,0.967,0.992
                                                   ,1,1.011,1.031,1.049,1.063))
data <- merge(data,defl_wa,by="Jahr")
data <- data[order(data$id,data$unr,data$Jahr),]
data$revenues_wa <- data$revenues1/data$defl_wa
dstat(data$revenues_wa/10^6,d=2)
dstat(data$revenues1/10^6,d=2)

# Erstelle die Output-Variable 'value_added_wa'
# revenues_wa (preisbereinigt) - intermediates (preisbereinigt mit Vorleistungsgüterindex)
data$value_added_wa <- data$revenues_wa - data$intermediates
cor(data$value_added3,data$value_added_wa,use="complete.obs")


# Erstelle data0 erst einmal ohne Beschränkung auf positive Inputs und Outputs
# -----------------------------------------------------------------------------
# um später analysieren zu können, wieviele Unternehmen durch diese Restriktion herausfallen.
data0 <- data 

# Bereinige Datensatz um konstante Reihen.
# ---------------------------------------
data0 <- subset(data0,select=-c(id_ns,id_he1,id_he2,id_he3,id_he4,id_th,id_sh,jab,public
                                ,Lueckenjahre,out_labour,out_lohn,out_int,out_umsatz))



# Kontrolliere für die Präsenz in bestimmten Untersektoren
# ---------------------------------------------------------
data0$sa_EVU <- ifelse(is.na(data0$U_ABS_EF1011_sum)==FALSE & data0$U_ABS_EF1011_sum>0,1,0)
data0$sa_TK <- ifelse(is.na(data0$U_ABS_EF1051_sum)==FALSE & data0$U_ABS_EF1051_sum>0,1,0)
data0$sa_SK <- ifelse(is.na(data0$U_ABS_EF1041_sum)==FALSE & data0$U_ABS_EF1041_sum>0,1,0)
data0$wm_NWG <- ifelse((is.na(data0$B_waerme_EF1011a)==FALSE & data0$B_waerme_EF1011a>0) 
                       | (is.na(data0$B_waerme_EF1010)==FALSE & data0$B_waerme_EF1010>0),1,0)
data0$wm_HH <- ifelse(is.na(data0$B_waerme_EF1011b)==FALSE & data0$B_waerme_EF1011b>0,1,0)



# Erstelle die PPI in den einzelnen Sektoren.
# ------------------------------------------
# Quelle: GENESIS. Erzeugerpreisindizes gewerblicher Produkte. GP 2009, Basisjahr=2010.
# Da bei der Stromverteilung vor 2005 keine Werte verfügbar sind, setze dort 2003=2004=2005.

# 6-Steller, GP09-351310 Dienstleistung der Elektrizitätsverteilung:
defl_sn <- cbind("Jahr"=seq(2003,2014),"defl_sn"=log(c(112.9,112.9,112.9,110.9,102.3,94.5,98.8,100
                                                       ,99.8,106.3,119.7,126.4)))
# 5-Steller, GP09-35222 Dienstl.d.Gasversorg.d.Rohrleit.u.d.Handels m.Gas
defl_ga <- cbind("Jahr"=seq(2003,2014),"defl_ga"=log(c(70.9,68.5,82.8,103,100.5,119.8,109.3,100
                                                       ,114.5,126.2,127.1,121.8)))
# 5-Steller, GP09-36001  Wasser
defl_wa <- cbind("Jahr"=seq(2003,2014),"defl_wa"=log(c(88.6,91.2,93.4,94.6,95.8,96.7,99.2,100,101.1
                                                       ,103.1,104.9,106.3)))
# 9-Steller, GP09-353010031  Fernwärme,Belieferung eines Mehrfamilienwohnhauses
defl_wm_HH <- cbind("Jahr"=seq(2003,2014),"defl_wm_HH"=log(c(74.5,74.6,80.2,90.1,95.8,101.6,107.3
                                                             ,100,106.8,116.1,119.8,119)))
# 9-Steller, GP09-353010032  Fernwärme, Belieferung eines Nichtwohngebäudes
defl_wm_NWG <- cbind("Jahr"=seq(2003,2014),"defl_wm_NWG"=log(c(73.2,73.2,78.9,89.3,94.8,100.3,106.2
                                                               ,100,107.2,116.2,119.6,118.5)))


# Erstelle die Indizes für die Bereinigung der Brennstoffkosten (intermediates)
# -----------------------------------------------------------------------------
# Quelle Brennstoffpreise: Destatis. Daten zur Energiepreisentwicklung. Lange Reihen.

# GP09-051 Einfuhrpreise Steinkohle
defl_se_hc <- cbind("Jahr"=seq(2003,2014),"defl_se_hc"=log(c(39.2,54.7,57.4,64.7,69.8,109.1,110.5
                                                             ,100,117.8,109.9,93.3,85.7)))
# GP09-052 Erzeugerpreise Braunkohle
defl_se_bk <- cbind("Jahr"=seq(2003,2014),"defl_se_bk"=log(c(86.4,86.0,87.9,90.9,94.1,92.0,97.0
                                                             ,100,109.9,113.0,114.1,113.0)))
# GP09-062 Einfuhrpreise Erdgas
defl_se_eg <- cbind("Jahr"=seq(2003,2014),"defl_se_eg"=log(c(55.7,52.9,72.9,98.4,91.4,128.1,95.9
                                                             ,100,124.7,132.1,122.3,105.4)))
# GP09-1920 28 005 Erzeugerpreise Schweres Heizöl
# Anmerkung: leichtes vs. schweres Heizöl- siehe .pdf zur Energiepreisentwicklung, schweres Heizöl 
# wird hauptsächlich zur Strom und Wärmeerzeugung verwendet. 87% werden im Inland erzeugt und nur 
# 17% importiert, verwende daher Erzeugerpreise anstelle von Einfuhrpreisen.
defl_se_oil <- cbind("Jahr"=seq(2003,2014),"defl_se_oil"=log(c(46.9,43.8,61.6,75.4,74.0,101.6,77.2
                                                               ,100,131.1,145.8,129.6,114.9)))


# Erfasse auch die tatsächlichen Preisniveaus für Steinkohle, Erdgas und Öl
# -------------------------------------------------------------------------
# Quelle: BMWi (2016): Energiedaten Stand Januar 2016. Sheet 26.
# V:\projects\current\efficiency\02_KOMIED\04_paper_current\19_integrierte_unternehmen\01_data

# Rohöl, EUR/t
p_oil <- cbind("Jahr"=seq(2003,2014),"p_oil"=log(c(190.13,221.74,314.47,379.01,389.24,484.14,324.22
                                                   ,446.00,592.68,642.71,611.42,554.94)))
# Steinkohlen EUR/t SKE 
p_hc <- cbind("Jahr"=seq(2003,2014),"p_hc"=log(c(39.87,55.36,65.02,61.76,68.24,112.48,78.81,85.33
                                                 ,106.80,93.02,79.09,72.74)))

# Erdgas EUR/10^13J
p_eg <- cbind("Jahr"=seq(2003,2014),"p_eg"=log(c(340.1,328.8,447.9,592.6,555.0,745.0,579.4,572.6
                                                 ,713.3,806.7,765.6,653.8)))




# Anspielen der Indizes an den Datensatz
# --------------------------------------
data0 <- merge(data0,defl_sn,by="Jahr")
data0 <- merge(data0,defl_ga,by="Jahr")
data0 <- merge(data0,defl_wa,by="Jahr")
data0 <- merge(data0,defl_wm_HH,by="Jahr")
data0 <- merge(data0,defl_wm_NWG,by="Jahr")
data0 <- merge(data0,defl_se_hc,by="Jahr")
data0 <- merge(data0,defl_se_bk,by="Jahr")
data0 <- merge(data0,defl_se_eg,by="Jahr")
data0 <- merge(data0,defl_se_oil,by="Jahr")
data0 <- merge(data0,p_oil,by="Jahr")
data0 <- merge(data0,p_hc,by="Jahr")
data0 <- merge(data0,p_eg,by="Jahr")
data0 <- data0[order(data0$id,data0$unr,data0$Jahr),]


# Logarithmiere die Stromabsatzpreise
# ----------------------------------------------
# Skalierung: Multiplikation mit 100, um negative Log-Werte zu verhindern. Problem (?): Die 
# logarithmierten Preise für Strom liegen jetzt teilweise unter den PPI für die restlichen Sektoren.
# Das dürfte bei der Regression zu dem counterintuitive result führen, dass Stromlieferanten 
# scheinbar schlechtere Umsätze erzielen als andere Sektoren.
data0$p_sa_log <- log(data0$p_sa*100)
data0$p_TK_log <- log(data0$p_TK*100)
data0$p_SK_log <- log(data0$p_SK*100)
data0$p_EVU_log <- log(data0$p_EVU*100)
data0$p_LV_log <- log(data0$p_LV*100)
data0$p_HH_log <- log(data0$p_HH*100)
data0$p_VG_log <- log(data0$p_VG*100)
data0$p_BC_log <- log(data0$p_BC*100)


# Problem 2: Firmen, die keine Stromlieferanten sind, haben in den Preisen NA stehen. Bei der 
# Regressionsgleichung später ergibt das für diese Firmen p_sa*sa=NA*0=NA und die Beobachtungen 
# fallen weg. Setze daher allelogarithmierten Preise für Firmen, die keine Stromlieferanten sind, 
# auf 0 (Kann erst nach dem PPI-Vergleich geschehen, da sonst dstat verzehrt ist durch die Nullen.)
data0$p_sa_log[is.na(data0$p_sa_log)==TRUE] <- 0
data0$p_TK_log[is.na(data0$p_TK_log)==TRUE]<- 0
data0$p_SK_log[is.na(data0$p_SK_log)==TRUE]<- 0
data0$p_EVU_log[is.na(data0$p_EVU_log)==TRUE]<- 0
data0$p_LV_log[is.na(data0$p_LV_log)==TRUE]<- 0
data0$p_HH_log[is.na(data0$p_HH_log)==TRUE]<- 0
data0$p_VG_log[is.na(data0$p_VG_log)==TRUE]<- 0
data0$p_BC_log[is.na(data0$p_BC_log)==TRUE]<- 0
# Jetzt sollte es keine NAs mehr geben.
dstat(data0$p_sa_log,d=2)


# Definiere fuel dummies.
# ----------------------
data0$HETGruppen[is.na(data0$HETGruppen)==TRUE] <- 0
data0$se_hc <- ifelse(data0$HETGruppen==1,1,0)
data0$se_bk <- ifelse(data0$HETGruppen==2,1,0)
data0$se_oil <- ifelse(data0$HETGruppen==3,1,0)
data0$se_gas <- ifelse(data0$HETGruppen==4,1,0)
data0$se_water <- ifelse(data0$HETGruppen==5,1,0)
data0$se_waste <- ifelse(data0$HETGruppen==12,1,0)
data0$se_sonst <- ifelse(data0$HETGruppen>=13,1,0)

# Fasse zusammen in fuel Gruppen.
# -------------------------------
data0$se_bio <- ifelse(data0$HETGruppen==9 | data0$HETGruppen==10,1,0)
data0$se_coal <- ifelse(data0$se_hc==1 | data0$se_bk==1,1,0)
data0$se_EE <- ifelse(data0$HETGruppen==6 | data0$HETGruppen==7 | data0$HETGruppen==8 
                      | data0$HETGruppen==11 ,1,0)
data0$se_EE2 <- ifelse(data0$se_EE==1 | data0$se_water==1 | data0$se_bio==1 | data0$se_sonst==1,1,0)



# Umkodierung der Mehrheitsverhältnisse
# -------------------------------------
# 0: rein-öfftl.
# 1: private Minderheitenbeteiligung
data0$eigentuemer2[data0$eigentuemer2==2] <- 0



# Fasse die Rechtsform in fünf Kategorien zusammen.
# ------------------------------------------------
data0$status <- NA
data0$status[data0$Rechtsform_Zahl==9 | data0$Rechtsform_Zahl==7
             |((data0$Rechtsform_Zahl==10 | is.na(data0$Rechtsform_Zahl)==TRUE)
               & (is.na(data0$EF6)==FALSE & (data0$EF6==12 | data0$EF6==8)))
             ] <- 1
data0$status[data0$Rechtsform_Zahl==8 
             | ((data0$Rechtsform_Zahl==10 | is.na(data0$Rechtsform_Zahl)==TRUE) 
                & is.na(data0$EF6)==FALSE & data0$EF6>=20 & data0$EF6<=22)
             ] <- 2
data0$status[(data0$Rechtsform_Zahl>=1 & data0$Rechtsform_Zahl<=5) 
             | ((data0$Rechtsform_Zahl==10 | is.na(data0$Rechtsform_Zahl)==TRUE)  
                & is.na(data0$EF6)==FALSE & data0$EF6>=1 & data0$EF6<=6)
             ] <- 3
data0$status[data0$Rechtsform_Zahl==6
             | ((data0$Rechtsform_Zahl==10 | is.na(data0$Rechtsform_Zahl)==TRUE)  
                & is.na(data0$EF6)==FALSE & data0$EF6==7)
             ] <- 4
data0$status[data0$Rechtsform_Zahl==11] <- 5
summary(as.factor(data0$status))


# Erstelle fixed effects für die Rechtsform.
data0$Verband <- ifelse(data0$status==1,1,0)
data0$Eigenbetrieb <- ifelse(data0$status==2,1,0)
data0$unlisted <- ifelse(data0$status==3,1,0) 
data0$listed <- ifelse(data0$status==4,1,0)
# data0$mixed <- ifelse(data0$status==5,1,0)


# Kontrolliere für die Siedlungsstruktur
# --------------------------------------
data0$metro <- ifelse(data0$Siedlung==1,1,0)
data0$suburban <- ifelse(data0$Siedlung==2,1,0)
data0$rurald <- ifelse(data0$Siedlung==3,1,0)
data0$rurals <- ifelse(data0$Siedlung==4,1,0)
data0$s_mixed <- ifelse(data0$Siedlung==5,1,0)


# Outsourcing I: Kontrolliere für den Anteil fremder Dienstleistungen.
# --------------------------------------------------------------------
data0$shareF <- data0$fremdeDL/(data0$bruttolohn + data0$fremdeDL)

# Outsourcing II: Kontrolliere für den Anteil fremdbezogener Energie- und Wasser am Gesamtumsatz
# ----------------------------------------------------------------------------------------------
# Umkodieren der NAs-->0 in der Variable UK_Code4501
data0$UK_Code4501[is.na(data0$UK_Code4501)==TRUE] <- 0
data0$shareFEW <- data0$UK_Code4501/data0$revenues1

# Bereinigung des Datensatzes
# ---------------------------
# Bereinige Datensatz um alle Firmen, für die keine Rechtsform, keine Information über die 
# Mehrheitsverhältnisse oder keine Siedlungsstruktur verfügbar ist.
data0 <- subset(data0,is.na(data0$status)==FALSE 
                & is.na(data0$eigentuemer2)==FALSE
                & is.na(data0$Siedlung)==FALSE)

# Lasse zunächst die Firmen weg, die eine gemischte Siedlungsstruktur aufweisen
data0 <- subset(data0,Siedlung !=5)
addmargins(table(data0$Jahr)) 

# Gibt es doppelte Unternehmen?
table(table(index(data0)))
table(duplicated(data0, by=c("id","Jahr")))

# Entferne doppelte Unternehmen.
data0 <- unique(data0,by=c("id","Jahr"))
addmargins(table(data0$Jahr)) 

###################################################################################################
##                    		2) Deskriptive Analyse für Data Section im Paper 	                     ##
###################################################################################################

# Deklariere Datensatz als Paneldata und lege fest, welche Spalte das Individium i und welche
# die Zeitkomponente t enthält.
data_p <- pdata.frame(data0, index=c("id","Jahr"),row.names=FALSE)
pdim(data_p)


###################################################################################################
## 				2.1 Verteilung auf die Sektoren            			 ##
###################################################################################################

##########################
##  2.1.1 Subsamples    ##
##########################


# Bereinige vorab um die Braunkohlewerke aus dem Mixed-Sample, damit keine Restkategorie entsteht.
data_p <- subset(data_p, ((wa==1 & sa==0 & se==0 & sn==0 & wm==0 & ga==0)==FALSE
                 & (wa==0 & wm==0 & se==0 & (sa==1 | sn==1 | ga==1))==FALSE
                 & ((se == 1 | wm==1) & ga==0 & wa==0 & sa==0 & sn==0)==FALSE
                 & (wa==0 & wm==0 & se==1 & (sa==1 | sn==1 | ga==1))==FALSE
                  & se_bk==1)==FALSE)

addmargins(table(data_p$Jahr)) 

##################################
##  2.1.1.1 Aufteilung 1        ##
##################################

# Mixed utilities
# ---------------
data_mixed0 <- subset(data_p,(wa==1 & sa==0 & se==0 & sn==0 & wm==0 & ga==0)==FALSE
                     & (wa==0 & wm==0 & se==0 & (sa==1 | sn==1 | ga==1))==FALSE
                     & ((se == 1 | wm==1) & ga==0 & wa==0 & sa==0 & sn==0)==FALSE
                     & (wa==0 & wm==0 & se==1 & (sa==1 | sn==1 | ga==1))==FALSE)
addmargins(table(data_mixed0$Jahr))

# Wieviele Unternehmen fallen raus aufgrund der Restriktion, dass Inputs>0 und Output>0?

# fremde DL:
data_mixed_fl <- subset(data_mixed0, fremdeDL>0)
addmargins(table(data_mixed_fl$Jahr))

# labour:
data_mixed_l <- subset(data_mixed0, bruttolohn>0)
addmargins(table(data_mixed_l$Jahr))

# capital:
data_mixed_k <- subset(data_mixed0, K_adj>0)
addmargins(table(data_mixed_k$Jahr))

# value_added:
data_mixed_v <- subset(data_mixed0, value_added3>0)
addmargins(table(data_mixed_v$Jahr))

# Reproduktion des Samples aus der Schätzung (alle Inputs zusammen >0)
data_mixed <- subset(data_mixed0, fremdeDL>0 & bruttolohn>0 & K_adj>0 & value_added3>0)
addmargins(table(data_mixed$Jahr))

# Mit diesem Sample 'data_mixed' wird in den Analysen in (2.2) gearbeitet.



# Pure water utilities
# --------------------
data_w0 <- subset(data_p, wa==1 & sa==0 & se==0 & sn==0 & wm==0 & ga==0)
addmargins(table(data_w0$Jahr))


# Wieviele Unternehmen fallen raus aufgrund der Restriktion, dass Inputs>0 und Output>0?

# fremde DL:
data_w_fl <- subset(data_w0, fremdeDL>0)
addmargins(table(data_w_fl$Jahr))

# labour:
data_w_l <- subset(data_w0, bruttolohn>0)
addmargins(table(data_w_l$Jahr))

# capital:
data_w_k <- subset(data_w0, K_adj>0)
addmargins(table(data_w_k$Jahr))

# value_added:
data_w_v <- subset(data_w0, value_added_wa>0)
addmargins(table(data_w_v$Jahr))

# Reproduktion des Samples aus der Schätzung (alle Inputs zusammen >0)
data_w <- subset(data_w0, fremdeDL>0 & bruttolohn>0 & K_adj>0 & value_added_wa>0)
addmargins(table(data_w$Jahr))

# Mit diesem Sample 'data_w' wird in den Analysen in (2.2) gearbeitet.


# Liberalised (electricity & gas)
# -------------------------------
data_sg0 <- subset(data_p, (wa==0 & wm==0 & (sa==1 | sn==1 | ga==1)))
addmargins(table(data_sg0$Jahr))


# Wieviele Unternehmen fallen raus aufgrund der Restriktion, dass Inputs>0 und Output>0?

# fremde DL:
data_sg_fl <- subset(data_sg0, fremdeDL>0)
addmargins(table(data_sg_fl$Jahr))

# labour:
data_sg_l <- subset(data_sg0, bruttolohn>0)
addmargins(table(data_sg_l$Jahr))

# capital:
data_sg_k <- subset(data_sg0, K_adj>0)
addmargins(table(data_sg_k$Jahr))

# value_added:
data_sg_v <- subset(data_sg0, value_added3>0)
addmargins(table(data_sg_v$Jahr))

# Reproduktion des Samples aus der Schätzung (alle Inputs zusammen >0)
data_sg <- subset(data_sg0, fremdeDL>0 & bruttolohn>0 & K_adj>0 & value_added3>0)
addmargins(table(data_sg$Jahr))

# Mit diesem Sample 'data_sg' wird in den Analysen in (2.2) gearbeitet.



# Erzeuger (Wärme und Strom)
# --------------------------
data_ez0 <- subset(data_p,(se == 1 | wm==1) & ga==0 & wa==0 & sa==0 & sn==0)
addmargins(table(data_ez0$Jahr))


# Wieviele Unternehmen fallen raus aufgrund der Restriktion, dass Inputs>0 und Output>0?

# fremde DL:
data_ez_fl <- subset(data_ez0, fremdeDL>0)
addmargins(table(data_ez_fl$Jahr))

# labour:
data_ez_l <- subset(data_ez0, bruttolohn>0)
addmargins(table(data_ez_l$Jahr))

# capital:
data_ez_k <- subset(data_ez0, K_adj>0)
addmargins(table(data_ez_k$Jahr))

# value_added:
data_ez_v <- subset(data_ez0, value_added3>0)
addmargins(table(data_ez_v$Jahr))

# Reproduktion des Samples aus der Schätzung (alle Inputs zusammen >0)
data_ez <- subset(data_ez0, fremdeDL>0 & bruttolohn>0 & K_adj>0 & value_added3>0)
addmargins(table(data_ez$Jahr))

# Mit diesem Sample 'data_ez' wird in den Analysen in (2.2) gearbeitet.



# All Utilities
# -------------
# Note: hier sollte die Verwendung von value_added3 oder value_added_wa egal sein, da sie sich nur
# im PPI unterscheiden.

# Definition through merger of subsamples
data_p <- rbind(as.data.frame(data_mixed),as.data.frame(data_w),as.data.frame(data_sg)
                 ,as.data.frame(data_ez))
data_p <- pdata.frame(as.data.frame(data_p),index=c("id","Jahr"),row.names=FALSE)
addmargins(table(data_p$Jahr))

# Pooled dataset data0:
# Bereinige vorab um die Braunkohlewerke aus dem Mixed-Sample, damit keine Restkategorie entsteht.
data0 <- subset(data0, ((wa==1 & sa==0 & se==0 & sn==0 & wm==0 & ga==0)==FALSE
                          & (wa==0 & wm==0 & se==0 & (sa==1 | sn==1 | ga==1))==FALSE
                          & ((se == 1 | wm==1) & ga==0 & wa==0 & sa==0 & sn==0)==FALSE
                          & (wa==0 & wm==0 & se==1 & (sa==1 | sn==1 | ga==1))==FALSE
                          & se_bk==1)==FALSE)


# Definition through pooling 'data_p'
data0 <- as.data.frame(data_p)
addmargins(table(data0$Jahr))
class(data0)



##################################################
# 2.1.1.2 Aufteilung 2 (single sector firms)    ##
##################################################


# Pure electricity generators
data_se <- subset(data_p, wa==0 & wm==0 & se==1 & sn==0 &sa==0 & ga==0)
addmargins(table(data_se$Jahr))

# Pure electricity and heat generators
data_sw <- subset(data_p, (se == 1 | wm==1) & ga==0 & wa==0 & sa==0 & sn==0)
addmargins(table(data_sw$Jahr))

# Pure heat utilities
data_wm <- subset(data_p, wa==0 & wm==1 & se==0 & sn==0 &sa==0 & ga==0)
addmargins(table(data_wm$Jahr))

# Pure electricity DSOs
data_sn <- subset(data_p, wa==0 & wm==0 & se==0 & sn==1 &sa==0 & ga==0)
addmargins(table(data_sn$Jahr))

# Pure electricity traders/retailers
data_sa <- subset(data_p, wa==0 & wm==0 & se==0 & sn==0 &sa==1 & ga==0)
addmargins(table(data_sa$Jahr))

# Pure gas utilities
data_ga <- subset(data_p, wa==0 & wm==0 & se==0 & sn==0 &sa==0 & ga==1)
addmargins(table(data_ga$Jahr))

# Mixed water utilities
data_w_mixed <- subset(data_p, wa==1 & (sa==1 | se==1 | sn==1 | wm==1 | ga==1))
addmargins(table(data_w_mixed$Jahr))

# Firmen, die angeblich gar nichts machen
data_empty <- subset(data_p, wa==0 & wm==0 & se==0 & sn==0 &sa==0 & ga==0)
nrow(data_empty)




####################################################
##  2.1.1.3 Restkategorie zum Gesamtdatensatz     ##
####################################################

# Sample, was in diesem Paper verwendet wird:
# data0 = data_mixed + data_w + data_sg + data_ez
addmargins(table(data0$Jahr))

# Zum Vergleich die jährlichen Fallzahlen der öffentlichen Unternehmen im Gesamtdatensatz
addmargins(table(data_original$Jahr[data_original$jab==1]))





#####################################################
## 2.1.2 Vertikale und horizontale Integration     ##
#####################################################


# Liberalised Sample
# ------------------

# Anzahl der Netzbetreiber im lib-Sample
addmargins(table(data_sg$sn,data_sg$Jahr))


# Anzahl der Bündellieferanten (Strom und Gas)
addmargins(table(subset(data_sg, ga==1 & sa==1 & sn==0 & se==0)$Jahr))

# Anzahl der Kombinetzbetreiber (Strom & Gas)
addmargins(table(subset(data_sg, ga==1 & sa==0 & sn==1 & se==0)$Jahr))

# Anzahl der integrierten Stromnetzbetreiber und -lieferanten
addmargins(table(subset(data_sg, ga==0 & sa==1 & sn==1 & se==0)$Jahr))

# Anzahl der reinen Stromlieferanten
addmargins(table(subset(data_sg, ga==0 & sa==1 & sn==0 & se==0)$Jahr))

# Anzahl der reinen Stromnetzbetreiber
addmargins(table(subset(data_sg, ga==0 & sa==0 & sn==1 & se==0)$Jahr))

# Anzahl der reinen Gasfirmen
addmargins(table(subset(data_sg, ga==1 & sa==0 & sn==0 & se==0)$Jahr))

# Anzahl der Allrounder (Stromnetz, Stromvertrieb, Gas)
addmargins(table(subset(data_sg, ga==1 & sa==1 & sn==1 & se==0)$Jahr))


# Anzahl der Firmen, die selbst erzeugen
addmargins(table(subset(data_sg, se==1)$Jahr))

# Anzahl der Firmen, die selbst erzeugen: integriert mit Netz
addmargins(table(subset(data_sg, se==1 & sn==1 & sa==0)$Jahr))

# Anzahl der Firmen, die selbst erzeugen: integriert mit Vertrieb
addmargins(table(subset(data_sg, se==1 & sn==0 & sa==1)$Jahr))

# Anzahl der Firmen, die selbst erzeugen: integriert mit Netz und Vertrieb
addmargins(table(subset(data_sg, se==1 & sn==1 & sa==1)$Jahr))




# Erzeuger
# --------

# Anzahl der KWK-Werke
addmargins(table(subset(data_ez, wm==1 & se==1)$Jahr))

# Anzahl der reinen Wärmefirmen
addmargins(table(subset(data_ez, wm==1 & se==0)$Jahr))

# Anzahl der reinen Stromerzeuger
addmargins(table(subset(data_ez, wm==0 & se==1)$Jahr))


# Wieviele der Wärme-Firmen erzeugen nur Wärme?
addmargins(table(subset(data_ez, B_waerme_EF1001>0 & (B_waerme_EF1011==0 
                                                      | is.na(B_waerme_EF1011)==TRUE))$Jahr))

# Wieviele der Wärme-Firmen vertreiben nur Wärme?
addmargins(table(subset(data_ez, B_waerme_EF1011>0 & (B_waerme_EF1001==0 
                                                      | is.na(B_waerme_EF1001)==TRUE))$Jahr))

# Wieviele der Wärme-Firmen machen beides?
addmargins(table(subset(data_ez, B_waerme_EF1001>0 & B_waerme_EF1011>0)$Jahr))



####################################################################################################
##                    2.2 Inputs und Outputs                                                      ##
####################################################################################################




################################################
##  2.2.1 Fremder Dienstleistungen            ##
################################################

  ################################################
  ##  2.2.1.1 Levels                            ##
  ################################################

# Alle Utilities
# --------------
dstat(as.data.frame(data_p$fremdeDL/10^6),d=2)

# Mixed Utilities
# ---------------
dstat(as.data.frame(data_mixed$fremdeDL/10^6),d=2)

# Utilities im Bereich Wasser
# --------------------------
dstat(as.data.frame(data_w$fremdeDL/10^6),d=2)

# Utilities in liberalisierten Sektoren (Strom, Gas)
# --------------------------------------------------
dstat(as.data.frame(data_sg$fremdeDL/10^6),d=2)

# Utilities im Bereich Erzeugung
# ------------------------------
dstat(as.data.frame(data_ez$fremdeDL/10^6),d=2)



  #########################################################################################
  ##  2.2.1.2 Anteil an Gesamtausgaben für Dienstleistungen (Bruttolöhne + fremde DL)    ##
  #########################################################################################

# Alle Utilities
# --------------
dstat(data_p$shareF,d=2)

# Mixed Utilities
# ---------------
dstat(data_mixed$shareF,d=2)

# Utilities im Bereich Wasser
# --------------------------
dstat(data_w$shareF,d=2)

# Utilities in liberalisierten Sektoren (Strom, Gas)
# --------------------------------------------------
dstat(data_sg$shareF,d=2)

# Utilities im Bereich Erzeugung
# --------------------------------
dstat(data_ez$shareF,d=2)


# Anteil Ausgaben für fremde DL am Umsatz
dstat(as.data.frame(data_p$service_share),d=2)


#########################################################################################
##  2.2.1.3 Zusammenhang Unternehmensgröße und Outsourcing I                           ##
#########################################################################################

# Korrelation zwischen Umsatz und shareF
cor(data0$revenues1,data0$shareF)

# Teile die Unternehmen in 4 Umsatz-Kategorien (0-q25,q25-q50,q50-q75,q75-q100)
# -----------------------------------------------------------------------------

# Alle Utilities
dstat(data_p$shareF[data_p$revenues1<quantile(data_p$revenues,probs=0.25)],d=2)
dstat(data_p$shareF[data_p$revenues1>=quantile(data_p$revenues,probs=0.25) 
                    & data_p$revenues1<quantile(data_p$revenues,probs=0.50)],d=2)
dstat(data_p$shareF[data_p$revenues1>=quantile(data_p$revenues,probs=0.50) 
                    & data_p$revenues1<quantile(data_p$revenues,probs=0.75)],d=2)
dstat(data_p$shareF[data_p$revenues1>=quantile(data_p$revenues,probs=0.75) 
                    & data_p$revenues1<quantile(data_p$revenues,probs=1)],d=2)


# Mixed Utilities
dstat(data_mixed$shareF[data_mixed$revenues1<quantile(data_mixed$revenues,probs=0.25)],d=2)
dstat(data_mixed$shareF[data_mixed$revenues1>=quantile(data_mixed$revenues,probs=0.25) 
                    & data_mixed$revenues1<quantile(data_mixed$revenues,probs=0.50)],d=2)
dstat(data_mixed$shareF[data_mixed$revenues1>=quantile(data_mixed$revenues,probs=0.50) 
                    & data_mixed$revenues1<quantile(data_mixed$revenues,probs=0.75)],d=2)
dstat(data_mixed$shareF[data_mixed$revenues1>=quantile(data_mixed$revenues,probs=0.75) 
                    & data_mixed$revenues1<quantile(data_mixed$revenues,probs=1)],d=2)

# Liberalised Utilities
dstat(data_sg$shareF[data_sg$revenues1<quantile(data_sg$revenues,probs=0.25)],d=2)
dstat(data_sg$shareF[data_sg$revenues1>=quantile(data_sg$revenues,probs=0.25) 
                    & data_sg$revenues1<quantile(data_sg$revenues,probs=0.50)],d=2)
dstat(data_sg$shareF[data_sg$revenues1>=quantile(data_sg$revenues,probs=0.50) 
                    & data_sg$revenues1<quantile(data_sg$revenues,probs=0.75)],d=2)
dstat(data_sg$shareF[data_sg$revenues1>=quantile(data_sg$revenues,probs=0.75) 
                    & data_sg$revenues1<quantile(data_sg$revenues,probs=1)],d=2)

# Water Utilities
dstat(data_w$shareF[data_w$revenues1<quantile(data_w$revenues,probs=0.25)],d=2)
dstat(data_w$shareF[data_w$revenues1>=quantile(data_w$revenues,probs=0.25) 
                    & data_w$revenues1<quantile(data_w$revenues,probs=0.50)],d=2)
dstat(data_w$shareF[data_w$revenues1>=quantile(data_w$revenues,probs=0.50) 
                    & data_w$revenues1<quantile(data_w$revenues,probs=0.75)],d=2)
dstat(data_w$shareF[data_w$revenues1>=quantile(data_w$revenues,probs=0.75) 
                    & data_w$revenues1<quantile(data_w$revenues,probs=1)],d=2)

# Erzeuger Utilities
dstat(data_ez$shareF[data_ez$revenues1<quantile(data_ez$revenues,probs=0.25)],d=2)
dstat(data_ez$shareF[data_ez$revenues1>=quantile(data_ez$revenues,probs=0.25) 
                    & data_ez$revenues1<quantile(data_ez$revenues,probs=0.50)],d=2)
dstat(data_ez$shareF[data_ez$revenues1>=quantile(data_ez$revenues,probs=0.50) 
                    & data_ez$revenues1<quantile(data_ez$revenues,probs=0.75)],d=2)
dstat(data_ez$shareF[data_ez$revenues1>=quantile(data_ez$revenues,probs=0.75) 
                    & data_ez$revenues1<quantile(data_ez$revenues,probs=1)],d=2)






##################################################
##  2.2.2 Lohnkosten und Durchschnittslöhne     ##
##################################################


  ######################################
  ##    2.2.2.1 Lohnkosten (Levels)   ##
  ######################################

# Alle Utilities
# --------------
dstat(as.data.frame(data_p$bruttolohn/10^6),d=2)

# Mixed Utilities
# ---------------
dstat(as.data.frame(data_mixed$bruttolohn/10^6),d=2)

# Utilities im Bereich Wasser
# ---------------------------
dstat(as.data.frame(data_w$bruttolohn/10^6),d=2)

# Utilities in liberalisierten Sektoren (Strom, Gas)
# --------------------------------------------------
dstat(as.data.frame(data_sg$bruttolohn/10^6),d=2)

# Utilities im Bereich Erzeugung
# --------------------------
dstat(as.data.frame(data_ez$bruttolohn/10^6),d=2)



  ###############################################
  ##    2.2.2.2 Anteil Bruttolöhne am Umsatz   ##
  ###############################################


# Alle Utilities
# --------------
dstat(as.data.frame(data_p$wage_share),d=2)

# Mixed Utilities
# ---------------
dstat(as.data.frame(data_mixed$wage_share),d=2)

# Utilities im Bereich Wasser
# ---------------------------
dstat(as.data.frame(data_w$wage_share),d=2)

# Utilities in liberalisierten Sektoren (Strom, Gas)
# --------------------------------------------------
dstat(as.data.frame(data_sg$wage_share),d=2)

# Utilities im Bereich Erzeugung
# --------------------------
dstat(as.data.frame(data_ez$wage_share),d=2)



###############################################
##    2.2.2.3 Durchschnittslöhne             ##
###############################################


# Alle Utilities
# --------------
dstat(as.data.frame(data_p$wage))

# Mixed Utilities
# ---------------
dstat(as.data.frame(data_mixed$wage))

# Utilities im Bereich Wasser
# ---------------------------
dstat(as.data.frame(data_w$wage))

# Utilities in liberalisierten Sektoren (Strom, Gas)
# --------------------------------------------------
dstat(as.data.frame(data_sg$wage))

# utilities im Bereich Erzeugung
# ------------------------------
dstat(as.data.frame(data_ez$wage))




###############################################
##  2.2.3 Kapitalgüter                       ##
###############################################

# Deskriptive Statistik Kapitalgüter Methode 'einfach'
# dstat(as.data.frame(data_p$K_t_defl/10^6),d=2)

# Deskriptive Statistik Kapitalgüter Methode 'PLM'
dstat(as.data.frame(data_p$K_adj/10^6),d=2)

# Korrelation zwischen beiden Maßen
# cor(data0$K_t_defl,data0$K_adj,use="complete.obs")


################################################
##  2.2.4 Umsätze                             ##
################################################

################################################################
#     2.2.4.1 Revenues (preisbereinigt mit aggregiertem PPI)  ##
################################################################

# Alle Utilities
# --------------
dstat(as.data.frame(data_p$revenues/10^6),d=2)

# Mixed Utilities
# ---------------------------
dstat(as.data.frame(data_mixed$revenues)/10^6,d=2)

# Utilities im Bereich Wasser
# ---------------------------
dstat(as.data.frame(data_w$revenues)/10^6,d=2)

# Utilities in liberalisierten Sektoren (Strom, Gas)
# --------------------------------------------------
dstat(as.data.frame(data_sg$revenues)/10^6,d=2)

# Utilities im Bereich Erzeugung
# ------------------------------
dstat(as.data.frame(data_ez$revenues)/10^6,d=2)



##############################################
#     2.2.4.2 Value added (before taxes)    ##
##############################################

# Alle Utilities
# --------------
dstat(as.data.frame(data_p$value_added3/10^6,d=2))

# Mixed Utilities
# ---------------------------
dstat(as.data.frame(data_mixed$value_added3)/10^6,d=2)

# Utilities im Bereich Wasser
# ---------------------------
dstat(as.data.frame(data_w$value_added3)/10^6,d=2)

# Utilities in liberalisierten Sektoren (Strom, Gas)
# --------------------------------------------------
dstat(as.data.frame(data_sg$value_added3)/10^6,d=2)

# Utilities im Bereich Erzeugung
# ------------------------------
dstat(as.data.frame(data_ez$value_added3)/10^6,d=2)



##################################################
#     2.2.4.3 Korrelation Strompreise und PPI	  ##
##################################################


# Übersicht über die Verteilung der logarithmierten Gesamt-Durchschnittspreise [EUR/MWh].
dstat(as.data.frame(data_p$p_sa_log),d=2)


# Entwicklung über die Jahre (akkurate Preissteigerung entsprechend des PPI?)
# Berechne PPI aus dem Datensatz und vergleiche mit tatsächlicher Entwicklung (Haushalte)
round(dstat(pdata.frame(data0, index=c("id","Jahr"),row.names=FALSE)$p_HH,d=2)[,4]
      /dstat(pdata.frame(data0, index=c("id","Jahr"),row.names=FALSE)$p_HH,d=2)[8,4],2)
# GP09-351112  Elektrischer Strom, bei Abgabe an Haushalte:
# 75.6 78 81.5 84.7 87.7 92 96.9 100 107.3 110.8


# Berechne PPI aus dem Datensatz und vergleiche mit tatsächlicher Entwicklung (SK)
round(dstat(pdata.frame(data0, index=c("id","Jahr"),row.names=FALSE)$p_SK,d=2)[,4]
      /dstat(pdata.frame(data0, index=c("id","Jahr"),row.names=FALSE)$p_SK,d=2)[8,4],2)
# GP09-3514-01  Elektr. Strom, bei Abgabe an Sondervertragskunden  
# 71.7 75 79.5 90.8 93.1 103.2 98 100 111.8 109.3


# Berechne PPI aus dem Datensatz und vergleiche mit tatsächlicher Entwicklung (EVU)
round(dstat(pdata.frame(data0, index=c("id","Jahr"),row.names=FALSE)$p_EVU,d=2)[,4]
      /dstat(pdata.frame(data0, index=c("id","Jahr"),row.names=FALSE)$p_EVU,d=2)[8,4],2)
# GP09-351111  Elektrischer Strom an Weiterverteiler (Fall 6)	
# 63.9 69 79.1 103.7 98.6 119.2 101.8 100 102.4 91







####################################################################################################
##                                    2.3 Rechtsform und Mehrheitsverhältnisse                    ##
####################################################################################################

################################################
##  2.3.1 Rechtsformen                        ##
################################################

# Alle Utilities
# --------------
addmargins(table(data0$Rechtsform_Zahl,data0$Jahr,useNA="ifany"))
addmargins(table(data0$status,data0$Jahr,useNA="ifany"))

# Mixed Utilities
# ---------------
addmargins(table(data_mixed$Rechtsform_Zahl,data_mixed$Jahr,useNA="ifany"))
addmargins(table(data_mixed$status,data_mixed$Jahr,useNA="ifany"))


# Utilities im Bereich Wasser
# ---------------------------
addmargins(table(data_w$Rechtsform_Zahl,data_w$Jahr,useNA="ifany"))
addmargins(table(data_w$status,data_w$Jahr,useNA="ifany"))


# Utilities in liberalisierten Sektoren (Strom, Gas)
# --------------------------------------------------
addmargins(table(data_sg$Rechtsform_Zahl,data_sg$Jahr,useNA="ifany"))
addmargins(table(data_sg$status,data_sg$Jahr,useNA="ifany"))


# Utilities im Bereich Erzeugung
# ------------------------------
addmargins(table(data_ez$Rechtsform_Zahl,data_ez$Jahr,useNA="ifany"))
addmargins(table(data_ez$status,data_ez$Jahr,useNA="ifany"))



#######################################################
##  2.3.2 Rechtsformenwechsel                        ##
#######################################################

# Wieviele Unternehmen haben ihre Rechtsform gewechselt? Wann war das?

# Weg 1:
# ------
# Berechne für jedes Unternehmen die Abweichung der jährlichen Rechtsform von der durchschnittlichen
# Rechtsform des Unternehmens (Within Transformation). Beträgt die Abweichung Null, hat das 
# Unternehmen über all die Jahre seine Rechtsform nicht geändert, d.h. mean(RF)=RF.
# Nutze die built-in function 'Within' im plm-package.
data_p$w_RF <- Within(data_p$status)
# Wieviele haben ihre Rechtsform geändert? (gesamter Zeitraum)
length(unique(data_p$unr[data_p$w_RF!=0]))


# Weg 2:
# ------
# Use aggregate().
w2_RF <- aggregate(cbind("w2_RF"=data0$status),by=list("unr"=data0$unr),mean,na.rm=TRUE)
data0 <- merge(data0,w2_RF,by="unr",all.x=TRUE)
length(unique(data0$unr[data0$w2_RF!=data0$status]))



# Weg 3:
# ------
# Berechne auch das Jahr, in dem die Rechtsform gewechselt wurde.
data0s <- subset(data0, is.na(data0$status)==FALSE)
data0s$ch <- NA

date()
for (i in levels(as.factor(data0s$unr))){
  for (j in as.factor(data0s$Jahr[data0s$unr==i])){
    # Da es sich um ein unbalanced panel handelt, ist nicht immer das Vorjahr verfügbar und
    # gängige lag()-Operatoren funktionieren nicht. Sie geben dann NA aus, anstatt die Rechtsform
    # mit dem letztmalig verfügbaren Jahr zu vergleichen.
    # Ermittle daher Vorgängerjahr, in dem Unternehmen zuletzt beobachtet wurde. Ist das aktuelle
    # Beobachtungsjahr j das erste Mal, dass das Unternehmen beobachtet wird (Start der Zeitreihe,
    # Vorgängerjahr existiert nicht), dann setze Vorgängerjahr=Beobachtungsjahr.
    z <- ifelse(which(data0s$Jahr[data0s$unr==i]==j)==1,j
                ,as.numeric(as.character(data0s$Jahr[data0s$unr==i][which(data0s$Jahr[data0s$unr==i]==j)-1])))
    # Vergleiche die Rechtsform im aktuellen Jahr mit der aus dem Vorgängerjahr: Ist sie höher,
    # setze ch=2, ist sie niedriger, setze ch=1, bleibt sie gleich, setze ch=NA.
    # Existiert kein Vorgängerjahr, wird das Beobachtungsjahr mit sich selbst verglichen und 
    # kein change (ch=NA) ermittelt.
    data0s$ch[data0s$unr==i & data0s$Jahr==j] <- ifelse(data0s$status[data0s$unr==i & data0s$Jahr==j]>data0s$status[data0s$unr==i & data0s$Jahr==z]
                                                               ,2
                                                               ,ifelse(data0s$status[data0s$unr==i & data0s$Jahr==j]<data0s$status[data0s$unr==i & data0s$Jahr==z]
              ,1
              ,NA))
  }}
date()

# Zur Kontrolle: Anzahl der Unternehmen im gesamten Zeitraum, die ihre Rechtsform verändert haben.
# Hier sollte das gleiche herauskommen wie bei Weg 1 und 2.
length(unique(data0s$unr[is.na(data0s$ch)==FALSE]))

# Aus welchen Subssamples stammen sie?
length(intersect(unique(data0s$unr[is.na(data0s$ch)==FALSE]),unique(data_w$unr)))
length(intersect(unique(data0s$unr[is.na(data0s$ch)==FALSE]),unique(data_mixed$unr)))
length(intersect(unique(data0s$unr[is.na(data0s$ch)==FALSE]),unique(data_sg$unr)))
length(intersect(unique(data0s$unr[is.na(data0s$ch)==FALSE]),unique(data_ez$unr)))


# Wieviele haben in eine private Rechtsform umgewandelt?
length(unique(data0s$unr[data0s$ch==2]))

length(intersect(unique(data0s$unr[data0s$ch==2]),unique(data_w$unr)))
length(intersect(unique(data0s$unr[data0s$ch==2]),unique(data_mixed$unr)))
length(intersect(unique(data0s$unr[data0s$ch==2]),unique(data_sg$unr)))
length(intersect(unique(data0s$unr[data0s$ch==2]),unique(data_ez$unr)))


# Wieviele haben in eine öffentliche Rechtsform umgewandelt?
length(unique(data0s$unr[data0s$ch==1]))

length(intersect(unique(data0s$unr[data0s$ch==1]),unique(data_w$unr)))
length(intersect(unique(data0s$unr[data0s$ch==1]),unique(data_mixed$unr)))
length(intersect(unique(data0s$unr[data0s$ch==1]),unique(data_sg$unr)))
length(intersect(unique(data0s$unr[data0s$ch==1]),unique(data_ez$unr)))



# Achtung, öffentlich + privat muss nicht gleich gesamt sein, wenn Firmen öfter hinundhergewechselt
# haben.

# Wann haben sie gewechselt?
addmargins(table(data0s$ch,data0s$Jahr,useNA="ifany"))
# Achtung: Die Summe kann höher liegen als für die unique()-firms, wenn Firmen öfter 
# hinundherwechseln.


################################################
##  2.3.3 Mehrheitsverhältnisse               ##
################################################


# Alle Utilities
# --------------
addmargins(table(data0$eigentuemer2,data0$Jahr,useNA="ifany"))

# Mixed Utilities
# ---------------------------
addmargins(table(data_mixed$eigentuemer2,data_mixed$Jahr,useNA="ifany"))

# Utilities im Bereich Wasser
# ---------------------------
addmargins(table(data_w$eigentuemer2,data_w$Jahr,useNA="ifany"))

# Utilities in liberalisierten Sektoren (Strom, Gas)
# --------------------------------------------------
addmargins(table(data_sg$eigentuemer2,data_sg$Jahr,useNA="ifany"))

# Utilities im Bereich Erzeugung
# ------------------------------
addmargins(table(data_ez$eigentuemer2,data_ez$Jahr,useNA="ifany"))


# Korrelation zwischen Eigentümerschaft und Rechtsform
cor(data0$eigentuemer2,data0$Rechtsform_Zahl,use="complete.obs")
cor(data0$eigentuemer2,data0$Eigenbetrieb,use="complete.obs")

# Korrelation im Wassersektor
cor(data_w$eigentuemer2,data_w$Eigenbetrieb,use="complete.obs")
cor(data_w$eigentuemer2,data_w$unlisted,use="complete.obs")

# Korrelation in den liberalisierten Sektoren
cor(data_sg$eigentuemer2,data_sg$unlisted,use="complete.obs")

# Korrelation unter den mixed utilities
cor(data_mixed$eigentuemer2,data_mixed$unlisted,use="complete.obs")
cor(data_mixed$eigentuemer2,data_mixed$eigen,use="complete.obs")
cor(data_mixed$eigentuemer2,data_mixed$Eigenbetrieb,use="complete.obs")





#######################################################
##    2.3.4 Eigentümerwechsel                        ##
#######################################################


# Wieviele Unternehmen haben ihren Eigentümer gewechselt (Teilprivatisierung on/off)?

# Weg 1:
# ------
# Berechne für jedes Unternehmen die Abweichung der jährlichen Eigentümerstruktur von der 
# durchschnittlichen Eigentümerstruktur des Unternehmens (Within Transformation). Beträgt die 
# Abweichung Null, hat dasUnternehmen über all die Jahre seine Eigentümerstruktur nicht geändert, 
# d.h. mean(ET)=ET. Nutze die built-in function 'Within' im plm-package.
data_p$w_ET <- Within(data_p$eigentuemer2)
# Wieviele haben ihre Eigentümerstruktur geändert? (gesamter Zeitraum)
length(unique(data_p$unr[data_p$w_ET!=0]))


# Weg 2:
# ------
# Use aggregate().
w2_ET <- aggregate(cbind("w2_ET"=data0$eigentuemer2),by=list("unr"=data0$unr),mean,na.rm=TRUE)
data0 <- merge(data0,w2_ET,by="unr",all.x=TRUE)
length(unique(data0$unr[data0$w2_ET!=data0$eigentuemer2]))



# Weg 3:
# ------
# Berechne auch das Jahr, in dem die Eigentümerstruktur gewechselt wurde.
data0e <- subset(data0, is.na(data0$eigentuemer2)==FALSE)
data0e$ch <- NA

date()
for (i in levels(as.factor(data0e$unr))){
  for (j in as.factor(data0e$Jahr[data0e$unr==i])){
    # Da es sich um ein unbalanced panel handelt, ist nicht immer das Vorjahr verfügbar und
    # gängige lag()-Operatoren funktionieren nicht. Sie geben dann NA aus, anstatt den Eigentümer
    # mit dem letztmalig verfügbaren Jahr zu vergleichen.
    # Ermittle daher Vorgängerjahr, in dem Unternehmen zuletzt beobachtet wurde. Ist das aktuelle
    # Beobachtungsjahr j das erste Mal, dass das Unternehmen beobachtet wird (Start der Zeitreihe,
    # Vorgängerjahr existiert nicht), dann setze Vorgängerjahr=Beobachtungsjahr.
    z <- ifelse(which(data0e$Jahr[data0e$unr==i]==j)==1,j
                ,as.numeric(as.character(data0e$Jahr[data0e$unr==i][which(data0e$Jahr[data0e$unr==i]==j)-1])))
    # Vergleiche die Eigentümerstruktur im aktuellen Jahr mit der aus dem Vorgängerjahr: Ist sie 
    # höher, setze ch=2, ist sie niedriger, setze ch=1, bleibt sie gleich, setze ch=NA.
    # Existiert kein Vorgängerjahr, wird das Beobachtungsjahr mit sich selbst verglichen und 
    # kein change (ch=NA) ermittelt.
    data0e$ch[data0e$unr==i & data0e$Jahr==j] <- ifelse(data0e$eigentuemer2[data0e$unr==i & data0e$Jahr==j]>data0e$eigentuemer2[data0e$unr==i & data0e$Jahr==z]
                                                        ,2
                                                        ,ifelse(data0e$eigentuemer2[data0e$unr==i & data0e$Jahr==j]<data0e$eigentuemer2[data0e$unr==i & data0e$Jahr==z]
                                                                ,1
                                                                ,NA))
  }}
date()
# Zur Kontrolle: Anzahl der Unternehmen im gesamten Zeitraum, die ihre Eigentümerstruktur verändert 
# haben. Hier sollte das gleiche herauskommen wie bei Weg 1 und 2.
length(unique(data0e$unr[is.na(data0e$ch)==FALSE]))

# Aus welchen Subssamples stammen sie?
length(intersect(unique(data0e$unr[is.na(data0e$ch)==FALSE]),unique(data_w$unr)))
length(intersect(unique(data0e$unr[is.na(data0e$ch)==FALSE]),unique(data_mixed$unr)))
length(intersect(unique(data0e$unr[is.na(data0e$ch)==FALSE]),unique(data_sg$unr)))
length(intersect(unique(data0e$unr[is.na(data0e$ch)==FALSE]),unique(data_ez$unr)))

# Wieviele haben teilprivatisiert?
length(unique(data0e$unr[data0e$ch==2]))

length(intersect(unique(data0e$unr[data0e$ch==2]),unique(data_w$unr)))
length(intersect(unique(data0e$unr[data0e$ch==2]),unique(data_mixed$unr)))
length(intersect(unique(data0e$unr[data0e$ch==2]),unique(data_sg$unr)))
length(intersect(unique(data0e$unr[data0e$ch==2]),unique(data_ez$unr)))

# Wieviele haben ihre Anteile zurückgekauft (entprivatisiert)?
length(unique(data0e$unr[data0e$ch==1]))

length(intersect(unique(data0e$unr[data0e$ch==1]),unique(data_w$unr)))
length(intersect(unique(data0e$unr[data0e$ch==1]),unique(data_mixed$unr)))
length(intersect(unique(data0e$unr[data0e$ch==1]),unique(data_sg$unr)))
length(intersect(unique(data0e$unr[data0e$ch==1]),unique(data_ez$unr)))

# Achtung, öffentlich + privat muss nicht gleich gesamt sein, wenn Firmen öfter hinundhergewechselt
# haben.

# Wann haben sie gewechselt?
addmargins(table(data0e$ch,data0e$Jahr,useNA="ifany"))
# Achtung: Die Summe kann höher liegen als für die unique()-firms, wenn Firmen öfter hinundher-
# wechseln.


####################################################################################################
##              2.4 weitere Kontrollvariablen                                                     ##
####################################################################################################


################################################
##  2.4.1 Siedlungsstruktur                   ##
################################################

# Alle Utilities
# --------------
addmargins(table(data0$Siedlung,data0$Jahr,useNA="ifany"))

# Mixed Utilities
# ---------------
addmargins(table(data_mixed$Siedlung,data_mixed$Jahr,useNA="ifany"))

# Utilities im Bereich Wasser
# ---------------------------
addmargins(table(data_w$Siedlung,data_w$Jahr,useNA="ifany"))

# Utilities in liberalisierten Sektoren (Strom, Gas)
# --------------------------------------------------
addmargins(table(data_sg$Siedlung,data_sg$Jahr,useNA="ifany"))

# Utilities im Bereich Erzeugung
# ------------------------------
addmargins(table(data_ez$Siedlung,data_ez$Jahr,useNA="ifany"))



###################################
##    2.4.2 Hauptenergieträger   ##
###################################

# Allgemein
addmargins(table(data0$HETGruppen,data0$Jahr))

# Liberalisierter Sektor
# -----------------------
addmargins(table(data_sg$HETGruppen,data_sg$Jahr))

# Unter den Mixed Utilities
# -------------------------
addmargins(table(data_mixed$HETGruppen,data_mixed$Jahr))

# Bei den Erzeugern
# -----------------
addmargins(table(data_ez$HETGruppen,data_ez$Jahr))

# Welche Technologie haben die reinen Stromkraftwerke, die ab 2008 dazukommen?
addmargins(table(subset(data_ez,se==1 & wm==0 & Eintrittsjahr>=2008)$HETGruppen
                 ,subset(data_ez,se==1 & wm==0 & Eintrittsjahr>=2008)$Jahr))

# Machen die Stromerzeuger wirklich nichts mit Wärme?

# Nettowärmeerzeugung Anlagen (Jahressumme)
addmargins(table(subset(data_ez, wm==0 & se==1 & B_kraftw_EF2101U3>0)$Jahr))

# Nettowärmeerzeugung Anlagen darunter KWK (Jahressumme)
addmargins(table(subset(data_ez, wm==0 & se==1 & B_kraftw_EF2101U4>0)$Jahr))

# Nettowärmeerzeugung insgesamt (Jahressumme)
addmargins(table(subset(data_ez, wm==0 & se==1 & B_kraftw_EF2201U5>0)$Jahr))

# Nettowärmeerzeugung darunter KWK (Jahressumme)
addmargins(table(subset(data_ez, wm==0 & se==1 & B_kraftw_EF2201U6>0)$Jahr))

# Wärmabgabe insgesamt aus KWK Prozessen (Jahressumme)
addmargins(table(subset(data_ez, wm==0 & se==1 & B_kraftw_EF701_sum>0)$Jahr))


########################################
##    2.4.3 Customer Structure Strom  ##
########################################

data_sa <- subset(data0, sa==1)
addmargins(table(data_sa$Jahr))

# Wieviele unique Firmen sind das?
length(unique(data_sa$unr))

  ##############################################################################
  ##    2.4.3.1 Anteil Tarifkunden (residential customers) am Endverbrauch    ##
  ##############################################################################

data_sa$ShareTK<-ifelse(data_sa$U_ABS_EF1051_sum>0
                          ,data_sa$U_ABS_EF1051_sum/data_sa$U_ABS_EF1061,0)

# Wieviele Beobachtungen verkaufen gar nicht an Tarifkunden?
length(which(data_sa$ShareTK==0|is.na(data_sa$ShareTK)==TRUE)==TRUE)

# Wieviele Firmen sind das?
length(unique(data_sa$unr[data_sa$ShareTK==0|is.na(data_sa$ShareTK)==TRUE]))

# Und wie verteilt sich das Maß auf die übrigen Beobachtungen?
dstat(data_sa$ShareTK[data_sa$ShareTK>0 & is.na(data_sa$ShareTK)==FALSE],d=2)


  #################################################################################
  ##    2.4.3.2 Anteil Sondervertragskunden (large customers) am Endverbrauch    ##
  #################################################################################

data_sa$ShareSK<-ifelse(data_sa$U_ABS_EF1041_sum>0
                        ,data_sa$U_ABS_EF1041_sum/data_sa$U_ABS_EF1061,0)

# Wieviele Beobachtungen verkaufen gar nicht an Sonderkunden?
length(which(data_sa$ShareSK==0|is.na(data_sa$ShareSK)==TRUE)==TRUE)

# Wieviele Firmen sind das?
length(unique(data_sa$unr[data_sa$ShareSK==0|is.na(data_sa$ShareSK)==TRUE]))

# Und wie verteilt sich das Maß auf die übrigen Beobachtungen? 
dstat(data_sa$ShareSK[data_sa$ShareSK>0 & is.na(data_sa$ShareSK)==FALSE],d=2)


  #################################################################################
  ##    2.4.3.3 Anteil Weiterverteilung (other utilities) an Gesamtabgabe        ##
  #################################################################################

data_sa$ShareWV<-ifelse(is.na(data_sa$U_ABS_EF1011_sum)==FALSE
                          ,data_sa$U_ABS_EF1011_sum/(data_sa$U_ABS_EF1061
                                                     +ifelse(is.na(data_sa$U_ABS_EF1011_sum)==TRUE,0
                                                             ,data_sa$U_ABS_EF1011_sum)),0)

# Wieviele Beobachtungen verteilen nicht weiter?
length(which(data_sa$ShareWV==0|is.na(data_sa$ShareWV)==TRUE)==TRUE)

# Wieviele Firmen sind das?
length(unique(data_sa$unr[data_sa$ShareWV==0|is.na(data_sa$ShareWV)==TRUE]))

# Und wie verteilt sich das Maß auf die übrigen Beobachtungen?
dstat(data_sa$ShareWV[data_sa$ShareWV>0 & is.na(data_sa$ShareWV)==FALSE],d=2)

#################################################################################
##    2.4.3.4 Korrelation zwischen den Shares                                  ##
#################################################################################

cor(data_sa$ShareTK,data_sa$ShareWV,use="complete.obs")



########################################
##    2.4.4 Customer Structure Wärme  ##
########################################

data_wm_pooled <- subset(data0, wm==1)
addmargins(table(data_wm_pooled$Jahr))
# Wieviele unique Firmen sind das?
length(unique(data_wm_pooled$unr))

  ##########################################################
  ##    2.4.4.1 Anteil Haushaltskunden am Endverbrauch    ##
  ##########################################################

data_wm_pooled$ShareHH <- ifelse(data_wm_pooled$B_waerme_EF1011b>0
                          ,data_wm_pooled$B_waerme_EF1011b/data_wm_pooled$B_waerme_EF1011,0)


# Wieviele Beobachtungen verkaufen gar nicht an Haushalte?
length(which(data_wm_pooled$ShareHH==0|is.na(data_wm_pooled$ShareHH)==TRUE)==TRUE)

# Wieviele Firmen sind das?
length(unique(data_wm_pooled$unr[data_wm_pooled$ShareHH==0|is.na(data_wm_pooled$ShareHH)==TRUE]))

# Und wie verteilt sich das Maß auf die übrigen Beobachtungen?
dstat(data_wm_pooled$ShareHH[data_wm_pooled$ShareHH>0 & is.na(data_wm_pooled$ShareHH)==FALSE],d=2)

  ##############################################################################
  ##    2.4.4.2 Anteil verabeitendes Gewerbe und Gewinnung am Endverbrauch    ##
  ##############################################################################

data_wm_pooled$ShareVG <- ifelse(data_wm_pooled$B_waerme_EF1011a>0
                                 ,data_wm_pooled$B_waerme_EF1011a/data_wm_pooled$B_waerme_EF1011,0)

# Wieviele Beobachtungen verkaufen gar nicht an Haushalte?
length(which(data_wm_pooled$ShareVG==0|is.na(data_wm_pooled$ShareVG)==TRUE)==TRUE)

# Wieviele Firmen sind das?
length(unique(data_wm_pooled$unr[data_wm_pooled$ShareVG==0|is.na(data_wm_pooled$ShareVG)==TRUE]))

# Und wie verteilt sich das Maß auf die übrigen Beobachtungen?
dstat(data_wm_pooled$ShareVG[data_wm_pooled$ShareVG>0 & is.na(data_wm_pooled$ShareVG)==FALSE],d=2)


  ##############################################################################
  ##    2.4.4.3 Anteil sonstige Endkonsumenten am Endverbrauch                ##
  ##############################################################################

data_wm_pooled$ShareSo <- ifelse(data_wm_pooled$B_waerme_EF1011c>0
                                 ,data_wm_pooled$B_waerme_EF1011c/data_wm_pooled$B_waerme_EF1011,0)

# Wieviele Beobachtungen verkaufen gar nicht an sonstige Endkonsumenten?
length(which(data_wm_pooled$ShareSo==0|is.na(data_wm_pooled$ShareSo)==TRUE)==TRUE)

# Wieviele Firmen sind das?
length(unique(data_wm_pooled$unr[data_wm_pooled$ShareSo==0|is.na(data_wm_pooled$ShareSo)==TRUE]))

# Und wie verteilt sich das Maß auf die übrigen Beobachtungen?
dstat(data_wm_pooled$ShareSo[data_wm_pooled$ShareSo>0 & is.na(data_wm_pooled$ShareSo)==FALSE],d=2)



  #################################################################################
  ##    2.4.4.4 Anteil Weiterverteilung (other utilities) an Gesamtabgabe        ##
  #################################################################################

data_wm_pooled$ShareWV<-ifelse(is.na(data_wm_pooled$B_waerme_EF1010)==FALSE
                        ,data_wm_pooled$B_waerme_EF1010/(data_wm_pooled$B_waerme_EF1011
                                                    +ifelse(is.na(data_wm_pooled$B_waerme_EF1010)==TRUE
                                                            ,0,data_wm_pooled$B_waerme_EF1010)),0)

# Wieviele Beobachtungen verteilen nicht weiter?
length(which(data_wm_pooled$ShareWV==0|is.na(data_wm_pooled$ShareWV)==TRUE)==TRUE)

# Wieviele Firmen sind das?
length(unique(data_wm_pooled$unr[data_wm_pooled$ShareWV==0|is.na(data_wm_pooled$ShareWV)==TRUE]))

# Und wie verteilt sich das Maß auf die übrigen Beobachtungen?
dstat(data_wm_pooled$ShareWV[data_wm_pooled$ShareWV>0 & is.na(data_wm_pooled$ShareWV)==FALSE],d=2)



#################################################################################
##    2.4.4.5 Korrelation zwischen den Shares                                  ##
#################################################################################

cor(data_wm_pooled$ShareHH,data_wm_pooled$ShareVG,use="complete.obs")
cor(data_wm_pooled$ShareHH,data_wm_pooled$ShareWV,use="complete.obs")
cor(data_wm_pooled$ShareVG,data_wm_pooled$ShareWV,use="complete.obs")



########################################################################
##  2.4.5 Outsourcing II: Anteil fremdbezogener Energie- und Wasser   ##
########################################################################

# Macht es einen unterschied in den NAs, ob man nur die Umsätze aus E+W verwendet?
# --------------------------------------------------------------------------------
data_p$shareFEW2 <- data_p$UK_Code4501/data_p$UK_Code2001
dstat(as.data.frame(data_p$shareFEW),d=2)
dstat(as.data.frame(data_p$shareFEW2),d=2)


# Entwicklung der Oursourcing-intensity II pro Subsample und über die Jahre
# -------------------------------------------------------------------------

# Alle Utilities
# --------------
# Wie hoch ist der Anteil der Unternehmen, die nicht outsourcen?
round(dstat(data_p$shareFEW)[,13]/dstat(data_p$shareFEW)[,12],2)

# Zur Outputprüfung: Fallzahlen hinter den Shares
dstat(data_p$shareFEW)[,13]
dstat(data_p$shareFEW)[,12]

# Wie hoch ist die Outsourcing-Rate bei den Unternehmen, die outsourcen?
data_p0 <- subset(data_p,shareFEW>0)
dstat(data_p0$shareFEW,d=2)


# Mixed Utilities
# ---------------
# Wie hoch ist der Anteil der Unternehmen, die nicht outsourcen?
round(dstat(data_mixed$shareFEW)[,13]/dstat(data_mixed$shareFEW)[,12],2)

# Zur Outputprüfung: Fallzahlen hinter den Shares
dstat(data_mixed$shareFEW)[,13]
dstat(data_mixed$shareFEW)[,12]

# Wie hoch ist die Outsourcing-Rate bei den Unternehmen, die outsourcen?
data_mixed0 <- subset(data_mixed,shareFEW>0)
dstat(data_mixed0$shareFEW,d=2)




# Utilities im Bereich Wasser
# --------------------------
# Wie hoch ist der Anteil der Unternehmen, die nicht outsourcen?
round(dstat(data_w$shareFEW)[,13]/dstat(data_w$shareFEW)[,12],2)

# Zur Outputprüfung: Fallzahlen hinter den Shares
dstat(data_w$shareFEW)[,13]
dstat(data_w$shareFEW)[,12]

# Wie hoch ist die Outsourcing-Rate bei den Unternehmen, die outsourcen?
data_w0 <- subset(data_w,shareFEW>0)
dstat(data_w0$shareFEW,d=2)




# Utilities in liberalisierten Sektoren (Strom, Gas)
# --------------------------------------------------
# Wie hoch ist der Anteil der Unternehmen, die nicht outsourcen?
round(dstat(data_sg$shareFEW)[,13]/dstat(data_sg$shareFEW)[,12],2)

# Zur Outputprüfung: Fallzahlen hinter den Shares
dstat(data_sg$shareFEW)[,13]
dstat(data_sg$shareFEW)[,12]

# Wie hoch ist die Outsourcing-Rate bei den Unternehmen, die outsourcen?
data_sg0 <- subset(data_sg,shareFEW>0)
dstat(data_sg0$shareFEW,d=2)




# Subsample1: Und wenn man die reinen Gaserzeuger getrennt betrachtet?
# --------------------------------------------------------------------
round(dstat(data_ga$shareFEW)[,13]/dstat(data_ga$shareFEW)[,12],2)

# Zur Outputprüfung: Fallzahlen hinter den Shares
dstat(data_ga$shareFEW)[,13]
dstat(data_ga$shareFEW)[,12]

# Anteil Nicht-Outsourcer
data_ga0 <- subset(data_ga,shareFEW>0)
# Bedeutung FEW in Umsätzen für Outsourcer
dstat(data_ga0$shareFEW,d=2)

# Subsample2: liberalisierte Firmen ohne reine Gaserzeuger
# --------------------------------------------------------
data_sg2 <- subset(data_sg, (wa==0 & wm==0 & se==0 & sn==0 &sa==0 & ga==1)==FALSE)
# Anteil Nicht-Outsourcer
round(dstat(data_sg2$shareFEW)[,13]/dstat(data_sg2$shareFEW)[,12],2)

# Zur Outputprüfung: Fallzahlen hinter den Shares
dstat(data_sg2$shareFEW)[,13]
dstat(data_sg2$shareFEW)[,12]

# Bedeutung FEW in Umsätzen für Outsourcer
data_sg20 <- subset(data_sg2,shareFEW>0)
dstat(data_sg20$shareFEW,d=2)



# Utilities im Bereich Erzeugung
# --------------------------------
# Wie hoch ist der Anteil der Unternehmen, die nicht outsourcen?
round(dstat(data_ez$shareFEW)[,13]/dstat(data_ez$shareFEW)[,12],2)

# Zur Outputprüfung: Fallzahlen hinter den Shares
dstat(data_ez$shareFEW)[,13]
dstat(data_ez$shareFEW)[,12]

# Wie hoch ist die Outsourcing-Rate bei den Unternehmen, die outsourcen?
data_ez0 <- subset(data_ez,shareFEW>0)
dstat(data_ez0$shareFEW,d=2)


# Korrelation zwischen beiden Outsourcing-Maßen (I outsourced labour & II outsourced production)
# ----------------------------------------------------------------------------------------------
cor(data_p$shareF,data_p$shareFEW,use="complete.obs")
cor(data_mixed$shareF,data_mixed$shareFEW,use="complete.obs")
cor(data_w$shareF,data_w$shareFEW,use="complete.obs")
cor(data_sg$shareF,data_sg$shareFEW,use="complete.obs")
cor(data_ez$shareF,data_ez$shareFEW,use="complete.obs")


# Korrelation zwischen beiden Outsourcing-Maßen (I UK_Code5501 & II UK_Code4501)
# ------------------------------------------------------------------------------
cor(data_p$UK_Code5501,data_p$UK_Code4501,use="complete.obs")
cor(data_mixed$UK_Code5501,data_mixed$UK_Code4501,use="complete.obs")
cor(data_w$UK_Code5501,data_w$UK_Code4501,use="complete.obs")
cor(data_sg$UK_Code5501,data_sg$UK_Code4501,use="complete.obs")
cor(data_ez$UK_Code5501,data_ez$UK_Code4501,use="complete.obs")



####################################################################################################
##                                2.5 Entry/Exit                                                  ##
####################################################################################################

# Eintritt und Austritt
cbind("Entry"=addmargins(table(data0$Eintrittsjahr,useNA="ifany"))
      ,"Exit"=addmargins(table(data0$Austrittsjahr,useNA="ifany")))

# Beobachtungsdauer
addmargins(table(as.factor(data0$Dauer),dnn="Anzahl Jahre"))

# Unique Anzahl der Unternehmen über alle Jahre im Sample
length(unique(data_p$id))

# Unique mixed utilities (über alle Jahre)
length(unique(data_mixed$id))

# Unique water utilities (über alle Jahre)
length(unique(data_w$id))

# Unique liberalised  (über alle Jahre)
length(unique(data_sg$id))

# Unique Erzeuger(über alle Jahre)
length(unique(data_ez$id))


####################################################################################################
##                                2.6 Repräsentativität                                           ##
####################################################################################################

########################################
##  2.6.1 Abgabe an Letztverbraucher  ##
########################################

# Gesamtmenge verteilter Strom an Letztverbraucher [TWh]
dstat(data_p$U_netzb_EF203_sum/10^6,d=2)

# Gesamtmenge abgegebene Wärme an Letztverbraucher [TWh]
dstat(data_p$B_waerme_EF1011/10^6,d=2)

# Gesamtmenge verkaufter Strom an Letztverbraucher [TWh]
dstat(data_p$U_ABS_EF1061/10^6,d=2)

# Gesamtmenge verkauftes Wasser an Letztverbraucher [TWh]
dstat(data_p$UK_Code8601/10^3,d=2)



###############################################################
##  2.6.2 Vergleich der Wärmeerzeugungsvariable aus TM066K   ##
###############################################################

# Nettowärmeerzeugung Anlagen (Jahressumme)
dstat(data_p$B_kraftw_EF2101U3/10^3,d=2)

# Nettowärmeerzeugung Anlagen darunter KWK (Jahressumme)
dstat(data_p$B_kraftw_EF2101U4/10^3,d=2)

# Nettowärmeerzeugung insgesamt (Jahressumme)
dstat(data_p$B_kraftw_EF2201U5/10^3,d=2)

# Nettowärmeerzeugung darunter KWK (Jahressumme)
dstat(data_p$B_kraftw_EF2201U6/10^3,d=2)

# Wärmabgabe insgesamt aus KWK Prozessen (Jahressumme)
dstat(data_p$B_kraftw_EF701_sum/10^3,d=2)

###################################################################################################
date()
############################## End of file ########################################################
