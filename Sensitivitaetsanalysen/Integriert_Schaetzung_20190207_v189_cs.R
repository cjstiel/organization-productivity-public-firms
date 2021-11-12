###################################################################################################
###################################################################################################
### Titel des Projekts: KOMIED
### Projekt-Nr. 2094-2012
###################################################################################################
###################################################################################################
### Projektleitung: Astrid Cullmann (DIW Berlin)
### Datengrundlage: AFiD-Panel Energieunternehmen & Energiebetriebe, JAB, URS (Organschaft)
### 
### Dateiname des Programmcodes: Integriert_Schaetzung_20190205_v189_cs.R
### erstellt: 07.02.2019
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
### Kapitel 4: Produktitivätsmessung bei integrierten Unternehmen (Data Deskriptives) 
### Kapitel 5: Produktitivätsmessung bei integrierten Unternehmen (Strukturelle Schätzung) -SKRIPT-
###
###
####################################################################################################
### Grundriss: In diesem Programm wird die Produktivität integrierter Unternehmen mittels der ACF
###            Methode geschätzt.
####################################################################################################
###
### Version 189:  - Formulierung mit Translog-Modell | Wagebill | Daten auf Unternehmensebene
###               - pure water utilities 2003-2014
###               - Sensitivitätsanalyse Interactions LOM mit SE
###     	  - Zusammenfassung Eigenbetrieb / Zweckverband
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
#Pfad3<-"Q:\\AFS\\55_FDZ\\Forschungsprojekte\\2012-2094 DIW - Richter (4)\\KDFV\\2017_08_07_cs_2\\"

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
Pfad1 <- ("V:/projects/current/efficiency/02_KOMIED/04_paper_current/19_integrierte_unternehmen_cs/02_R_code/05_Produktivitaet_Schaetzung/Arbeitsdaten/")
Pfad2 <- ("V:/projects/current/efficiency/02_KOMIED/04_paper_current/19_integrierte_unternehmen_cs/02_R_code/05_Produktivitaet_Schaetzung/Daten/")
Pfad3 <- ("V:/projects/current/efficiency/02_KOMIED/04_paper_current/19_integrierte_unternehmen_cs/02_R_code/05_Produktivitaet_Schaetzung/localoutput/")
.libPaths("V:\\projects\\current\\efficiency\\02_KOMIED\\04_paper_current\\19_integrierte_unternehmen_cs\\02_R_code\\00_packages")


######################################
##   0.3   Log-file & Time Stamp    ##
######################################


setwd(Pfad3)

# Log-file (Das Log-File ist nicht unbedingt nötig, da das FDZ den Console-Output (source+ouput) 
# abspeichert.)
sink(file="R_Log.txt",type = c("output", "message"), split=TRUE)

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
library(optimx)
library(boot)
library(car)
library(ggplot2)
library(micEconCES)
library(lmtest)
library(readstata13)


############################################
##	0.5	Daten einlesen		                ##
############################################


# Arbeitsdatensatz einlesen (DIW & FDZ)
# -------------------------------------
data <- read.dta(file.path(Pfad1,"data_public_single_final_cs_v3.dta"))


class(data)
dim(data)


#--- Tell R to use fixed notation instead of exponential notation, i.e. display 500000 instead of 
# 5*e+05 (especially in plots)
options(scipen=999)




##########################################################
## 	0.6	Selbstgeschriebene Funktionen  	              	##         
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
  mat <- matrix(NA, ncol=19, nrow=ncol(X))
  colnames(mat) <- c("Q1","Q5","Q25","Med","Mean","Q75","Q95","Q99","Disp50", "Disp90","Var", "SD"
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
  mat[,9] <- round(apply(X, 2, quantile, probs=0.75, na.rm=T)/apply(X, 2, quantile, probs=0.25, na.rm=T),digits=d)
  mat[,10] <- round(apply(X, 2, quantile, probs=0.95, na.rm=T)/apply(X, 2, quantile, probs=0.05, na.rm=T),digits=d)
  mat[,11] <- round(apply(X, 2, var,na.rm=T),digits=d)
  mat[,12] <- round(apply(X, 2, sd, na.rm=T), digits=d)
  mat[,13] <- round(apply(X, 2, sum, na.rm=T), digits=d)
  mat[,14] <- round(apply(X, 2, nobs),digits=d)
  for (i in 1:ncol(X)) {mat[i,15] <- length(which(X[,i]==0))[1]}
  for (i in 1:ncol(X)) {mat[i,16] <- length(which(X[,i]=="."))[1]}
  for (i in 1:ncol(X)) {mat[i,17] <- length(which(is.na(X[,i])))[1]}
  mat[,18] <- round(apply(X, 2, max, na.rm=T), digits=d)
  for (i in 1:ncol(X)) {mat[i,19] <- round(sort(X[,i],decreasing=T)[2],digits=d)}
  return(mat)
}



####################################################################################################
# Funktion gmm_moment_condition                                                                    #
# -----------------------------                                                                    #
# Diese Funktion ist das Kernstück des ACF-Algorithmus. Sie beschreibt den Optimierungsprozess.    #
# Die Gleichungen aus dem Markov-Prozess und jene zur Berechnung der Produktivität mittels Phi     #
# werden im Optimierungsprozess zu Nebenbedingungen und müssen daher im Optimierungsprozess        #
# enthalten sein.                                                                                  #
#                                                                                                  #
# Es gibt insgesamt 5 Nebenbedingungen.                                                            #
# (1) omega_it = Phi_it - Inputs_it * betas                                                        #
# (2) omega_it-1 = Phi_it-1 - Inputs_it-1 * betas                                                  #
# (3) omega_pol = 1 + omega_it-1 + (omega_it-1)^2 + (omega_it-1)^3 + legal_status_it-1             #
# (4) g_p = inv(omega_pol'*omega_pol)*omega_pol'*omega                                             #
# (5) innovation_it = omega_it - omega_pol*g_p                                                     #
#                                                                                                  #
# gmm_moment condition: Die finalen Variablen (v_ijt) für die Moment Conditions werden sukzessive  #
# aus den 5 Nebenbedingungen hergeleitet. Abschließend wird die skalare Zielfunktion aus den       #
# Moment Conditions wiedergegeben.                                                                 #
####################################################################################################



gmm_moment_condition <- function(betas){
  omega <<- data_gmm$Phi - Inputs_gmm%*%betas -Inputs_fixed%*%betas_fixed
  lag_omega <<- data_gmm$lag_Phi-lag_Inputs_gmm%*%betas - lag_Inputs_fixed%*%betas_fixed
  omega_pol <<- cbind(rep(1,n),lag_omega,lag_omega^2,lag_omega^3
                      ,data_gmm$lag1_privlaw
                      ,data_gmm$lag1_privlaw*data_gmm$lag1_eigentuemer2
                      ,data_gmm$shareF
                      ,data_gmm$shareFEW
                      ,data_gmm$shareF*data_gmm$shareFEW
                      ,data_gmm$shareF*data_gmm$lag1_privlaw*data_gmm$lag1_eigentuemer2
                      ,data_gmm$shareFEW*data_gmm$lag1_privlaw*data_gmm$lag1_eigentuemer2
                      ,data_gmm$shareF*data_gmm$lag1_privlaw
                      ,data_gmm$shareFEW*data_gmm$lag1_privlaw
                      )
  AR1 <<- lm(omega ~ lag_omega+ I(lag_omega^2) + I(lag_omega^3) 
             + data_gmm$lag1_privlaw 
             + I(data_gmm$lag1_privlaw*data_gmm$lag1_eigentuemer2) 
             + data_gmm$shareF
             + data_gmm$shareFEW
             + I(data_gmm$shareF*data_gmm$shareFEW)
             + I(data_gmm$shareF*data_gmm$lag1_privlaw*data_gmm$lag1_eigentuemer2) 
             + I(data_gmm$shareFEW*data_gmm$lag1_privlaw*data_gmm$lag1_eigentuemer2)
             + I(data_gmm$shareF*data_gmm$lag1_privlaw)
             + I(data_gmm$shareFEW*data_gmm$lag1_privlaw)
             )
  g_b <<- as.vector(AR1$coefficients)
  innovation <<- omega - omega_pol%*%g_b
  moment_condition <- t(t(instr_gmm)%*%innovation)%*%(t(instr_gmm)%*%innovation)
  return(as.vector(moment_condition))
}

####################################################################################################
# Funktion boot.acf										                                                             #
# -----------------										                                                             # 
# Teil 1 des Bootstrappings der Standardfehler für die Parameter der Produktion.            		   #
# Funktion, die den kompletten ACF-Algorithmus beinhaltet (First + Second Stage) und am Ende die   #
# Koeffizienten der Second Stage ausgibt.							                  				                   #
####################################################################################################


boot.acf <- function(data,indices,method){
  data_boot <<- data[indices,]
  invisible(capture.output(data_bp <<- pdata.frame(data_boot, index=c("id","Jahr"))))
  first_stage_m <<- lm(va3_m ~ l_m + f_m + k_m + I(0.5*l_m^2) +  I(0.5*f_m^2) + I(0.5*k_m^2) 
                      + l_m:f_m + l_m:k_m + f_m:k_m + suburban + rurald + rurals + l_m:k_m:f_m 
                      + l_m:I(f_m^2) + f_m:I(k_m^2) + l_m:I(k_m^2) + f_m:I(l_m^2) + k_m:I(f_m^2) 
                      + k_m:I(l_m^2) + I(l_m^2):I(k_m^2) + I(l_m^2):I(f_m^2) + I(f_m^2):I(k_m^2) 
                      + k_m:I(l_m^2):f_m + l_m:I(k_m^2):f_m + l_m:I(f_m^2):k_m  
                      + f_m:I(l_m^2):I(k_m^2) + k_m:I(l_m^2):I(f_m^2) + l_m:I(k_m^2):I(f_m^2)
                      + I(l_m^2):I(f_m^2):I(k_m^2),data_bp)
  data_bp$Phi <<- first_stage_m$fitted.values
  data_bp$lag_Phi <<- lag(data_bp$Phi)
  betas1 <<- as.vector(first_stage_m$coefficients)
  Inputs <<- as.matrix(cbind(rep(1,nrow(data_bp)),data_bp$l_m,data_bp$f_m,data_bp$k_m
                            ,0.5*(data_bp$l_m)^2,0.5*(data_bp$f_m)^2,0.5*(data_bp$k_m)^2
                            ,data_bp$suburban,data_bp$rurald,data_bp$rurals,data_bp$l_m*data_bp$f_m
                            ,data_bp$l_m*data_bp$k_m,data_bp$f_m*data_bp$k_m))
  data_bp$lag1_Verband <- lag(data_bp$Verband)
  data_bp$lag1_unlisted <- lag(data_bp$unlisted)
  data_bp$lag1_privlaw <- lag(data_bp$privlaw)
  data_bp$lag1_eigentuemer2 <- lag(data_bp$eigentuemer2)
  data_bp$lag1_shareF <- lag(data_bp$shareF)
  data_bp$lag1_shareFEW <- lag(data_bp$shareFEW)
  data_gmm <<- subset(data_bp,is.na(lag_Phi)==FALSE)
  n <<- nrow(data_gmm)
  Inputs_gmm <<- cbind(rep(1,nrow(data_gmm)),"l_m"=data_gmm$l_m,"f_m"=data_gmm$f_m
                          ,"k_m"=data_gmm$k_m,"l2_m"=0.5*data_gmm$l_m^2,"f2_m"=0.5*data_gmm$f_m^2
                          ,"k2_m"=0.5*data_gmm$k_m^2,"suburban"=data_gmm$suburban
                          ,"rurald"=data_gmm$rurald,"rurals"=data_gmm$rurals
                          ,"l_m_f_m"=data_gmm$l_m*data_gmm$f_m,"l_m_k_m"=data_gmm$l_m*data_gmm$k_m
                          ,"f_m_k_m"=data_gmm$f_m*data_gmm$k_m)
  lag_Inputs_all <<- as.matrix(cbind(rep(1,nrow(data_bp)),lag(data_bp$l_m),lag(data_bp$f_m)
                                    ,lag(data_bp$k_m),0.5*lag(data_bp$l_m)^2,0.5*lag(data_bp$f_m)^2
                                    ,0.5*lag(data_bp$k_m)^2,lag(data_bp$suburban)
                                    ,lag(data_bp$rurald),lag(data_bp$rurals)
                                    ,lag(data_bp$l_m)*lag(data_bp$f_m)
                                    ,lag(data_bp$l_m)*lag(data_bp$k_m)
                                    ,lag(data_bp$f_m)*lag(data_bp$k_m)))
  lag_Inputs_gmm <<- na.omit(lag_Inputs_all)
  Inputs_fixed <<- as.matrix(rep(0,nrow(data_gmm)))
  lag_Inputs_fixed <<- as.matrix(rep(0,nrow(data_gmm)))
  lag_Inputs_fixed <<- na.omit(lag_Inputs_fixed)
  instr <<- cbind("const"=rep(1,nrow(data_bp)),"l_m"=data_bp$l_m,"lag_f_m"=lag(data_bp$f_m)
                  ,"k_m"=data_bp$k_m,"l2_m"=data_bp$l_m^2,"lag_f2_m"=lag(data_bp$f_m)^2
                  ,"k2_m"=data_bp$k_m^2,data_bp$suburban,data_bp$rurald,data_bp$rurals
                  ,"l_m_lag_f_m"=data_bp$l_m*lag(data_bp$f_m),"l_m_k_m"=data_bp$l_m*data_bp$k_m
                  ,"lag_f_m_k_m"=lag(data_bp$f_m)*data_bp$k_m)
  instr_gmm <<- na.omit(instr)
  starting_values <<- lm(va3_m ~ l_m + f_m + k_m + I(0.5*l_m^2) +  I(0.5*f_m^2) + I(0.5*k_m^2) 
                         + l_m:f_m + l_m:k_m + f_m:k_m + suburban + rurald + rurals,data_bp)
  betas_basic_m <<- as.vector(starting_values$coefficients)
  
  initial_betas <<- betas_basic_m[c(1:13)]
  betas_fixed <<- 0
  optimization <<- optimx(par=initial_betas,fn=gmm_moment_condition, method=method)
  betas22_boot <<- rbind(optimization$p1[1],optimization$p2[1],optimization$p3[1],optimization$p4[1]
                        ,optimization$p5[1],optimization$p6[1],optimization$p7[1]
                        ,optimization$p8[1],optimization$p9[1],optimization$p10[1]
                        ,optimization$p11[1],optimization$p12[1],optimization$p13[1])
  return(betas22_boot)
}


####################################################################################################
# Funktion clusterBootSE									                                                         #
# ----------------------									                                                         #
# Teil 2 des Bootstrappings der Standardfehler für die Parameter der Produktionsfunktion.	         #
# Hier werden die Beobachtungen für die jeweiligen Bootstrapsamples gezogen. Es werden keine 	     #
# einzelnen Beobachtungen sondern ganze Unternehmen für ein Subsample ausgewählt.		               #
# D.h. wenn ein Unternehmen dabei ist, ist es mit all seinen Beobachtungen t dabei.		             #
#												                                                                           #
# source: https://diffuseprior.wordpress.com/2013/01/12/the-cluster-bootstrap/#comments		         #
####################################################################################################


clusterBootSE<-function(data,method,B){
  # Definiere Indexvariable
  clusters<-unique(data[,"id"])
  # Lege leere Matrix an, in die später die Koeffizienten aller Subsamples B abgelegt werden
  sterrs <- matrix(NA, nrow=B, ncol=13)
  # Starte mit dem Sampling
  for(i in 1:B){
    # Sample von den Unternehmensnummern
    units<-sample(clusters,size=length(clusters),replace=T)
    # Identifiziere im Originaldatensatz alle Beobachtungen t von Unternehmensnummern, die im 
    # aktuellen Subsample i gelandet sind sind.
    df.bs<-sapply(units,function(x)which(data[,"id"]==x))
    # Ziehe diese Beobachtungen aus dem Originaldatensatz und fasse sie in neuem Dataframe zusammen.
    df.bs<-data[unlist(df.bs),]
    # Wende den ACF-Algorithmus auf das Subsample i an und speichere die Koeffizienten in der Zeile
    # i. Es werden alle Beobachtungen aus dem jeweiligen Subsample i verwendet (1:dim(df.bs)[1]), um
    # die ACF-Koeffizienten zu berechnen (vgl. beim boot.package werden mittels 'indices' Subsamples 
    # gezogen, hier wurden die subsamples schon oben separat gezogen).
    sterrs[i,]<-boot.acf(data=df.bs,method=method,1:dim(df.bs)[1])
    # Sage, wann 50% bzw. 100% der Zeit vergangen ist (optional)
    # cat("\r",i*100/B," % done ");flush.console()
  }
  # Lege eine Tabelle an, in der links die Originalwerte stehen und rechts die SE aus der Varianz 
  # der Koeffizienten aller Subsamples B berechnet werden.
  val1 <- cbind(boot.acf(data=data,method=method,indices=1:dim(data0)[1]),apply(sterrs,2,sd))
  val <- cbind(val1,val1[,1]/val1[,2])
  colnames(val) <- c("Estimate","Std. Error","t-value")
  cat("\n")
  return(val)
}




###################################################################################################
##					  Start							 ##
###################################################################################################

####################################################################################################
##            1) Aufbereitung der Variablen                                                       ##
####################################################################################################


################################
##  1.1 Inputs und Outputs    ##
################################


# Preisbereinige die Revenues mit dem PPI für Wasser
# --------------------------------------------------
# Quelle: Destatis. Erzeugerpreisindizes gewerblicher Produkte. GP 2009, Basisjahr=2010.
# 5-Steller, GP09-36001  Wasser
defl_wa <- cbind("Jahr"=seq(2003,2014),"defl_wa"=c(0.886,0.912,0.934,0.946,0.958,0.967,0.992
                                                       ,1,1.011,1.031,1.049,1.063))
data <- merge(data,defl_wa,by="Jahr")
data <- data[order(data$id,data$unr,data$Jahr),]
data$revenues_wa <- data$revenues1/data$defl_wa
dstat(data$revenues_wa/10^6,d=2)
dstat(data$revenues1/10^6,d=2)

  
# Erstelle die neue Output-Variable 'value_added3'
# ------------------------------------------------
# revenues_wa (preisbereinigt) - intermediates (preisbereinigt mit Vorleistungsgüterindex)
data$value_added3 <- data$revenues_wa - data$intermediates


# Wähle aufgrund der Logarithmierung nur die Utilities aus, die positive Werte in den Inputs und 
# Outputs angegeben haben.
# ------------------------
data0 <- subset(data, fremdeDL>0 & K_adj>0 & bruttolohn>0 & value_added3>0)
addmargins(table(data0$Jahr))



# Logarithmiere die Inputs und Outputs.
# ------------------------------------
data0$l <- log(data0$bruttolohn)
data0$k <- log(data0$K_adj)
data0$f <- log(data0$fremdeDL)
data0$w <- log(data0$wage)
data0$va <- log(data0$value_added2)
data0$va3 <- log(data0$value_added3)

# Erstelle auch median-bereinige die Inputs und Outputs.
# ------------------------------------------------------
data0$l_m <- log(data0$bruttolohn)-log(median(data0$bruttolohn,na.rm=TRUE))
data0$k_m <- log(data0$K_adj)-log(median(data0$K_adj,na.rm=TRUE))
data0$f_m <- log(data0$fremdeDL)-log(median(data0$fremdeDL,na.rm=TRUE))
data0$w_m <- log(data0$wage)-log(median(data0$wage,na.rm=TRUE))
data0$va_m <- log(data0$value_added2)-log(median(data0$value_added2,na.rm=TRUE))
data0$va3_m <- log(data0$value_added3)-log(median(data0$value_added3,na.rm=TRUE))



####################################
## 1.2 Sektorendummies            ##
####################################

data0$HETGruppen[is.na(data0$HETGruppen)==TRUE] <- 0
data0$se_bk <- ifelse(data0$HETGruppen==2,1,0)



####################################
##  1.3 Preise/PPI/Deflation      ##
####################################

# entfällt


########################################
##  1.4 Organisation Variablen        ##
########################################

# Umkodierung der Mehrheitsverhältnisse
# -------------------------------------
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
data0$privlaw <- ifelse(data0$status==3|data0$status==4,1,0)


# Kontrolliere für die Siedlungsstruktur
# --------------------------------------
data0$metro <- ifelse(data0$Siedlung==1,1,0)
data0$suburban <- ifelse(data0$Siedlung==2,1,0)
data0$rurald <- ifelse(data0$Siedlung==3,1,0)
data0$rurals <- ifelse(data0$Siedlung==4,1,0)
summary(as.factor(data0$Siedlung))


# Outsourcing I: Kontrolliere für den Anteil fremder Dienstleistungen.
# --------------------------------------------------------------------
data0$shareF <- data0$fremdeDL/(data0$bruttolohn + data0$fremdeDL)

# Outsourcing II: Kontrolliere für den Anteil fremdbezogener Energie- und Wasser am Gesamtumsatz
# ------------------------------------------------------------------------------------------------
# Umkodieren der NAs-->0 in der Variable UK_Code4501
data0$UK_Code4501[is.na(data0$UK_Code4501)==TRUE] <- 0
data0$shareFEW <- data0$UK_Code4501/data0$revenues1




########################################
##  1.5 Weitere Kontrollvariablen     ##
########################################


# Erstelle die Fixed effects für 'size'
# ------------------------------------
data0$size_small <- ifelse(data0$revenues1<10000000 & data0$beschaeftigte<=49,1,0)
data0$size_med <- ifelse(data0$size_small==0 
                         & data0$revenues1<50000000 & data0$beschaeftigte<=249,1,0)
data0$size_large <- ifelse(data0$size_small==0 & data0$size_med==0 
                           & (data0$revenues1>=50000000 | data0$beschaeftigte>249),1,0)

summary(as.factor(data0$size_small))
summary(as.factor(data0$size_med))
summary(as.factor(data0$size_large))

# Wurden alle Unternehmen einer Kategorie zugeordnet?
nrow(subset(data0, size_small==1 | size_med==1 | size_large==1))==nrow(data0)



# Erstelle die Variable 'Investitionsintensität'
# ----------------------------------------------
# Investitionsintensität =  Investitionen/Revenues
data0$inv_int <- data0$investment/data0$revenues1
dstat(as.data.frame(data0$inv_int),d=2)


# Zeit-Effekte
# ------------
data0$t2003 <- ifelse(data0$Jahr==2003,1,0)
data0$t2004 <- ifelse(data0$Jahr==2004,1,0)
data0$t2005 <- ifelse(data0$Jahr==2005,1,0)
data0$t2006 <- ifelse(data0$Jahr==2006,1,0)
data0$t2007 <- ifelse(data0$Jahr==2007,1,0)
data0$t2008 <- ifelse(data0$Jahr==2008,1,0)
data0$t2009 <- ifelse(data0$Jahr==2009,1,0)
data0$t2010 <- ifelse(data0$Jahr==2010,1,0)
data0$t2011 <- ifelse(data0$Jahr==2011,1,0)
data0$t2012 <- ifelse(data0$Jahr==2012,1,0)
data0$t2013 <- ifelse(data0$Jahr==2013,1,0)
data0$t2014 <- ifelse(data0$Jahr==2014,1,0)




####################################################################################################
##    2) Bereinigung des Datensatzes und Auswahl des Subsets                                      ##
####################################################################################################

# Bereinigung
# -----------
# Bereinige Datensatz um alle Firmen, für die keine Rechtsform, keine Information über die 
# Mehrheitsverhältnisse oder keine Siedlungsstruktur verfügbar ist. Lasse außerdem die wenigen
# Braunkohlewerke weg, da sie den Bootstrap im H&P-Panel blockieren.
data0 <- subset(data0,is.na(data0$status)==FALSE 
                & is.na(data0$eigentuemer2)==FALSE
                & is.na(data0$Siedlung)==FALSE)

# Lasse zunächst die Firmen weg, die eine gemischte Siedlungsstruktur aufweisen
data0 <- subset(data0,Siedlung !=5)


# Wahl des Subsets
# ----------------
# Nimm nur reine Wasserfirmen.
data0 <- subset(data0,  wa==1 & sa==0 & se==0 & sn==0 & wm==0 & ga==0)


# Bereinige Datensatz um konstante Reihen.
# ---------------------------------------
data0 <- subset(data0,select=-c(id_ns,id_he1,id_he2,id_he3,id_he4,id_th,id_sh,jab,public
                                ,Lueckenjahre,out_labour,out_lohn,out_int,out_umsatz
                                ,B_waerme_EF1001,B_waerme_EF1010,B_waerme_EF1011,B_waerme_EF1011a
                                ,B_waerme_EF1011b,B_waerme_EF1011c,B_waerme_EF1011d,U_ABS_EF1061
                                ,U_ABS_EF1062,U_ABS_EF1012_sum,U_ABS_EF1041_sum,U_ABS_EF1042_sum
                                ,U_ABS_EF1051_sum,U_ABS_EF1052,U_ABS_EF1071,U_ABS_EF1072
                                ,U_ABS_EF1081,U_ABS_EF1082,U_ABS_EF1091,U_ABS_EF1092
                                ,B_kraftw_EF2201U2))



# Umwandlung in Paneldatensatz
# ----------------------------
# Deklariere Datensatz als Paneldata und lege fest, welche Spalte das Individium i und welche
# die Zeitkomponente t enthält.
data_p <- pdata.frame(data0, index=c("id","Jahr"),row.names=FALSE)
pdim(data_p)


# Erstelle einen lineraren Zeitdummy
# ----------------------------------
# 2003=1, 2004=2, ...
data_p$t <- as.numeric(factor(data_p$Jahr))





####################################################################################################
##      3) Analyse der Entscheidungen für Reorganisation                                          ##
####################################################################################################


# Regressiere die Ausgaben für fremde Dienstleistungen auf folgende Komponenten:
# shareF = F(size,proximity,wages,innovation,customer structure,technology,corporatisation,
#            ownership)

# Approximiere wie folgt:
# size                      | Kategorien anhand der Arbeitnehmer und des Umsatzes (KMU-Definition)
# proximity der supplier    | Siedlungsstruktur
# Arbeitskosten             | firmenspezifische Stundenlöhne
# Innovationstätigkeit      | Gesamtinvestitionen (evtl. auch F&E, Umweltschutz, Software?)
# Technology                | Sektordummies bzw. fuel use
# Corporatisation           | Private Rechtsform
# Ownership                 | private Minderheitsbeteiligung
# Kundenstruktur im Stromabsatz | Anteil TK,SK,WV
# Kundenstruktur im Wärmeabsatz | Anteil HH,VG,So,WV



##########################################
##    3.1 Outsourcing 1: Arbeitnehmer   ##
##########################################



# Grundmodell mit Controls in t-1
# -------------------------------
out1_OLS1 <- lm(shareF ~  t + I(t^2) 
                # andere Reorganisation-Variablen
               + lag(data_p$Verband) + lag(data_p$privlaw)  + lag(data_p$eigentuemer2) 
               + lag(data_p$shareFEW)
                # Unternehmensgröße und Produktionsprozess
                + size_med + size_large + lag(data_p$inv_int) + lag(data_p$wage)
                # Environment
                + suburban + rurald + rurals
                ,data_p)
summary(out1_OLS1)

# Grundmodell mit Interaktion in den Controls in t-1
# --------------------------------------------------
out1_OLS2 <- lm(shareF ~  t + I(t^2) 
               # andere Reorganisation-Variablen
               + lag(data_p$Verband) + lag(data_p$privlaw)  
               + I(lag(data_p$Verband)*lag(data_p$eigentuemer2))
               + I(lag(data_p$privlaw)*lag(data_p$eigentuemer2))
               + lag(data_p$shareFEW)
               # Unternehmensgröße und Produktionsprozess
               + size_med + size_large + lag(data_p$inv_int) + lag(data_p$wage)
               # Environment
               + suburban + rurald + rurals
               ,data_p)
summary(out1_OLS2)




##########################################
##    3.2 Outsourcing 2: Produktion     ##
##########################################



# Grundmodell mit Controls in t-1
# -------------------------------
out2_OLS <- lm(shareFEW ~  t + I(t^2)
                # andere Reorganisation-Variablen
                + lag(data_p$Verband) + lag(data_p$privlaw)  + lag(data_p$eigentuemer2) 
                + lag(data_p$shareF)
                # Unternehmensgröße und Produktionsprozess
                + size_med + size_large + lag(data_p$inv_int) + lag(data_p$wage)
                # Environment
                + suburban + rurald + rurals
                ,data_p)
summary(out2_OLS)


# Grundmodell mit Interaktion in den Controls in t-1
# --------------------------------------------------
out2_OLS2 <- lm(shareFEW ~  t + I(t^2) 
               # andere Reorganisation-Variablen
               + lag(data_p$Verband) + lag(data_p$privlaw)  
               + I(lag(data_p$Verband)*lag(data_p$eigentuemer2))
               + I(lag(data_p$privlaw)*lag(data_p$eigentuemer2))
               + lag(data_p$shareF)
               # Unternehmensgröße und Produktionsprozess
               + size_med + size_large + lag(data_p$inv_int) + lag(data_p$wage)
               # Environment
               + suburban + rurald + rurals
               ,data_p)
summary(out2_OLS2)





###################################################################################################
##                    		4) Strukturelle Schätzung				    	                                 ##
###################################################################################################


###############################################################################################
##  4.1 Basismodell mit OLS ohne ACF-Algorithmus                                             ##
###############################################################################################

# Nimm für die Produktionsfunktion im Grundmodell eine Translog-Funktion an.

# Output: Value added (Revenues - Materialaufwand - Energieeinkauf - Steuern)
# Inputs: hours worked, fremde DL, Kapital

# Die sich daraus ergebende Produktionsfunktion:
# value-added_it = beta_l * l_it + beta_s * s_it + beta_k * k_it  + beta_ll * l_it^2 
#                  + beta_ss * s_it^2  + beta_kk * k_it^2 + 0.5 * beta_ls * lit * s_it 
#                  + 0.5 * b_lk * l_it * k_it + 0.5 * b_ks * k_it * s_it + w_it + u_it

# Füge Dummies für die Siedlungsstruktur hinzu (Referenzkategorie: metropolitan areas):
# value-added_it = beta_l * l_it + beta_s * s_it + beta_k * k_it  + beta_ll * l_it^2 
#                  + beta_ss * s_it^2  + beta_kk * k_it^2 + 0.5 * beta_ls * lit * s_it 
#                  + 0.5 * b_lk * l_it * k_it + 0.5 * b_ks * k_it * s_it
#                  + d_1 * suburban_it + d_2 * rurald_it + d_3 * rurals_it
#                  + w_it + u_it


# Modell in Levels
# ----------------
first_stage_OLS <- lm(va3 ~
                          # Inputs der Produktionsfunktion:
                          l + f + k + I(0.5*l^2) +  I(0.5*f^2) + I(0.5*k^2) 
                        + l:f+ l:k + f:k
                        # Siedlungs controls
                        + suburban + rurald + rurals
                        ,data_p)
summary(first_stage_OLS)



# Modell medianbereinigt
# ----------------------
first_stage_OLS_m <- lm(va3_m ~
                    # Inputs der Produktionsfunktion:
                    l_m + f_m + k_m + I(0.5*l_m^2) +  I(0.5*f_m^2) + I(0.5*k_m^2) 
                    + l_m:f_m + l_m:k_m + f_m:k_m
                    # Siedlungs controls
                    + suburban + rurald + rurals
                    ,data_p)
summary(first_stage_OLS_m)


# Speichere die Koeffizienten.
betas_basic_m <- as.vector(first_stage_OLS_m$coefficients)


###############################################################################################
##  4.2 First-stage estimation (OLS): Translog-Modell mit Wage bill                          ##
###############################################################################################

# First stage OLS estimation in ACF (2005). Eliminiert iid error u_it.

# Die proxy function for productivity lautet
# omega_it = h_t(l_it,s_it,k_it,X_it)
# Sie wird als Polynom 2. Grades modelliert. Je nachdem, ob man die Siedlungsdummies X_it in der 
# 2nd stage mitschätzt, nimmt man an, dass die input choice nicht extra von der Nachfrage abhängt 
# (vgl. Vandenberghe et al. 2013 und DeLoecker 2011, Fox/Smeets 2011.)
# Beachte dass alle Terme ersten Grades des Polynoms mit den Termen aus der Produktionsfunktion
# 'verschmelzen' (nicht separat identifizierbar), was der Grund dafür ist, dass die Koeffizienten 
# der First Stage verzerrt sind.


# Modell medianbereinigt
# ----------------------
first_stage_m <- lm(va3_m ~
                  # Inputs der Produktionsfunktion:
                  l_m + f_m + k_m + I(0.5*l_m^2) +  I(0.5*f_m^2) + I(0.5*k_m^2) 
                  + l_m:f_m + l_m:k_m + f_m:k_m
                  # Population density controls
                  + suburban + rurald + rurals
                  # weitere proxy terms
                  + l_m:k_m:f_m 
                  + l_m:I(f_m^2) + f_m:I(k_m^2) + l_m:I(k_m^2) + f_m:I(l_m^2) + k_m:I(f_m^2) 
                  + k_m:I(l_m^2) 
                  + I(l_m^2):I(k_m^2) + I(l_m^2):I(f_m^2) + I(f_m^2):I(k_m^2) 
                  + k_m:I(l_m^2):f_m + l_m:I(k_m^2):f_m + l_m:I(f_m^2):k_m  
                  + f_m:I(l_m^2):I(k_m^2) + k_m:I(l_m^2):I(f_m^2) + l_m:I(k_m^2):I(f_m^2)
                  + I(l_m^2):I(f_m^2):I(k_m^2)
                  ,data_p)
summary(first_stage_m)

# Speichere die Koeffizienten der First Stage (Intercept & betas)
betas1 <- as.vector(first_stage_m$coefficients)

# Predict Phi
data_p$Phi <- first_stage_m$fitted.values
length(data_p$Phi)
length(which(is.na(data_p$Phi)==FALSE))
cor(data_p$Phi,data_p$va3_m)

# Konstruiere Lag-Phi
data_p$lag_Phi <- lag(data_p$Phi)
length(data_p$lag_Phi)
length(which(is.na(data_p$lag_Phi)==FALSE))

# Store residuals for later use.
data_p$exp_u_it <- exp(first_stage_m$residuals)


#######################################################################
##  4.3  Second-stage estimation: Vorbereiten der Lag-Variablen      ##
#######################################################################

# Für alle Modelle zutreffend
# ---------------------------

# Fasse alle Inputs in einer Matrix zusammen. Die Reihenfolge muss exakt jener der Koeffizienten der
# OLS- Schätzung entsprechen. Sollte sich ein Input in der Produktionsfunktion ändern, muss auch die
# Input und Instrumenten-Matrix angepasst werden.
# Note: In der OLS kann man die Translog-Gewichtung (0.5) direkt über die Funktionsgleichung 
# abbilden lassen, bei der GMM wird die funktionale Form indirekt über die Input-Matrix bestimmt.
Inputs <- as.matrix(cbind(rep(1,nrow(data_p)),data_p$l_m,data_p$f_m,data_p$k_m,0.5*(data_p$l_m)^2
                          ,0.5*(data_p$f_m)^2,0.5*(data_p$k_m)^2,data_p$suburban,data_p$rurald
                          ,data_p$rurals,data_p$l_m*data_p$f_m,data_p$l_m*data_p$k_m
                          ,data_p$f_m*data_p$k_m))
dim(Inputs)


# Konstruiere die t-1-Variablen für die Rechtsform
data_p$lag1_Verband <- lag(data_p$Verband)
data_p$lag1_unlisted <- lag(data_p$unlisted)
data_p$lag1_privlaw <- lag(data_p$privlaw)


# Konstruiere die t-1-Variable für die Eigentümerschaft
data_p$lag1_eigentuemer2 <- lag(data_p$eigentuemer2)

# Konstruiere die t-1-Variable für das Outsourcing
data_p$lag1_shareF <- lag(data_p$shareF)
data_p$lag1_shareFEW <- lag(data_p$shareFEW)



# Eliminiere im Gesamtdatensatz alle Beobachtungen, für die keine lag(Phi) vorliegen.
data_gmm <- subset(data_p,is.na(lag_Phi)==FALSE)
pdim(data_gmm)
n <- nrow(data_gmm)

# Gnereriere für die spätere Auswertung auch die vollständige Input-Matrix auf gmm-Basis.
# At time t
Inputs_gmm_all <- cbind(rep(1,nrow(data_gmm)),"l_m"=data_gmm$l_m,"f_m"=data_gmm$f_m
                        ,"k_m"=data_gmm$k_m,"l2_m"=0.5*data_gmm$l_m^2,"f2_m"=0.5*data_gmm$f_m^2
                        ,"k2_m"=0.5*data_gmm$k_m^2,"suburban"=data_gmm$suburban
                        ,"rurald"=data_gmm$rurald,"rurals"=data_gmm$rurals
                        ,"l_m_f_m"=data_gmm$l_m*data_gmm$f_m,"l_m_k_m"=data_gmm$l_m*data_gmm$k_m
                        ,"f_m_k_m"=data_gmm$f_m*data_gmm$k_m)
dim(Inputs_gmm_all)

# At time t-1
lag_Inputs_all <- as.matrix(cbind(rep(1,nrow(data_p)),lag(data_p$l_m),lag(data_p$f_m),lag(data_p$k_m)
                                  ,0.5*lag(data_p$l_m)^2,0.5*lag(data_p$f_m)^2,0.5*lag(data_p$k_m)^2
                                  ,lag(data_p$suburban),lag(data_p$rurald),lag(data_p$rurals)
                                  ,lag(data_p$l_m)*lag(data_p$f_m),lag(data_p$l_m)*lag(data_p$k_m)
                                  ,lag(data_p$f_m)*lag(data_p$k_m)))
lag_Inputs_gmm_all <- na.omit(lag_Inputs_all)



# Modell 3: alle Dummies in die Second stage.
# -------------------------------------------
# -------------------------------------------

# Zur Berechnung von omega in der GMM-Funktion:
# --------------------------------------------
# Inputs, über deren Koeffizienten optimiert werden soll:
Inputs_gmm3 <- Inputs_gmm_all

# Fixed Inputs, für die in der 2nd stage keine neuen Koeffizienten berechnet werden:
Inputs_fixed3 <- as.matrix(rep(0,nrow(data_gmm)))
dim(Inputs_fixed3)



# Zur Berechnung von lag_omega in der GMM-Funktion:
# -------------------------------------------------
# Lagged Inputs, über deren Koeffizienten optimiert werden soll:
lag_Inputs_gmm3 <- lag_Inputs_gmm_all

# Lagged fixed Inputs, für die in der 2nd stage keine neuen Koeffizienten berechnet werden:
lag_Inputs_fixed3 <- as.matrix(rep(0,nrow(data_gmm)))
lag_Inputs_fixed3 <- na.omit(lag_Inputs_fixed3)
dim(lag_Inputs_fixed3)




# Zur Wahl der Instrumente Z in der GMM-Funktion
# ----------------------------------------------
instr3 <- cbind("const"=rep(1,nrow(data_p)),"l_m"=data_p$l_m,"lag_f_m"=lag(data_p$f_m)
                ,"k_m"=data_p$k_m,"l2_m"=data_p$l_m^2,"lag_f2_m"=lag(data_p$f_m)^2
                ,"k2_m"=data_p$k_m^2,data_p$suburban,data_p$rurald,data_p$rurals
                ,"l_m_lag_f_m"=data_p$l_m*lag(data_p$f_m),"l_m_k_m"=data_p$l_m*data_p$k_m
                ,"lag_f_m_k_m"=lag(data_p$f_m)*data_p$k_m)
instr_gmm3 <- na.omit(instr3)
dim(instr_gmm3)


#########################################################
##  4.4  Second-stage estimation: GMM Optimierung      ##
#########################################################

# Die Zielfunktion der GMM Optimization sind die Moment Conditions E[(Z'v)'*(Z'v)]=0. Siehe 
# Erläuterungen im Kasten über der gmm_moment_condition-Funktion im Abschnitt (0.7).


# Modell 3: alle Koeffizienten in der 2nd stage
# ----------------------------------------------

# Anpassen der Startwerte
initial_betas <- betas_basic_m[c(1:13)]
betas_fixed <- 0

# Auswahl der Inputs, für die Koeffizienten geschätzt werden sollen.
Inputs_gmm <- Inputs_gmm3
lag_Inputs_gmm <- lag_Inputs_gmm3

# Auswahl der fixed Inputs, für die keine neuen Koeffizienten geschätzt werden sollen.
Inputs_fixed <- Inputs_fixed3
lag_Inputs_fixed <- lag_Inputs_fixed3


# Auswahl der Instrumente.
instr_gmm <- instr_gmm3

# Ausführen der Optimierung
optimization <- optimx(par=initial_betas,fn=gmm_moment_condition, method=c("nlminb","nlm"))
print(optimization)

# Die Optimierung hat funktioniert, wenn optimx$convcode=0 ist.
# Zum Vergleich hier noch einmal die Startwerte (OLS ohne GMM-Algorithmus).
betas_basic_m[c(1:13)]


# Wähle die Koeffizienten aus, deren Zielfunktionswert ('value') am kleinsten ist (beste 
# Minimierung).
j <- which.min(optimization$value)


# Wähle die Koeffizienten aus, deren Zielfunktionswert ('value') am kleinsten ist (beste 
# Minimierung).
# Koeffizienten der Second-Stage (betas):
betas2 <- rbind(optimization$p1[j],optimization$p2[j],optimization$p3[j],optimization$p4[j]
                 ,optimization$p5[j],optimization$p6[j],optimization$p7[j],optimization$p8[j]
                 ,optimization$p9[j],optimization$p10[j],optimization$p11[j],optimization$p12[j]
                 ,optimization$p13[j])





#########################################################
##  4.5 Bootstrapping der Standardfehler               ##
#########################################################

# Bootstrappe nun die Standardfehler für die Parameter aus der Second Stage. 
# Dazu wird die Funktion boot.acf verwendet, die den ACF-Algorithmus zusammenfasst. In der Funktion
# clusterBootSE werden B bootstrap subsamples aus dem Originaldatensatz data_p gezogen und für 
# diese die Koeffizienten der Produktionsfunktion berechnet. Aus dem sample der B Koeffizienten 
# wird dann jeweils die Varianz der Originalkoeffizienten berechnet.

# Note1: Wann immer sich etwas im ACF-Algorithmus ändert, muss die Funktion boot.acf angepasst 
#        werden. 

# Die clusterBootSE-Funktion verlangt 4 Angaben:
# (1) data: Die Daten, aus denen das Sample gezogen wird. Es sollte sich um den Grunddatensatz ohne
#     Panelstruktur handeln, also um den Datensatz so wie er am Ende von Abschnitt 1 vorliegt.
# (3) method: Hier wird das numerische Verfahren ausgewählt, welches im ACF-Algorithmus des 
#     Bootstraps verwendet werden soll. Es muss dem Verfahren entsprechen, auf dessen Grundlage die
#     Koeffizienten im Abschnitts (2.4) berechnet wurden.
# (4) B: Anzahl der Replikationen. 

# Führe das Clustered Bootstrapping aus.
# --------------------------------------
date()
clusterBootSE(data=data0,method=rownames(optimization)[j],B=500)
date()

# Zur Kontrolle: Die erste Spalte sollte die Koeffizienten aus Abschnitt (2.4) 'betas22' ergeben.
# Bei erstmaliger Ausführung empfiehlt es sich, im Abschnitt (0.7) in der Definition der 
# clusterBootSE-Funktion die Zeile '# cat("\r",i*100/B," % done ");flush.console()' einzukommen-
# tieren, um zu sehen, wielange das Bootstrapping dauert.


###############################################################################################
##	                            	5) Auswertung	allgemein			                               ##
###############################################################################################


########################################################
## 	5.1 Berechnung der Produktivitätswerte            ##
########################################################

# Setze den finalen Parametervektor aus den Koeffizienten aus der ersten Stufe und der zweiten
# Stufe zusammen, je nachdem, in welcher Stufe sie identifiziert wurden.

# Modell 3
betas_final <- betas2
data_p$omega2 <- data_p$Phi - Inputs%*%betas_final
data_p$omega2e <- exp(data_p$Phi - Inputs%*%betas_final)
data_gmm$omega2 <- data_gmm$Phi - Inputs_gmm_all%*%betas_final
data_gmm$omega2e <- exp(data_gmm$omega2)


########################################################
## 	5.2 Productivity Dispersion                       ##
########################################################

dstat(data_p$omega2e,d=3)
dstat(data_gmm$omega2e,d=3)


##################################################################################
## 	5.3 Output Elasticities                                                     ##
##################################################################################


#################################
##  5.3.1 data_p               ##
#################################

# Output elasticity for bruttolohn
# ---------------------------------
# elasticity_l = b_l + b_ll*l + b_lf*f + b_kl*k
data_p$elasticity_lohn <- (betas_final[2] + betas_final[5]*Inputs[,2] + betas_final[11]*Inputs[,3]
                           + betas_final[12]*Inputs[,4])


# Output elasticity for fremde DL
# -------------------------------
# elasticity_v = b_f + b_ff*f + b_lf*l + b_kv*k
data_p$elasticity_fdl <- (betas_final[3] + betas_final[6]*Inputs[,3] + betas_final[11]*Inputs[,2] 
                          + betas_final[13]*Inputs[,4])


# Output elasticity for capital
# -----------------------------
# elasticity_k = b_k + b_kk*k + b_lk*l + b_kf*f
data_p$elasticity_cap <- (betas_final[4] + betas_final[7]*Inputs[,4] + betas_final[12]*Inputs[,2] 
                          + betas_final[13]*Inputs[,3])

# Returns to scale
data_p$rts <- data_p$elasticity_lohn + data_p$elasticity_fdl + data_p$elasticity_cap


# Vergleiche die Ausprägung der Output-Elastizitäten und der Returns to Scale
# ----------------------------------------------------------------------------
# pro Jahr
dstat(data_p$elasticity_lohn,d=3)
dstat(data_p$elasticity_fdl,d=3)
dstat(data_p$elasticity_cap,d=3)
dstat(data_p$rts,d=3)

# insgesamt
dstat(as.data.frame(data_p$elasticity_lohn),d=3)
dstat(as.data.frame(data_p$elasticity_fdl),d=3)
dstat(as.data.frame(data_p$elasticity_cap),d=3)
dstat(as.data.frame(data_p$rts),d=3)

# Anzahl der Firmen pro Jahr mit DRS, CRS, IRS
addmargins(table(data_p$Jahr[data_p$rts<1],useNA="ifany",dnn="DRS"))
addmargins(table(data_p$Jahr[data_p$rts==1],useNA="ifany",dnn="CRS"))
addmargins(table(data_p$Jahr[data_p$rts>1],useNA="ifany",dnn="IRS"))


#################################
##  5.3.2 data_gmm             ##
#################################


# Output elasticity for bruttolohn
# ---------------------------------
# elasticity_l = b_l + b_ll*l + b_lf*f + b_kl*k
data_gmm$elasticity_lohn <- (betas_final[2] + betas_final[5]*Inputs_gmm_all[,2] 
                             + betas_final[11]*Inputs_gmm_all[,3] 
                             + betas_final[12]*Inputs_gmm_all[,4])


# Output elasticity for fremde DL
# -------------------------------
# elasticity_v = b_f + b_ff*f + b_lf*l + b_kv*k
data_gmm$elasticity_fdl <- (betas_final[3] + betas_final[6]*Inputs_gmm_all[,3] 
                            + betas_final[11]*Inputs_gmm_all[,2] 
                            + betas_final[13]*Inputs_gmm_all[,4])


# Output elasticity for capital
# -----------------------------
# elasticity_k = b_k + b_kk*k + b_lk*l + b_kf*f
data_gmm$elasticity_cap <- (betas_final[4] + betas_final[7]*Inputs_gmm_all[,4] 
                            + betas_final[12]*Inputs_gmm_all[,2] 
                            + betas_final[13]*Inputs_gmm_all[,3])

# Returns to scale
data_gmm$rts <- data_gmm$elasticity_lohn + data_gmm$elasticity_fdl + data_gmm$elasticity_cap


# Vergleiche die Ausprägung der Output-Elastizitäten und der Returns to Scale
# ----------------------------------------------------------------------------
# pro Jahr
dstat(data_gmm$elasticity_lohn,d=3)
dstat(data_gmm$elasticity_fdl,d=3)
dstat(data_gmm$elasticity_cap,d=3)
dstat(data_gmm$rts,d=3)

# insgesamt
dstat(as.data.frame(data_gmm$elasticity_lohn),d=3)
dstat(as.data.frame(data_gmm$elasticity_fdl),d=3)
dstat(as.data.frame(data_gmm$elasticity_cap),d=3)
dstat(as.data.frame(data_gmm$rts),d=3)

# Anzahl der Firmen pro Jahr mit DRS, CRS, IRS
addmargins(table(data_gmm$Jahr[data_gmm$rts<1],useNA="ifany",dnn="DRS"))
addmargins(table(data_gmm$Jahr[data_gmm$rts==1],useNA="ifany",dnn="CRS"))
addmargins(table(data_gmm$Jahr[data_gmm$rts>1],useNA="ifany",dnn="IRS"))




####################################################################################################
## 	        6) Auswertung Linkl between Reorganisation and Productivity Productivity              ##
####################################################################################################




############################################
##      6.1.1 Re-run AR(1) process        ##
#################++#########################

# Hier können nur die Firmen untersucht werden, für die das lag t-1 beobachtet wird.
# Ordne jeder Firma ihren Produktitivätswert sowie den des Vorjahres zu.


data_gmm$lag_omega2 <- data_gmm$lag_Phi - lag_Inputs_gmm_all%*%betas_final
data_gmm <- pdata.frame(data.frame(data_gmm),index=c("id","Jahr"),row.names=FALSE)
data_gmm <- pdata.frame(data.frame(data_gmm),index=c("id","Jahr"),row.names=FALSE)



# Pooling (OLS) 
# -------------

AR1_expost2 <- plm(omega2 ~ lag_omega2 + I(lag_omega2^2) + I(lag_omega2^3)
                   + lag1_privlaw + I(lag1_privlaw*lag1_eigentuemer2) + shareF 
                   + shareFEW
             + I(shareF*shareFEW)
             + I(shareF*lag1_privlaw*lag1_eigentuemer2) 
             + I(shareFEW*lag1_privlaw*lag1_eigentuemer2)
             + I(shareF*lag1_privlaw)
             + I(shareFEW*lag1_privlaw)
                   ,data=data_gmm,model="pooling",effect="time",index=c("id"))
summary(AR1_expost2)


# Compute WHITE standard errors robust against heteroscedasticity and autocorrelation:
coeftest(AR1_expost2,vcov=vcovHC(AR1_expost2,method="arellano",cluster=c("group")))


# Check for autocorrelation and heteroscedasticity
# ------------------------------------------------

# Is autocorrelation a concern?
dwtest(omega2 ~ lag_omega2 + I(lag_omega2^2) + I(lag_omega2^3) + lag1_privlaw 
       + I(lag1_privlaw*lag1_eigentuemer2) 
       + shareF + shareFEW
             + I(shareF*shareFEW)
             + I(shareF*lag1_privlaw*lag1_eigentuemer2) 
             + I(shareFEW*lag1_privlaw*lag1_eigentuemer2)
             + I(shareF*lag1_privlaw)
             + I(shareFEW*lag1_privlaw)
       ,data=data_gmm)

# Is heteroscedasticity a concern?
bptest(AR1_expost2)



##########################################
##      6.1.2 Hypothesis Testing        ##
##########################################


# (private minority)
# --------------------
# Testet, ob die private Beteiligung bei GmbHs einen signifikanten Einfluss auf die 
# Produktivität hat.
linearHypothesis(AR1_expost2,"I(lag1_privlaw * lag1_eigentuemer2)-lag1_privlaw=0"
                 ,vcov=vcovHC(AR1_expost2,method="arellano",cluster=c("group")))


# Private minority & outsourcing
# -------------------------------
# Testet, ob die private Beteiligung einen signifikanten Einfluss auf den pass-through von 
# Outsourcing I auf Productivity hat.
linearHypothesis(AR1_expost2
                 ,"I(shareF * lag1_privlaw * lag1_eigentuemer2)- I(shareF * lag1_privlaw)=0"
                 ,vcov=vcovHC(AR1_expost2,method="arellano",cluster=c("group")))

# Testet, ob die private Beteiligung einen signifikanten Einfluss auf den pass-through von 
# Outsourcing II auf Productivity hat.
linearHypothesis(AR1_expost2
                 ,"I(shareFEW * lag1_privlaw * lag1_eigentuemer2)- I(shareFEW * lag1_privlaw)=0"
                 ,vcov=vcovHC(AR1_expost2,method="arellano",cluster=c("group")))



# Private Rechtsform & outsourcing
# --------------------------------
# Testet, ob die private Rechtsform bei rein-öfftl. Unternehmen einen signifikanten Einfluss auf den
# pass-through von Outsourcing I auf die Produktivität hat.
linearHypothesis(AR1_expost2
                 ,"I(shareF * lag1_privlaw) - shareF=0"
                 ,vcov=vcovHC(AR1_expost2,method="arellano",cluster=c("group")))

# Testet, ob die private Rechtsform bei rein-öfftl. Unternehmen einen signifikanten Einfluss auf den
# pass-through von Outsourcing II auf die Produktivität hat.
linearHypothesis(AR1_expost2
                 ,"I(shareFEW * lag1_privlaw) - shareFEW=0"
                 ,vcov=vcovHC(AR1_expost2,method="arellano",cluster=c("group")))




#####################################################################
##    6.2 Regress Productivity on Reorganisation Variables         ##
#####################################################################

# Regress productivity on the legal status to see whether there is any further hint at the relevance
# of the legal status.

# Recode data_p, damit die Regression funktioniert.
data_p <- pdata.frame(as.data.frame(data_p),index=c("id","Jahr"),row.names=FALSE)
data_p <- pdata.frame(as.data.frame(data_p),index=c("id","Jahr"),row.names=FALSE)

# Kontrolliere zusätzlich für die Firmengröße (z.B. über revenues), da Eigenbetriebe tendenziell 
# kleiner sind als AGs, für die Präsenz in den einzelnen Sektoren und für die Siedlungsstruktur.



############################################
##  6.2.1 Regression ohne Zeittrend       ##
############################################

# Lasse den Zeittrend im Intercept weg.
# Vorteil ist, dass der aggregierte Produktivitätszuwachs durch Reorganisation nicht herausgerechnet
# wird.
# Treten ab 2008 vermehrt privatwirtschaftliche Unternehmen auf und sind diese produktiver, werden
# sie mit Eigenbetrieben aus dem Jahr 2003 verglichen und der Zugewinn auf die private Rechtsform
# zurückgeführt. Es könnte ja tatsächlich sein, dass die private Rechtsform produktiver ist und
# dies ab 2008 zu einem aggregierten Produktivitätszuwachs geführt hat. Dieser aggregierte 
# 'endogene' Gewinn geht bei Modellen mit Zeittrend (6.2.1) verloren.


explain_pty2 <- plm(omega2 ~ 
                   lag1_privlaw 
                   + I(lag1_privlaw*lag1_eigentuemer2) 
                   + shareF 
                   + shareFEW 
                   + I(shareF*shareFEW)
                   + I(shareF*lag1_privlaw*lag1_eigentuemer2) 
                   + I(shareFEW*lag1_privlaw*lag1_eigentuemer2)
                   + I(shareF*lag1_privlaw)
                   + I(shareFEW*lag1_privlaw)
                   + size_med
                   + size_large
                   + suburban + rurald + rurals
                   ,data=data_p,model="pooling",effect="time",index=c("id"))
summary(explain_pty2)

# Is autocorrelation a concern?
dwtest(omega2 ~  lag1_privlaw + I(lag1_privlaw*lag1_eigentuemer2) 
       + shareF + shareFEW 
       + I(shareF*shareFEW)
       + I(shareF*lag1_privlaw*lag1_eigentuemer2) 
       + I(shareFEW*lag1_privlaw*lag1_eigentuemer2)
       + I(shareF*lag1_privlaw)
       + I(shareFEW*lag1_privlaw)
       + size_med + size_large + suburban + rurald + rurals
       ,data=data_p)

# Is heteroscedasticity a concern?
bptest(explain_pty2)

# WHITE standard errors robust against heteroscedasticity and autocorrelation?
coeftest(explain_pty2,vcov=vcovHC(explain_pty2,method="arellano"))



############################################
##  6.2.2 Hypothesentests       ##
############################################



# Private minority
# --------------------
# Testet, ob die private Beteiligung bei GmbHs einen signifikanten Einfluss auf die 
# Produktivität hat.
linearHypothesis(explain_pty2,"I(lag1_privlaw * lag1_eigentuemer2)-lag1_privlaw=0"
                 ,vcov=vcovHC(explain_pty2,method="arellano",cluster=c("group")))


# Private minority & outsourcing
# -------------------------------
# Testet, ob die private Beteiligung einen signifikanten Einfluss auf den pass-through von 
# Outsourcing I auf Productivity hat.
linearHypothesis(explain_pty2
                 ,"I(shareF * lag1_privlaw * lag1_eigentuemer2)- I(shareF * lag1_privlaw)=0"
                 ,vcov=vcovHC(explain_pty2,method="arellano",cluster=c("group")))

# Testet, ob die private Beteiligung einen signifikanten Einfluss auf den pass-through von 
# Outsourcing II auf Productivity hat.
linearHypothesis(explain_pty2
                 ,"I(shareFEW * lag1_privlaw * lag1_eigentuemer2)- I(shareFEW * lag1_privlaw)=0"
                 ,vcov=vcovHC(explain_pty2,method="arellano",cluster=c("group")))



# Private Rechtsform & outsourcing
# --------------------------------
# Testet, ob die private Rechtsform bei rein-öfftl. Unternehmen einen signifikanten Einfluss auf den
# pass-through vonOutsourcing I auf die Produktivität hat.
linearHypothesis(explain_pty2
                 ,"I(shareF * lag1_privlaw) - shareF=0"
                 ,vcov=vcovHC(explain_pty2,method="arellano",cluster=c("group")))

# Testet, ob die private Rechtsform bei rein-öfftl. Unternehmen einen signifikanten Einfluss auf den
# pass-through von Outsourcing II auf die Produktivität hat.
linearHypothesis(explain_pty2
                 ,"I(shareFEW * lag1_privlaw) - shareFEW=0"
                 ,vcov=vcovHC(explain_pty2,method="arellano",cluster=c("group")))


###################################################################################################
date()
############################## End of file ########################################################
