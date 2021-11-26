###################################################################################################
###################################################################################################
### Titel des Projekts: KOMIED
### Projekt-Nr. 2094-2012
###################################################################################################
###################################################################################################
### Projektleitung: Astrid Cullmann (DIW Berlin)
### Datengrundlage: AFiD-Panel Energieunternehmen & Energiebetriebe, JAB, URS (Organschaft)
### 
### Dateiname des Programmcodes: Integriert_Schaetzung_20170825_v174_cs.R
### erstellt: 25.08.2017
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
### Version 174:  - Formulierung mit Translog-Modell | Wagebill | Disaggregierte Daten
###               - Mixed Utilities ohne Wasserfirmen und ohne reine Strom-/Gas-DSOs/Retailer sowie 
###                 ohne reine Erzeuger (Strom und Wärme) 2003-2014
###               - Sensitivitätsanalyse shareF t-2, mit SE
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
#Pfad3<-"Q:\\AFS\\55_FDZ\\Forschungsprojekte\\2012-2094 DIW - Richter (4)\\KDFV\\2017_08_09_cs_2\\"

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


# Arbeitsdatensatz einlesen (DIW und FDZ)
# ---------------------------------------
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
                      ,data_gmm$lag1_unlisted
                      ,data_gmm$lag1_listed
                      ,data_gmm$lag1_unlisted*data_gmm$lag1_eigentuemer2
                      ,data_gmm$lag1_listed*data_gmm$lag1_eigentuemer2
                      ,data_gmm$lag2_shareF,data_gmm$shareFEW
                      #,data_gmm$shareF*data_gmm$shareFEW
                      #,data_gmm$shareF*data_gmm$lag1_privlaw*data_gmm$lag1_eigentuemer2
                      #,data_gmm$shareFEW*data_gmm$lag1_privlaw*data_gmm$lag1_eigentuemer2
                      #,data_gmm$shareF*data_gmm$lag1_privlaw
                      #,data_gmm$shareFEW*data_gmm$lag1_privlaw
                      )
  AR1 <<- lm(omega ~ lag_omega + I(lag_omega^2) + I(lag_omega^3) 
             + data_gmm$lag1_unlisted 
             + data_gmm$lag1_listed
             + I(data_gmm$lag1_unlisted*data_gmm$lag1_eigentuemer2)
             + I(data_gmm$lag1_listed*data_gmm$lag1_eigentuemer2)
             + data_gmm$lag2_shareF + data_gmm$shareFEW 
             #+ I(data_gmm$shareF*data_gmm$shareFEW)
             #+ I(data_gmm$shareF*data_gmm$lag1_privlaw*data_gmm$lag1_eigentuemer2) 
             #+ I(data_gmm$shareFEW*data_gmm$lag1_privlaw*data_gmm$lag1_eigentuemer2)
             #+ I(data_gmm$shareF*data_gmm$lag1_privlaw)
             #+ I(data_gmm$shareFEW*data_gmm$lag1_privlaw)
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
                      + l_m:f_m + l_m:k_m + f_m:k_m + I(ga*defl_ga) + I(wa*defl_wa) + I(sa*p_sa_log)
                      + I(wm_NWG*defl_wm_NWG) + I(se_gas*defl_se_eg) + I(se_oil*defl_se_oil) 
                      + I(se_EE2*log(100)) + suburban + rurald + rurals
                      + l_m:k_m:f_m + l_m:I(f_m^2) + f_m:I(k_m^2) + l_m:I(k_m^2) + f_m:I(l_m^2) 
                      + k_m:I(f_m^2) + k_m:I(l_m^2) + I(l_m^2):I(k_m^2) + I(l_m^2):I(f_m^2) 
                      + I(f_m^2):I(k_m^2) + k_m:I(l_m^2):f_m + l_m:I(k_m^2):f_m + l_m:I(f_m^2):k_m  
                      + f_m:I(l_m^2):I(k_m^2) + k_m:I(l_m^2):I(f_m^2) + l_m:I(k_m^2):I(f_m^2)
                      + I(l_m^2):I(f_m^2):I(k_m^2),data_bp)
  betas1 <<- as.vector(first_stage_m$coefficients)
  data_bp$Phi <<- first_stage_m$fitted.values
  data_bp$lag_Phi <<- lag(data_bp$Phi)
  Inputs <<- as.matrix(cbind(rep(1,nrow(data_bp)),data_bp$l_m,data_bp$f_m,data_bp$k_m
                             ,0.5*(data_bp$l_m)^2,0.5*(data_bp$f_m)^2,0.5*(data_bp$k_m)^2
                             ,data_bp$ga*data_bp$defl_ga
                             ,data_bp$wa*data_bp$defl_wa
                             ,data_bp$sa*data_bp$p_sa_log
                             ,data_bp$wm_NWG*data_bp$defl_wm_NWG
                             ,data_bp$se_gas*data_bp$defl_se_eg
                             ,data_bp$se_oil*data_bp$defl_se_oil
                             ,data_bp$se_EE2*log(100)
                            ,data_bp$suburban,data_bp$rurald,data_bp$rurals
                            ,data_bp$l_m*data_bp$f_m,data_bp$l_m*data_bp$k_m
                            ,data_bp$f_m*data_bp$k_m))
  data_bp$lag1_unlisted <- lag(data_bp$unlisted)
  data_bp$lag1_listed <- lag(data_bp$listed)
  data_bp$lag1_eigentuemer2 <- lag(data_bp$eigentuemer2)
  data_bp$lag1_shareF <- lag(data_bp$shareF)
  data_bp$lag2_shareF <- lag(data_bp$shareF,2)
  data_bp$lag1_shareFEW <- lag(data_bp$shareFEW)
  data_gmm <<- subset(data_bp,is.na(lag_Phi)==FALSE & is.na(lag2_shareF)==FALSE)
  n <<- nrow(data_gmm)
  Inputs_gmm <<- as.matrix(cbind(rep(1,nrow(data_gmm)),data_gmm$l_m,data_gmm$f_m,data_gmm$k_m
                                 ,0.5*(data_gmm$l_m)^2,0.5*(data_gmm$f_m)^2,0.5*(data_gmm$k_m)^2
                                 ,data_gmm$ga*data_gmm$defl_ga
                                 ,data_gmm$wa*data_gmm$defl_wa
                                 ,data_gmm$sa*data_gmm$p_sa_log
                                 ,data_gmm$wm_NWG*data_gmm$defl_wm_NWG
                                 ,data_gmm$se_gas*data_gmm$defl_se_eg
                                 ,data_gmm$se_oil*data_gmm$defl_se_oil
                                 ,data_gmm$se_EE2*log(100)
                                 ,data_gmm$suburban,data_gmm$rurald,data_gmm$rurals
                                 ,data_gmm$l_m*data_gmm$f_m,data_gmm$l_m*data_gmm$k_m
                                 ,data_gmm$f_m*data_gmm$k_m))
  lag_Inputs <<- as.matrix(cbind(rep(1,nrow(data_bp)),lag(data_bp$l_m),lag(data_bp$f_m)
                                 ,lag(data_bp$k_m),0.5*(lag(data_bp$l_m))^2,0.5*(lag(data_bp$f_m))^2
                                 ,0.5*(lag(data_bp$k_m))^2
                                 ,lag(data_bp$ga)*lag(data_bp$defl_ga)
                                 ,lag(data_bp$wa)*lag(data_bp$defl_wa)
                                 ,lag(data_bp$sa)*lag(data_bp$p_sa_log)
                                 ,lag(data_bp$wm_NWG)*lag(data_bp$defl_wm_NWG)
                                 ,lag(data_bp$se_gas)*lag(data_bp$defl_se_eg)
                                 ,lag(data_bp$se_oil)*lag(data_bp$defl_se_oil)
                                 ,lag(data_bp$se_EE2)*log(100)
                                 ,lag(data_bp$suburban),lag(data_bp$rurald),lag(data_bp$rurals)
                                 ,lag(data_bp$l_m)*lag(data_bp$f_m)
                                 ,lag(data_bp$l_m)*lag(data_bp$k_m)
                                 ,lag(data_bp$f_m)*lag(data_bp$k_m)
                                 ,lag(data_bp$shareF,2)))
  lag_Inputs_gmm <<- na.omit(lag_Inputs)
  lag_Inputs_gmm <<- lag_Inputs_gmm[,1:20]
  Inputs_fixed <<- as.matrix(rep(0,nrow(data_gmm)))
  lag_Inputs_fixed <<- as.matrix(rep(0,nrow(data_gmm)))
  lag_Inputs_fixed <<- na.omit(lag_Inputs_fixed)
  instr <<- cbind(rep(1,nrow(data_bp)),data_bp$l_m,lag(data_bp$f_m),data_bp$k_m,data_bp$l_m^2
                  ,lag(data_bp$f_m)^2,data_bp$k_m^2,data_bp$ga*data_bp$defl_ga
                 ,data_bp$wa*data_bp$defl_wa,lag(data_bp$sa)*lag(data_bp$p_sa_log)
                 ,data_bp$wm_NWG*data_bp$defl_wm_NWG,data_bp$se_gas*data_bp$defl_se_eg
                 ,data_bp$se_oil*data_bp$defl_se_oil
                 ,data_bp$se_EE2*log(100),data_bp$suburban,data_bp$rurald,data_bp$rurals
                 ,data_bp$l_m*lag(data_bp$f_m),data_bp$l_m*data_bp$k_m,lag(data_bp$f_m)*data_bp$k_m
                 ,lag(data_bp$shareF,2))
  instr_gmm <<- na.omit(instr)
  instr_gmm <<- instr_gmm[,1:20]
  starting_values <<- lm(va3_m ~ l_m + f_m + k_m + I(0.5*l_m^2) +  I(0.5*f_m^2) + I(0.5*k_m^2) 
                         + l_m:f_m + l_m:k_m + f_m:k_m + I(ga*defl_ga) + I(wa*defl_wa) 
                         + I(sa*p_sa_log) + I(wm_NWG*defl_wm_NWG) + I(se_gas*defl_se_eg) 
                         + I(se_oil*defl_se_oil) + I(se_EE2*log(100)) + suburban + rurald + rurals
                         ,data_bp)
  betas_basic_m <<- as.vector(starting_values$coefficients)
  initial_betas <<- betas_basic_m[c(1:20)]
  betas_fixed <<- 0
  optimization <<- optimx(par=initial_betas,fn=gmm_moment_condition, method=method)
  betas22_boot <<- rbind(optimization$p1[1],optimization$p2[1],optimization$p3[1],optimization$p4[1]
                        ,optimization$p5[1],optimization$p6[1],optimization$p7[1]
                        ,optimization$p8[1],optimization$p9[1],optimization$p10[1]
                        ,optimization$p11[1],optimization$p12[1],optimization$p13[1]
                        ,optimization$p14[1],optimization$p15[1],optimization$p16[1]
                        ,optimization$p17[1],optimization$p18[1],optimization$p19[1]
                        ,optimization$p20[1])
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
  sterrs <- matrix(NA, nrow=B, ncol=20)
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

# Erstelle die neue Output-Variable 'value_added3'
# ------------------------------------------------
# revenues1 (nicht preisbereinigt) - intermediates (preisbereinigt mit Vorleistungsgüterindex)
data$value_added3 <- data$revenues1 - data$intermediates
cor(data$value_added2,data$value_added3,use="complete.obs")


# Wähle aufgrund der Logarithmierung nur die Utilities aus, die positive Werte in den Inputs und 
# Outputs angegeben haben.
# ------------------------
data0 <- subset(data, fremdeDL>0 & K_adj>0 & bruttolohn>0 & value_added3>0)
addmargins(table(data0$Jahr))


# Bereinige Datensatz um konstante Reihen.
# ---------------------------------------
data0 <- subset(data0,select=-c(id_ns,id_he1,id_he2,id_he3,id_he4,id_th,id_sh,jab,public
                                ,Lueckenjahre,out_labour,out_lohn,out_int,out_umsatz,out_p_LV
                                ,out_p_EVU,out_p_TK,out_p_SK,out_p_HH,out_p_VG,out_p_BC,out_p_sa))


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





###################################
##   1.2 Sektordummies           ##
###################################

# Kontrolliere für die Präsenz in bestimmten Untersektoren:
# ---------------------------------------------------------
data0$sa_EVU <- ifelse(is.na(data0$U_ABS_EF1011_sum)==FALSE & data0$U_ABS_EF1011_sum>0,1,0)
data0$sa_TK <- ifelse(is.na(data0$U_ABS_EF1051_sum)==FALSE & data0$U_ABS_EF1051_sum>0,1,0)
data0$sa_SK <- ifelse(is.na(data0$U_ABS_EF1041_sum)==FALSE & data0$U_ABS_EF1041_sum>0,1,0)
data0$wm_NWG <- ifelse((is.na(data0$B_waerme_EF1011a)==FALSE & data0$B_waerme_EF1011a>0) 
                       | (is.na(data0$B_waerme_EF1010)==FALSE & data0$B_waerme_EF1010>0),1,0)
data0$wm_HH <- ifelse(is.na(data0$B_waerme_EF1011b)==FALSE & data0$B_waerme_EF1011b>0,1,0)



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





####################################
##  1.3 Preise/PPI/Deflation      ##
####################################

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



########################################
##  1.4 Organisation Variablen        ##
########################################

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
data0$privlaw <- ifelse(data0$status==3 | data0$status==4,1,0)


# Kontrolliere für die Siedlungsstruktur
# --------------------------------------
data0$metro <- ifelse(data0$Siedlung==1,1,0)
data0$suburban <- ifelse(data0$Siedlung==2,1,0)
data0$rurald <- ifelse(data0$Siedlung==3,1,0)
data0$rurals <- ifelse(data0$Siedlung==4,1,0)



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



# Erstelle die Variable 'Kundenstruktur im Stromsektor'
# ------------------------------------------------------
data0$ShareTK<-ifelse(data0$U_ABS_EF1051_sum>0
                      ,data0$U_ABS_EF1051_sum/data0$U_ABS_EF1061,0)

data0$ShareSK<-ifelse(data0$U_ABS_EF1041_sum>0
                      ,data0$U_ABS_EF1041_sum/data0$U_ABS_EF1061,0)

data0$ShareWV<-ifelse(is.na(data0$U_ABS_EF1011_sum)==FALSE
                      ,data0$U_ABS_EF1011_sum/(data0$U_ABS_EF1061
                                               +ifelse(is.na(data0$U_ABS_EF1011_sum)==TRUE,0
                                                       ,data0$U_ABS_EF1011_sum)),0)

data0$ShareTK[is.na(data0$ShareTK)==TRUE] <- 0
data0$ShareSK[is.na(data0$ShareSK)==TRUE] <- 0
data0$ShareWV[is.na(data0$ShareWV)==TRUE] <- 0

dstat(as.data.frame(data0$ShareTK),d=2)
dstat(as.data.frame(data0$ShareSK),d=2)
dstat(as.data.frame(data0$ShareWV),d=2)


# Erstelle die Variable 'Kundenstruktur im Wärmesektor'
# -----------------------------------------------------
data0$ShareHH <- ifelse(data0$B_waerme_EF1011b>0
                        ,data0$B_waerme_EF1011b/data0$B_waerme_EF1011,0)

data0$ShareVG <- ifelse(data0$B_waerme_EF1011a>0
                        ,data0$B_waerme_EF1011a/data0$B_waerme_EF1011,0)

data0$ShareSo <- ifelse(data0$B_waerme_EF1011c>0
                        ,data0$B_waerme_EF1011c/data0$B_waerme_EF1011,0)

data0$ShareWV_w<-ifelse(is.na(data0$B_waerme_EF1010)==FALSE
                        ,data0$B_waerme_EF1010/(data0$B_waerme_EF1011
                                                +ifelse(is.na(data0$B_waerme_EF1010)==TRUE
                                                        ,0,data0$B_waerme_EF1010)),0)

data0$ShareHH[is.na(data0$ShareHH)==TRUE] <- 0
data0$ShareSo[is.na(data0$ShareSo)==TRUE] <- 0
data0$ShareVG[is.na(data0$ShareVG)==TRUE] <- 0
data0$ShareWV_w[is.na(data0$ShareWV_w)==TRUE] <- 0

dstat(as.data.frame(data0$ShareHH),d=2)
dstat(as.data.frame(data0$ShareSo),d=2)
dstat(as.data.frame(data0$ShareVG),d=2)
dstat(as.data.frame(data0$ShareWV_w),d=2)


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
# Braunkohlewerke weg, da sie den Bootstrap blockieren.
data0 <- subset(data0,is.na(data0$status)==FALSE 
                & is.na(data0$eigentuemer2)==FALSE
                & is.na(data0$Siedlung)==FALSE
                & se_bk==0)

# Lasse zunächst die Firmen weg, die eine gemischte Siedlungsstruktur aufweisen
data0 <- subset(data0,Siedlung !=5)

# Bereinige noch einmal um konstante Reihen.
data0 <- subset(data0,select=-c(se_bk))




# Wahl des Subsets
# ----------------
# Schließe reine Wasserfirmen, reine Strom-/Gasfirmen sowie Erzeuger (Strom & Wärme) aus.
data0 <- subset(data0,(wa==1 & sa==0 & se==0 & sn==0 & wm==0 & ga==0)==FALSE
                      & (wa==0 & wm==0 & se==0 & (sa==1 | sn==1 | ga==1))==FALSE
                      & (wa==0 & wm==0 & se==1 & (sa==1 | sn==1 | ga==1))==FALSE
                      & ((se == 1 | wm==1) & ga==0 & wa==0 & sa==0 & sn==0)==FALSE)


# Gesamtes Sample (all utilities)
data0all <- data0


# Umwandlung in Paneldatensatz
# ----------------------------
# Deklariere Datensatz als Paneldata und lege fest, welche Spalte das Individium i und welche
# die Zeitkomponente t enthält.
data_p <- pdata.frame(data0, index=c("id","Jahr"),row.names=FALSE)
pdim(data_p)

data_p_all <- pdata.frame(data0all, index=c("id","Jahr"),row.names=FALSE)
pdim(data_p_all)

# Erstelle einen lineraren Zeitdummy
# ----------------------------------
# 2003=1, 2004=2, ...
data_p$t <- as.numeric(factor(data_p$Jahr))
data_p_all$t <- as.numeric(factor(data_p_all$Jahr))

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




# Grundmodell mit Controls in t-1 (all utilities)
# -------------------------------------------------
out1_OLS1 <- lm(shareF ~  t + I(t^2) 
               # andere Reorganisation-Variablen
               + lag(data_p$privlaw)  + lag(data_p$eigentuemer2) + lag(data_p$shareFEW)
               # Unternehmensgröße und Produktionsprozess
               + size_med + size_large + lag(data_p$inv_int) + lag(data_p$wage)
               # Sektoren und Kundenstruktur
               + sn + ga + wa + se_gas + se_oil + se_hc + se_waste + se_bio + se_EE + se_water 
               + se_sonst
               # Environment
               + suburban + rurald + rurals + lag(data_p$ShareTK) + lag(data_p$ShareWV) 
               + lag(data_p$ShareHH) + lag(data_p$ShareWV_w)
               ,data_p_all)
summary(out1_OLS1)


# Grundmodell mit Interaktionen in Controls in t-1 (all utilities)
# -----------------------------------------------------------------
out1_OLS2 <- lm(shareF ~  t + I(t^2) 
                # andere Reorganisation-Variablen
                + lag(data_p$privlaw)  + I(lag(data_p$privlaw)*lag(data_p$eigentuemer2))
                + lag(data_p$shareFEW)
                # Unternehmensgröße und Produktionsprozess
                + size_med + size_large + lag(data_p$inv_int) + lag(data_p$wage)
                # Sektoren und Kundenstruktur
                + sn + ga + wa + se_gas + se_oil + se_hc + se_waste + se_bio + se_EE + se_water 
                + se_sonst
                # Environment
                + suburban + rurald + rurals + lag(data_p$ShareTK) + lag(data_p$ShareWV) 
                + lag(data_p$ShareHH) + lag(data_p$ShareWV_w)
                ,data_p_all)
summary(out1_OLS2)




# Grundmodell mit Controls in t-1 (mixed utilities)
# -------------------------------------------------
out1_OLS3 <- lm(shareF ~  t + I(t^2) 
                # andere Reorganisation-Variablen
                + lag(data_p$privlaw)  + lag(data_p$eigentuemer2) + lag(data_p$shareFEW)
                # Unternehmensgröße und Produktionsprozess
                + size_med + size_large + lag(data_p$inv_int) + lag(data_p$wage)
                # Sektoren und Kundenstruktur
                + sn + ga + wa + se_gas + se_oil + se_hc + se_waste + se_bio + se_EE + se_water 
                + se_sonst
                # Environment
                + suburban + rurald + rurals + lag(data_p$ShareTK) + lag(data_p$ShareWV) 
                + lag(data_p$ShareHH) + lag(data_p$ShareWV_w)
                ,data_p)
summary(out1_OLS3)


# Grundmodell mit Interaktionen in Controls in t-1 (mixed utilities)
# ------------------------------------------------------------------
out1_OLS4 <- lm(shareF ~  t + I(t^2) 
                # andere Reorganisation-Variablen
                + lag(data_p$privlaw)  + I(lag(data_p$privlaw)*lag(data_p$eigentuemer2))
                + lag(data_p$shareFEW)
                # Unternehmensgröße und Produktionsprozess
                + size_med + size_large + lag(data_p$inv_int) + lag(data_p$wage)
                # Sektoren und Kundenstruktur
                + sn + ga + wa + se_gas + se_oil + se_hc + se_waste + se_bio + se_EE + se_water 
                + se_sonst
                # Environment
                + suburban + rurald + rurals + lag(data_p$ShareTK) + lag(data_p$ShareWV) 
                + lag(data_p$ShareHH) + lag(data_p$ShareWV_w)
                ,data_p)
summary(out1_OLS4)




##########################################
##    3.2 Outsourcing 2: Produktion     ##
##########################################



# Grundmodell mit Controls in t-1 (all utilities)
# -----------------------------------------------
out2_OLS1 <- lm(shareFEW ~  t + I(t^2) 
                # andere Reorganisation-Variablen
                + lag(data_p$privlaw)  + lag(data_p$eigentuemer2) + lag(data_p$shareF)
                # Unternehmensgröße und Produktionsprozess
                + size_med + size_large + lag(data_p$inv_int) + lag(data_p$wage)
                # Sektoren und Kundenstruktur
                + sn + ga + wa + se_gas + se_oil + se_hc + se_waste + se_bio + se_EE + se_water 
                + se_sonst
                # Environment
                + suburban + rurald + rurals + lag(data_p$ShareTK) + lag(data_p$ShareWV) 
                + lag(data_p$ShareHH) + lag(data_p$ShareWV_w)
                ,data_p_all)
summary(out2_OLS1)


# Grundmodell mit Interaktionen in Controls in t-1 (all utilities)
# ----------------------------------------------------------------
out2_OLS2 <- lm(shareFEW ~  t + I(t^2) 
                # andere Reorganisation-Variablen
                + lag(data_p$privlaw)  + I(lag(data_p$privlaw)*lag(data_p$eigentuemer2))
                + lag(data_p$shareF)
                # Unternehmensgröße und Produktionsprozess
                + size_med + size_large + lag(data_p$inv_int) + lag(data_p$wage)
                # Sektoren und Kundenstruktur
                + sn + ga + wa + se_gas + se_oil + se_hc + se_waste + se_bio + se_EE + se_water 
                + se_sonst
                # Environment
                + suburban + rurald + rurals + lag(data_p$ShareTK) + lag(data_p$ShareWV) 
                + lag(data_p$ShareHH) + lag(data_p$ShareWV_w)
                ,data_p_all)
summary(out2_OLS2)




# Grundmodell mit Controls in t-1 (mixed utilities)
# -------------------------------------------------
out2_OLS3 <- lm(shareFEW ~  t + I(t^2)
               # andere Reorganisation-Variablen
               + lag(data_p$privlaw)  + lag(data_p$eigentuemer2) + lag(data_p$shareF)
               # Unternehmensgröße und Produktionsprozess
               + size_med + size_large + lag(data_p$inv_int) + lag(data_p$wage)
               # Sektoren und Kundenstruktur
               + sn + ga + wa + se_gas + se_oil + se_hc + se_waste + se_bio + se_EE + se_water 
               + se_sonst
               # Environment
               + suburban + rurald + rurals + lag(data_p$ShareTK) + lag(data_p$ShareWV) 
               + lag(data_p$ShareHH) + lag(data_p$ShareWV_w)
               ,data_p)
summary(out2_OLS3)


# Grundmodell mit Interaktionen in Controls in t-1 (mixed utilities)
# ------------------------------------------------------------------
out2_OLS4 <- lm(shareFEW ~  t + I(t^2)
                # andere Reorganisation-Variablen
                + lag(data_p$privlaw)  + I(lag(data_p$privlaw)*lag(data_p$eigentuemer2))
                + lag(data_p$shareF)
                # Unternehmensgröße und Produktionsprozess
                + size_med + size_large + lag(data_p$inv_int) + lag(data_p$wage)
                # Sektoren und Kundenstruktur
                + sn + ga + wa + se_gas + se_oil + se_hc + se_waste + se_bio + se_EE + se_water 
                + se_sonst
                # Environment
                + suburban + rurald + rurals + lag(data_p$ShareTK) + lag(data_p$ShareWV) 
                + lag(data_p$ShareHH) + lag(data_p$ShareWV_w)
                ,data_p)
summary(out2_OLS4)







###################################################################################################
##                    		4) Strukturelle Schätzung				    	                                 ##
###################################################################################################



###############################################################################################
##  4.1 Basismodell mit OLS ohne ACF-Algorithmus                                             ##
###############################################################################################

# Nimm für die Produktionsfunktion im Grundmodell eine Translog-Funktion an.

# Output: Value added (Revenues - Materialaufwand - Energieeinkauf - Steuern)
# Inputs: wage bill, fremde DL, Kapital

# Die sich daraus ergebende Produktionsfunktion:
# value-added_it = beta_l * l_it + beta_s * s_it + beta_k * k_it  + beta_ll * l_it^2 
#                  + beta_ss * s_it^2  + beta_kk * k_it^2 + 0.5 * beta_ls * lit * s_it 
#                  + 0.5 * b_lk * l_it * k_it + 0.5 * b_ks * k_it * s_it + w_it + u_it

# Füge Dummies für die Präsenz in einzelnen Sektoren hinzu sowie für die Siedlungsstruktur hinzu 
# (Referenzkategorie: metropolitan areas):
# value-added_it = beta_l * l_it + beta_s * s_it + beta_k * k_it  + beta_ll * l_it^2 
#                  + beta_ss * s_it^2  + beta_kk * k_it^2 + 0.5 * beta_ls * lit * s_it 
#                  + 0.5 * b_lk * l_it * k_it + 0.5 * b_ks * k_it * s_it
#                  + gamma_1* sa_it + gamma_2 * sn_it + gamma_3 * se_it + gamma_4 * wa_it 
#                  + gamma_5 * wm_it + gamma_6 * ga_it + + gamma_7 * suburban_it 
#                  + gamma_8 * rurald_it + + gamma_8 * rurals_it
#                  + w_it + u_it



# Grundmodell in Logs, medianbereinigt und mit allen Sektor-/fuel Covariates und Zeittrends
# -----------------------------------------------------------------------------------------


# Pro & Contra Zeittrend im Intercept
# -----------------------------------
# Contra: Ein Zeittrend im Intercept ( z.B. t + t^2) würde auf Zeittrend in pty hinweisen. 
# Kontrolliert man hier schon für den Zeittrend, wäre LOM etwas unbegründet, da das Produktivitäts-
# wachstum hier schon herausgefiltert wird und im LOM dann nicht mehr erklärt werden kann.

# Pro: Produktivitätswachstum könnte auch durch exogene Faktoren wie z.B. intensified competion
# entstehen. Zeittrend t würde dies herausfiltern, so dass nur noch endogenes Produktivitätswachstum
# durhc Reorganisation erklärt wird. Jedoch filtert Zeittrend auch _aggregiertes_ endogenes 
# Wachstum heraus, sobald genügend Firmen die gleiche Maßnahme ergreifen.

# Fazit: Lieber rauslassen. Vgl. auch die Diskussion am Ende in der Auswertung 'Regression' (6.2).

# Sollte ein Zeittrend im Intercept bestehen bleiben, müsste für die jährliche Auswertung der
# median productivity auf jeden Fall mit dem Intercept-Zeittrend multipliziert werden.


first_stage_OLS_m1 <- lm(va3_m ~
                        # Inputs der Produktionsfunktion:
                        l_m + f_m + k_m + I(0.5*l_m^2) +  I(0.5*f_m^2) + I(0.5*k_m^2) 
                        + l_m:f_m + l_m:k_m + f_m:k_m
                        # Sector controls
                        +  I(sn*defl_sn) + I(ga*defl_ga) + I(wa*defl_wa) + I(sa*p_sa_log) 
                        + I(wm_HH*defl_wm_HH) + I(wm_NWG*defl_wm_NWG) 
                        # Fuel types
                        + I(se_gas*defl_se_eg) + I(se_oil*defl_se_oil) + I(se_hc*defl_se_hc)  
                        + I(se_waste*log(100)) + I(se_bio*log(100)) 
                        + I(se_EE*log(100)) + I(se_water*log(100)) + I(se_sonst*log(100))
                        # Siedlungscontrols
                        + suburban + rurald + rurals
                        ,data_p)
summary(first_stage_OLS_m1)


# Grundmodell in Logs, medianbereinigt und nur mit signifikanten Sektor-/fuel covariates
# --------------------------------------------------------------------------------------
first_stage_OLS_m2 <- lm(va3_m ~
                        # Inputs der Produktionsfunktion:
                        l_m + f_m + k_m + I(0.5*l_m^2) +  I(0.5*f_m^2) + I(0.5*k_m^2) 
                         + l_m:f_m + l_m:k_m + f_m:k_m
                         # Sector controls
                         + I(ga*defl_ga) + I(wa*defl_wa) + I(sa*p_sa_log) + I(wm_NWG*defl_wm_NWG) 
                         # Fuel types
                         + I(se_gas*defl_se_eg) + I(se_oil*defl_se_oil)
                         + I(se_EE2*log(100))
                         # Siedlungscontrols
                         + suburban + rurald + rurals
                         ,data_p)
summary(first_stage_OLS_m2)


# Speichere die Koeffizienten.
betas_basic_m <- as.vector(first_stage_OLS_m2$coefficients)



###############################################################################################
##  4.2 First-stage estimation (OLS): Translog-Modell mit Wage bill                          ##
###############################################################################################

# First stage OLS estimation in ACF (2005). Eliminiert iid error u_it.

# Die proxy function for productivity lautet
# omega_it = h_t(l_it,s_it,k_it,X_it)
# Sie wird als Polynom 2. Grades modelliert. Je nachdem, ob man die sector controls und 
# Siedlungsdummies X_it in der 2nd stage mitschätzt, nimmt man an, dass die input choice nicht extra 
# vom Produktportfolio und der Nachfrage abhängt (vgl. Vandenberghe et al. 2013 und DeLoecker 
# 2011, Fox/Smeets 2011.)
# Beachte dass alle Terme ersten Grades des Polynoms mit den Termen aus der Produktionsfunktion
# 'verschmelzen' (nicht separat identifizierbar), was der Grund dafür ist, dass die Koeffizienten 
# der First Stage verzerrt sind.


# Modell medianbereinigt
# ----------------------
first_stage_m <- lm(va3_m ~
                    # Inputs der Produktionsfunktion:
                    l_m + f_m + k_m + I(0.5*l_m^2) +  I(0.5*f_m^2) + I(0.5*k_m^2) 
                    + l_m:f_m + l_m:k_m + f_m:k_m
                    # Sector controls
                    + I(ga*defl_ga) + I(wa*defl_wa) + I(sa*p_sa_log) + I(wm_NWG*defl_wm_NWG) 
                    # Fuel types
                    + I(se_gas*defl_se_eg) + I(se_oil*defl_se_oil)
                    + I(se_EE2*log(100))
                    # Siedlungscontrols
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
cor(data_p$Phi,data_p$va3_m,use="complete.obs")

# Konstruiere Lag-Phi
data_p$lag_Phi <- lag(data_p$Phi)
length(data_p$lag_Phi)
length(which(is.na(data_p$lag_Phi)==FALSE))

# Store residuals for later use.
data_p$exp_u_it <- exp(first_stage_m$residuals)


#######################################################################
##  4.3  Second-stage estimation: Vorbereiten der Lag-Variablen      ##
#######################################################################


# Fasse alle Inputs in einer Matrix zusammen. Die Reihenfolge muss exakt jener der Koeffizienten der
# OLS- Schätzung entsprechen. Sollte sich ein Input in der Produktionsfunktion ändern, muss auch die
# Input und Instrumenten-Matrix angepasst werden.
# Note: In der OLS kann man die Translog-Gewichtung (0.5) direkt über die Funktionsgleichung 
# abbilden lassen, bei der GMM wird die funktionale Form indirekt über die Input-Matrix bestimmt.
Inputs <- as.matrix(cbind(rep(1,nrow(data_p)),data_p$l_m,data_p$f_m,data_p$k_m,0.5*(data_p$l_m)^2
                          ,0.5*(data_p$f_m)^2,0.5*(data_p$k_m)^2
                          ,data_p$ga*data_p$defl_ga
                          ,data_p$wa*data_p$defl_wa,data_p$sa*data_p$p_sa_log
                          ,data_p$wm_NWG*data_p$defl_wm_NWG
                          ,data_p$se_gas*data_p$defl_se_eg,data_p$se_oil*data_p$defl_se_oil
                          ,data_p$se_EE2*log(100)
                          ,data_p$suburban,data_p$rurald,data_p$rurals
                          ,data_p$l_m*data_p$f_m,data_p$l_m*data_p$k_m,data_p$f_m*data_p$k_m))
dim(Inputs)


# Konstruiere die t-1-Variablen für die Rechtsform
data_p$lag1_unlisted <- lag(data_p$unlisted)
data_p$lag1_listed <- lag(data_p$listed)
data_p$lag1_privlaw <- lag(data_p$privlaw)

# Konstruiere die t-1-Variable für die Eigentümerschaft
data_p$lag1_eigentuemer2 <- lag(data_p$eigentuemer2)

# Konstruiere die t-1-Variable für das Outsourcing
data_p$lag1_shareF <- lag(data_p$shareF)
data_p$lag2_shareF <- lag(data_p$shareF,2)
data_p$lag1_shareFEW <- lag(data_p$shareFEW)


# Eliminiere im Gesamtdatensatz alle Beobachtungen, für die keine lag(Phi) vorliegen.
data_gmm <- subset(data_p,is.na(lag_Phi)==FALSE & is.na(lag2_shareF)==FALSE)
pdim(data_gmm)
n <- nrow(data_gmm)

# Gnereriere für die spätere Auswertung auch die vollständige Input-Matrix auf gmm-Basis.
# At time t
Inputs_gmm_all <- as.matrix(cbind(rep(1,nrow(data_gmm)),data_gmm$l_m,data_gmm$f_m,data_gmm$k_m
                                  ,0.5*(data_gmm$l_m)^2,0.5*(data_gmm$f_m)^2,0.5*(data_gmm$k_m)^2
                                  ,data_gmm$ga*data_gmm$defl_ga
                                  ,data_gmm$wa*data_gmm$defl_wa,data_gmm$sa*data_gmm$p_sa_log
                                  ,data_gmm$wm_NWG*data_gmm$defl_wm_NWG
                                  ,data_gmm$se_gas*data_gmm$defl_se_eg
                                  ,data_gmm$se_oil*data_gmm$defl_se_oil
                                  ,data_gmm$se_EE2*log(100)
                                  ,data_gmm$suburban,data_gmm$rurald,data_gmm$rurals
                                  ,data_gmm$l_m*data_gmm$f_m,data_gmm$l_m*data_gmm$k_m
                                  ,data_gmm$f_m*data_gmm$k_m))

dim(Inputs_gmm_all)

# At time t-1
lag_Inputs_all <- as.matrix(cbind(rep(1,nrow(data_p)),lag(data_p$l_m),lag(data_p$f_m)
                                  ,lag(data_p$k_m),0.5*(lag(data_p$l_m))^2
                                  ,0.5*(lag(data_p$f_m))^2,0.5*(lag(data_p$k_m))^2
                                  ,lag(data_p$ga)*lag(data_p$defl_ga)
                                  ,lag(data_p$wa)*lag(data_p$defl_wa)
                                  ,lag(data_p$sa)*lag(data_p$p_sa_log)
                                  ,lag(data_p$wm_NWG)*lag(data_p$defl_wm_NWG)
                                  ,lag(data_p$se_gas)*lag(data_p$defl_se_eg)
                                  ,lag(data_p$se_oil)*lag(data_p$defl_se_oil)
                                  ,lag(data_p$se_EE2)*log(100)
                                  ,lag(data_p$suburban),lag(data_p$rurald),lag(data_p$rurals)
                                  ,lag(data_p$l_m)*lag(data_p$f_m),lag(data_p$l_m)*lag(data_p$k_m)
                                  ,lag(data_p$f_m)*lag(data_p$k_m)
                                  ,lag(data_p$shareF,2)))
lag_Inputs_gmm_all <- na.omit(lag_Inputs_all)
lag_Inputs_gmm_all <- lag_Inputs_gmm_all[,1:20]
dim(lag_Inputs_gmm_all)


# Zur Berechnung von omega in der GMM-Funktion:
# --------------------------------------------
# Inputs, über deren Koeffizienten optimiert werden soll:
Inputs_gmm <- Inputs_gmm_all
dim(Inputs_gmm)

# Fixed Inputs, für die in der 2nd stage keine neuen Koeffizienten berechnet werden:
Inputs_fixed <- as.matrix(rep(0,nrow(data_gmm)))
dim(Inputs_fixed)



# Zur Berechnung von lag_omega in der GMM-Funktion:
# -------------------------------------------------
# Lagged Inputs, über deren Koeffizienten optimiert werden soll:
lag_Inputs_gmm <- lag_Inputs_gmm_all
dim(lag_Inputs_gmm)

# Lagged fixed Inputs, für die in der 2nd stage keine neuen Koeffizienten berechnet werden:
lag_Inputs_fixed <- as.matrix(rep(0,nrow(data_gmm)))
lag_Inputs_fixed <- na.omit(lag_Inputs_fixed)
dim(lag_Inputs_fixed)


# Zur Wahl der Instrumente Z in der GMM-Funktion
# ----------------------------------------------
instr <- cbind(rep(1,nrow(data_p)),data_p$l_m,lag(data_p$f_m),data_p$k_m
               ,data_p$l_m^2,lag(data_p$f_m)^2,data_p$k_m^2
               ,data_p$ga*data_p$defl_ga
               ,data_p$wa*data_p$defl_wa,lag(data_p$sa)*lag(data_p$p_sa_log)
               ,data_p$wm_NWG*data_p$defl_wm_NWG
               ,data_p$se_gas*data_p$defl_se_eg,data_p$se_oil*data_p$defl_se_oil
               ,data_p$se_EE2*log(100)
               ,data_p$suburban,data_p$rurald,data_p$rurals
               ,data_p$l_m*lag(data_p$f_m),data_p$l_m*data_p$k_m,lag(data_p$f_m)*data_p$k_m
               ,lag(data_p$shareF,2))
instr_gmm <- na.omit(instr)
instr_gmm <- instr_gmm[,1:20]
dim(instr_gmm)




#########################################################
##  4.4  Second-stage estimation: GMM Optimierung      ##
#########################################################

# Die Zielfunktion der GMM Optimization sind die Moment Conditions E[(Z'v)'*(Z'v)]=0. Siehe 
# Erläuterungen im Kasten über der gmm_moment_condition-Funktion im Abschnitt (0.7).

# Achtung, im Spieldatensatz gibt es nach Weglassen der Braunkohlekraftwerke nicht mehr genügend
# private AGs. Daher im Spieldatensatz die 'listed*eigentumer2'-Variable in der GMM-Funktion
# rauskommentieren.


# Modell 3: alle Koeffizienten in der 2nd stage
# ----------------------------------------------

# Anpassen der Startwerte
initial_betas <- betas_basic_m[c(1:20)]
betas_fixed <- 0


# Ausführen der Optimierung
optimization <- optimx(par=initial_betas,fn=gmm_moment_condition, method=c("nlminb","nlm"))
print(optimization)

# Die Optimierung hat funktioniert, wenn optimx$convcode=0 ist.
# Zum Vergleich hier noch einmal die Startwerte (OLS ohne GMM-Algorithmus).
betas_basic_m[c(1:20)]

# Wähle die Koeffizienten aus, deren Zielfunktionswert ('value') am kleinsten ist (beste 
# Minimierung).
j <- which.min(optimization$value)

# Koeffizienten der Second-Stage (betas):
betas2 <- rbind(optimization$p1[j],optimization$p2[j],optimization$p3[j],optimization$p4[j]
                 ,optimization$p5[j],optimization$p6[j],optimization$p7[j],optimization$p8[j]
                 ,optimization$p9[j],optimization$p10[j],optimization$p11[j],optimization$p12[j]
                 ,optimization$p13[j],optimization$p14[j],optimization$p15[j],optimization$p16[j]
                 ,optimization$p17[j],optimization$p18[j],optimization$p19[j],optimization$p20[j])


betas2


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
# Nimm die Methode mit dem besten Minimierungsergebnis (siehe Auswahl der betas2 über 'j').
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
betas_final <- betas2
data_p$omega2 <- data_p$Phi - Inputs%*%betas_final
data_p$omega2e <- exp(data_p$Phi - Inputs%*%betas_final)
data_gmm$omega2 <- data_gmm$Phi - Inputs_gmm_all%*%betas_final


########################################################
## 	5.2 Productivity Dispersion                       ##
########################################################


dstat(data_p$omega2e,d=3)
dstat(exp(data_gmm$omega2),d=3)

##################################################################################
## 	5.3 Output Elasticities                                                     ##
##################################################################################


##################################
##  5.3.1 data_p                ##
##################################

# Output elasticity for bruttolohn
# ---------------------------------
# elasticity_l = b_l + b_ll*l + b_lf*f + b_kl*k
data_p$elasticity_lohn <- (betas_final[2] + betas_final[5]*Inputs[,2] 
                           + betas_final[18]*Inputs[,3] + betas_final[19]*Inputs[,4])


# Output elasticity for fremde DL
# -------------------------------
# elasticity_v = b_f + b_ff*f + b_lf*l + b_kv*k
data_p$elasticity_fdl <- (betas_final[3] + betas_final[6]*Inputs[,3] 
                          + betas_final[18]*Inputs[,2] + betas_final[20]*Inputs[,4])



# Output elasticity for capital
# -----------------------------
# elasticity_k = b_k + b_kk*k + b_lk*l + b_kf*f
data_p$elasticity_cap <- (betas_final[4] + betas_final[7]*Inputs[,4] 
                          + betas_final[19]*Inputs[,2] + betas_final[20]*Inputs[,3])

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




##################################
##  5.3.2 data_gmm              ##
##################################


# Output elasticity for bruttolohn
# ---------------------------------
# elasticity_l = b_l + b_ll*l + b_lf*f + b_kl*k + b_lt*t + b_llt*l*t + b_lft*f*t + + b_klt*k*t
data_gmm$elasticity_lohn <- (betas_final[2] + betas_final[5]*Inputs_gmm_all[,2] 
                           + betas_final[18]*Inputs_gmm_all[,3] 
                           + betas_final[19]*Inputs_gmm_all[,4])


# Output elasticity for fremde DL
# -------------------------------
# elasticity_v = b_f + b_ff*f + b_lf*l + b_kv*k + b_ft*t + b_fft*f*t + b_lft*l*t + + b_kft*k*t
data_gmm$elasticity_fdl <- (betas_final[3] + betas_final[6]*Inputs_gmm_all[,3] 
                          + betas_final[18]*Inputs_gmm_all[,2] 
                          + betas_final[20]*Inputs_gmm_all[,4])


# Output elasticity for capital
# -----------------------------
# elasticity_k = b_k + b_kk*k + b_lk*l + b_kf*f + b_kt*t + b_kkt*k*t + b_lkt*l*t + + b_kft*f*t
data_gmm$elasticity_cap <- (betas_final[4] + betas_final[7]*Inputs_gmm_all[,4] 
                          + betas_final[19]*Inputs_gmm_all[,2] 
                          + betas_final[20]*Inputs_gmm_all[,3])


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


##########################################
##      6.1 Re-run AR(1) process        ##
##########################################

# Hier können nur die Firmen untersucht werden, für die das lag t-1 beobachtet wird.
# Ordne jeder Firma ihren Produktitivätswert sowie den des Vorjahres zu.


data_gmm$lag_omega2 <- data_gmm$lag_Phi - lag_Inputs_gmm_all%*%betas_final
data_gmm <- pdata.frame(data.frame(data_gmm),index=c("id","Jahr"),row.names=FALSE)


# Pooling (OLS)
# --------------
AR1_expost <- plm(omega2 ~ lag_omega2 + I(lag_omega2^2) + I(lag_omega2^3) 
                  + lag1_unlisted 
                  + lag1_listed 
                  + I(lag1_unlisted*lag1_eigentuemer2) 
                  + I(lag1_listed*lag1_eigentuemer2) 
                  + lag2_shareF + shareFEW
                  #+ I(shareF*shareFEW)
                  #+ I(shareF*lag1_privlaw*lag1_eigentuemer2)
                  #+ I(shareFEW*lag1_privlaw*lag1_eigentuemer2)
                  #+ I(shareF*lag1_privlaw)
                  #+ I(shareFEW*lag1_privlaw)
                  ,data=data_gmm,model="pooling",effect="time")
summary(AR1_expost)



# Check for autocorrelation and heteroscedasticity
# ------------------------------------------------

# Is autocorrelation a concern?
dwtest(omega2 ~ lag_omega2 + I(lag_omega2^2) + I(lag_omega2^3) 
       + lag1_unlisted 
       + lag1_listed 
       + I(lag1_unlisted*lag1_eigentuemer2) 
       + I(lag1_listed*lag1_eigentuemer2)
       + lag2_shareF 
       + shareFEW
       #+ I(shareF*shareFEW)
       #+ I(shareF*lag1_privlaw*lag1_eigentuemer2)
       #+ I(shareFEW*lag1_privlaw*lag1_eigentuemer2)
       #+ I(shareF*lag1_privlaw)
       #+ I(shareFEW*lag1_privlaw)
       ,data=data_gmm)

# Is heteroscedasticity a concern?
bptest(AR1_expost)

# WHITE standard errors robust against heteroscedasticity and autocorrelation?
coeftest(AR1_expost,vcov=vcovHC(AR1_expost))



############################
##  6.2 Hypothesentests   ##
############################

# Private minority
# ----------------
# Testet, ob die private Beteiligung grundsätzlich einen signifikanten Einfluss auf die 
# Produktivität hat.
linearHypothesis(AR1_expost
                 ,"I(lag1_unlisted * lag1_eigentuemer2)-lag1_unlisted + I(lag1_listed * lag1_eigentuemer2) - lag1_listed=0"
                 ,vcov=vcovHC(AR1_expost,method="arellano"))

# Testet, ob die private Beteiligung bei GmbHs einen signifikanten Einfluss auf die 
# Produktivität hat.
linearHypothesis(AR1_expost,"I(lag1_unlisted * lag1_eigentuemer2)-lag1_unlisted=0"
                 ,vcov=vcovHC(AR1_expost,method="arellano"))

# Testet, ob die private Beteiligung bei AGs einen signifikanten Einfluss auf die 
# Produktivität hat.
linearHypothesis(AR1_expost,"I(lag1_listed * lag1_eigentuemer2) - lag1_listed=0"
                 ,vcov=vcovHC(AR1_expost,method="arellano"))




#####################################################################
##    6.2 Regress Productivity on Reorganisation Variables         ##
#####################################################################


# Regress productivity on reorganisation variables to see whether there is a baseline effect from
# reorganisation.

# Bereite Paneldatensatz so auf, dass keine NAs in den Index-Variablen sind.
data_p <- pdata.frame(as.data.frame(data_p),index=c("id","Jahr"),row.names=FALSE)


# Kontrolliere zusätzlich für die Firmengröße (z.B. über revenues), da Eigenbetriebe tendenziell
# kleiner sind als AGs, für die Präsenz in den einzelnen Sektoren und für die Siedlungsstruktur.



###########################################
##  6.2.1 Regression mit Zeiteffekten    ##
###########################################


# Kontrolliere für einen Zeittrend im Intercept, d.h. einen jährlichen aggregierten Produktivitäts-
# zuwachs. Die Controls geben dann Auskunft darüber, ob sich Unternehmen bereinigt um Jahreseffekte
# unterscheiden und sich dies auf die Organisationsstruktur zurückführen lässt.

# Das ist die konservative Schätzung, die hypothetisch z.B. für zwei Unternehmen im gleichen Jahr
# schaut, ob die Rechtsformwahl die Produktivität beeinflusst. So kann man sicher sein, dass der
# Effekt allein auf den Unterschied in der Organisation zurückzuführen ist und nicht darauf, dass
# privatwirtschaftliche Unternehmen vermehrt ab 2008 beobachtet werden und das allgemeine 
# Produktivitätslevel ab 2008 höher liegt. Dies könnte auch das Resultat gestiegenen Wettbewerbs-
# drucks sein.

explain_pty <- plm(omega2 ~ 
                   lag1_unlisted 
                   + lag1_listed 
                   + I(lag1_unlisted*lag1_eigentuemer2) 
                   + I(lag1_listed*lag1_eigentuemer2)
                   + lag2_shareF + shareFEW 
                   #+ I(shareF*shareFEW)
                   #+ I(shareF*lag1_privlaw*lag1_eigentuemer2)
                   #+ I(shareFEW*lag1_privlaw*lag1_eigentuemer2)
                   #+ I(shareF*lag1_privlaw)
                   #+ I(shareFEW*lag1_privlaw)
                   # size
                   + size_med + size_large
                   # Sector controls
                   +  I(sn*defl_sn) + I(ga*defl_ga) + I(wa*defl_wa) + I(sa*p_sa_log) 
                   + I(wm_HH*defl_wm_HH) + I(wm_NWG*defl_wm_NWG) 
                   # Fuel types
                   + I(se_gas*defl_se_eg) + I(se_oil*defl_se_oil) + I(se_hc*defl_se_hc) 
                   + I(se_waste*log(100)) + I(se_bio*log(100)) 
                   + I(se_EE*log(100)) + I(se_water*log(100)) + I(se_sonst*log(100))
                   # Siedlungscontrols
                   + suburban + rurald + rurals
                   # time effects 
                   + t2005 + t2006 + t2007 +t2008 +t2009 + t2010 + t2011 + t2012 + t2013 + t2014
                   ,data=data_p
                   ,model="pooling",effect="time")
summary(explain_pty)

# Is heteroscedasticity a concern?
bptest(explain_pty)

# Is autocorrelation a concern?
dwtest(omega2 ~
       lag1_unlisted 
       + lag1_listed 
       + I(lag1_unlisted*lag1_eigentuemer2) 
       + I(lag1_listed*lag1_eigentuemer2) 
       + lag2_shareF + shareFEW 
       #+ I(shareF*shareFEW)
       #+ I(shareF*lag1_privlaw*lag1_eigentuemer2)
       #+ I(shareFEW*lag1_privlaw*lag1_eigentuemer2)
       #+ I(shareF*lag1_privlaw)
       #+ I(shareFEW*lag1_privlaw)
       # size
       + size_med + size_large
       # Sector controls
       +  I(sn*defl_sn) + I(ga*defl_ga) + I(wa*defl_wa) + I(sa*p_sa_log) 
       + I(wm_HH*defl_wm_HH) + I(wm_NWG*defl_wm_NWG) 
       # Fuel types
       + I(se_gas*defl_se_eg) + I(se_oil*defl_se_oil) + I(se_hc*defl_se_hc) 
       + I(se_waste*log(100)) + I(se_bio*log(100)) 
       + I(se_EE*log(100)) + I(se_water*log(100)) + I(se_sonst*log(100))
       # Siedlungscontrols
       + suburban + rurald + rurals
       # time effects
       + t2005 + t2006 + t2007 +t2008 +t2009 + t2010 + t2011 + t2012 + t2013 + t2014
       ,data=data_p)


# WHITE standard errors robust against heteroscedasticity and autocorrelation?
coeftest(explain_pty,vcov=vcovHC(explain_pty,method="arellano"))




##############################
## 6.2.2 Hypothesentests    ##
##############################


# Privat Beteiligung
# ------------------
# Testet, ob die private Beteiligung grundsätzlich einen signifikanten Einfluss auf die 
# Produktivität hat.
linearHypothesis(explain_pty
                 ,"I(lag1_unlisted * lag1_eigentuemer2)-lag1_unlisted + I(lag1_listed * lag1_eigentuemer2) - lag1_listed=0"
                 ,vcov=vcovHC(explain_pty,method="arellano"))

# Testet, ob die private Beteiligung bei GmbHs einen signifikanten Einfluss auf die 
# Produktivität hat.
linearHypothesis(explain_pty,"I(lag1_unlisted * lag1_eigentuemer2)-lag1_unlisted=0"
                 ,vcov=vcovHC(explain_pty,method="arellano"))

# Testet, ob die private Beteiligung bei AGs einen signifikanten Einfluss auf die 
# Produktivität hat.
linearHypothesis(explain_pty,"I(lag1_listed * lag1_eigentuemer2) - lag1_listed=0"
                 ,vcov=vcovHC(explain_pty,method="arellano"))



############################################
##  6.2.3 Regression ohne Zeiteffekte     ##
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
                   lag1_unlisted 
                   + lag1_listed 
                   + I(lag1_unlisted*lag1_eigentuemer2) 
                   + I(lag1_listed*lag1_eigentuemer2)
                   + lag2_shareF + shareFEW 
                   #+ I(shareF*shareFEW)
                   #+ I(shareF*lag1_privlaw*lag1_eigentuemer2)
                   #+ I(shareFEW*lag1_privlaw*lag1_eigentuemer2)
                   #+ I(shareF*lag1_privlaw)
                   #+ I(shareFEW*lag1_privlaw)
                   # size
                   + size_med + size_large
                   # Sector controls
                   +  I(sn*defl_sn) + I(ga*defl_ga) + I(wa*defl_wa) + I(sa*p_sa_log) 
                   + I(wm_HH*defl_wm_HH) + I(wm_NWG*defl_wm_NWG) 
                   # Fuel types
                   + I(se_gas*defl_se_eg) + I(se_oil*defl_se_oil) + I(se_hc*defl_se_hc) 
                   + I(se_waste*log(100)) + I(se_bio*log(100)) 
                   + I(se_EE*log(100)) + I(se_water*log(100)) + I(se_sonst*log(100))
                   # Siedlungscontrols
                   + suburban + rurald + rurals,data=data_p,model="pooling",effect="time")
summary(explain_pty2)

# Is heteroscedasticity a concern?
bptest(explain_pty2)

# Is autocorrelation a concern?
dwtest(omega2 ~ 
       lag1_unlisted 
       + lag1_listed 
       + I(lag1_unlisted*lag1_eigentuemer2) 
       + I(lag1_listed*lag1_eigentuemer2) 
       + lag2_shareF + shareFEW 
       #+ I(shareF*shareFEW)
       #+I(shareF*lag1_privlaw*lag1_eigentuemer2)
       #+ I(shareFEW*lag1_privlaw*lag1_eigentuemer2)
       #+ I(shareF*lag1_privlaw)
       #+ I(shareFEW*lag1_privlaw)
       # size
       + size_med + size_large
       # Sector controls
       +  I(sn*defl_sn) + I(ga*defl_ga) + I(wa*defl_wa) + I(sa*p_sa_log) 
       + I(wm_HH*defl_wm_HH) + I(wm_NWG*defl_wm_NWG) 
       # Fuel types
       + I(se_gas*defl_se_eg) + I(se_oil*defl_se_oil) + I(se_hc*defl_se_hc) 
       + I(se_waste*log(100)) + I(se_bio*log(100)) 
       + I(se_EE*log(100)) + I(se_water*log(100)) + I(se_sonst*log(100))
       # Siedlungscontrols
       + suburban + rurald + rurals,data=data_p)


# WHITE standard errors robust against heteroscedasticity and autocorrelation?
coeftest(explain_pty2,vcov=vcovHC(explain_pty2,method="arellano"))


###############################
##  6.2.4 Hpyothesentests    ##
###############################

# Private Beteiligung
# Testet, ob die private Beteiligung grundsätzlich einen signifikanten Einfluss auf die 
# Produktivität hat.
linearHypothesis(explain_pty2
                 ,"I(lag1_unlisted * lag1_eigentuemer2)-lag1_unlisted + I(lag1_listed * lag1_eigentuemer2) - lag1_listed=0"
                 ,vcov=vcovHC(explain_pty2,method="arellano"))

# Testet, ob die private Beteiligung bei GmbHs einen signifikanten Einfluss auf die 
# Produktivität hat.
linearHypothesis(explain_pty2,"I(lag1_unlisted * lag1_eigentuemer2)-lag1_unlisted=0"
                 ,vcov=vcovHC(explain_pty2,method="arellano"))

# Testet, ob die private Beteiligung bei AGs einen signifikanten Einfluss auf die 
# Produktivität hat.
linearHypothesis(explain_pty2,"I(lag1_listed * lag1_eigentuemer2) - lag1_listed=0"
                 ,vcov=vcovHC(explain_pty2,method="arellano"))


###################################################################################################
date()
############################## End of file ########################################################
