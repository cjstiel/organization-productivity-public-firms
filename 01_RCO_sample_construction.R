#================================================================================================
#
# author: Caroline Stiel, DIW Berlin (German Institute for Economic Research), 2019
#
#================================================================================================
#
# title: Remunicipalization, Corporatization, and Outsourcing: Public Firm Performance after  
#        Reorganization
# ------------------------------------------------------------------------------------------------
#
#
# structure
# ---------
# PART 1: sample construction (01_RCO_sample_construction.R) | this file
# PART 2: data descriptives (02_RCO_descriptives.R)
# PART 3: basic estimation
#      3a: estimation for multi-utilities (03a_RCO_main_sample.R)
#      3b: estimation for water utilities (03b_RCO_water.R)
#      3c: estimation for electricity and gas utilities (03c_RCO_electricity_gas.R)
#      3d: estimation for heat and power plants (03d_RCO_heat_power_plants.R)
# PART 4: estimation with interactions in LOM
#      4a: interactions for multi-utilities (04a_RCO_interactions_main.R)
#      4b: interactions for water (04b_RCO_interactions_water.R)
#      4c: interactions for elec&gas (04c_RCO__interactions_elecgas.R)
#      4d: interactions for heat&power (04d_RCO_interactions_heatpower.R)
# PART 5: sensitivity analyses
#      5a: excludes gas utilities (05a_RCO_SensAn_wo_gas.R)
#      5b: lag outsourcing (05b_RCO_SensAn_lag1_out.R)
#      5c: lag2 outsourcing (05c_RCO_SensAn_lag2_out.R)
#      5d: time-varying pdt technology for multi-utilities (05d_RCO_SensAn_time_main.R)
#      5e: time-varying pdt technology for elec&gas (05d_RCO_SensAn_time_elec_gas.R)
#
#
# -----------------------------------------------------------------------------------------------
#
#				PART 1: SAMPLE CONSTRUCTION
#
# -----------------------------------------------------------------------------------------------
# content: This file constructs the estimation sample from the merged survey data.
# 	To learn more about how the surveys are merged, visit <gitlab-url>.
# version: 20
#================================================================================================


#================================================================================================
#				0) Preparation                                              
#================================================================================================


date()


#================================================================================================
#  0.1 Packages	     	      
#================================================================================================


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
library(readstata13)


#================================================================================================
# 0.2 Define functions to be used in the analysis              	        
#================================================================================================


#================================================================================================
# Function 'dstat'                                                                                   
# ---------------                                                                                 
# Calculates descriptive statistics: Provides q1, q5 q25 q50, q75, q95, q99 quantiles, 
# ratio q75/q25, ratio 95/q5, mean, variance, standard deviation, sum, number of zeroes, 
# number of '.', NA's, max, second-hightes value.
#                                                                                                 
# Inputs: X - data frame with the variables for which descriptives statistics should be calculated
# Inputs: d - number of digits to which R shall round (default=0)                      
#================================================================================================

dstat <- function(X,d){
  X <- as.matrix(X)
  mat <- matrix(NA, ncol=19, nrow=ncol(X))
  colnames(mat) <- c("q1","q5","q25","med","mean","q75","q95","q99","disp50", "disp90","var", "SD"
                     , "sum","non-NAs", "zeroes", "number '.'","NAs","max","second-highest")
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



#================================================================================================
# 0.3 Load data
#================================================================================================

# Load data set
# -------------
data <- read.dta13(file.path(Path2,"/<original data set name>.dta"))  

class(data)
dim(data)



#================================================================================================
# 1) Convert and recode data
#================================================================================================

#================================================================================================
# 1.1 Energy statistics (KSE etc.)
#================================================================================================

# Convert some variables to numeric format
# ----------------------------------------
if(class(data$wz_b) != "numeric") data$wz_b <- as.numeric(data$wz_b)
if(class(data$wz_u) != "numeric") data$wz_u <- as.numeric(data$wz_u)  
if(class(data$bnr) != "numeric") data$bnr <- as.numeric(data$bnr)
if(class(data$unr) != "numeric") data$unr <- as.numeric(data$unr)

data$ags_u_new <- ifelse(data$ags_u=="." | data$ags_u=="",NA,as.numeric(data$ags_u))
if(class(data$ags_u_new) != "numeric") data$ags_u_new <- as.numeric(data$ags_u_new)


# Extract county info (Kreisebene) from location identifier (Allgemeiner Gemeindeschluessel: AGS)
# -----------------------------------------------------------------------------------------------
summary(as.factor(nchar(data$ags_u_new)))

# AGs with 8 digits: first 5 digits
# AGS with 7 digits: first 4 digits
# AGS with 5 digits: all 5 digits
# AGS with 4 digits: all 4 digits
# AGS with 2 digits: fill in with '000'
# AGS with 1 digit: fill in with '0000'
.
data$Kreis <- ifelse(nchar(data$ags_u_new)==8,substr(data$ags_u_new,1,5)
                            ,ifelse(nchar(data$ags_u_new)==7,substr(data$ags_u_new,1,4)
                                   ,ifelse(nchar(data$ags_u_new)==2 | nchar(data$ags_u_new)==1
                                          ,paste(data$ags_u_new,"000",sep="")
                                          ,data$ags_u_new)))


# Drop observations with missing values in the index variables 'unr' and 'Jahr'
# ----------------------------------------------------------------------------
dim(data)
data$unr[data$unr=="."] <- NA
data$unr[data$unr==Inf] <- NA
data$unr[data$unr==-Inf] <- NA
data <- data[!is.na(data$unr),]
dim(data)

data$Jahr[data$Jahr=="."] <- NA
data$Jahr[data$Jahr==Inf] <- NA
data$Jahr[data$Jahr==-Inf] <- NA
data <- data[!is.na(data$Jahr),]
dim(data)

length(which(is.na(data$bnr)==TRUE))
length(which(is.na(data$bnr)==TRUE))


# Recode survey participation variables to binary (0/1) variable
# --------------------------------------------------------------
if(class(data$TM064_b) == "factor") data$TM064_b <- as.character(data$TM064_b)
data$TM064_b[data$TM064_b=="ja"] <- 1
data$TM064_b[data$TM064_b=="nein" | is.na(data$TM064_b)==TRUE | data$TM064_b==""] <- 0
if(class(data$TM064_b) == "character") data$TM064_b <- as.numeric(data$TM064_b)
if(class(data$TM065_b) == "factor") data$TM065_b <- as.character(data$TM065_b)
data$TM065_b[data$TM065_b=="ja"] <- 1
data$TM065_b[data$TM065_b=="nein" | is.na(data$TM065_b)==TRUE | data$TM065_b==""] <- 0
if(class(data$TM065_b) == "character") data$TM065_b <- as.numeric(data$TM065_b)
if(class(data$TM066K_b) == "factor") data$TM066K_b <- as.character(data$TM066K_b)
data$TM066K_b[data$TM066K_b=="ja"] <- 1
data$TM066K_b[data$TM066K_b=="nein" | is.na(data$TM066K_b)==TRUE | data$TM066K_b==""] <- 0
if(class(data$TM066K_b) == "character") data$TM066K_b <- as.numeric(data$TM066K_b)
if(class(data$TM070_u) == "factor") data$TM070_u <- as.character(data$TM070_u)
data$TM070_u[data$TM070_u=="ja"] <- 1
data$TM070_u[data$TM070_u=="nein" | is.na(data$TM070_u)==TRUE | data$TM070_u==""] <- 0
if(class(data$TM070_u) == "character") data$TM070_u <- as.numeric(data$TM070_u)
if(class(data$TM083_u) == "factor") data$TM083_u <- as.character(data$TM083_u)
data$TM083_u[data$TM083_u=="ja"] <- 1
data$TM083_u[data$TM083_u=="nein" | is.na(data$TM083_u)==TRUE | data$TM083_u==""] <- 0
if(class(data$TM083_u) == "character") data$TM083_u <- as.numeric(data$TM083_u)
if(class(data$TM066N_u) == "factor") data$TM066N_u <- as.character(data$TM066N_u)
data$TM066N_u[data$TM066N_u=="ja"] <- 1
data$TM066N_u[data$TM066N_u=="nein" | is.na(data$TM066N_u)==TRUE | data$TM066N_u==""] <- 0
if(class(data$TM066N_u) == "character") data$TM066N_u <- as.numeric(data$TM066N_u)


# Recode sector activities: NA -> 0
# ---------------------------------
data$UI_Code11_1[is.na(data$UI_Code11_1)==TRUE] <- 0
data$UI_Code11_2[is.na(data$UI_Code11_2)==TRUE] <- 0
data$UI_Code11_3[is.na(data$UI_Code11_3)==TRUE] <- 0
data$UI_Code11_4[is.na(data$UI_Code11_4)==TRUE] <- 0
data$UI_Code11_5[is.na(data$UI_Code11_5)==TRUE] <- 0
data$UI_Code11_6[is.na(data$UI_Code11_6)==TRUE] <- 0
data$UI_Code11_7[is.na(data$UI_Code11_7)==TRUE] <- 0
data$BI_Code11_1[is.na(data$BI_Code11_1)==TRUE] <- 0
data$BI_Code11_2[is.na(data$BI_Code11_2)==TRUE] <- 0
data$BI_Code11_3[is.na(data$BI_Code11_3)==TRUE] <- 0
data$BI_Code11_4[is.na(data$BI_Code11_4)==TRUE] <- 0
data$BI_Code11_5[is.na(data$BI_Code11_5)==TRUE] <- 0
data$BI_Code11_6[is.na(data$BI_Code11_6)==TRUE] <- 0
data$BI_Code11_7[is.na(data$BI_Code11_7)==TRUE] <- 0


# Recode federal states to numbers
# --------------------------------
# firm level
levels(as.factor(data$bl_u))
data$bl <- as.numeric(factor(data$bl_u))
levels(as.factor(data$bl))

# plant level
levels(as.factor(data$bl_b))
data$bl_b_new <- as.numeric(factor(data$bl_b))
levels(as.factor(data$bl_b_new))


# Replace missing information for federal states at firm level with info from plant level
# ----------------------------------------------------------------------------------------
length(which(is.na(data$bl)==TRUE)==TRUE)
data$bl[is.na(data$bl)==TRUE] <- data$bl_b_new[is.na(data$bl)==TRUE]
length(which(is.na(data$bl)==TRUE)==TRUE)
levels(as.factor(data$bl))


# Replace missing information for NACE code at firm level with info from plant level
# ----------------------------------------------------------------------------------
length(which(is.na(data$wz_u)==TRUE)==TRUE)
data$wz_u[is.na(data$wz_u)==TRUE] <- data$wz_b[is.na(data$wz_u)==TRUE]
length(which(is.na(data$wz_u)==TRUE)==TRUE)


# Recode legal form information from KSE survey as number
# -------------------------------------------------------
levels(as.factor(data$Rechtsform))
data$Rechtsform_Zahl[data$Rechtsform=="keine Angaben"] <- 0
data$Rechtsform_Zahl[data$Rechtsform=="Einzelfirma"] <- 1
data$Rechtsform_Zahl[data$Rechtsform=="ohg"] <- 2
data$Rechtsform_Zahl[data$Rechtsform=="kg"] <- 3
data$Rechtsform_Zahl[data$Rechtsform=="GmbH & Co KG"] <- 4
data$Rechtsform_Zahl[data$Rechtsform=="GmbH"] <- 5
data$Rechtsform_Zahl[data$Rechtsform=="AG bzw. KGaA"] <- 6
data$Rechtsform_Zahl[data$Rechtsform=="Genossenschaft"] <- 7
data$Rechtsform_Zahl[data$Rechtsform=="Eigenbetrieb"] <- 8
data$Rechtsform_Zahl[data$Rechtsform=="Verband"] <- 9
data$Rechtsform_Zahl[data$Rechtsform=="sonstiges"] <- 10
levels(as.factor(data$Rechtsform_Zahl))


# Generate variable with fuel type information (power/heat plants)
# ----------------------------------------------------------------
# tabulate original variable from the survey
levels(as.factor(data$Hauptenergietraeger))

# hard coal: 1
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & data$Hauptenergietraeger>= 1 & data$Hauptenergietraeger<= 5] <- 1 
# lignite: 2
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & data$Hauptenergietraeger>= 11 & data$Hauptenergietraeger<= 17] <- 2 
# oil: 3
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & data$Hauptenergietraeger>= 21 & data$Hauptenergietraeger<= 27] <- 3 
# natural gas: 4
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & (data$Hauptenergietraeger>= 31 & data$Hauptenergietraeger<= 35)] <- 4
# water: 5
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & data$Hauptenergietraeger>= 41 & data$Hauptenergietraeger<= 44] <- 5                
# wind: 6
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & data$Hauptenergietraeger== 45] <- 6 
# solar: 7
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & data$Hauptenergietraeger== 46] <- 7 
# geothermal energy: 8
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & data$Hauptenergietraeger== 47] <- 8 
# biomass: 9
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & data$Hauptenergietraeger>= 51 & data$Hauptenergietraeger<= 52] <- 9    
# biogas: 10
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & data$Hauptenergietraeger>= 53 & data$Hauptenergietraeger<= 56] <- 10                
# other RES: 11
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & data$Hauptenergietraeger== 57] <- 11

# waste: 12
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & data$Hauptenergietraeger>= 61 & data$Hauptenergietraeger<= 63] <- 12 

# nuclear power: 13
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & data$Hauptenergietraeger== 71] <- 13

# miscellaneous: 14
data$HETGruppen[is.na(data$Hauptenergietraeger)==FALSE 
                & (data$Hauptenergietraeger>= 72 & data$Hauptenergietraeger<= 81)] <- 14

levels(as.factor(data$HETGruppen))

summary(as.factor(data$Hauptenergietraeger))



#================================================================================================
# 1.2 JAB survey (Jahresabschluesse oeffentlicher Fonds, Einrichtungen und Unternehmen)
#================================================================================================


# Check the format of the identifier 'jab'
# ----------------------------------------
# Note: The variable 'jab' identifies all entities that took part in the JAB survey.
dstat(data$jab)
length(which(data$jab==Inf))
length(which(data$jab==-Inf))
data$jab[data$jab=="."] <- 0
addmargins(table(data$jab,data$Jahr,useNA="ifany"))

class(data$jab)
if(class(data$jab) != "numeric") data$jab <- as.numeric(data$jab)
class(data$jab)


# Convert location identifier (AGS) to numeric format
# ---------------------------------------------------
data$EF7 <- ifelse(data$EF7=="." | data$EF7=="",NA,data$EF7)
data$EF7_n <- as.numeric(data$EF7)

summary(data$EF7)
dstat(data$EF7_n)


# Recode ownership type
# ---------------------
# Note: Needs to be recoded for the years 2012-2014 to obtain consistent results through the whole
# sample period since as the survey setup slightly changed after 2010.
class(data$EF10)
data$EF10_neu <- ifelse((data$Jahr==2012 | data$Jahr==2013 | data$Jahr==2014) & data$EF10==1,1
                        ,ifelse((data$Jahr==2012 | data$Jahr==2013 | data$Jahr==2014) & data$EF10==2,6
                                ,ifelse(data$EF10==11 | data$EF10==13,2
                                        ,ifelse(data$EF10==12 | data$EF10==14,7
                                                ,ifelse(data$EF10==21 | data$EF10==23 | data$EF10==31 
                                                        | data$EF10==33 | data$EF10==41 | data$EF10==43
                                                        | data$EF10==61 | data$EF10==63 | data$EF10==71
                                                        | data$EF10==73 | data$EF10==81 | data$EF10==83 
                                                        | data$EF10==93 | data$EF10==51 | data$EF10==53,3
                                                        ,ifelse(data$EF10==22 | data$EF10==24 | data$EF10==32
                                                                | data$EF10==34 | data$EF10==42 | data$EF10==44
                                                                | data$EF10==62 | data$EF10==64 | data$EF10==72
                                                                | data$EF10==74 | data$EF10==82 | data$EF10==84
                                                                | data$EF10==94 | data$EF10==52 | data$EF10==54,8
                                                                ,ifelse(data$EF10==147 | data$EF10==103,4
                                                                        ,ifelse(data$EF10==148 | data$EF10==104,9
                                                                                ,ifelse(data$EF10==125 | data$EF10==135,5
                                                                                        ,ifelse(data$EF10==126 | data$EF10==136,0,data$EF10))))))))))
levels(as.factor(data$EF10_neu))
class(data$EF10_neu)


# Generate variable with ownership information (fully public vs. mixed ownership)
# --------------------------------------------------------------------------------
# Note: Based on variable EF10 from the JAB survey.

# fully public based on variable EF10, categories 1-5
data$eigentuemer[is.na(data$EF10_neu)==FALSE & data$EF10_neu>0 & data$EF10_neu<=5] <- 2

# fully public: A single owner who is not in mixed ownership
data$eigentuemer[is.na(data$EF10_neu)==TRUE & data$EF20==1 & data$EF21U1_1!=9] <- 2

# fully public: Entity has the legal form 'Eigenbetrieb'
data$eigentuemer[is.na(data$EF10_neu)==TRUE & data$Rechtsform=="Eigenbetrieb"] <- 2

# mixed ownership (public majority) based on variable EF10, categories 0, 6-9
data$eigentuemer[is.na(data$EF10_neu)==FALSE & (data$EF10_neu==0 | data$EF10_neu>5)] <- 1

addmargins(table(data$eigentuemer,data$Jahr,useNA="ifany"))
class(data$eigentuemer)
if(class(data$eigentuemer) != "numeric") data$eigentuemer <- as.numeric(data$eigentuemer)
addmargins(table(data$eigentuemer,data$Jahr,useNA="ifany"))



#================================================================================================
# 1.3 URS (Unternehmensregister)
#================================================================================================


# Convert public firm identifier to numeric format
# ------------------------------------------------
table(data$urs_public,data$Jahr,useNA="ifany")
if(class(data$urs_public) != "numeric") data$urs_public <- as.numeric(data$urs_public)
table(data$urs_public,data$Jahr,useNA="ifany")



#================================================================================================
# 2) Choose sample                                     
#================================================================================================


# Identify plants whose parent firms have their main activities (NACE code) in sectors other than
# sewerage and waste management
# -----------------------------------------------------------------------------------------------
# Reason: The survey setup changed in 2008 to include sewerage and waste management firms.
# To be consistent over time and to avoid a structural break in 2008, exclude these firms from 
# the analysis. Furthermore, exclude electricity and gas TSOs as there are too few companies
# in Germany (4 electricity TSOs in total) to conform with the RDC's data privacy rules.
data0_old <- subset(data, grepl(data$wz_u,pattern="37")==FALSE 
                    & grepl(data$wz_u,pattern="38")==FALSE 
                    & grepl(data$wz_u,pattern="39")==FALSE
                    & grepl(data$wz_u,pattern="3512")==FALSE
                    & grepl(data$wz_u,pattern="4012")==FALSE
                    & grepl(data$wz_u,pattern="3522")==FALSE
                    & grepl(data$wz_u,pattern="4022")==FALSE)


# Keep plants of firms that showed up before 2008
# -----------------------------------------------
# To avoid plant drop out in 2008, keep all plants in the sample whose parent firms figured in 
# the sample before 2008 and would only be dropped because their NACE code switched to sewerage
# or waste.

# For this, identify all firm-year observations in the original data set who have at least one
# plant-year observation in the data set 'data0_old'. Returns a list of row numbers.
data0 <-sapply(unique(data0_old$unr),function(x)which(data[,"unr"]==x))

# Draw these observations from the original data set and store them in new data frame.
data0 <-data[unlist(data0),]


# How many firms are concerned?
# -----------------------------
data_U <- aggregate(cbind("jab"=data0$jab
                          ,"bl"=data0$bl
                          ,"TM070_u"=data0$TM070_u
                          ,"TM066N_u"=data0$TM066N_u
                          ,"TM066K_b"=data0$TM066K_b
                          ,"TM083_u"=data0$TM083_u
                          ,"TM064_b"=data0$TM064_b
                          ,"UK_Code8601"=data0$UK_Code8601
                          ,"UK_Code8701"=data0$UK_Code8701
                          ,"wz_u"=data0$wz_u
                          ,"eigentuemer"=data0$eigentuemer)
                    , by=list("Jahr"=data0$Jahr,"unr"=data0$unr),mean,na.rm=TRUE)

addmargins(table(data_U$jab,data_U$Jahr,useNA="ifany"))



#================================================================================================
# 3) Generate control variables for the economic activity (industries)                                     
#================================================================================================

#================================================================================================
# 3.1 NACE code
#================================================================================================

# Generate a uniform 4-digit NACE code
# ------------------------------------
dstat(data0$wz_u)
data0$wz <- as.numeric(substr(data0$wz_u,1,4))
dstat(data0$wz)



#================================================================================================
# 3.2 Impute the fuel type for gas-fired power and heat plants in 2012
#================================================================================================

# Reason: In 2012, there is a unusual decline in gas-fired power plants vis-à-vis 2011 and 2013
# together with an increase in missing values.
table(data0$HETGruppen,data0$Jahr,useNA="ifany")

# Select the relevant variables
# -----------------------------
data_gkw <- subset(data0, select=c(bnr,Jahr,HETGruppen))

# Identify all power plants that are gas-fired in 2011 and 2013
# -------------------------------------------------------------
data_gkw$dgkw[data_gkw$Jahr==2011 & is.na(data_gkw$HETGruppen)==FALSE & data_gkw$HETGruppen==4] <- 1
data_gkw$dgkw[data_gkw$Jahr==2013 & is.na(data_gkw$HETGruppen)==FALSE & data_gkw$HETGruppen==4] <- 1
data1 <- aggregate(cbind("dgkw"=dgkw) ~ bnr, data_gkw,sum,na.rm=TRUE)

# Normalize to 1 and merge information to original data set
# ---------------------------------------------------------
data1$dgkw <- ifelse(data1$dgkw==2,1,NA)
data0 <- merge(data0,data1,by=c("bnr"),all.x=TRUE)

# Replace fuel type for all plants with missing fuel type in 2012 which are gas-fired in 2011 and 2013
# ----------------------------------------------------------------------------------------------------
data0$HETGruppen <- ifelse(is.na(data0$dgkw)==FALSE & data0$Jahr==2012,4,data0$HETGruppen)
table(data0$HETGruppen,data0$Jahr,useNA="ifany")



#================================================================================================
# 3.3 Industry fixed effects
#================================================================================================

# Generate industry fixed effects. The industry fixed effects are not mutually exclusive and
# allow to charactereize the product space of multi-product firms.

# electricity retail
# ------------------
# criterion: participation in survey no. 83
data0$sa <- ifelse(data0$TM083_u==1,1,0)

# electricity distribution
# ------------------------
# criterion: participation in surveys no. 70 or no. 66N
data0$sn <- ifelse((data0$TM070_u>0 | data0$TM066N_u>0),1,0)

# electricity generation
# ----------------------
# criterion: participation in survey no. 66K
data0$se <- ifelse(data0$TM066K_b==1,1,0)

# heat supply
# -----------
# criterion: participation in survey no. 64_b or positive number of employees
# in the field of heat supply reported in survey no. 65.
data0$wm <- ifelse(data0$TM064_b==1 | (is.na(data0$B_MBE_EF15_mean)==FALSE 
                                     & data0$B_MBE_EF15_mean>0),1,0)
# gas supply
# ----------
# criterion: reported gas activity in KSE survey (firm/plant-level) or positive number of employees
# in the field of gas supply reported in survey no. 65.
data0$ga <- ifelse(data0$UI_Code11_3==1 | data0$BI_Code11_3==1 
                  | (is.na(data0$B_MBE_EF13_mean)==FALSE 
                     & data0$B_MBE_EF13_mean>0),1,0)
# water supply
# ------------
# criterion: reported water activity in KSE survey (firm/plant-level) or positive amount of water 
# sold reported in KSE survey or positive number of employees in the field of water supply reported 
# in survey no. 65 or NACE code of 3600 or 4100.
data0$wa <- ifelse(data0$UI_Code11_4==1 | data0$BI_Code11_4==1
                  | (is.na(data0$UK_Code8701)==FALSE & data0$UK_Code8701>0) 
                  | (is.na(data0$B_MBE_EF17_mean)==FALSE 
                     & data0$B_MBE_EF17_mean>0) 
                  | is.na(data0$wz)==FALSE & (data0$wz==3600 | data0$wz==4100),1,0)

# sewerage
# --------
# criterion: reported sewerage activity in KSE survey (firm/plant-level) or NACE code of 3700, 
# 9000, or 9001.
data0$aw <- ifelse(data0$UI_Code11_5==1 | data0$BI_Code11_5==1 | is.na(data0$wz)==FALSE 
                  & (data0$wz==3700 | data0$wz==9000 | data0$wz==9001),1,0)

# waste management
# ----------------
data0$af <- ifelse(data0$UI_Code11_6==1 | data0$BI_Code11_6==1 | is.na(data0$wz)==FALSE 
                  & (grepl(data0$wz,pattern="38")==TRUE
                     | grepl(data0$wz,pattern="39")==TRUE
                     | data0$wz==9002),1,0)


# Have all plants been sorted into at least one industry?
# -------------------------------------------------------
data0_rest <- subset(data0, sa==0 & sn==0 & se==0 & wa==0 & aw==0 & af==0)
nrow(data0_rest)


# Label the remaining plants together with the waste management plants as 'miscellaneous'
# ---------------------------------------------------------------------------------------
data0$afs <- ifelse(data0$sa==0 & data0$sn==0 & data0$se==0 & data0$wa==0 & data0$aw==0,1,0)


# Check: There should be no more plants without industry affiliation
# ------------------------------------------------------------------
data0_rest2 <- subset(data0, sa==0 & sn==0 & se==0 & wa==0 & aw==0 & afs==0)
nrow(data0_rest2)



#================================================================================================
# 4) Aggregate plant-level data to firm level
#================================================================================================

#================================================================================================
# 4.1 Aggregation
#================================================================================================

# Step 1: Replicate data set
# --------------------------
data_single <- data0


# Step 2: Compute mean of all variables observed at the firm level (=remain unchanged)
# ------------------------------------------------------------------------------------
data_single_mean <- aggregate(cbind("jab"=data_single$jab
                                    ,"urs_public"=data_single$urs_public
                                    ,"EF20"=data_single$EF20
                                    ,"EF6"=data_single$EF6
                                    ,"eigentuemer"=data_single$eigentuemer
                                    ,"wz"=data_single$wz
                                    ,"bl"=data_single$bl
                                    ,"Kreis"=as.numeric(data_single$Kreis)
				                            ,"ags_u_new"=data_single$ags_u_new
                                    ,"EF7"=data_single$EF7
                                    ,"Siedlung"=data_single$Siedlung
                                    ,"Rechtsform_Zahl"=data_single$Rechtsform_Zahl
                                    ,"HETGruppen"=data_single$HETGruppen
                                    ,"UK_Code1501"=data_single$UK_Code1501
                                    ,"EF24_0180"=data_single$EF24_0180
                                    ,"UK_Code1601"=data_single$UK_Code1601
                                    ,"UK_Code2001"=data_single$UK_Code2001
                                    ,"UK_Code2501"=data_single$UK_Code2501
                                    ,"EF24_0401"=data_single$EF24_0401
                                    ,"UK_Code3701"=data_single$UK_Code3701
                                    ,"EF24_0421"=data_single$EF24_0421
                                    ,"EF24_0422"=data_single$EF24_0422
                                    ,"EF24_0424"=data_single$EF24_0424
                                    ,"UK_Code5501"=data_single$UK_Code5501
                                    ,"UK_Code4501"=data_single$UK_Code4501
                                    ,"UK_Code4901"=data_single$UK_Code4901
                                    ,"UK_Code5001"=data_single$UK_Code5001
                                    ,"EF24_0426"=data_single$EF24_0426
                                    ,"UK_Code5201"=data_single$UK_Code5201
                                    ,"EF24_0427"=data_single$EF24_0427
                                    ,"UK_Code5301"=data_single$UK_Code5301
                                    ,"UK_Code6101"=data_single$UK_Code6101
				                            ,"UK_Code7401"=data_single$UK_Code7401
				                            ,"UK_Code8501"=data_single$UK_Code8501
				                            ,"UK_Code8601"=data_single$UK_Code8601
				                            ,"UK_Code8701"=data_single$UK_Code8701
				                            ,"UK_Code8801"=data_single$UK_Code8801
				                            ,"UK_Code8901"=data_single$UK_Code8901
				                            ,"UK_Code9001"=data_single$UK_Code9001
				                            ,"UK_Code9101"=data_single$UK_Code9101
                                    ,"U_ABS_EF1061"=data_single$U_ABS_EF1061
                                    ,"U_ABS_EF1062"=data_single$U_ABS_EF1062
                                    ,"U_ABS_EF1011_sum"=data_single$U_ABS_EF1011_sum
                                    ,"U_ABS_EF1012_sum"=data_single$U_ABS_EF1012_sum
                                    ,"U_ABS_EF1041_sum"=data_single$U_ABS_EF1041_sum
                                    ,"U_ABS_EF1042_sum"=data_single$U_ABS_EF1042_sum
                                    ,"U_ABS_EF1051_sum"=data_single$U_ABS_EF1051_sum
                                    ,"U_ABS_EF1052"=data_single$U_ABS_EF1052
                                    ,"U_ABS_EF1071"=data_single$U_ABS_EF1071
                                    ,"U_ABS_EF1072"=data_single$U_ABS_EF1072
                                    ,"U_ABS_EF1081"=data_single$U_ABS_EF1081
                                    ,"U_ABS_EF1082"=data_single$U_ABS_EF1082
                                    ,"U_ABS_EF1091"=data_single$U_ABS_EF1091
                                    ,"U_ABS_EF1092"=data_single$U_ABS_EF1092
                                    ,"U_netzb_EF203_sum"=data_single$U_netzb_EF203_sum
                                    ,"EF24_9901"=data_single$EF24_9901
                                    ,"EF24_9902"=data_single$EF24_9902
                                    ,"UI_Code4001"=data_single$UI_Code4001
				                            ,"UI_Code8101"=data_single$UI_Code8101
                                    ,"EF24_9905"=data_single$EF24_9905
                                    ,"EF24_9906"=data_single$EF24_9906
                                    ,"EF24_9907"=data_single$EF24_9907
                                    ,"EF24_9911"=data_single$EF24_9911)
                              ,by=list("unr"=data_single$unr,"Jahr"=data_single$Jahr),mean, na.rm=TRUE)



# Step 3: Aggregate plant-level variables to firm level (sum)
# -----------------------------------------------------------
# Notes: Use sum function for quantity data (e.g., input and output volumes)
data_single_sum <- aggregate(cbind("B_kraftw_EF2101U1"=data_single$B_kraftw_EF2101U1
                                   ,"B_kraftw_EF2101U2"=data_single$B_kraftw_EF2101U2
                                   ,"B_kraftw_EF2101U3"=data_single$B_kraftw_EF2101U3
                                   ,"B_kraftw_EF2101U4"=data_single$B_kraftw_EF2101U4
                                   ,"B_kraftw_EF2201U2"=data_single$B_kraftw_EF2201U2
                                   ,"B_kraftw_EF2201U3"=data_single$B_kraftw_EF2201U3
                                   ,"B_kraftw_EF2201U4"=data_single$B_kraftw_EF2201U4
                                   ,"B_kraftw_EF2201U5"=data_single$B_kraftw_EF2201U5
                                   ,"B_kraftw_EF2201U6"=data_single$B_kraftw_EF2201U6
                                   ,"B_kraftw_EF701_sum"=data_single$B_kraftw_EF701_sum
                                   ,"B_waerme_EF1001"=data_single$B_waerme_EF1001
                                   ,"B_waerme_EF1010"=data_single$B_waerme_EF1010
                                   ,"B_waerme_EF1011"=data_single$B_waerme_EF1011
                                   ,"B_waerme_EF1011a"=data_single$B_waerme_EF1011a
                                   ,"B_waerme_EF1011b"=data_single$B_waerme_EF1011b
                                   ,"B_waerme_EF1011c"=data_single$B_waerme_EF1011c
                                   ,"B_waerme_EF1011d"=data_single$B_waerme_EF1011d
                                   ,"B_MBE_EF11_mean"=data_single$B_MBE_EF11_mean
                                   ,"B_MBE_EF13_mean"=data_single$B_MBE_EF13_mean
                                   ,"B_MBE_EF15_mean"=data_single$B_MBE_EF15_mean
                                   ,"B_MBE_EF17_mean"=data_single$B_MBE_EF17_mean)
                             ,by=list("unr"=data_single$unr,"Jahr"=data_single$Jahr),sum,na.rm=TRUE)


# Step 4: Aggregate plant-level variables to firm level (max)
# -----------------------------------------------------------
# Notes: Use max function for binary varables (fixed effects).
data_single_max <- aggregate(cbind("TM066K_b"=data_single$TM066K_b
                                   ,"TM064_b"=data_single$TM064_b
                                   ,"TM066N_u"=data_single$TM066N_u
                                   ,"TM083_u"=data_single$TM083_u
                                   ,"TM070_u"=data_single$TM070_u
                                   ,"UI_Code11_1"=data_single$UI_Code11_1
                                   ,"UI_Code11_2"=data_single$UI_Code11_2
                                   ,"UI_Code11_3"=data_single$UI_Code11_3
                                   ,"UI_Code11_4"=data_single$UI_Code11_4
                                   ,"UI_Code11_5"=data_single$UI_Code11_5
                                   ,"UI_Code11_6"=data_single$UI_Code11_6
                                   ,"UI_Code11_7"=data_single$UI_Code11_7
                                   ,"BI_Code11_1"=data_single$BI_Code11_1
                                   ,"BI_Code11_2"=data_single$BI_Code11_2
                                   ,"BI_Code11_3"=data_single$BI_Code11_3
                                   ,"BI_Code11_4"=data_single$BI_Code11_4
                                   ,"BI_Code11_5"=data_single$BI_Code11_5
                                   ,"BI_Code11_6"=data_single$BI_Code11_6
                                   ,"BI_Code11_7"=data_single$BI_Code11_7
                                   ,"wa"=data_single$wa
                                   ,"aw"=data_single$aw
                                   ,"sa"=data_single$sa
                                   ,"sn"=data_single$sn
                                   ,"se"=data_single$se
                                   ,"ga"=data_single$ga
                                   ,"wm"=data_single$wm
                                   ,"afs"=data_single$afs)
                             ,by=list("unr"=data_single$unr,"Jahr"=data_single$Jahr),max,na.rm=TRUE)



# Merge the three data sets
# -------------------------
data_single_U0 <- merge(data_single_mean,data_single_sum,by=c("unr","Jahr"))
data_single_U <- merge(data_single_U0,data_single_max,by=c("unr","Jahr"))
addmargins(table(data_single_U$Jahr,useNA="ifany"))


# Rename index variable 'unr' -> 'id'
# -----------------------------------
data_single_U$id <- data_single_U$unr
data_all <- data_single_U


#================================================================================================
# 4.2 Cleaning
#================================================================================================


# Settlement structure
# --------------------
# create a new category '5 = mixed settlement structure' for non-integer settlement structures 
# after aggregation
length(which(data_all$Siedlung!=round(data_all$Siedlung,0))==TRUE)
data_all$Siedlung[data_all$Siedlung!=round(data_all$Siedlung,0)] <- 5

# NACE codes
# ----------
# Create a new category '9999 = mixed NACE codes' for non-integer NACE codes after aggregation
length(which(data_all$wz!=round(data_all$wz,0))==TRUE)
data_all$wz[data_all$wz!=round(data_all$wz,0)] <- 9999

# Federal states
# --------------
# Create a new category '17 = mixed location firm' for non-integer federal states  after 
# aggregation 
length(which(data_all$bl!=round(data_all$bl,0))==TRUE)
data_all$bl[data_all$bl!=round(data_all$bl,0)] <- 17

# Municipality
# ------------
# Create a new category '99999999 = mixed location firm' for non-integer municipality codes 
# after aggregation
length(which(data_all$ags_u_new!=round(data_all$ags_u_new,0))==TRUE)
data_all$ags_u_new[data_all$ags_u_new!=round(data_all$ags_u_new,0)] <- 99999999

# Mixed ownership
# ----------------
# If a firm has mixed ownership values < 2, it must have at least one plant in mixed ownership
# so that we will classify the whole firm as being in 'mixed ownership' 
data_all$eigentuemer_new[is.na(data_all$eigentuemer)==FALSE & data_all$eigentuemer<2] <- 1
data_all$eigentuemer_new[is.na(data_all$eigentuemer)==FALSE & data_all$eigentuemer==2] <- 2
data_all$eigentuemer_new[is.na(data_all$eigentuemer)==TRUE] <- NA
summary(as.factor(data_all$eigentuemer_new))

# Fuel type
# ---------
# Create a new category '15 = mixed fuels' for non-integer fuel types after aggregation
length(which(data_all$HETGruppen!=round(data_all$HETGruppen,0))==TRUE)
data_all$HETGruppen[data_all$HETGruppen!=round(data_all$HETGruppen,0)] <- 15


# Generate for each utility their entry and exit year into the data set
# ---------------------------------------------------------------------
entry <- as.data.frame(aggregate(cbind("Eintrittsjahr"=data_all$Jahr)
                                 ,by=list("id"=data_all$id),min,na.rm=TRUE))
exit <- as.data.frame(aggregate(cbind("Austrittsjahr"=data_all$Jahr)
                                ,by=list("id"=data_all$id),max,na.rm=TRUE))
entryexit <- merge(entry,exit,by=c("id"),all=FALSE)
data_all <- merge(data_all,entryexit,by=c("id"),all=TRUE)



#================================================================================================
# 5) Definition of a 'public firm'		            	                                   
#================================================================================================

# time stamp
# ----------
date()

#================================================================================================
# 5.1 Number of matches with JAB survey 
#================================================================================================

# The survey 'Jahresabschluesse oeffentlicher Fonds, Einrichtungen und Unternehmen' collects 
# information on all firms where public entities hold at least 50 percent of the shares/votes.

# How many firms have matches with the JAB survey?
# ------------------------------------------------
addmargins(table(data_all$Jahr[data_all$jab==1]))

# For how many firms do we have at least one plant with a match in the JAB survey?
# --------------------------------------------------------------------------------
addmargins(table(data_all$Jahr[data_all$jab>0 & data_all$jab!=1]))



#================================================================================================
# 5.2 Interpolate public ownership
#================================================================================================

#================================================================================================
# 5.2.1 Continuity of participation in JAB survey             
#================================================================================================

# Idea:
# -----
# If a firm participated in the JAB survey in the previous and following year, it is very likely
# that it is also a public firm in the current year despite a missing match.


# Algorithm:
# ----------
# 1) Build a subset with all observations of a federal state.
# 2) Firms whose ownership status is to be replaced shall fulfill 3 conditions:
#    a) There is no match with the JAB survey in the current year.
#    b) There is a match with the JAB survey in the previous year.
#    c) There is a match with the JAB survey in the following year.
# 3) Firms that fulfill all three conditions have the auxiliary binary variable set to 1 (vs. NA).
# 4) Merge the binary variable to the original data set. Note: The relevant firms will have the
#    binary variable set to 1 for all years where they are observed.
# 5) Set public = 1 for all firm whose binary variable != NA in the 'missing' year. For all other 
#    firms and years, keep information from original JAB variable. 
#    Notes: Use binary variable != NA since binary == 1 leads to wrong results.


# Niedersachsen 2005
# ------------------
data_ns <- subset(data_all, bl==3, select=c(id,Jahr,bl,jab))
data_ns$id1[data_ns$Jahr==2005 & data_ns$jab==0] <- 1
data_ns$id1[data_ns$Jahr==2004 & data_ns$jab==1] <- 1
data1 <- aggregate(cbind("id_ns"=id1) ~ id, data_ns,sum)
data1$id_ns <- ifelse(data1$id_ns==2,1,NA)
data_all_new <- merge(data_all,data1,by=c("id"),all.x=TRUE)
data_all_new$public <- ifelse(is.na(data_all_new$id_ns)==FALSE & data_all_new$Jahr==2005,1,data_all_new$jab)


# Hessen 2003
# -----------
data_he <- subset(data_all, bl==6, select=c(id,Jahr,bl,jab))
data_he$id1[data_he$Jahr==2003 & data_he$jab==0] <- 1
data_he$id1[data_he$Jahr==2005 & data_he$jab==1] <- 1
data1 <- aggregate(cbind("id_he1"=id1) ~ id, data_he,sum)
data1$id_he1 <- ifelse(data1$id_he1==2,1,NA)
data_all_new <- merge(data_all_new,data1,by=c("id"),all.x=TRUE)
data_all_new$public <- ifelse(is.na(data_all_new$id_he1)==FALSE & data_all_new$Jahr==2003,1
                            ,data_all_new$public)

# Hessen 2004
# -----------
data_he$id2[data_he$Jahr==2004 & data_he$jab==0] <- 1
data_he$id2[data_he$Jahr==2005 & data_he$jab==1] <- 1
data1 <- aggregate(cbind("id_he2"=id2) ~ id, data_he,sum)
data1$id_he2 <- ifelse(data1$id_he2==2,1,NA)
data_all_new <- merge(data_all_new,data1,by=c("id"),all.x=TRUE)
data_all_new$public <- ifelse(is.na(data_all_new$id_he2)==FALSE & data_all_new$Jahr==2004,1
                            ,data_all_new$public)


# Hessen 2006
# -----------
data_he$id3[data_he$Jahr==2006 & data_he$jab==0] <- 1
data_he$id3[data_he$Jahr==2005 & data_he$jab==1] <- 1
data1 <- aggregate(cbind("id_he3"=id3) ~ id, data_he,sum)
data1$id_he3 <- ifelse(data1$id_he3==2,1,NA)
data_all_new <- merge(data_all_new,data1,by=c("id"),all.x=TRUE)
data_all_new$public <- ifelse(is.na(data_all_new$id_he3)==FALSE & data_all_new$Jahr==2006,1
                            ,data_all_new$public)


# Hessen 2007
# -----------
data_he$id4[data_he$Jahr==2007 & data_he$jab==0] <- 1
data_he$id4[data_he$Jahr==2005 & data_he$jab==1] <- 1
data1 <- aggregate(cbind("id_he4"=id4) ~ id, data_he,sum)
data1$id_he4 <- ifelse(data1$id_he4==2,1,NA)
data_all_new <- merge(data_all_new,data1,by=c("id"),all.x=TRUE)
data_all_new$public <- ifelse(is.na(data_all_new$id_he4)==FALSE & data_all_new$Jahr==2007,1
                            ,data_all_new$public)


# Thueringen 2006
# ---------------
data_th <- subset(data_all,bl==16, select=c(id,Jahr,bl,jab))
data_th$id1[data_th$Jahr==2006 & data_th$jab==0] <- 1
data_th$id1[data_th$Jahr==2005 & data_th$jab==1] <- 1
data_th$id1[data_th$Jahr==2007 & data_th$jab==1] <- 1
data1 <- aggregate(cbind("id_th"=id1) ~ id, data_th,sum)
data1$id_th <- ifelse(data1$id_th==3,1,NA)
data_all_new <- merge(data_all_new,data1,by=c("id"),all.x=TRUE)
data_all_new$public <- ifelse(is.na(data_all_new$id_th)==FALSE & data_all_new$Jahr==2006,1
                            ,data_all_new$public)


# Schleswig-Holstein
# ------------------
data_sh <- subset(data_all, bl==1, select=c(id,Jahr,bl,jab))
data_sh$id1[data_sh$Jahr==2003 & data_sh$jab==0] <- 1
data_sh$id1[data_sh$Jahr==2004 & data_sh$jab==1] <- 1
data1 <- aggregate(cbind("id_sh"=id1) ~ id, data_sh,sum)
data1$id_sh <- ifelse(data1$id_sh==2,1,NA)
data_all_new <- merge(data_all_new,data1,by=c("id"),all.x=TRUE)
data_all_new$public <- ifelse(is.na(data_all_new$id_sh)==FALSE & data_all_new$Jahr==2003,1
                            ,data_all_new$public)


#================================================================================================
# 5.2.2 Legal form 'Eigenbetrieb'            
#================================================================================================

# Only fully publicly-owned utilities can choose the legal form 'Eigenbetrieb'. Hence all firms 
# that report there legal status to be 'Eigenbetrieb' must be (fully) public.

# Declare all utilities with the legal form 'Eigenbetrieb' to be public
# ---------------------------------------------------------------------
data_all_new$public <- ifelse(is.na(data_all_new$Rechtsform_Zahl)==FALSE 
                            & data_all_new$Rechtsform_Zahl==8,1,data_all_new$public)


#================================================================================================
# 5.2.3 'urs public'variable
#================================================================================================

# For the years 2013 and 2014, additionally use information from the company register (URS) on
# public ownership 

# Set all utilities with urs_public>0 to public
# ---------------------------------------------
data_all_new$public <- ifelse(is.na(data_all_new$urs_public)==FALSE & data_all_new$urs_public>0,1
                              ,data_all_new$public)


#================================================================================================
# 5.2.4 Check: Result of interpolation          
#================================================================================================

# Check result of interpolation ('jab' (old variable ) -> 'public' (new variable))
# ---------------------------------------------------------------------------------
summary(as.factor(data_all_new$jab))
summary(as.factor(data_all_new$public))

# Table public utilities by federal state
# ---------------------------------------
addmargins(table(data_all_new$bl[data_all_new$public>0],data_all_new$Jahr[data_all_new$public>0]
                 ,useNA="ifany"))

# Table private utilities by federal state
# -----------------------------------------
addmargins(table(data_all_new$bl[data_all_new$public==0],data_all_new$Jahr[data_all_new$public==0]
                 ,useNA="ifany"))


# time stamp
# ----------
date()



#================================================================================================
# 5.3 Interpolation distinction between full/mixed ownership
#================================================================================================


# Compute the average of the categorical variable characterising full/mixed ownership. Use the
# min function, i.e. assume if a firm is in mixed ownership during the years where its status is
# observed it will also be in mixed ownership during the years where the information is missing.

# Apply min function
# ------------------
data3 <- aggregate(cbind("eigner_neu"=data_all_new$eigentuemer_new),by=list("id"=data_all_new$id)
                   ,min,na.rm=TRUE)
data3$eigner_neu[data3$eigner_neu==Inf] <- NA

# Merge the time-invariant information on full/mixed ownership due to the original data set
# -----------------------------------------------------------------------------------------
data_all_new <- merge(data_all_new,data3,by=c("id"),all.x=TRUE)

# Generate a new variable 'eigentuemer2'
# --------------------------------------
# Replaces the information on full/mixed ownership in the years w/o status info with the 
# time-invariant variable
data_all_new$eigentuemer2 <- ifelse(data_all_new$public==1 & is.na(data_all_new$eigentuemer_new)==TRUE
                                    ,data_all_new$eigner_neu,data_all_new$eigentuemer_new)

# Compare frequencies before and after interpolation
# --------------------------------------------------
addmargins(table(data_all_new$eigentuemer_new,data_all_new$Jahr,useNA="ifany"))
addmargins(table(data_all_new$eigentuemer2,data_all_new$Jahr,useNA="ifany"))


#================================================================================================
# 5.4 Define subset of public firms for the analysis
#================================================================================================

# Drop private firms
# ------------------
data_public <- subset(data_all_new, public==1)


#================================================================================================
# 6) Balanced Panel?
#================================================================================================


# entry and exit into sample 'data_public'
# -----------------------------------------
# Eintrittsjahr = first-time observation in full panel 'data_all'
# Baseyear = first-time observation in public panel 'data_public'.

entry_public <- as.data.frame(aggregate(cbind("baseyear"=data_public$Jahr)
                                        ,by=list("id"=data_public$id),min,na.rm=TRUE))
exit_public <- as.data.frame(aggregate(cbind("lastyear"=data_public$Jahr)
                                       ,by=list("id"=data_public$id),max,na.rm=TRUE))
entryexit_public <- merge(entry_public,exit_public,by=c("id"),all=FALSE)
data_public <- merge(data_public,entryexit_public,by="id",all=TRUE)

cbind("Entry"=addmargins(table(data_public$baseyear,useNA="ifany"))
      ,"Exit"=addmargins(table(data_public$lastyear,useNA="ifany")))



# Number of years observed
# ------------------------
# Count the number of years for each utility. Subsequently, table frequencies.
Dauer <- aggregate(cbind("Dauer"=data_public$Jahr),by=list("id"=data_public$id),length)
table(as.factor(Dauer$Dauer),dnn="Anzahl Jahre")
data_public <- merge(data_public,Dauer,by=c("id"),all.x=TRUE)


# How many public firms are not continuosly observed?
# ---------------------------------------------------
# Notes: This means that either the firm is not in the main sample or the public status
# is not available despite the interpolation procedures applied.
data_public$Lueckenjahre <- (data_public$lastyear-data_public$baseyear+1-data_public$Dauer)
summary(as.factor(data_public$Lueckenjahre))

# Drop all firms which are not continuously observed
# --------------------------------------------------
data_public <- subset(data_public,Lueckenjahre==0)
addmargins(table(data_public$Jahr))


#================================================================================================
# 7) Generate input variables for the production function estimation
#================================================================================================

date()

# Notes: Most of the variables are available both in the KSE survey (Kostenstrukturerhebung bei 
# Unternehmen der Energie- und Wasserversorgung, sowie bei Unternehmen zur Abwasser- und Abfall-
# entsorgung und Beseitigung von Umweltverschmutzungen) and the JAB survey (Jahresabschluesse 
# oeffentlicher Fonds, Einrichtungen und Unternehmen). 

# The former contains balance sheet data information for both private and public firms, while
# the latter contains balance sheet data for public firms only.

# Most of the time, we use the information from the KSE survey. If data is missing, we use the
# information from the JAB survey. The only exception is data on the capital stock, this 
# information is only available in the JAB survey.


#================================================================================================
# 7.1 Capital stock
#================================================================================================

# Use the perpetual inventory method (PIM) to compute the capital stock based on the initial 
# capital stock and current investments minus depreciation.


#================================================================================================
# 7.1.1 Initial capital stock
#================================================================================================

# Definition of initial capital stock (K_0)
# -----------------------------------------
# K_0 = K_start[Baseyear]
# K_start = EF24_9901-EF24_9906 ('assets at the beginning of the year' - 'depreciation at the 
# beginning of year')


# Calculate K_start
# -----------------
data_public$K_start <- data_public$EF24_9901 - data_public$EF24_9906
data_public$K_start[data_public$K_start==0 | is.nan(data_public$K_start)==TRUE] <- NA


# For how many firm-year observations is the information missing?
# ---------------------------------------------------------------
dstat(data_public$K_start/10^6,d=2)


# Build the subset of firm-year observations for which values have to be imputed
# ------------------------------------------------------------------------------
data8 <- subset(data_public, is.na(data_public$K_start)==TRUE)
data9 <-sapply(unique(data8$id),function(x)which(data_public[,"id"]==x))
data9 <-data_public[unlist(data9),]


# Calculate the growth rate of assets in the remaining years
# ----------------------------------------------------------
# K_growth_t = (K_t+1_start - K_t_start)/K_t_start
# Idea: Checks for each utility whether in the current and next year information on tangible
# assets are available. If so, the growth rate is calculated, otherwise left NA.

for (i in levels(as.factor(data9$id)))
{
  for (j in seq(2003,max(unique(data9$Jahr))-1))
  {ifelse(data9$id==i & data9$Jahr==j & is.na(data9$K_start[data9$id==i & data9$Jahr==j])==FALSE
          & is.na(data9$K_start[data9$id==i & data9$Jahr==j+1])==FALSE
          ,data9$capgrowth[data9$id==i & data9$Jahr==j] <-  (data9$K_start[data9$id==i & data9$Jahr==j+1]-data9$K_start[data9$id==i & data9$Jahr==j])/data9$K_start[data9$id==i & data9$Jahr==j]
          ,NA)
  }}
dstat(data9$capgrowth,d=2)


# Next, calculate average growth rate g over all years (per utility)
# ------------------------------------------------------------------
capgrowth_av <- aggregate(cbind("capgrowth_av"=data9$capgrowth),by=list("id"=data9$id),mean
                          ,na.rm=TRUE)
data9 <- merge(data9,capgrowth_av,by="id")
dstat(data9$capgrowth_av,d=2)

date()

# Impute missing information on K_start with linear interpolation
# ---------------------------------------------------------------
# K_t+k = (1+g)^k*K_t
# Idea: Checks for each firm-year observation whether information on K_start is available.
# If not, but if K_start is available in one of the following years, impute current K_start using
# the average growth rate as stated in the formula.

data9$cap_new3 <- NA
date()
for (i in levels(as.factor(data9$id))){
  for (j in levels(as.factor(data9$Jahr[data9$id==i]))){
    for (z in levels(as.factor(data9$Jahr[data9$id==i]))){
      ifelse(data9$id==i & data9$Jahr==j & is.na(data9$K_start[data9$id==i & data9$Jahr==j])==TRUE
             & is.na(data9$K_start[data9$id==i & data9$Jahr==z])==FALSE
             & is.na(data9$cap_new3[data9$id==i & data9$Jahr==j])==TRUE
             ,data9$cap_new3[data9$id==i & data9$Jahr==j] <-  data9$K_start[data9$id==i & data9$Jahr==z]/(1+data9$capgrowth_av[data9$id==i & data9$Jahr==j])^(as.numeric(z)-as.numeric(j))
             ,data9$cap_new3[data9$id==i & data9$Jahr==j])
    }}}
date()


# Merge imputed values to original data set
# -----------------------------------------
data_public <- merge(data_public,subset(data9,select=c(id,Jahr,cap_new3)),by=c("id","Jahr"),all.x=TRUE)

# Replace missing values - if possible - by imputed values
# --------------------------------------------------------
data_public$K_start_new <- ifelse(is.na(data_public$K_start)==FALSE,data_public$K_start,data_public$cap_new3)
dstat(data_public$K_start_new/10^6,d=2)

# Correct for implausible values
# ------------------------------
data_public$K_start_new[data_public$K_start_new>max(data_public$K_start,na.rm=TRUE)] <- NA
data_public$K_start_new[data_public$K_start_new<min(data_public$K_start,na.rm=TRUE)] <- NA

# Deflate values with PPI for investment goods
# --------------------------------------------
data_public$K_start_new_defl <- data_public$K_start_new/data_public$PI_invest
dstat(data_public$K_start_new_defl/10^6,2)


# Compute initial capital stock
# ----------------------------
for (i in as.factor(data_public$id))
{data_public$K0[data_public$id==i] <- data_public$K_start_new_defl[data_public$id==i & data_public$Jahr==data_public$baseyear]}
dstat(data_public$K0/10^6,2)


#================================================================================================
# 7.1.2 Depreciation
#================================================================================================


# Depreciation rate per year
# --------------------------
# d=EF24_9907/EF24_9901 (depreciation in current year/assets at beginning of the year)
data_public$depreciation <- data_public$EF24_9907/data_public$EF24_9901
data_public$depreciation[data_public$depreciation==Inf] <- NA
data_public$depreciation[is.nan(data_public$depreciation)==TRUE] <- NA
data_public$depreciation[data_public$depreciation==0] <- NA
data_public$depreciation[data_public$depreciation>1] <- NA

dstat(data_public$depreciation,3)


# Compute average over all years (2003-2014) for each utility
# -----------------------------------------------------------
for (i in as.factor(data_public$id))
{data_public$av_depreciation[data_public$id==i] <- mean(data_public$depreciation[data_public$id==i]
                                            ,na.rm=TRUE)}
dstat(data_public$av_depreciation,3)



#================================================================================================
# 7.1.3 Investments
#================================================================================================



# Yearly investment (deflated with investment index)
# --------------------------------------------------
# I_t = EF24_9905-EF24_9901 (assets at the end of the year - assets at the beginning of the year)
# Use information on investments into tangible assets from the investment survey if the 
# information is missing in the JAB survey.
data_public$investment <- data_public$EF24_9905-data_public$EF24_9901
data_public$investment <- ifelse((data_public$investment==0 | is.na(data_public$investment)==TRUE)
                           & is.na(data_public$UI_Code4001)==FALSE
                           ,data_public$UI_Code4001
                           ,data_public$investment)
dstat(data_public$investment/10^6,2)


# Deflate with PPI for investment goods
# -------------------------------------
data_public$invest_defl <- data_public$investment/data_public$PI_invest
dstat(data_public$invest_defl/10^6,2)



#================================================================================================
# 7.1.4 Estimate capital stock with PIM
#================================================================================================


# -----------------------------------------------------------------------------------------
# Formula:
# K_t+1 = (1-d)*K_t + I_t+1/PI_t+1
# ------------------------------------------------------------------------------------------

date()
for (i in levels(as.factor(data_public$id))){
  ifelse(data_public$id==i & data_public$Jahr==data_public$baseyear,
         data_public$K_adj[data_public$id==i & data_public$Jahr==data_public$baseyear] <- ((1-data_public$av_depreciation[data_public$id==i & data_public$Jahr==data_public$baseyear])
                                                                   *data_public$K0[data_public$id==i & data_public$Jahr==data_public$baseyear]
                                                                   + data_public$invest_defl[data_public$id==i & data_public$Jahr==data_public$baseyear])
         ,NA)
  for (k in seq(1,max(unique(data_public$Jahr)-2003))){
    ifelse(data_public$id==i & data_public$Jahr==data_public$baseyear+k,
           data_public$K_adj[data_public$id==i & data_public$Jahr==data_public$baseyear+k] <- ((1-data_public$av_depreciation[data_public$id==i & data_public$Jahr==data_public$baseyear+k])
                                                                       *data_public$K_adj[data_public$id==i & data_public$Jahr==data_public$baseyear+k-1]
                                                                       + data_public$invest_defl[data_public$id==i & data_public$Jahr==data_public$baseyear+k])
           ,data_public$K_adj)
  }}
date()


dstat(data_public$K_adj/10^6,d=2)



#================================================================================================
# 7.2 Labour (Employees, FTE)
#================================================================================================

# Number of employees in KSE survey
# ---------------------------------
dstat(data_public$UK_Code1501)

# Number of employees in JAB survey
# ---------------------------------
dstat(data_public$EF24_0180)

# Correlation between KSE variable and JAB variable
# -------------------------------------------------
cor(data_public$UK_Code1501,data_public$EF24_0180,use="complete.obs")
data_public$cor_labour <- ifelse(data_public$EF24_0180!=0,data_public$UK_Code1501/data_public$EF24_0180,NA)
dstat(data_public$cor_labour,d=2)

# Use employees from KSE data - if not available, use employees from JAB survey
# ----------------------------------------------------------------------------- 
data_public$beschaeftigte <- ifelse(is.na(data_public$UK_Code1501)==FALSE 
                                    & data_public$UK_Code1501>0,data_public$UK_Code1501
                                    ,data_public$EF24_0180)
dstat(data_public$beschaeftigte)


#================================================================================================
# 7.3 Hourly wages
#================================================================================================


# Compute labour costs in KSE survey 
# ----------------------------------
# labour costs = wage bill + mandatory social contributions + other social contributions
data_public$bruttolohn_KSE <- (data_public$UK_Code5001 + data_public$UK_Code5201 
                               + ifelse(is.na(data_public$UK_Code5301)==FALSE
                                        ,data_public$UK_Code5301,0))
dstat(data_public$bruttolohn_KSE/10^6,d=2)

# Compute labour costs in JAB survey 
# ----------------------------------
data_public$bruttolohn_JAB <- data_public$EF24_0426 + data_public$EF24_0427
dstat(data_public$bruttolohn_JAB/10^6,d=2)

# Correlation between KSE variable and JAB variable
# -------------------------------------------------
cor(data_public$bruttolohn_KSE,data_public$bruttolohn_JAB,use="complete.obs")
data_public$cor_lohn <- ifelse(data_public$bruttolohn_JAB!=0,data_public$bruttolohn_KSE/data_public$bruttolohn_JAB,NA)
dstat(data_public$cor_lohn,d=2)


# Use labour costs from KSE data - if not available, use labour costs from JAB survey
# ----------------------------------------------------------------------------------- 
data_public$bruttolohn1 <- ifelse(is.na(data_public$bruttolohn_KSE)==FALSE 
                                  & data_public$bruttolohn_KSE>0
                                  ,data_public$bruttolohn_KSE
                                    ,data_public$bruttolohn_JAB)


# Deflate with German labour costs index
# --------------------------------------
data_public$bruttolohn <- data_public$bruttolohn1/data_public$lohn_deflation_index
dstat(data_public$bruttolohn/10^6,d=2)


# Calculate average hourly wage using the number of hours worked from the KSE survey
# ---------------------------------------------------------------------------------
data_public$wage1 <- ifelse((data_public$bruttolohn>0&data_public$UK_Code1601>0)
                           ,data_public$bruttolohn/data_public$UK_Code1601,NA)
dstat(data_public$wage1)


# If the firm-level hourly wage is not available, use industry average
# --------------------------------------------------------------------
data_public$wage <- ifelse(is.na(data_public$wage1)==TRUE,mean(data_public$wage1,na.rm=TRUE),
                                data_public$wage1)
dstat(data_public$wage)



#================================================================================================
# 7.4 External services
#================================================================================================


# Costs for external services in KSE survey
# ------------------------------------------
dstat(data_public$UK_Code5501/10^6,d=2)


# Costs for external services in JAB survey
# ------------------------------------------
dstat(data_public$EF24_0422/10^6,d=2)


# Correlation between KSE variable and JAB variable
# -------------------------------------------------
cor(data_public$UK_Code5501,data_public$EF24_0422,use="complete.obs")
data_public$cor_fremdeDL <- ifelse(data_public$EF24_0422!=0,data_public$UK_Code5501/data_public$EF24_0422,NA)
dstat(data_public$cor_fremdeDL,d=2)


# Use external services from KSE data - if not available, use external services from JAB survey
# ---------------------------------------------------------------------------------------------
data_public$fremdeDL1 <- ifelse(is.na(data_public$UK_Code5501)==FALSE 
                                    & data_public$UK_Code5501>0,data_public$UK_Code5501
                                    ,data_public$EF24_0422)



# Deflate with PPI for NACE M (professional services)
# ----------------------------------------------------
data_public <- data_public[order(data_public$id,data_public$Jahr),]
data_public$fremdeDL <- data_public$fremdeDL1/data_public$fdl_deflation_index
dstat(data_public$fremdeDL/10^6,d=2)



#================================================================================================
# 7.5 Material / Intermediates                        
#================================================================================================


# Material expenditures KSE
# --------------------------
dstat(data_public$UK_Code3701/10^6,d=2)


# Material expenditures JAB
# --------------------------
dstat(data_public$EF24_0421/10^6,d=2)


# Correlation between KSE variable and JAB variable
# -------------------------------------------------
cor(data_public$UK_Code3701,data_public$EF24_0421,use="complete.obs")
data_public$cor_mat <- ifelse(data_public$EF24_0421!=0,data_public$UK_Code3701/data_public$EF24_0421,NA)
dstat(data_public$cor_mat,d=2)

# The JAB variable is much larger than the KSE variable. Add 'commodities' and 'procured energy
# and water' to 'material expenditure' in the survey. Obtains 'intermediates'.

data_public$intermediates_KSE <- (data_public$UK_Code3701
                                  +ifelse(is.na(data_public$UK_Code4501)==FALSE
                                          ,data_public$UK_Code4501,0)
                                  +ifelse(is.na(data_public$UK_Code4901)==FALSE
                                          ,data_public$UK_Code4901,0))


# Compare again the correlation between KSE variable and JAB variable
# -------------------------------------------------------------------
cor(data_public$intermediates_KSE,data_public$EF24_0421,use="complete.obs")
data_public$cor_int <- ifelse(data_public$EF24_0421!=0,data_public$intermediates_KSE/data_public$EF24_0421,NA)
dstat(data_public$cor_int,d=2)


# Use intermediates from KSE data - if not available, use intermediates from JAB survey
# -------------------------------------------------------------------------------------
data_public$intermediates1 <- ifelse(is.na(data_public$intermediates_KSE)==FALSE 
                                     & data_public$intermediates_KSE>0
                                     ,data_public$intermediates_KSE,data_public$EF24_0421)


# Deflate with German PPI for intermediate goods
# ----------------------------------------------
data_public$intermediates <- data_public$intermediates1/data_public$int_deflation_index
dstat(data_public$intermediates/10^6,d=2)



#================================================================================================
# 8) Generate output variable for the production function estimation
#================================================================================================


# Revenues in KSE survey
# ----------------------
dstat(data_public$UK_Code2501/10^6,d=2)


# Revenues in JAB survey
# ----------------------
dstat(data_public$EF24_0401/10^6,d=2)


# Correlation between KSE variable and JAB variable
# -------------------------------------------------
cor(data_public$UK_Code2501,data_public$EF24_0401,use="complete.obs")
data_public$cor_umsatz <- ifelse(data_public$EF24_0401!=0,data_public$UK_Code2501/data_public$EF24_0401,NA) 
dstat(data_public$cor_umsatz,d=2)


# Use revenues from KSE survey - if not available, use revenues from JAB survey
# ------------------------------------------------------------------------------
data_public$revenues1 <- ifelse(is.na(data_public$UK_Code2501)==FALSE & data_public$UK_Code2501>0
                                     ,data_public$UK_Code2501,data_public$EF24_0401)
dstat(data_public$revenues1/10^6,d=2)


# Deflate revenues with the German PPI of NACE 35/36 
# --------------------------------------------------
data_public$revenues <- data_public$revenues1/data_public$rev_deflation_index
dstat(data_public$revenues/10^6,d=2)


# Compute value added = revenues - intermediates
# -----------------------------------------------
data_public$value_added <- data_public$revenues-data_public$intermediates
dstat(data_public$value_added/10^6,d=2)


# Compute value added = revenues - intermediates - taxes
# ------------------------------------------------------
data_public$value_added2 <- (data_public$revenues-data_public$intermediates
                             -ifelse(is.na(data_public$UK_Code6101)==FALSE,data_public$UK_Code6101
                                     ,0))
dstat(data_public$value_added2/10^6,d=2)


# Compute value added = revenues (not deflated) - intermediates (deflated)
# ------------------------------------------------------------------------
data_public$value_added3 <- data_public$revenues1 - data_public$intermediates
cor(data_public$value_added2,data_public$value_added3,use="complete.obs")


# Compute value added = revenues (deflated with water PPI) - intermediates (deflated)
# ----------------------------------------------------------------------------------
data_public$revenues_wa <- data_public$revenues1/data_public$defl_wa
data_public$value_addedw <- data_public$revenues_wa - data_public$intermediates



#================================================================================================
# 9) Firm-level output prices for electricity
#================================================================================================


# Firm-level unit price for electricity sold to end-consumers [EUR/kWh]
# ---------------------------------------------------------------------
data_public$p_LV <- data_public$U_ABS_EF1062/data_public$U_ABS_EF1061
dstat(data_public$p_LV,d=2)


# Firm-level unit price for electricity sold to other utilities [EUR/kWh]
# ---------------------------------------------------------------------
data_public$p_EVU <- data_public$U_ABS_EF1012_sum/data_public$U_ABS_EF1011_sum
dstat(data_public$p_EVU,d=2)


# Firm-level unit price for electricity sold to small customers [EUR/kWh]
# -----------------------------------------------------------------------
data_public$p_TK <- data_public$U_ABS_EF1052/data_public$U_ABS_EF1051_sum
dstat(data_public$p_TK,d=2)


# Firm-level unit price for electricity sold to large customers [EUR/kWh]
# ---------------------------------------------------------------------
data_public$p_SK <- data_public$U_ABS_EF1042_sum/data_public$U_ABS_EF1041_sum
dstat(data_public$p_SK,d=2)


# Firm-level unit price for electricity sold to households [EUR/kWh]
# ---------------------------------------------------------------------
data_public$p_HH <- data_public$U_ABS_EF1082/data_public$U_ABS_EF1081
dstat(data_public$p_HH,d=2)


# Firm-level unit price for electricity sold to manufacturing [EUR/kWh]
# ---------------------------------------------------------------------
data_public$p_VG <- data_public$U_ABS_EF1072/data_public$U_ABS_EF1071
dstat(data_public$p_VG,d=2)


# Firm-level unit price for electricity sold to other end-consumers [EUR/kWh]
# ---------------------------------------------------------------------------
data_public$p_BC <- data_public$U_ABS_EF1092/data_public$U_ABS_EF1091
dstat(data_public$p_BC,d=2)


# Firm-level unit price for electricity sold to end-consumers and utilities [EUR/kWh]
# -----------------------------------------------------------------------------------
data_public$p_sa <- (data_public$U_ABS_EF1062+data_public$U_ABS_EF1012_sum)/(data_public$U_ABS_EF1061+data_public$U_ABS_EF1011_sum)
dstat(data_public$p_sa,d=2)


#================================================================================================
# 10) Variable costs share in revenues
#================================================================================================

# labour costs share
# -------------------
data_public$wage_share <- data_public$bruttolohn/data_public$revenues
dstat(data_public$wage_share,d=2)


# external services cost share
# ----------------------------
data_public$service_share <- data_public$fremdeDL/data_public$revenues
dstat(data_public$service_share,d=2)


# intermediates cost share
# ------------------------
data_public$int_share <-  data_public$intermediates/data_public$revenues
dstat(data_public$int_share,d=2)


#================================================================================================
# 11) Generate organisation variables
#================================================================================================

#================================================================================================
# 11.1 Ownership
#================================================================================================

# Recode ownership
# ----------------
# 0: purely public
# 1: private minority
data_public$eigentuemer2[data_public$eigentuemer2==2] <- 0
addmargins(table(data_public$eigentuemer2,data_public$Jahr,useNA="ifany"))


#================================================================================================
# 11.2 Legal form
#================================================================================================

# Legal form (KSE vsurvey)
# ------------------------
addmargins(table(data_public$Rechtsform_Zahl,data_public$Jahr,useNA="ifany"))

# Legal form (JAB survey)
# -----------------------
addmargins(table(data_public$EF6,data_public$Jahr,useNA="ifany"))


# Summarise in 5 categories using information from both surveys
# -------------------------------------------------------------
data_public$status <- NA
data_public$status[data_public$Rechtsform_Zahl==9 | data_public$Rechtsform_Zahl==7
             |((data_public$Rechtsform_Zahl==10 | is.na(data_public$Rechtsform_Zahl)==TRUE)
               & (is.na(data_public$EF6)==FALSE & (data_public$EF6==12 | data_public$EF6==8)))
             ] <- 1
data_public$status[data_public$Rechtsform_Zahl==8 
             | ((data_public$Rechtsform_Zahl==10 | is.na(data_public$Rechtsform_Zahl)==TRUE) 
                & is.na(data_public$EF6)==FALSE & data_public$EF6>=20 & data_public$EF6<=22)
             ] <- 2
data_public$status[(data_public$Rechtsform_Zahl>=1 & data_public$Rechtsform_Zahl<=5) 
             | ((data_public$Rechtsform_Zahl==10 | is.na(data_public$Rechtsform_Zahl)==TRUE)  
                & is.na(data_public$EF6)==FALSE & data_public$EF6>=1 & data_public$EF6<=6)
             ] <- 3
data_public$status[data_public$Rechtsform_Zahl==6
             | ((data_public$Rechtsform_Zahl==10 | is.na(data_public$Rechtsform_Zahl)==TRUE)  
                & is.na(data_public$EF6)==FALSE & data_public$EF6==7)
             ] <- 4
data_public$status[data_public$Rechtsform_Zahl==11] <- 5
summary(as.factor(data_public$status))


# Generate legal form fixed effects
# ---------------------------------
data_public$Verband <- ifelse(data_public$status==1,1,0)
data_public$Eigenbetrieb <- ifelse(data_public$status==2,1,0)
data_public$unlisted <- ifelse(data_public$status==3,1,0) 
data_public$listed <- ifelse(data_public$status==4,1,0)
data_public$privlaw <- ifelse(data_public$status==3 | data_public$status==4,1,0)


#================================================================================================
# 11.3 Outsourcing
#================================================================================================

# Outsourcing I: Share of external services in total services
# -----------------------------------------------------------
data_public$shareF <- data_public$fremdeDL/(data_public$bruttolohn + data_public$fremdeDL)

# Outsourcing II: Share of procured energy and water in total revenue
# -------------------------------------------------------------------
# Umkodieren der NAs-->0 in der Variable UK_Code4501
data_public$UK_Code4501[is.na(data_public$UK_Code4501)==TRUE] <- 0
data_public$shareFEW <- data_public$UK_Code4501/data_public$revenues1



#================================================================================================
# 12) Other controls
#================================================================================================


#================================================================================================
# 12.1 Industry and fuel fixed effects
#================================================================================================


# Industry fixed effects (product mix) 
# ------------------------------------
data_public$sa_EVU <- ifelse(is.na(data_public$U_ABS_EF1011_sum)==FALSE & data_public$U_ABS_EF1011_sum>0,1,0)
data_public$sa_TK <- ifelse(is.na(data_public$U_ABS_EF1051_sum)==FALSE & data_public$U_ABS_EF1051_sum>0,1,0)
data_public$sa_SK <- ifelse(is.na(data_public$U_ABS_EF1041_sum)==FALSE & data_public$U_ABS_EF1041_sum>0,1,0)
data_public$wm_NWG <- ifelse((is.na(data_public$B_waerme_EF1011a)==FALSE & data_public$B_waerme_EF1011a>0) 
                       | (is.na(data_public$B_waerme_EF1010)==FALSE & data_public$B_waerme_EF1010>0),1,0)
data_public$wm_HH <- ifelse(is.na(data_public$B_waerme_EF1011b)==FALSE & data_public$B_waerme_EF1011b>0,1,0)


# Fuel fixed effects
# ------------------
addmargins(table(data_public$HETGruppen[data_public$TM066K_b==1 | data_public$TM064_b==1]
                 ,data_public$Jahr[data_public$TM066K_b==1 | data_public$TM064_b==1],useNA="ifany"))
data_public$HETGruppen[is.na(data_public$HETGruppen)==TRUE] <- 0
data_public$se_hc <- ifelse(data_public$HETGruppen==1,1,0)
data_public$se_bk <- ifelse(data_public$HETGruppen==2,1,0)
data_public$se_oil <- ifelse(data_public$HETGruppen==3,1,0)
data_public$se_gas <- ifelse(data_public$HETGruppen==4,1,0)
data_public$se_water <- ifelse(data_public$HETGruppen==5,1,0)
data_public$se_waste <- ifelse(data_public$HETGruppen==12,1,0)
data_public$se_sonst <- ifelse(data_public$HETGruppen>=13,1,0)


# Join fuel categories in case of too few observations
data_public$se_bio <- ifelse(data_public$HETGruppen==9 | data_public$HETGruppen==10,1,0)
data_public$se_coal <- ifelse(data_public$se_hc==1 | data_public$se_bk==1,1,0)
data_public$se_EE <- ifelse(data_public$HETGruppen==6 | data_public$HETGruppen==7 | data_public$HETGruppen==8 
                      | data_public$HETGruppen==11 ,1,0)
data_public$se_EE2 <- ifelse(data_public$se_EE==1 | data_public$se_water==1 | data_public$se_bio==1 | data_public$se_sonst==1,1,0)


# Investments intensity
# ---------------------
data_public$inv_int <- data_public$investment/data_public$revenues1
dstat(as.data.frame(data_public$inv_int),d=2)


#================================================================================================
# 12.2 Customer structure
#================================================================================================


# Customer structure in the electricity sector
# --------------------------------------------
data_public$ShareTK<-ifelse(data_public$U_ABS_EF1051_sum>0
                      ,data_public$U_ABS_EF1051_sum/data_public$U_ABS_EF1061,0)

data_public$ShareSK<-ifelse(data_public$U_ABS_EF1041_sum>0
                      ,data_public$U_ABS_EF1041_sum/data_public$U_ABS_EF1061,0)

data_public$ShareWV<-ifelse(is.na(data_public$U_ABS_EF1011_sum)==FALSE
                      ,data_public$U_ABS_EF1011_sum/(data_public$U_ABS_EF1061
                                               +ifelse(is.na(data_public$U_ABS_EF1011_sum)==TRUE,0
                                                       ,data_public$U_ABS_EF1011_sum)),0)

data_public$ShareTK[is.na(data_public$ShareTK)==TRUE] <- 0
data_public$ShareSK[is.na(data_public$ShareSK)==TRUE] <- 0
data_public$ShareWV[is.na(data_public$ShareWV)==TRUE] <- 0

dstat(as.data.frame(data_public$ShareTK),d=2)
dstat(as.data.frame(data_public$ShareSK),d=2)
dstat(as.data.frame(data_public$ShareWV),d=2)


# Customer structure in the heat sector
# -------------------------------------
data_public$ShareHH <- ifelse(data_public$B_waerme_EF1011b>0
                        ,data_public$B_waerme_EF1011b/data_public$B_waerme_EF1011,0)

data_public$ShareVG <- ifelse(data_public$B_waerme_EF1011a>0
                        ,data_public$B_waerme_EF1011a/data_public$B_waerme_EF1011,0)

data_public$ShareSo <- ifelse(data_public$B_waerme_EF1011c>0
                        ,data_public$B_waerme_EF1011c/data_public$B_waerme_EF1011,0)

data_public$ShareWV_w<-ifelse(is.na(data_public$B_waerme_EF1010)==FALSE
                        ,data_public$B_waerme_EF1010/(data_public$B_waerme_EF1011
                                                +ifelse(is.na(data_public$B_waerme_EF1010)==TRUE
                                                        ,0,data_public$B_waerme_EF1010)),0)

data_public$ShareHH[is.na(data_public$ShareHH)==TRUE] <- 0
data_public$ShareSo[is.na(data_public$ShareSo)==TRUE] <- 0
data_public$ShareVG[is.na(data_public$ShareVG)==TRUE] <- 0
data_public$ShareWV_w[is.na(data_public$ShareWV_w)==TRUE] <- 0

dstat(as.data.frame(data_public$ShareHH),d=2)
dstat(as.data.frame(data_public$ShareSo),d=2)
dstat(as.data.frame(data_public$ShareVG),d=2)
dstat(as.data.frame(data_public$ShareWV_w),d=2)


#================================================================================================
# 12.2 Settlement, time, and size fixed effects
#================================================================================================

# Settlement structure fixed effects
# -----------------------------------
addmargins(table(data_public$Siedlung,data_public$Jahr,useNA="ifany"))
data_public$metro <- ifelse(data_public$Siedlung==1,1,0)
data_public$suburban <- ifelse(data_public$Siedlung==2,1,0)
data_public$rurald <- ifelse(data_public$Siedlung==3,1,0)
data_public$rurals <- ifelse(data_public$Siedlung==4,1,0)


# time fixed effects
# ------------------
data_public$t2003 <- ifelse(data_public$year==2003,1,0)
data_public$t2004 <- ifelse(data_public$year==2004,1,0)
data_public$t2005 <- ifelse(data_public$year==2005,1,0)
data_public$t2006 <- ifelse(data_public$year==2006,1,0)
data_public$t2007 <- ifelse(data_public$year==2007,1,0)
data_public$t2008 <- ifelse(data_public$year==2008,1,0)
data_public$t2009 <- ifelse(data_public$year==2009,1,0)
data_public$t2010 <- ifelse(data_public$year==2010,1,0)
data_public$t2011 <- ifelse(data_public$year==2011,1,0)
data_public$t2012 <- ifelse(data_public$year==2012,1,0)
data_public$t2013 <- ifelse(data_public$year==2013,1,0)
data_public$t2014 <- ifelse(data_public$year==2014,1,0)

# generate time trend
# --------------------
# 2003=1, 2004=2, ...
data_public$t <- as.numeric(factor(data_public$year))


# Firm size fixed effects
# -----------------------
data_public$size_small <- ifelse(data_public$revenues1<10000000 & data_public$beschaeftigte<=49,1,0)
data_public$size_med <- ifelse(data_public$size_small==0 
                         & data_public$revenues1<50000000 & data_public$beschaeftigte<=249,1,0)
data_public$size_large <- ifelse(data_public$size_small==0 & data_public$size_med==0 
                           & (data_public$revenues1>=50000000 | data_public$beschaeftigte>249),1,0)

summary(as.factor(data_public$size_small))
summary(as.factor(data_public$size_med))
summary(as.factor(data_public$size_large))

# Were all firms sorted into a category?
nrow(subset(data_public, size_small==1 | size_med==1 | size_large==1))==nrow(data_public)



#================================================================================================
# 13) Select estimation sample                                                       
#================================================================================================

#================================================================================================
# 13.1 Drop outliers
#================================================================================================


# Q99.9 qantile: Drop the right 0.001 tail of the input and output distributions
# ------------------------------------------------------------------------------

out_q999 <- data_public$id[as.numeric(data_public$fremdeDL)> quantile(data_public$fremdeDL,0.999,na.rm=TRUE)
                            | as.numeric(data_public$K_adj)>quantile(data_public$K_adj,0.999,na.rm=TRUE)
                            | as.numeric(data_public$bruttolohn)>quantile(data_public$bruttolohn,0.999,na.rm=TRUE)
                            | as.numeric(data_public$intermediates)>quantile(data_public$intermediates,0.999,na.rm=TRUE)
                            | as.numeric(data_public$revenues)>quantile(data_public$revenues,0.999,na.rm=TRUE)
                            ]

outlier_q999 <- data_public[(data_public$id %in% c(out_q999)),]
addmargins(table(outlier_q999$Jahr,dnn="Anzahl Outlier basierend auf q99.9"))
data_public_clear <- data_public[!(data_public$id %in% c(out_q999)),]
addmargins(table(data_public_clear$Jahr,useNA="ifany"))



# Q0.001 qantile: Drop the left 0.001 tail of the input and output distributions
# ------------------------------------------------------------------------------

out_q0001 <- data_public_clear$id[as.numeric(data_public_clear$fremdeDL)< quantile(data_public_clear$fremdeDL,0.001,na.rm=TRUE)
                             | as.numeric(data_public_clear$K_adj)<quantile(data_public_clear$K_adj,0.001,na.rm=TRUE)
                             | as.numeric(data_public_clear$bruttolohn)<quantile(data_public_clear$bruttolohn,0.001,na.rm=TRUE)
                             | as.numeric(data_public_clear$intermediates)<quantile(data_public_clear$intermediates,0.001,na.rm=TRUE)
                             | as.numeric(data_public_clear$revenues)<quantile(data_public_clear$revenues,0.001,na.rm=TRUE)
                             ]

outlier_q0001 <- data_public_clear[(data_public_clear$id %in% c(out_q0001)),]
addmargins(table(outlier_q0001$Jahr,dnn="Anzahl Outlier basierend auf q0.001"))
data_public_clear <- data_public_clear[!(data_public_clear$id %in% c(out_q0001)),]
addmargins(table(data_public_clear$Jahr,useNA="ifany"))


#================================================================================================
# 13.1.4 Missing values in key variables
#================================================================================================

# Drop all firms without information on legal status, ownership, and settlement structure.
# ----------------------------------------------------------------------------------------
data_public_clear <- subset(data_public_clear,is.na(data_public_clear$status)==FALSE 
                	& is.na(data_public_clear$eigentuemer2)==FALSE
                	& is.na(data_public_clear$Siedlung)==FALSE)

# Drop firms with inconsistent reports on settlement structure
# ------------------------------------------------------------
data_public_clear <- subset(data_public_clear,Siedlung !=5)

# Choose only firms with positive values in inputs and outputs
# ------------------------------------------------------------
# (as we will logarithmise inputs and outputs)
data_public_clear <- subset(data_public_clear, fremdeDL>0 & K_adj>0 & bruttolohn>0 & value_added3>0)
addmargins(table(data_public_clear$year))


#================================================================================================
# 13.2 Frequencies
#================================================================================================


# How many observations do we have per year?
# ------------------------------------------
addmargins(table(data_public_clear$Jahr))


# with positive values for capital: K_adj>0
# -----------------------------------------
data_public_K <- subset(data_public_clear,K_adj>0)
addmargins(table(data_public_K$Jahr))


# with positive values for labour: bruttolohn>0
# ---------------------------------------------
data_public_L <- subset(data_public_clear,bruttolohn>0)
addmargins(table(data_public_L$Jahr))



#================================================================================================
# 14) Clean and save data set                                                             
#================================================================================================


# Remove constant variables
# -------------------------
data_public_clear <- subset(data_public_clear,select=-c(id_ns,id_he1,id_he2,id_he3,id_he4
				,id_th,id_sh,jab,public,Lueckenjahre,out_labour,out_lohn
				,out_int,out_umsatz,out_p_LV,out_p_EVU,out_p_TK,out_p_SK,out_p_HH
				,out_p_VG,out_p_BC,out_p_sa))

# Export data set
# ---------------
write.dta(data_public_clear,paste(Path1,"data_public_final.dta",sep=""))


#================================================================================================
date()
#===========================End of file==========================================================