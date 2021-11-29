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
# PART 1: sample construction (01_RCO_sample_construction_v20.R)
# PART 2: data descriptives (02_RCO_descriptives_v24.R) | this file
# PART 3: basic estimation
#      3a: estimation for mixed utilities (03a_RCO_main_sample_v180.R)
#      3b: estimation for water utilities (03b_RCO_water_v188.R)
#      3c: estimation for electricity and gas utilities (03c_RCO_electricity_gas_v169.R)
#      3d: estimation for heat and power plants (03d_RCO_heat_power_plants_v168.R)
# PART 4: estimation with interactions in LOM
#      4a: interactions for mixed utilities (04a_RCO_interactions_main_v184.R)
#      4b: interactions for water (04b_RCO_interactions_water_v189.R)
#      4c: interactions for elec&gas (04c_RCO__interactions_elecgas_v170.R)
#      4d: interactions for heat&power (04d_RCO_interactions_heatpower_v187.R)
# PART 5: sensitivity analyses
#      5a: excludes gas utilities (05a_RCO_SensAn_wo_gas_v172.R)
#      5b: lag outsourcing (05b_RCO_SensAn_lag1_out_v173.R)
#      5c: lag2 outsourcing (05c_RCO_SensAn_lag2_out_v174.R)
#      5d: time-varying pdt technology (05d_RCO_SensAn_time_v183.R)
#
#
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
# ------------
data <- read.dta13(file.path(Path1,"/data_public_final.dta"))  


class(data)
dim(data)


# Recode as panel data
# --------------------
data_p <- pdata.frame(data, index=c("id","year"),row.names=FALSE)
pdim(data_p)



#================================================================================================
# 1) Data preparation
#================================================================================================

#================================================================================================
# 1.1 Generate subsamples
#================================================================================================
 
# The whole data set is divided into 4 mutually exclusive data sets (see Section 6.1 in the 
# paper).

# data_p = data_mixed + data_w + data_sg + data_ez


# mixed utilities
# ----------------
datamixed <- subset(data_p,(wa==1 & sa==0 & se==0 & sn==0 & wm==0 & ga==0)==FALSE
                      & (wa==0 & wm==0 & se==0 & (sa==1 | sn==1 | ga==1))==FALSE
                      & (wa==0 & wm==0 & se==1 & (sa==1 | sn==1 | ga==1))==FALSE
                      & ((se == 1 | wm==1) & ga==0 & wa==0 & sa==0 & sn==0)==FALSE)
addmargins(table(data_mixed$Jahr))
length(unique(data_mixed$id))


# pure water utilities
# --------------------
data_w <- subset(data_p, wa==1 & sa==0 & se==0 & sn==0 & wm==0 & ga==0)
addmargins(table(data_w$Jahr))
length(unique(data_w$id))



# electricity & gas
# ------------------
data_sg <- subset(data_p, (wa==0 & wm==0 & (sa==1 | sn==1 | ga==1)))
addmargins(table(data_sg$Jahr))
length(unique(data_sg$id))


# heat and power plants
# ----------------------
data_ez <- subset(data_p,(se == 1 | wm==1) & ga==0 & wa==0 & sa==0 & sn==0)
addmargins(table(data_ez$Jahr))
length(unique(data_ez$id))



#================================================================================================
# 2) Descriptive statistics for inputs and outputs (Table 5 in the paper)
#================================================================================================

#================================================================================================
# 2.1 External services [S in mio EUR]
#================================================================================================


# mixed utilities
# ---------------
dstat(as.data.frame(data_mixed$fremdeDL/10^6),d=2)

# water utilities
# ---------------
dstat(as.data.frame(data_w$fremdeDL/10^6),d=2)

# electricity and gas
# -------------------
dstat(as.data.frame(data_sg$fremdeDL/10^6),d=2)

# heat and power plants
# ---------------------
dstat(as.data.frame(data_ez$fremdeDL/10^6),d=2)


#================================================================================================
# 2.2 Labor [L in mio EUR]
#================================================================================================


# mixed utilities
# ---------------
dstat(as.data.frame(data_mixed$bruttolohn/10^6),d=2)

# water utilities
# ---------------
dstat(as.data.frame(data_w$bruttolohn/10^6),d=2)

# electricity and gas
# -------------------
dstat(as.data.frame(data_sg$bruttolohn/10^6),d=2)

# heat and power plants
# ---------------------
dstat(as.data.frame(data_ez$bruttolohn/10^6),d=2)



#================================================================================================
# 2.3 Wages [w in EUR]
#================================================================================================


# mixed utilities
# ---------------
dstat(as.data.frame(data_mixed$wage))

# water utilities
# ---------------
dstat(as.data.frame(data_w$wage))

# electricity and gas
# -------------------
dstat(as.data.frame(data_sg$wage))

# heat and power plants
# ---------------------
dstat(as.data.frame(data_ez$wage))



#================================================================================================
# 2.4 Capital [K in mio EUR]
#================================================================================================


# mixed utilities
# ---------------
dstat(as.data.frame(data_mixed$K_adj)/10^6,d=2)

# water utilities
# ---------------
dstat(as.data.frame(data_w$K_adj)/10^6,d=2)

# electricity and gas utilities
# -----------------------------
dstat(as.data.frame(data_sg$K_adj)/10^6,d=2)

# heat and power plants
# ---------------------
dstat(as.data.frame(data_ez$K_adj)/10^6,d=2)



#================================================================================================
# 2.5 Output [Y in Mio EUR]
#================================================================================================


# mixed utilities
# ---------------
dstat(as.data.frame(data_mixed$value_added3)/10^6,d=2)

# water utilities
# ---------------
dstat(as.data.frame(data_w$value_added3)/10^6,d=2)

# electricity and gas utilities
# -----------------------------
dstat(as.data.frame(data_sg$value_added3)/10^6,d=2)

# heat and power plants
# ---------------------
dstat(as.data.frame(data_ez$value_added3)/10^6,d=2)



#================================================================================================
# 3) Descriptive statistics for organisational variable (Figures 1 to 3 in the paper)
#================================================================================================

#================================================================================================
# 3.1 Mixed ownership
#================================================================================================


# mixed utilities
# ---------------
addmargins(table(data_mixed$eigentuemer2,data_mixed$Jahr,useNA="ifany"))

# water utilities
# ---------------
addmargins(table(data_w$eigentuemer2,data_w$Jahr,useNA="ifany"))

# electricity and gas
# -------------------
addmargins(table(data_sg$eigentuemer2,data_sg$Jahr,useNA="ifany"))

# heat and power plants
# ---------------------
addmargins(table(data_ez$eigentuemer2,data_ez$Jahr,useNA="ifany"))



#================================================================================================
# 3.2 Legal form
#================================================================================================


# mixed utilities
# ---------------
addmargins(table(data_mixed$status,data_mixed$Jahr,useNA="ifany"))


# water utilities
# ---------------
addmargins(table(data_w$status,data_w$Jahr,useNA="ifany"))


# electricity and gas
# -------------------
addmargins(table(data_sg$status,data_sg$Jahr,useNA="ifany"))


# heat and power plants
# ---------------------
addmargins(table(data_ez$status,data_ez$Jahr,useNA="ifany"))



#================================================================================================
# 3.3 Outsourcing
#================================================================================================

#================================================================================================
# 3.3.1 Outsourcing intensity: services
#================================================================================================

# mixed utilities
# ---------------
dstat(data_mixed$shareF,d=2)

# water utilities
# ---------------
dstat(data_w$shareF,d=2)

# electricity and gas
# -------------------
dstat(data_sg$shareF,d=2)

# heat and power plants
# ---------------------
dstat(data_ez$shareF,d=2)



#================================================================================================
# 3.3.2 Outsourcing intensity: production
#================================================================================================

# mixed utilities
# ---------------
# number of utilities that do not outsource
round(dstat(data_mixed$shareFEW)[,13]/dstat(data_mixed$shareFEW)[,12],2)

# outsourcing intensity of those that do outsource
data_mixed0 <- subset(data_mixed,shareFEW>0)
dstat(data_mixed0$shareFEW,d=2)


# water utilities
# ---------------
# number of utilities that do not outsource
round(dstat(data_w$shareFEW)[,13]/dstat(data_w$shareFEW)[,12],2)


# outsourcing intensity of those that do outsource
data_w0 <- subset(data_w,shareFEW>0)
dstat(data_w0$shareFEW,d=2)



# electricity and gas
# -------------------
# number of utilities that do not outsource
round(dstat(data_sg$shareFEW)[,13]/dstat(data_sg$shareFEW)[,12],2)

# outsourcing intensity of those that do outsource
data_sg0 <- subset(data_sg,shareFEW>0)
dstat(data_sg0$shareFEW,d=2)



# heat and power plants
# ---------------------
# number of utilities that do not outsource
round(dstat(data_ez$shareFEW)[,13]/dstat(data_ez$shareFEW)[,12],2)

# outsourcing intensity of those that do outsource
data_ez0 <- subset(data_ez,shareFEW>0)
dstat(data_ez0$shareFEW,d=2)


#================================================================================================
# 4) production environment: settlement structure (Table 6 in the paper)
#================================================================================================


# all utilities
# ---------------
addmargins(table(data_p$Siedlung,data_p$Jahr,useNA="ifany"))



#================================================================================================
date()
#===========================End of file==========================================================