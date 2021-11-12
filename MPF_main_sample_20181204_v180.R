#================================================================================================
#
# author: Caroline Stiel, DIW Berlin (German Institute for Economic Research)
#
#================================================================================================
#
# Title: Corporatization, Remunicipalization, and Outsourcing: Public Firm Performance after  
#        Reorganization
# ------------------------------------------------------------------------------------------------
#
#
# file structure
# --------------
# part 1: sample construction 
# part 2: data descriptives
# part 3: estimation (MPF_main_sample_20181204_v180.R, this file) 
#
#
#
#================================================================================================


#================================================================================================
#				0) Preparation                                              
#================================================================================================


# Clean memory
# ------------ 
rm(list=ls())

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
library(micEconCES)
library(lmtest)
library(readstata13)


#================================================================================================
# 0.2 Load data
#================================================================================================


# load data set
# -------------
data <- read.dta(file.path(Pfad1,"data_public_single_final_cs_v3.dta"))


class(data)
dim(data)


# Use fixed notation instead of exponential notation
# --------------------------------------------------
options(scipen=999)



#================================================================================================
# 0.3 Define functions to be used in the analysis              	        
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
# Function 'gmm_moment_condition'                                                                    
# ----------------------------- 
# This functon, describing the optimisation routine, builds the core of the ACF algorithm. It
# minimises the objective function (the moment conditions), while the equation from the Markov process
# and those calculating TFP are constraints in the optimisation process. 
#
# In total, there are 5 constraints:
#                                                           
# (1) omega_it = Phi_it - Inputs_it * betas                                                        
# (2) omega_it-1 = Phi_it-1 - Inputs_it-1 * betas                                                  
# (3) omega_pol = 1 + omega_it-1 + (omega_it-1)^2 + (omega_it-1)^3 + controls            
# (4) g_p = inv(omega_pol'*omega_pol)*omega_pol'*omega                                             
# (5) innovation_it = omega_it - omega_pol*g_p                                                     
#                                                                                                  
# gmm_moment condition: The innovation in productivity (v_it), which forms the moment
# condition together with the betas, is calculated in constraint (5). After that, the 
# objective function of the optimisation process, the sample moment condition, is 
# calculated in 'moment_condition'. The output is a scalar that the optimisation
# routine seeks to minimise.
#================================================================================================                                                          

gmm_moment_condition <- function(betas){
  omega <<- data_gmm$Phi - Inputs_gmm%*%betas -Inputs_fixed%*%betas_fixed
  lag_omega <<- data_gmm$lag_Phi-lag_Inputs_gmm%*%betas - lag_Inputs_fixed%*%betas_fixed
  omega_pol <<- cbind(rep(1,n),lag_omega,lag_omega^2,lag_omega^3
                      ,data_gmm$lag1_privlaw
                      ,data_gmm$lag1_privlaw*data_gmm$lag1_eigentuemer2
                      ,data_gmm$shareF,data_gmm$shareFEW
                      #,data_gmm$shareF*data_gmm$shareFEW
                      #,data_gmm$shareF*data_gmm$lag1_privlaw*data_gmm$lag1_eigentuemer2
                      #,data_gmm$shareFEW*data_gmm$lag1_privlaw*data_gmm$lag1_eigentuemer2
                      #,data_gmm$shareF*data_gmm$lag1_privlaw
                      #,data_gmm$shareFEW*data_gmm$lag1_privlaw
                      )
  AR1 <<- lm(omega ~ lag_omega + I(lag_omega^2) + I(lag_omega^3) 
             + data_gmm$lag1_privlaw 
             + I(data_gmm$lag1_privlaw*data_gmm$lag1_eigentuemer2)
             + data_gmm$shareF + data_gmm$shareFEW 
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


#================================================================================================
# Function boot.acf										                                                             
# -----------------
# Part I of the bootstrapping procedure for estimating the standard errors of the production 
# function's parameters.										                                                                 # It contains the full ACF algorithm (first + second stage) and returns the coefficients of the
# second stage.    		   
#================================================================================================

boot.acf <- function(data,indices,method){
  data_boot <<- data[indices,]
  invisible(capture.output(data_bp <<- pdata.frame(data_boot, index=c("id","year"))))
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
  data_bp$lag1_privlaw <- lag(data_bp$privlaw)
  data_bp$lag1_eigentuemer2 <- lag(data_bp$eigentuemer2)
  data_bp$lag1_shareF <- lag(data_bp$shareF)
  data_bp$lag2_shareF <- lag(data_bp$shareF,2)
  data_bp$lag1_shareFEW <- lag(data_bp$shareFEW)
  data_gmm <<- subset(data_bp,is.na(lag_Phi)==FALSE)
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
                                 ,lag(data_bp$f_m)*lag(data_bp$k_m)))
  lag_Inputs_gmm <<- na.omit(lag_Inputs)
  Inputs_fixed <<- as.matrix(rep(0,nrow(data_gmm)))
  lag_Inputs_fixed <<- as.matrix(rep(0,nrow(data_gmm)))
  lag_Inputs_fixed <<- na.omit(lag_Inputs_fixed)
  instr <<- cbind(rep(1,nrow(data_bp)),data_bp$l_m,lag(data_bp$f_m),data_bp$k_m,data_bp$l_m^2
                  ,lag(data_bp$f_m)^2,data_bp$k_m^2,data_bp$ga*data_bp$defl_ga
                 ,data_bp$wa*data_bp$defl_wa,lag(data_bp$sa)*lag(data_bp$p_sa_log)
                 ,data_bp$wm_NWG*data_bp$defl_wm_NWG,data_bp$se_gas*data_bp$defl_se_eg
                 ,data_bp$se_oil*data_bp$defl_se_oil
                 ,data_bp$se_EE2*log(100),data_bp$suburban,data_bp$rurald,data_bp$rurals
                 ,data_bp$l_m*lag(data_bp$f_m),data_bp$l_m*data_bp$k_m,lag(data_bp$f_m)*data_bp$k_m)
  instr_gmm <<- na.omit(instr)
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



#=================================================================================================
# Funktion clusterBootSE									                                                         
# ----------------------
# Part II of the bootstrapping procedure for estimating the standard errors of the production 
# function's parameters. 		
# The function draws the observations for the bootstrap sample. It applies clustering, i.e. the
# function does not draw single observations but all observations of a firm. 							                                                         
#
# source: https://diffuseprior.wordpress.com/2013/01/12/the-cluster-bootstrap		        
#================================================================================================

clusterBootSE<-function(data,method,B){
  # Define index variable
  clusters<-unique(data[,"id"])
  # Generate empty matric for storing the ACF coefficients
  sterrs <- matrix(NA, nrow=B, ncol=20)
  # Start sampling
  for(i in 1:B){
    # Sample from firm IDs
    units<-sample(clusters,size=length(clusters),replace=T)
    # In the main sample, identify all observations t of the firm IDs that are part of the
    # current subsample b
    df.bs<-sapply(units,function(x)which(data[,"id"]==x))
    # Draw these observations from the main sample and store them in a new data frame
    df.bs<-data[unlist(df.bs),]
    # Apply ACF algorithm to subsample b and store coefficients in row 'i'. The function uses
    # All observations from subsample b (1:dim(df.bs)[1]) to calculate the ACF coefficients 
    sterrs[i,]<-boot.acf(data=df.bs,method=method,1:dim(df.bs)[1])
    # Tells us about the progress of the job
    cat("\r",i*100/B," % done ");flush.console()
  }
  # Table with the ACF coefficients from the main sample (first column), SE calculated from 
  # the variance of all subsamples 1...B (second column), and t-values (third column)
  val1 <- cbind(boot.acf(data=data,method=method,indices=1:dim(data0)[1]),apply(sterrs,2,sd))
  val <- cbind(val1,val1[,1]/val1[,2])
  colnames(val) <- c("Estimate","Std. Error","t-value")
  cat("\n")
  return(val)
}



#=================================================================================================
# 1) Data preparation                                                       
#=================================================================================================

#=================================================================================================
##  1.1 Inputs and outputs
#=================================================================================================

# Generate new output variable 'value_added3'
# -------------------------------------------
# revenues1 (nicht preisbereinigt) - intermediates (preisbereinigt mit Vorleistungsgüterindex)
data$value_added3 <- data$revenues1 - data$intermediates
cor(data$value_added2,data$value_added3,use="complete.obs")


# Choose only firms with positive values in inputs and outputs (as we will logarithmise inputs and
# outputs)
# ------------------------------------------------------------------------------------------------
data0 <- subset(data, fremdeDL>0 & K_adj>0 & bruttolohn>0 & value_added3>0)
addmargins(table(data0$year))


# Remove constant variables
# -------------------------
data0 <- subset(data0,select=-c(id_ns,id_he1,id_he2,id_he3,id_he4,id_th,id_sh,jab,public
                                ,Lueckenjahre,out_labour,out_lohn,out_int,out_umsatz,out_p_LV
                                ,out_p_EVU,out_p_TK,out_p_SK,out_p_HH,out_p_VG,out_p_BC,out_p_sa))

# Logarithmize inputs and outputs
# -------------------------------
data0$l <- log(data0$bruttolohn)
data0$k <- log(data0$K_adj)
data0$f <- log(data0$fremdeDL)
data0$w <- log(data0$wage)
data0$va <- log(data0$value_added2)
data0$va3 <- log(data0$value_added3)


# Adjust inputs and outputs for the median
# ----------------------------------------
data0$l_m <- log(data0$bruttolohn)-log(median(data0$bruttolohn,na.rm=TRUE))
data0$k_m <- log(data0$K_adj)-log(median(data0$K_adj,na.rm=TRUE))
data0$f_m <- log(data0$fremdeDL)-log(median(data0$fremdeDL,na.rm=TRUE))
data0$w_m <- log(data0$wage)-log(median(data0$wage,na.rm=TRUE))
data0$va_m <- log(data0$value_added2)-log(median(data0$value_added2,na.rm=TRUE))
data0$va3_m <- log(data0$value_added3)-log(median(data0$value_added3,na.rm=TRUE))



#=================================================================================================
# 1.2 sector fixed effects
#=================================================================================================

# Industry fixed effects 
# ----------------------
data0$sa_EVU <- ifelse(is.na(data0$U_ABS_EF1011_sum)==FALSE & data0$U_ABS_EF1011_sum>0,1,0)
data0$sa_TK <- ifelse(is.na(data0$U_ABS_EF1051_sum)==FALSE & data0$U_ABS_EF1051_sum>0,1,0)
data0$sa_SK <- ifelse(is.na(data0$U_ABS_EF1041_sum)==FALSE & data0$U_ABS_EF1041_sum>0,1,0)
data0$wm_NWG <- ifelse((is.na(data0$B_waerme_EF1011a)==FALSE & data0$B_waerme_EF1011a>0) 
                       | (is.na(data0$B_waerme_EF1010)==FALSE & data0$B_waerme_EF1010>0),1,0)
data0$wm_HH <- ifelse(is.na(data0$B_waerme_EF1011b)==FALSE & data0$B_waerme_EF1011b>0,1,0)



# Fuel fixed effects
# ------------------
data0$HETGruppen[is.na(data0$HETGruppen)==TRUE] <- 0
data0$se_hc <- ifelse(data0$HETGruppen==1,1,0)
data0$se_bk <- ifelse(data0$HETGruppen==2,1,0)
data0$se_oil <- ifelse(data0$HETGruppen==3,1,0)
data0$se_gas <- ifelse(data0$HETGruppen==4,1,0)
data0$se_water <- ifelse(data0$HETGruppen==5,1,0)
data0$se_waste <- ifelse(data0$HETGruppen==12,1,0)
data0$se_sonst <- ifelse(data0$HETGruppen>=13,1,0)



# Join fuel categories in case of too few observations
# ----------------------------------------------------
data0$se_bio <- ifelse(data0$HETGruppen==9 | data0$HETGruppen==10,1,0)
data0$se_coal <- ifelse(data0$se_hc==1 | data0$se_bk==1,1,0)
data0$se_EE <- ifelse(data0$HETGruppen==6 | data0$HETGruppen==7 | data0$HETGruppen==8 
                      | data0$HETGruppen==11 ,1,0)
data0$se_EE2 <- ifelse(data0$se_EE==1 | data0$se_water==1 | data0$se_bio==1 | data0$se_sonst==1,1,0)





#=================================================================================================
# 1.3 Prices      
#=================================================================================================


# Logarithmize electricity prices
# -------------------------------
data0$p_sa_log <- log(data0$p_sa*100)
data0$p_TK_log <- log(data0$p_TK*100)
data0$p_SK_log <- log(data0$p_SK*100)
data0$p_EVU_log <- log(data0$p_EVU*100)
data0$p_LV_log <- log(data0$p_LV*100)
data0$p_HH_log <- log(data0$p_HH*100)
data0$p_VG_log <- log(data0$p_VG*100)
data0$p_BC_log <- log(data0$p_BC*100)

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



#=================================================================================================
#  1.4 Organisation variables       
#=================================================================================================

# Recode ownership
# ----------------
# 0: purely public
# 1: private minority
data0$eigentuemer2[data0$eigentuemer2==2] <- 0


# Legal form: Define 5 categories
# -------------------------------
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


# Legal form fixed effects
# ------------------------
data0$Verband <- ifelse(data0$status==1,1,0)
data0$Eigenbetrieb <- ifelse(data0$status==2,1,0)
data0$unlisted <- ifelse(data0$status==3,1,0) 
data0$listed <- ifelse(data0$status==4,1,0)
data0$privlaw <- ifelse(data0$status==3 | data0$status==4,1,0)


# Outsourcing I: Share of external services in total services
# -----------------------------------------------------------
data0$shareF <- data0$fremdeDL/(data0$bruttolohn + data0$fremdeDL)

# Outsourcing II: Share of procured energy and water in total revenue
# -------------------------------------------------------------------
# Umkodieren der NAs-->0 in der Variable UK_Code4501
data0$UK_Code4501[is.na(data0$UK_Code4501)==TRUE] <- 0
data0$shareFEW <- data0$UK_Code4501/data0$revenues1



#=================================================================================================
#  1.5 Other covariates     
#=================================================================================================


# Settlement structure fixed effects
# -----------------------------------
data0$metro <- ifelse(data0$Siedlung==1,1,0)
data0$suburban <- ifelse(data0$Siedlung==2,1,0)
data0$rurald <- ifelse(data0$Siedlung==3,1,0)
data0$rurals <- ifelse(data0$Siedlung==4,1,0)


# Firm size fixed effects
# -----------------------
data0$size_small <- ifelse(data0$revenues1<10000000 & data0$beschaeftigte<=49,1,0)
data0$size_med <- ifelse(data0$size_small==0 
                         & data0$revenues1<50000000 & data0$beschaeftigte<=249,1,0)
data0$size_large <- ifelse(data0$size_small==0 & data0$size_med==0 
                           & (data0$revenues1>=50000000 | data0$beschaeftigte>249),1,0)

summary(as.factor(data0$size_small))
summary(as.factor(data0$size_med))
summary(as.factor(data0$size_large))

# Were all firms sorted into a category?
nrow(subset(data0, size_small==1 | size_med==1 | size_large==1))==nrow(data0)


# Investments intensity
# ---------------------
# Investitionsintensität =  Investitionen/Revenues
data0$inv_int <- data0$investment/data0$revenues1
dstat(as.data.frame(data0$inv_int),d=2)



# Customer structure in the electricity sector
# --------------------------------------------
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


# Customer structure in the heat sector
# -------------------------------------
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


# time fixed effects
# ------------------
data0$t2003 <- ifelse(data0$year==2003,1,0)
data0$t2004 <- ifelse(data0$year==2004,1,0)
data0$t2005 <- ifelse(data0$year==2005,1,0)
data0$t2006 <- ifelse(data0$year==2006,1,0)
data0$t2007 <- ifelse(data0$year==2007,1,0)
data0$t2008 <- ifelse(data0$year==2008,1,0)
data0$t2009 <- ifelse(data0$year==2009,1,0)
data0$t2010 <- ifelse(data0$year==2010,1,0)
data0$t2011 <- ifelse(data0$year==2011,1,0)
data0$t2012 <- ifelse(data0$year==2012,1,0)
data0$t2013 <- ifelse(data0$year==2013,1,0)
data0$t2014 <- ifelse(data0$year==2014,1,0)



#=================================================================================================
#    2) Data cleaning and sample defintion                                      
#=================================================================================================

# Clean
# -----
# Drop all firms without information on legal status, ownership, and settlement structure. Drop
# the few lignite plants as they block the bootstrap.
data0 <- subset(data0,is.na(data0$status)==FALSE 
                & is.na(data0$eigentuemer2)==FALSE
                & is.na(data0$Siedlung)==FALSE
                & se_bk==0)

# Drop firms with inconsistent reports on settlement structure
# ------------------------------------------------------------
data0 <- subset(data0,Siedlung !=5)

# Drop constant columns
# ---------------------
data0 <- subset(data0,select=-c(se_bk))


# Define main sample
# ------------------
# drop pure water firms (analysed separately), pure electricity and gas firms (analysed separately),
# as well as power and heat plants (analysed separately)
data0 <- subset(data0,(wa==1 & sa==0 & se==0 & sn==0 & wm==0 & ga==0)==FALSE
                      & (wa==0 & wm==0 & se==0 & (sa==1 | sn==1 | ga==1))==FALSE
                      & (wa==0 & wm==0 & se==1 & (sa==1 | sn==1 | ga==1))==FALSE
                      & ((se == 1 | wm==1) & ga==0 & wa==0 & sa==0 & sn==0)==FALSE)

data0all <- data0


# Recode as panel data
# --------------------
data_p <- pdata.frame(data0, index=c("id","year"),row.names=FALSE)
pdim(data_p)

data_p_all <- pdata.frame(data0all, index=c("id","year"),row.names=FALSE)
pdim(data_p_all)


# generate time trend
# --------------------
# 2003=1, 2004=2, ...
data_p$t <- as.numeric(factor(data_p$year))
data_p_all$t <- as.numeric(factor(data_p_all$year))


#=================================================================================================
#     3) Reorganisation choice                                         
#=================================================================================================

#=================================================================================================
# 3.1 Outsourcing 1: External services
#=================================================================================================

# Regress the share of external services on the following components:
# shareF = F(size,proximity,wages,innovation,customer structure,technology,corporatisation,
#            ownership)

  
# Basic model with interactions in organisational variables (mixed utilities)
# ---------------------------------------------------------------------------
out1_OLS4 <- lm(shareF ~  t + I(t^2) 
                 # organisation variables
                 + lag(data_p$privlaw)  + I(lag(data_p$privlaw)*lag(data_p$eigentuemer2))
                 + lag(data_p$shareFEW)
                 # firm size and production process
                 + size_med + size_large + lag(data_p$inv_int) + lag(data_p$wage)
                 # industry and customer structure
                 + sn + ga + wa + se_gas + se_oil + se_hc + se_waste + se_bio + se_EE + se_water 
                 + se_sonst
                 # environment
                 + suburban + rurald + rurals + lag(data_p$ShareTK) + lag(data_p$ShareWV) 
                 + lag(data_p$ShareHH) + lag(data_p$ShareWV_w)
                 ,data_p)
summary(out1_OLS4)
 
 

#=================================================================================================
# 3.2 Outsourcing 2: Production
#=================================================================================================
 
# Regress the share of procured energy and water on the following components:
# shareF = F(size,proximity,wages,innovation,customer structure,technology,corporatisation,
#            ownership) 

# Grundmodell mit Interaktionen in Controls in t-1 (mixed utilities)
# ------------------------------------------------------------------
out2_OLS4 <- lm(shareFEW ~  t + I(t^2)
                # organisation variale
                + lag(data_p$privlaw)  + I(lag(data_p$privlaw)*lag(data_p$eigentuemer2))
                + lag(data_p$shareF)
                # firm size and production process
                + size_med + size_large + lag(data_p$inv_int) + lag(data_p$wage)
                # industry and customer structure
                + sn + ga + wa + se_gas + se_oil + se_hc + se_waste + se_bio + se_EE + se_water 
                + se_sonst
                # environment
                + suburban + rurald + rurals + lag(data_p$ShareTK) + lag(data_p$ShareWV) 
                + lag(data_p$ShareHH) + lag(data_p$ShareWV_w)
                ,data_p)
summary(out2_OLS4)


#=================================================================================================
# 4) Structural estimation				    	
#=================================================================================================


#=================================================================================================
# 4.1 base model with OLS                                            
#=================================================================================================

# Model production function as a translog function:

# Output: value added (revenues - materials - energy (own use) - taxes)
# Inputs: labour (wage bill), external services, capital

# This obtains the following production function:
# value-added_it = beta_l * l_it + beta_s * s_it + beta_k * k_it  + beta_ll * l_it^2 
#                  + beta_ss * s_it^2  + beta_kk * k_it^2 + 0.5 * beta_ls * lit * s_it 
#                  + 0.5 * b_lk * l_it * k_it + 0.5 * b_ks * k_it * s_it + w_it + u_it

# Add industry and settlement fixed effects (reference category: metropolitain areas):
# value-added_it = beta_l * l_it + beta_s * s_it + beta_k * k_it  + beta_ll * l_it^2 
#                  + beta_ss * s_it^2  + beta_kk * k_it^2 + 0.5 * beta_ls * lit * s_it 
#                  + 0.5 * b_lk * l_it * k_it + 0.5 * b_ks * k_it * s_it
#                  + gamma_1* sa_it + gamma_2 * sn_it + gamma_3 * se_it + gamma_4 * wa_it 
#                  + gamma_5 * wm_it + gamma_6 * ga_it + + gamma_7 * suburban_it 
#                  + gamma_8 * rurald_it + + gamma_8 * rurals_it
#                  + w_it + u_it


first_stage_OLS_m2 <- lm(va3_m ~
                        # production function inputs
                        l_m + f_m + k_m + I(0.5*l_m^2) +  I(0.5*f_m^2) + I(0.5*k_m^2) 
                         + l_m:f_m + l_m:k_m + f_m:k_m
                         # sector fixed effects
                         + I(ga*defl_ga) + I(wa*defl_wa) + I(sa*p_sa_log) + I(wm_NWG*defl_wm_NWG) 
                         # fuel types
                         + I(se_gas*defl_se_eg) + I(se_oil*defl_se_oil)
                         + I(se_EE2*log(100))
                         # settlement fixed effects
                         + suburban + rurald + rurals
                         ,data_p)
summary(first_stage_OLS_m2)


# Store coefficients
betas_basic_m <- as.vector(first_stage_OLS_m2$coefficients)



#=================================================================================================
# 4.2 First-stage estimation (OLS)                         
#=================================================================================================

# First stage OLS estimation in ACF (2005). Eliminates error u_it.

# Production function 
# -------------------
first_stage_m <- lm(va3_m ~
                    # production function inputs
                    l_m + f_m + k_m + I(0.5*l_m^2) +  I(0.5*f_m^2) + I(0.5*k_m^2) 
                    + l_m:f_m + l_m:k_m + f_m:k_m
                    # industry fixed effects
                    + I(ga*defl_ga) + I(wa*defl_wa) + I(sa*p_sa_log) + I(wm_NWG*defl_wm_NWG) 
                    # fuel types
                    + I(se_gas*defl_se_eg) + I(se_oil*defl_se_oil)
                    + I(se_EE2*log(100))
                    # settlement fixed effects
                    + suburban + rurald + rurals
                    # proxy terms
                    + l_m:k_m:f_m 
                    + l_m:I(f_m^2) + f_m:I(k_m^2) + l_m:I(k_m^2) + f_m:I(l_m^2) + k_m:I(f_m^2) 
                    + k_m:I(l_m^2) 
                    + I(l_m^2):I(k_m^2) + I(l_m^2):I(f_m^2) + I(f_m^2):I(k_m^2) 
                    + k_m:I(l_m^2):f_m + l_m:I(k_m^2):f_m + l_m:I(f_m^2):k_m  
                    + f_m:I(l_m^2):I(k_m^2) + k_m:I(l_m^2):I(f_m^2) + l_m:I(k_m^2):I(f_m^2)
                    + I(l_m^2):I(f_m^2):I(k_m^2)
                    ,data_p)
summary(first_stage_m)


# store coefficients from the first stage
# ---------------------------------------
betas1 <- as.vector(first_stage_m$coefficients)

# Predict Phi
# -----------
data_p$Phi <- first_stage_m$fitted.values
length(data_p$Phi)
length(which(is.na(data_p$Phi)==FALSE))
cor(data_p$Phi,data_p$va3_m,use="complete.obs")

# Construct Lag-Phi
# -----------------
data_p$lag_Phi <- lag(data_p$Phi)
length(data_p$lag_Phi)
length(which(is.na(data_p$lag_Phi)==FALSE))

# Store residuals for later use
# -----------------------------
data_p$exp_u_it <- exp(first_stage_m$residuals)


#=================================================================================================
# 4.3  Second-stage estimation: Preparing the lags
#=================================================================================================

# Combine all inputs in a matrix (full first-stage sample)
# --------------------------------------------------------
# Note: The order must be identical to that of the OLS estimation's coefficients. 
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


# Generate lag for legal form variable
# ------------------------------------
data_p$lag1_unlisted <- lag(data_p$unlisted)
data_p$lag1_listed <- lag(data_p$listed)
data_p$lag1_privlaw <- lag(data_p$privlaw)


# Generate lag for ownership variable
# -----------------------------------
data_p$lag1_eigentuemer2 <- lag(data_p$eigentuemer2)


# Generate lag for outsourcing variables
# --------------------------------------
data_p$lag1_shareF <- lag(data_p$shareF)
data_p$lag2_shareF <- lag(data_p$shareF,2)
data_p$lag1_shareFEW <- lag(data_p$shareFEW)


# Drop all observations with lag(phi) == missing
# ----------------------------------------------
data_gmm <- subset(data_p,is.na(lag_Phi)==FALSE)
pdim(data_gmm)
n <- nrow(data_gmm)


# Generate input matrix based on second-stage sample (at time t)
# ---------------------------------------------------------------
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

# Generate input matrix based on second-stage sample (at time t-1)
# -----------------------------------------------------------------
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
                                  ,lag(data_p$f_m)*lag(data_p$k_m)))
lag_Inputs_gmm_all <- na.omit(lag_Inputs_all)
dim(lag_Inputs_gmm_all)


# Choose inputs whose coefficients are updated in 2nd stage (at time t)
# ---------------------------------------------------------------------
Inputs_gmm <- Inputs_gmm_all
dim(Inputs_gmm)


# Choose inputs whose coefficients are not updated in the 2nd stage (at time t)
# -----------------------------------------------------------------------------
Inputs_fixed <- as.matrix(rep(0,nrow(data_gmm)))
dim(Inputs_fixed)


# Choose inputs whose coefficients are updated in 2nd stage (at time t-1)
# -----------------------------------------------------------------------
lag_Inputs_gmm <- lag_Inputs_gmm_all
dim(lag_Inputs_gmm)


# Choose inputs whose coefficients are not updated in the 2nd stage (at time t-1)
# -------------------------------------------------------------------------------
lag_Inputs_fixed <- as.matrix(rep(0,nrow(data_gmm)))
lag_Inputs_fixed <- na.omit(lag_Inputs_fixed)
dim(lag_Inputs_fixed)


# Choose instruments for moment function
# --------------------------------------
instr <- cbind(rep(1,nrow(data_p)),data_p$l_m,lag(data_p$f_m),data_p$k_m
               ,data_p$l_m^2,lag(data_p$f_m)^2,data_p$k_m^2
               ,data_p$ga*data_p$defl_ga
               ,data_p$wa*data_p$defl_wa,lag(data_p$sa)*lag(data_p$p_sa_log)
               ,data_p$wm_NWG*data_p$defl_wm_NWG
               ,data_p$se_gas*data_p$defl_se_eg,data_p$se_oil*data_p$defl_se_oil
               ,data_p$se_EE2*log(100)
               ,data_p$suburban,data_p$rurald,data_p$rurals
               ,data_p$l_m*lag(data_p$f_m),data_p$l_m*data_p$k_m,lag(data_p$f_m)*data_p$k_m)
instr_gmm <- na.omit(instr)
dim(instr_gmm)


#=================================================================================================
# 4.4  Second-stage estimation: GMM optimisation     
#=================================================================================================

# The GMM's objective function is the moment condition E[(Z'v)'*(Z'v)]=0.

# Choose starting values
# ----------------------
initial_betas <- betas_basic_m[c(1:20)]
betas_fixed <- 0


# Run GMM optimisation routine
# ----------------------------
optimization <- optimx(par=initial_betas,fn=gmm_moment_condition, method=c("nlminb","nlm"))
print(optimization)


# Choose best optimisation routine (lowest value for objective function)
# ----------------------------------------------------------------------
j <- which.min(optimization$value)


# Store coefficients from the second stage (betas)
# ------------------------------------------------
betas2 <- rbind(optimization$p1[j],optimization$p2[j],optimization$p3[j],optimization$p4[j]
                 ,optimization$p5[j],optimization$p6[j],optimization$p7[j],optimization$p8[j]
                 ,optimization$p9[j],optimization$p10[j],optimization$p11[j],optimization$p12[j]
                 ,optimization$p13[j],optimization$p14[j],optimization$p15[j],optimization$p16[j]
                 ,optimization$p17[j],optimization$p18[j],optimization$p19[j],optimization$p20[j])


# display coefficients
# --------------------
betas2


#=================================================================================================
# 4.5 Bootstrapping the SE              
#=================================================================================================

# In this step, we bootstrap the SE for the coefficients from the second stage.
# We use the function 'ClusterBootSE' (see beginning of script) that draws the B bootstrap 
# subsamples from the full data set and the function 'boot.acf' that summarises the ACF algorithm.

# The function 'clusterBootSE' requires 3 inputs:
# (1) data: The data set from which the B bootstrap samples are to be drawn. It should be the
#     main data set without panel structure as available at the end of section (1).
# (2) method: Choose the numerical approach for the optimisation routine in the ACF algorithm.
#     It should be identical to the one used in section (4.4).
# (3) B: Number of replications. 

# Boostrap the SE
# ---------------
date()
clusterBootSE(data=data0,method=rownames(optimization)[j],B=999)
date()

# Check: The first column should correspond to the coefficients 'beta22' from section (4.4). 


#=================================================================================================
# 5) Results			                               
#=================================================================================================


#=================================================================================================
# 5.1 Calculating productivity
#=================================================================================================


betas_final <- betas2

# productivity values (logs)
# --------------------------
data_p$omega2 <- data_p$Phi - Inputs%*%betas_final
data_gmm$omega2 <- data_gmm$Phi - Inputs_gmm_all%*%betas_final

# productivity values (levels)
# ----------------------------
data_p$omega2e <- exp(data_p$Phi - Inputs%*%betas_final)


#=================================================================================================
# 5.2 Productivity Dispersion
#=================================================================================================

dstat(data_p$omega2e,d=3)
dstat(exp(data_gmm$omega2),d=3)


#=================================================================================================
# 5.3 Output Elasticities                                                     
#=================================================================================================


# Calculate output elasticity for labour
# --------------------------------------
# elasticity_l = b_l + b_ll*l + b_lf*f + b_kl*k
data_p$elasticity_lohn <- (betas_final[2] + betas_final[5]*Inputs[,2] 
                           + betas_final[18]*Inputs[,3] + betas_final[19]*Inputs[,4])


# Calculate output elasticity for external services
# -------------------------------------------------
# elasticity_v = b_f + b_ff*f + b_lf*l + b_kv*k
data_p$elasticity_fdl <- (betas_final[3] + betas_final[6]*Inputs[,3] 
                          + betas_final[18]*Inputs[,2] + betas_final[20]*Inputs[,4])


# Calculate output elasticity for capital
# ---------------------------------------
# elasticity_k = b_k + b_kk*k + b_lk*l + b_kf*f
data_p$elasticity_cap <- (betas_final[4] + betas_final[7]*Inputs[,4] 
                          + betas_final[19]*Inputs[,2] + betas_final[20]*Inputs[,3])

# Calculate returns to scale
# --------------------------
data_p$rts <- data_p$elasticity_lohn + data_p$elasticity_fdl + data_p$elasticity_cap


# Descriptive statistics of the output elasticities and the returns to scale
# --------------------------------------------------------------------------

# per year
# --------
dstat(data_p$elasticity_lohn,d=3)
dstat(data_p$elasticity_fdl,d=3)
dstat(data_p$elasticity_cap,d=3)
dstat(data_p$rts,d=3)

# in total
# --------
dstat(as.data.frame(data_p$elasticity_lohn),d=3)
dstat(as.data.frame(data_p$elasticity_fdl),d=3)
dstat(as.data.frame(data_p$elasticity_cap),d=3)
dstat(as.data.frame(data_p$rts),d=3)

# number of firms with DRS, CRS, IRS
# ----------------------------------
addmargins(table(data_p$year[data_p$rts<1],useNA="ifany",dnn="DRS"))
addmargins(table(data_p$year[data_p$rts==1],useNA="ifany",dnn="CRS"))
addmargins(table(data_p$year[data_p$rts>1],useNA="ifany",dnn="IRS"))



#=================================================================================================
# 6) Link between reorganisation and productivity
#=================================================================================================


#=================================================================================================
# 6.1 Productivity growth (Markov process for productivity)       
#=================================================================================================


# Calculate lag-omega (omega_t-1) for the LOM
# -------------------------------------------
data_gmm$lag_omega2 <- data_gmm$lag_Phi - lag_Inputs_gmm_all%*%betas_final
data_gmm <- pdata.frame(data.frame(data_gmm),index=c("id","year"),row.names=FALSE)
data_gmm <- pdata.frame(data.frame(data_gmm),index=c("id","year"),row.names=FALSE)


# Re-run AR(1) process to obtain LOM coefficients
# ------------------------------------------------
AR1_expost <- plm(omega2 ~ lag_omega2 + I(lag_omega2^2) + I(lag_omega2^3) 
                  + lag1_privlaw
                  + I(lag1_privlaw*lag1_eigentuemer2) 
                  + shareF + shareFEW
                  ,data=data_gmm,model="pooling",effect="time", index=c("id"))
summary(AR1_expost)


# Is autocorrelation a concern (Durbin-Watson test)?
# --------------------------------------------------
dwtest(omega2 ~ lag_omega2 + I(lag_omega2^2) + I(lag_omega2^3) 
       + lag1_privlaw
       + I(lag1_privlaw*lag1_eigentuemer2) 
       + shareF 
       + shareFEW
       ,data=data_gmm)


# Is heteroscedasticity a concern (Breusch-Pagan test)?
# -----------------------------------------------------
bptest(AR1_expost)


# WHITE standard errors robust against heteroscedasticity and autocorrelation
# ---------------------------------------------------------------------------
coeftest(AR1_expost,vcov=vcovHC(AR1_expost,method="arellano",cluster=c("group")))


# Hypothesis tests for mixed ownership
# ------------------------------------
# The influence of mixed ownership cannot be directly inferred from the regression since it 
# constitutes the difference between the coefficient for mixed corporation and the one for corporations
# in full public ownership as we have to exclude the effect of legal form.


# Does mixed ownership (private minority) has a significant influence on productivity growth?
linearHypothesis(AR1_expost
                 ,"I(lag1_privlaw * lag1_eigentuemer2)-lag1_privlaw=0"
                 ,vcov=vcovHC(AR1_expost,method="arellano",cluster=c("group")))


#=================================================================================================
# 6.2 Productivity levels
#=================================================================================================


# Regress productivity on organisation variables to see whether there is a baseline effect from
# organisation.


# Remove false NAs in index variables
# -----------------------------------
data_p <- pdata.frame(as.data.frame(data_p),index=c("id","year"),row.names=FALSE)
data_p <- pdata.frame(as.data.frame(data_p),index=c("id","year"),row.names=FALSE)


explain_pty2 <- plm(omega2 ~ 
                   lag1_privlaw
                   + I(lag1_privlaw*lag1_eigentuemer2) 
                   + shareF + shareFEW 
                   + size_med + size_large
                   +  I(sn*defl_sn) + I(ga*defl_ga) + I(wa*defl_wa) + I(sa*p_sa_log) 
                   + I(wm_HH*defl_wm_HH) + I(wm_NWG*defl_wm_NWG) 
                   + I(se_gas*defl_se_eg) + I(se_oil*defl_se_oil) + I(se_hc*defl_se_hc) 
                   + I(se_waste*log(100)) + I(se_bio*log(100)) 
                   + I(se_EE*log(100)) + I(se_water*log(100)) + I(se_sonst*log(100))
                   + suburban + rurald + rurals,data=data_p,model="pooling",effect="time"
                   ,index=c("id"))
summary(explain_pty2)


# Is heteroscedasticity a concern (Breusch-Pagan test)?
# -----------------------------------------------------
bptest(explain_pty2)


# Is autocorrelation a concern (Durbin-Watson test)?
# --------------------------------------------------
dwtest(omega2 ~ 
       lag1_privlaw
       + I(lag1_privlaw*lag1_eigentuemer2) 
       + shareF + shareFEW 
       + size_med + size_large
       +  I(sn*defl_sn) + I(ga*defl_ga) + I(wa*defl_wa) + I(sa*p_sa_log) 
       + I(wm_HH*defl_wm_HH) + I(wm_NWG*defl_wm_NWG) 
       + I(se_gas*defl_se_eg) + I(se_oil*defl_se_oil) + I(se_hc*defl_se_hc) 
       + I(se_waste*log(100)) + I(se_bio*log(100)) 
       + I(se_EE*log(100)) + I(se_water*log(100)) + I(se_sonst*log(100))
       + suburban + rurald + rurals,data=data_p)


# WHITE standard errors robust against heteroscedasticity and autocorrelation
# ---------------------------------------------------------------------------
coeftest(explain_pty2,vcov=vcovHC(explain_pty2,method="arellano",cluster=c("group")))



# Hypothesis tests for mixed ownership
# ------------------------------------
# The influence of mixed ownership cannot be directly inferred from the regression since it 
# constitutes the difference between the coefficient for mixed corporation and the one for corporations
# in full public ownership as we have to exclude the effect of legal form.

linearHypothesis(explain_pty2
                 ,"I(lag1_privlaw * lag1_eigentuemer2)-lag1_privlaw=0"
                 ,vcov=vcovHC(explain_pty2,method="arellano",cluster=c("group")))


=================================================================================================
date()
========================================== End of file ==========================================