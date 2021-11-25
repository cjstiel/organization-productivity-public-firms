#================================================================================================
#
# author: Caroline Stiel, DIW Berlin (German Institute for Economic Research)
#
#================================================================================================
#
# Title: Remunicipalization, Corporatization, and Outsourcing: Public Firm Performance after  
#        Reorganization
# ------------------------------------------------------------------------------------------------
#
#
#  						MASTER FILE
#
#
# 					calls the different programs
#
#================================================================================================


#================================================================================================
# 1) Set up R-environment
#================================================================================================


# clean memory 
#--------------
rm(list = ls())



# Use fixed notation instead of exponential notation
#---------------------------------------------------
options(scipen = 999)



#================================================================================================
# 2) Define working environment
#================================================================================================


# Variable 'RDC' defines the working environment
#-----------------------------------------------
# 0: own computer
# 1: remote access to original data set at research data centre (RDC)
 
RDC <- 1  

# Working environment 0: own computer
# -----------------------------------
if (RDC == 0)  {
	# Path1: directory where working data is saved
	Path1   <- "" 
	# Path2: directory where original data is stored
	Path2   <- "" 
	# path3: directory where all the programs are saved
	Path3   <- ""
	# Path 4: directory where output and logfiles are saved
	Path4   <- ""
	.libPaths("")


# Working environment 1: remote access to original data set at RDC
# ----------------------------------------------------------------
if (RDC == 1)  {
	Path1   <- "" 
	Path2   <- "" 
	Path3   <- "" 
	Path4	<- ""
	.libPaths("")

	
#================================================================================================
# 3) Run programs
#================================================================================================

# start the log file
#---------------------
sink(paste(Path4, "/Remunicipalization_Corporatization_Outsourcing.log", sep = ""), append = FALSE, type = c("output", "message"), split = TRUE)


# run programs
#--------------
source(paste(Path3, "/01_RCO_sample_construction_v20.R", sep = ""), echo = TRUE, max.deparse.length = 99999)
source(paste(Path3, "/02_RCO_descriptives_v24.R", sep = ""), echo = TRUE, max.deparse.length = 99999)
source(paste(Path3, "/03_RCO_main_sample_v180.R", sep = ""), echo = TRUE, max.deparse.length = 99999)


# close the log file
#-------------------
sink()
  

#================================================================================================
# date ()
#===============================End of file======================================================