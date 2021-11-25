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
#				1) Preparation                                              
#================================================================================================

#==============================================================================

# 1. Set up R

#==============================================================================



# clean memory 

#--------------

rm(list = ls())



# Use fixed notation instead of exponential notation

#---------------------------------------------------
options(scipen = 999)





#==============================================================================

# 2. Define working environment

#==============================================================================




# The variable RDC describes, where the code is executed
:
#--------------------------------------------------------
#	--- 0: own computer

#	--- 1: remote access to original data set at RDC


 
RDC <- 1  


# Working environment 0: own computer

# -----------------------------------



if (RDC == 0)  {
  
	Path1   <- "" 
  
	Path2   <- "" 
  
	Path3   <- "" 
  
	.libPaths("")

 


# Working environment 1: remote access to original data set at RDC
# ----------------------------

------------------------------------

if (RDC == 1)  {
  
	Path1   <- "" 
  
	Path2   <- "" 
  
	Path3   <- "" 
  
	.libPaths("")





#==============================================================================

# 3. Call different R codes

#==============================================================================



# start the log file

#---------------------

sink(paste(outputpfad, "/", outputname, ".log", sep = ""), append = FALSE, type = c("output", "message"), split = TRUE)

  


# execute R codes

#-----------------

source(paste(syntaxpfad, "/", syntaxname1, ".r", sep = ""), echo = TRUE, max.deparse.length = 99999)

source(paste(syntaxpfad, "/", syntaxname2, ".r", sep = ""), echo = TRUE, max.deparse.length = 99999)

  


# close the log file

#-------------------

sink()
  

#==============================================================================

# End of file

#==============================================================================