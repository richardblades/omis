#'------------------------------------------------------------------------------------------
#'
#' 	s e s s i o n s D e m o 2 . R 
#'
#' 	Save R Data object - having first provided a summary.
#'
#' 	R ".rda" used rather than "rds". rds is by far the most convenient to use but alas
#'  R packages do not recognise them as a fully-fledged component. Consequently, OpenCPU
#' 	API ignores their presence.
#'
#' 	@param csvFilePrefix 	: CSV file name prefix
#' 	@param object 			: OpenCPU session object
#'
#'------------------------------------------------------------------------------------------
sessionsDemo2 <- function(
	csvFilePrefix,
	object
	)
  	{



#-------------------------------------------------------------------------------------------
#
#	Format numbers for readability.
#
#-------------------------------------------------------------------------------------------
	formatNumber <- function (number, decimalPlaces) {
  		if (is.character(number)) {
    		number <- as.numeric(number)
  		}
    	prettyNum(round(number, decimalPlaces), big.mark=",", preserve.width="none",
            trim=TRUE, nsmall=decimalPlaces)
	}



#-------------------------------------------------------------------------------------------
#	Override defaults
#-------------------------------------------------------------------------------------------
#  	options(max.print=999);
#  	options(width=120);

#-------------------------------------------------------------------------------------------
#	Provide summary
#-------------------------------------------------------------------------------------------
 	str(object)
# 	str(tail(object))
#  	print(summary(object))
#  	invisible()

#-------------------------------------------------------------------------------------------
#	
#	Save to external RData file. 
#
#	Add suffix of "Saved" to differentiate from original sessionsDemo file. Note there are
#	many ways of addressing/pointing to the desired file path within the /data/ folder. 
#
#	(1)	filePath <- paste0("~/R/omis/data/", csvFilePrefixSaved, ".rda")
# 	(2)	filePath <- paste0("data/", csvFilePrefixSaved, ".rda") 	
#  	(3)	filePath <- paste0(systemFile, "/", csvFilePrefixSaved, ".rda")
# 
#	Methods (1) and (2) are geared towards an interactive RStudio session and more crucially
#	actually reference the pre-build "development" package copy. Consequently, they are
#	missleading and of no benefit within the Production environment. Hence, method (3) must
#	be employed if the post-build production package is to be engaged. Finally, files held
#	within the /data/ directory can only be referenced in a direct manner id "DataLazy" is
#	set to "false" with the DESCRIPTION element. 
#
#-------------------------------------------------------------------------------------------
 	systemFile <- system.file("data", package="omis")
	csvFilePrefixSaved <- paste0(csvFilePrefix, "Saved")
  	filePath <- paste0(systemFile, "/", csvFilePrefixSaved, ".rda")
  	
#-------------------------------------------------------------------------------------------
#	Debug call
#-------------------------------------------------------------------------------------------
# 	user 	<- Sys.getenv("LOGNAME")
#	message <- paste("systemFile = ", systemFile, "***", "user = ", user, "***")
#  	stop(message)

	assign(csvFilePrefixSaved, object)
	save(list=csvFilePrefixSaved, file=filePath)
}
