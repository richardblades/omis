#'------------------------------------------------------------------------------------------
#'
#' 	u p l o a d C S V . R 
#'
#' 	Read, verify and save CSV file as .rda. 
#'
#' 	@param csvFileName 		:
#' 	@param csvFilePrefix 	:
#' 	@param csvHeader 		: "checked" or "unchecked"
#' 	@return dfCSV 			: data frame object
#'			
#'------------------------------------------------------------------------------------------
uploadCSV <- function(
	csvFileName,
	csvFilePrefix,
	csvHeader
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
# 	
#  	Verify CSV file type
#
#-------------------------------------------------------------------------------------------
  	if 	(!grepl(".csv$", csvFileName)) {
    	stop("Uploaded CSV file must be of type .csv<br>")
  	}
  	if 	(substring(tolower(csvFileName), nchar(csvFileName)-3) != ".csv") {
    	stop("Uploaded CSV file must end with .csv<br>")
  	}
  	
  	
  	
#-------------------------------------------------------------------------------------------
# 	
# 	Read in CSV file on to server but don't save as yet
#
#-------------------------------------------------------------------------------------------
  	if 	(csvHeader == "checked") {
  		dfCSV <- read.csv(csvFileName, stringsAsFactors=FALSE, header=TRUE,  sep=",")
  	} else {
  		dfCSV <- read.csv(csvFileName, stringsAsFactors=FALSE, header=FALSE, sep=",")
  	}
  	


#'------------------------------------------------------------------------------------------
#'
#' 	Save R Data object - having first provided a summary. 
#'
#' 	R ".rda" used rather than "rds". rds is by far the most convenient to use but alas
#'  R packages do not recognise them as a fully-fledged component. Consequently, OpenCPU
#' 	API ignores their presence.
#' 
#'------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------
#	Override defaults 
#-------------------------------------------------------------------------------------------
#  	options(max.print=999);
#  	options(width=120);

#-------------------------------------------------------------------------------------------
#	Provide summary
#-------------------------------------------------------------------------------------------
 	str(dfCSV)
# 	str(tail(dfCSV))
#  	print(summary(dfCSV))
#  	invisible()

#-------------------------------------------------------------------------------------------
#	Save to external RData file
#-------------------------------------------------------------------------------------------
 	systemFile <- system.file("data", package="omis")
  	filePath <- paste0(systemFile, "/", csvFilePrefix, ".rda")
	assign(csvFilePrefix, dfCSV)
	save(list=csvFilePrefix, file=filePath)



#-------------------------------------------------------------------------------------------
#	Return dfCSV object so that it may be reformatted for export.
#-------------------------------------------------------------------------------------------
		return(dfCSV)
}