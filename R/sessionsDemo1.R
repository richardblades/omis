#'------------------------------------------------------------------------------------------
#'
#' 	s e s s i o n s D e m o 1 . R 
#'
#' 	Read and verify CSV file. Hold on OpenCPU server against as session1 object. Provide
#'	simple plot is requested.
#'
#' 	@param csvAddition 	: "checked" or "unchecked"
#' 	@param csvFileName 	:
#' 	@param csvHeader 	: "checked" or "unchecked"
#' 	@param csvPlot 		: "checked" or "unchecked". 
#'	@return 			: multiple argument returnds are NOT supported by OpenCPU.
#'						.../R/.val is directly equivalent to what is returned by the
#'						the R function through the return() statement.
#'			
#'------------------------------------------------------------------------------------------
sessionsDemo1 <- function(
	csvAddition,
	csvFileName,
	csvHeader,
	csvPlot
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
    	stop("Uploaded CSV file must be of type .csv\n")
  	}
  	if 	(substring(tolower(csvFileName), nchar(csvFileName)-3) != ".csv") {
    	stop("Uploaded CSV file must end with .csv\n")
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
  	
  	
  	
#-------------------------------------------------------------------------------------------
# 	
# 	Produce simple plot to illustrate how graphics may be accessed.
#
#-------------------------------------------------------------------------------------------
  	if 	(csvPlot == "checked") {
#-------------------------------------------------------------------------------------------
#   Remove incomplete obs (is.NA) and outliers (2013 value atypically high). 
#-------------------------------------------------------------------------------------------
		df <- na.omit(dfCSV)
		df <- subset(df, year != 2013)
#-------------------------------------------------------------------------------------------
#   Re-scale turnover to enhance readability
#-------------------------------------------------------------------------------------------
		df$turnover47s07 <- df$turnover47s07/1000000
#-------------------------------------------------------------------------------------------
#	  Create date variable of class date to facilitate date based scale
#-------------------------------------------------------------------------------------------
		df$date <- as.Date(paste("01Jan", df$year, sep=""),"%d%b%Y")
		library(scales)
		dateBreaks <- seq(as.Date("1981-01-01"), as.Date("2012-01-01"), by="1 year")
#-------------------------------------------------------------------------------------------
#   Assemble ggplot
#-------------------------------------------------------------------------------------------
		library(ggplot2)
		p <- ggplot(df, aes(x=date, y=turnover47s07))
		p <- p + theme(axis.text=element_text(size=14)) 			# change axis labels
		p <- p + theme(axis.text.x=element_text(angle=90, hjust=1)) # rotate text labels
		p <- p + theme(axis.title=element_text(size=14)) 			# change axis titles
		p <- p + theme(text=element_text(size=14)) 	  # change all text except geom_text
		p <- p + theme(legend.text=element_text(size=14)) 			  # legend text size
    	p <- p + geom_point(shape=22, size=4, colour="darkred", fill="pink")
   		p <- p + geom_smooth(method=lm, level=0.95, colour="grey60", alpha=0.2)
		p <- p + scale_y_continuous(breaks=pretty_breaks(n=10), "turnover £m") 
		p <- p + scale_x_date(breaks=dateBreaks, labels=date_format("%Y"))
		p <- p + ggtitle("Plot of sessionsDemo.csv Upload") 	
		print(p) 
  	}
  	
  	
#-------------------------------------------------------------------------------------------
# 	
# 	If required, generate and return an additional data frame.
#
#	This is used to illustrate how more than one data frame can be returned. Essentially R
#	will only accept a single return arguement. However, this can be circumnavigated by
#	returning a "single arguement" that is infact a list of data items.
#
#	itemsToReturn1 <- list("dfCSV"=dfCSV, "dfAddition"=dfAddition)
#	itemsToReturn2 <- list(dfCSV, dfPlot)
#	
#	itemsToReturn1 can be seen to be a better format than itemsToReturn2 as subsequent
#	references are more readable...
#
#		csv		 <- list$dfCSV
#		addition <- list$dfAddition
#
#-------------------------------------------------------------------------------------------
	if 	(csvAddition == "checked") {
		dfAddition <- data.frame(x=c(300,200,100), y=c(9000,5000,12000))
#-------------------------------------------------------------------------------------------
# 	Define data frame list that is to be returned and later referenced as /R/.val.
#-------------------------------------------------------------------------------------------
		itemsToReturn <- list("dfCSV"=dfCSV, "dfAddition"=dfAddition)
		return(itemsToReturn)
	} else {
		return(dfCSV)
	}
}