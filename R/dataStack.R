#'------------------------------------------------------------------------------------------
#'
#' 	d a t a S t a c k . R
#'
#' 	Plot Group of related variables as a stack against time
#'
#' 	@param dataStack:	("businessCounts", "employment", "incomeResident",
#'						 "incomeWorkplace", "population", "profession", "qualifications",
#'						 "turnover")
#'  @param geography:	(nut1", "nuts3", "nuts4")
#'  @param options:		("oneHundred")
#'
#'------------------------------------------------------------------------------------------
dataStack <- function (
	dataStack,
	geography,
	options  
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
# 	Load data object for given geography.	
#
#-------------------------------------------------------------------------------------------		
 	systemFile <- system.file("data", package="omis")
 	rdaFile <- paste0(systemFile, "/", geography, "Year.rda")
	load(rdaFile)
	dfName <- paste0(geography, "Year")
	df <- get(dfName)	


#-------------------------------------------------------------------------------------------
# 	
#	Identify variables that compose the dataStack.
# 	
#-------------------------------------------------------------------------------------------		
	if  		(dataStack == "businessCounts") {
		variables	<- c("numBus47s07", "numBus55s07", "numBus90s07")
		
	} else if 	(dataStack == "employment") {
		if  (geography == "nuts1") {
			variables	<- c("femaleEmp", "femaleUnemp", "maleEmp", "maleUnemp")
		} else {
			variables	<- c("femaleEmp", "femaleSelfEmp", "maleEmp", "maleSelfEmp")
		}
		
	} else if	(dataStack == "incomeResident") {
		variables 	<- c("femaleResGrossWeek", "maleResGrossWeek")
		
	} else if	(dataStack == "incomeWorkplace") {
		variables 	<- c("femaleWorkGrossWeek", "maleWorkGrossWeek")
		
	} else if	(dataStack == "population") {
#-------------------------------------------------------------------------------------------
# 	Following lines are used to extend population analysis
#-------------------------------------------------------------------------------------------
		df$femaleOther 	<- ifelse (is.na(df$female16to64), df$femalePop,
							df$femalePop - df$female16to64)
		df$maleOther  	<- ifelse (is.na(df$male16to64), df$malePop,
							df$malePop - df$male16to64)
		variables 	<- c("femaleOther", "female16to64", "maleOther", "male16to64")
		
	} else if	(dataStack == "profession") {
		variables 	<- c("adminSec", "assocProf", "caringLeisure", "elementary", "manDir",
						 "processPlant", "profOcc", "retired","salesCust", "skillTrad",
						 "student")
		
	} else if	(dataStack == "qualifications") {
		variables 	<- c("qualNone", "qualNVQ1", "qualNVQ2", "qualNVQ3", "qualNVQ4",
						 "qualOther")
		
	} else if	(dataStack == "turnover") {
		variables 	<- c("turnover47s07", "turnover55s07", "turnover90s07")
	}
	


#-------------------------------------------------------------------------------------------
# 	
#	Reshape data to suit ggplot function
# 	
#-------------------------------------------------------------------------------------------
	columnSubset <- c("year", variables)
	dfWide <- subset(df, select=columnSubset)
	
#-------------------------------------------------------------------------------------------
# 	Remove rows where all columns bar "year" are set to NA
#-------------------------------------------------------------------------------------------
	dfWide <- dfWide[rowSums(is.na(dfWide)) != (ncol(dfWide)-1), ]
	#-------------------------------------------------------------------------------------------
# 	For the partially or full rows that remain set NA to 0, thereby extending the analysis.
#-------------------------------------------------------------------------------------------
	dfWide[is.na(dfWide)] <- 0

#-------------------------------------------------------------------------------------------
# 	List of column headings list excluding first column (which is year)
#-------------------------------------------------------------------------------------------
	columnNames <- names(dfWide)[-1]

	dfLong <- reshape(dfWide, direction="long", timevar="variable",
		varying=list(columnNames))

#-------------------------------------------------------------------------------------------
# 	Rationalise column names
#-------------------------------------------------------------------------------------------
  	names(dfLong)[2] <- "variable"
  	names(dfLong)[3] <- "value"
  	dfLong$variable <- factor(dfLong$variable, labels=columnNames)

#-------------------------------------------------------------------------------------------
# 	Create date variable of class date to facilitate date based scale
#-------------------------------------------------------------------------------------------
	dfLong$date <- as.Date(paste("01Jan",dfLong$year, sep=""),"%d%b%Y")

#-------------------------------------------------------------------------------------------
# 	Specify breaks as a date vector 
#-------------------------------------------------------------------------------------------
	library(scales)
	toYear <- paste0(as.numeric(strftime(max(dfLong$date), "%Y")), "-01-01")
	dateBreaks <- seq(as.Date("1981-01-01"), as.Date(toYear), by="1 year")



#-------------------------------------------------------------------------------------------
# 	
#   Assemble and call ggplot
# 	
#-------------------------------------------------------------------------------------------
	library(ggplot2)
	library(plyr) 											   # For the desc() function

  	p <- ggplot(dfLong, aes(x=date, y=value, order=desc(variable), fill=variable))
  	
	p <- p + theme(axis.text=element_text(size=14)) 				# change axis labels
	p <- p + theme(axis.text.x=element_text(angle=90, hjust=1)) 	# rotate text labels
	p <- p + theme(axis.title=element_text(size=14)) 				# change axis titles
	p <- p + theme(text=element_text(size=14)) 		  # change all text except geom_text
	p <- p + theme(legend.text=element_text(size=14)) 				  # legend text size
  	
#-------------------------------------------------------------------------------------------
# 	Note, postion="stack" is the default for geom_area but added here for clarity. 
#-------------------------------------------------------------------------------------------
  	if  ("oneHundred" %in% options) {
		p <- p + geom_area(colour="black", size=0.3, alpha=0.5, position = "fill")
  	} else {
		p <- p + geom_area(colour="black", size=0.3, alpha=0.5, position = "stack")
	}
	
#-------------------------------------------------------------------------------------------
#   Set axis attributes.
#-------------------------------------------------------------------------------------------
  	if  ("oneHundred" %in% options) {
		p <- p + scale_y_continuous(breaks=pretty_breaks(n=10), labels=percent)
  	} else {
		p <- p + scale_y_continuous(breaks=pretty_breaks(n=10)) 
	}
	p <- p + scale_x_date(breaks=dateBreaks, labels=date_format("%Y"))		# Use breaks
	p <- p + scale_fill_discrete(guide = guide_legend(reverse=TRUE))
		
#-------------------------------------------------------------------------------------------
# 	Let's go...
#-------------------------------------------------------------------------------------------
	print(p)
}
