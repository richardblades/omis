#'------------------------------------------------------------------------------------------
#' 	
#'  d a t a T i m e . R 
#'
#'  Plot user selected variables against time
#'
#' 	@param data: 		(Selected variables)
#'  @param geography:	("nuts1", "nuts3", "nuts4"),
#' 	@param options: 	("quarter", "overlay")
#'  
#'------------------------------------------------------------------------------------------
dataTime <- function (
	data,
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
#   Reformat quarterEnd as a date(YYYY-MM-DD) and year(YYYY).
#
#   strptime() takes a character vector and makes a date-time object of class
#	POSIXlt/POSIXct.
#	strftime() takes a date-time object of class POSIXlt/POSIXct and makes a character
#	vector.
#-------------------------------------------------------------------------------------------
	reformatQuarterEnd <- function(dfQuarter) {
		monthDays28 <- c("Feb")
		monthDays30 <- c("Apr", "Jun", "Sep", "Nov")
		for (i in 1:nrow(dfQuarter)) {
			mmm <- substr(dfQuarter[i, "quarterEnd"], 1, 3)
			yy  <- substr(dfQuarter[i, "quarterEnd"], 5, 6)
			if 			(mmm %in% monthDays28) {
				dd <- c("28")
			} else if 	(mmm %in% monthDays30) {
				dd <- c("30")
			} else {
				dd <- c("31")
			}
			dfQuarter[i, "date"] <- format(as.Date(paste0(dd, mmm, yy), "%d%b%y"),
										"%Y-%m-%d")
		}
		dfQuarter <- subset(dfQuarter, select=-c(quarterEnd))
		return(dfQuarter)
	}

#-------------------------------------------------------------------------------------------
# 	
# 	Load annual data object for given geography. 	
#
#-------------------------------------------------------------------------------------------		
 	systemFile <- system.file("data", package="omis")
 	rdaFile <- paste0(systemFile, "/", geography, "Year.rda")
  	load(rdaFile)
  	dfName <- paste0(geography, "Year")
 	df <- get(dfName)
	
#-------------------------------------------------------------------------------------------
# 	Generate date and keep columns of interest.	
#-------------------------------------------------------------------------------------------		
	df$date <- as.Date(paste0(df$year, "-01-01"), "%Y-%m-%d")
	df <- df[, colnames(df) %in% c("date", data)] 
	
#-------------------------------------------------------------------------------------------
# 	If user has requested quarterly data then we need to remove the correseponding variables
#	from the annual file.
#-------------------------------------------------------------------------------------------		
	if  ("quarter" %in% options) {
		quarterlyVars <- c("assocProf", "adminSec", "caringLeisure", "femaleEmp",
					"femaleSelfEmp", "femaleUnemp", "maleEmp", "maleSelfEmp", "maleUnemp",
					"manDir", "processPlant", "profOcc", "retired", "salesCust",
					"skillTrad", "student")
		df <- df[, ! (colnames(df) %in% quarterlyVars), drop=FALSE]
#-------------------------------------------------------------------------------------------		
#   Note, "drop=FALSE" is needed as the resultant may hold a single column (that of "date")
#   in which case R will coercse it to the lowest possible structure capable of holding the
#   data, specifically a vector. The issue then arises that as a vector "df" cannot then be
#   merged with "dfQuarter" later on. Refer to ?"[" for detailed explanation. 
#-------------------------------------------------------------------------------------------
	}
	
#-------------------------------------------------------------------------------------------
# 	
# 	If requested, load quarterly data and merge into the annual.	
#
#-------------------------------------------------------------------------------------------		
	if  ("quarter" %in% options) {
	    if  (geography == "nuts1" ) {	    
			rdaFile       <- paste0(systemFile, "/", geography, "QuarterEmp.rda")
			load(rdaFile)
			dfName        <- paste0(geography, "QuarterEmp")
			dfQuarterEmp  <- get(dfName)
			dfQuarterEmp  <- dfQuarterEmp[, 
							colnames(dfQuarterEmp) %in% c(data, "quarterEnd"), drop=FALSE] 
			dfQuarterEmp  <- reformatQuarterEnd(dfQuarterEmp)
		
			rdaFile       <- paste0(systemFile, "/", geography, "QuarterProf.rda")
			load(rdaFile)
			dfName <- paste0(geography, "QuarterProf")
			dfQuarterProf <- get(dfName)
			dfQuarterProf <- dfQuarterProf[,
							colnames(dfQuarterProf) %in% c(data, "quarterEnd"), drop=FALSE] 
			dfQuarterProf <- reformatQuarterEnd(dfQuarterProf)
		
			rdaFile       <- paste0(systemFile, "/", geography, "QuarterSIC.rda")
			load(rdaFile)
			dfName <- paste0(geography, "QuarterSIC")
			dfQuarterSIC  <- get(dfName)
			dfQuarterSIC$quarterEnd <- dfQuarterSIC$quarterSIC   # Establish commonality
			dfQuarterSIC  <- dfQuarterSIC[,
							colnames(dfQuarterSIC) %in% c(data, "quarterEnd"), drop=FALSE] 
			dfQuarterSIC  <- reformatQuarterEnd(dfQuarterSIC)
	    
	        dfQuarter     <- merge(dfQuarterEmp, dfQuarterProf, by=c("date"), all=TRUE)
 		    dfQuarter     <- merge(dfQuarter, dfQuarterSIC, by=c("date"), all=TRUE)
	    } else {
			rdaFile       <- paste0(systemFile, "/", geography, "Quarter.rda")
			load(rdaFile)
			dfName        <- paste0(geography, "Quarter")
			dfQuarter     <- get(dfName)
			dfQuarter     <- dfQuarter[, 
							colnames(dfQuarter) %in% c(data, "quarterEnd"), drop=FALSE] 
			dfQuarter     <- reformatQuarterEnd(dfQuarter)
		}
	    df <- merge(df, dfQuarter, by=c("date"), sort=TRUE, all=TRUE)	
	}

	
	
#-------------------------------------------------------------------------------------------
# 	
#	Reshape data to suit ggplot function
# 	
#-------------------------------------------------------------------------------------------
	columnSubset <- c("date", data)
	dfWide <- subset(df, select=columnSubset)

#-------------------------------------------------------------------------------------------
# 	Remove rows where all columns bar "date" are set to NA
#-------------------------------------------------------------------------------------------
	dfWide <- dfWide[rowSums(is.na(dfWide)) != (ncol(dfWide)-1), ]
	
#-------------------------------------------------------------------------------------------
# 	For the partially or full rows that remain set NA to 0, thereby extending the analysis.
#-------------------------------------------------------------------------------------------
#	dfWide[is.na(dfWide)] <- 0

#-------------------------------------------------------------------------------------------
# 	List of column headings list excluding first column (which is date)
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
# 	Remove rows where value is NA
#-------------------------------------------------------------------------------------------
  	dfLong <- dfLong[!(is.na(dfLong$value)), ]
  	
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

	p <- ggplot(dfLong, aes(x=date, y=value, colour=variable))
	
	p <- p + theme(axis.text=element_text(size=14)) 				# change axis labels
	p <- p + theme(axis.text.x=element_text(angle=90, hjust=1)) 	# rotate text labels
	p <- p + theme(axis.title=element_text(size=14)) 				# change axis titles
	p <- p + theme(text=element_text(size=14)) 		  # change all text except geom_text
	p <- p + theme(legend.text=element_text(size=14)) 				  # legend text size
	
	p <- p + geom_point(shape=21, size=2)
	
	p <- p + geom_line()
	
  	if  ("overlay" %in% options) {
  	    p <- p + scale_y_continuous(breaks=pretty_breaks(n=10)) 
  	} else {
		p <- p + facet_wrap( ~ variable, scales="free", ncol=1)
  		p <- p + theme(strip.background = element_blank()) 	   # remove strip background
  		p <- p + theme(strip.text = element_blank()) 		         # remove strip text
  		p <- p + scale_y_continuous(breaks=pretty_breaks(n=8)) 
  	}
	p <- p + scale_x_date(breaks=dateBreaks, labels=date_format("%Y"))		# Use breaks
	
#-------------------------------------------------------------------------------------------
#	Overlay shaded box depicting time based event
#-------------------------------------------------------------------------------------------
#    p <- p + annotate("rect", xmin=as.Date("2012-01-01"), xmax=as.Date(Sys.Date()),
#               ymin=min(dfLong$value), ymax=max(dfLong$value), alpha=.1, fill="black")
	
#-------------------------------------------------------------------------------------------
# 	Let's go...
#-------------------------------------------------------------------------------------------
	print(p)
}