#'------------------------------------------------------------------------------------------
#'
#'  n u t s 4 O c c u p a n c y . R
#'
#'  NUTS-4 | Plot occupancy against time
#'
#'  @param ???:
#'
#'------------------------------------------------------------------------------------------
nuts4Occupancy <- function (
	geography 		= c("nuts4"),
	metric 			= c("max", "mean", "p95"),
	occupancy 		= c("srvBed", "srvRoom", "scBed", "scUnit"),
	percentageScale	= c("fixed", "variable"),
   	profile         = c("yearByMonth", "trendMonthYear", "trendYear"),
	rangeFrom 		= c("2004-01-01"), 			    
	rangeTo 		= c("2015-12-31") 				  
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
 	rdaFile <- paste0(systemFile, "/", geography, "MonthOccupancy.rda")
	load(rdaFile)
	dfName <- paste0(geography, "MonthOccupancy")
	df <- get(dfName)	



#-------------------------------------------------------------------------------------------
#
# 	Apply user specified selection criteria.
#
#-------------------------------------------------------------------------------------------
	df$date <-	as.Date(paste0("01", substr(df$month, 1, 3) , df$year), "%d%b%Y")
	
#-------------------------------------------------------------------------------------------
#   Select data within given date range
#-------------------------------------------------------------------------------------------
	dfRange <- subset(df, date >= rangeFrom & date <= rangeTo)



#-------------------------------------------------------------------------------------------
#
#	Set "summariseBy" according to "profile" and generate supporting summary variables
#
#   strptime() takes a character vector and makes a date-time object of class
#	POSIXlt/POSIXct.
#	strftime() takes a date-time object of class POSIXlt/POSIXct and makes a character
#	vector.
#-------------------------------------------------------------------------------------------
	if 			(profile == "yearByMonth") {
	    summariseBy         <- c("monthNum")
	    dfRange$monthNum	<- strftime(dfRange$date,"%m") # (01... 12)
	    xlab                <- c("summarised by month")
	} else if 	(profile == "trendMonthYear") {
	    summariseBy         <- c("yearMonthNum")
	    dfRange$yearMonthNum<- strftime(dfRange$date,"%Y-%m") # (2016-01... 2016-12)
	    xlab                <- c("summarised by year and month")
	} else if 	(profile == "trendYear") {
	    summariseBy         <- c("year")
	    xlab                <- c("summarised by year")
	}



#-------------------------------------------------------------------------------------------
#
#	Generate metrics for given period.
#
#	Note "na.rm=TRUE" excludes "NA" from analysis and ensures result is not set to NA.
#
#-------------------------------------------------------------------------------------------
	library(plyr)

	if 			(metric == "max") 	{
		dfSummary <- ddply(dfRange, summariseBy, summarize,
	        srvBed  = max(srvBed, 	na.rm=TRUE),
	        srvRoom = max(srvRoom, 	na.rm=TRUE),
	        scBed 	= max(scBed, 	na.rm=TRUE),
	        scUnit 	= max(scUnit, 	na.rm=TRUE))
		ylab <- c("maximum occupancy % / month")
	} else if 	(metric == "mean") 	{
		dfSummary <- ddply(dfRange, summariseBy, summarize,
	        srvBed  = mean(srvBed, 	na.rm=TRUE),
	        srvRoom = mean(srvRoom, na.rm=TRUE),
	        scBed 	= mean(scBed, 	na.rm=TRUE),
	        scUnit 	= mean(scUnit, 	na.rm=TRUE))
		ylab <- c("mean occupancy % / month")
	} else if 	(metric == "p95") 	{
		dfSummary <- ddply(dfRange, summariseBy, summarize,
	        srvBed 	= quantile(srvBed, 	probs=0.95, na.rm=TRUE),
	        srvRoom = quantile(srvRoom, probs=0.95, na.rm=TRUE),
	        scBed 	= quantile(scBed, 	probs=0.95, na.rm=TRUE),
	        scUnit 	= quantile(scUnit, 	probs=0.95, na.rm=TRUE))
		ylab <- c("95th percentile occupancy % / month")
	}
	
#-------------------------------------------------------------------------------------------
#	Correct ylab where profile is trendMonthYear as metric is irrelevent.
#-------------------------------------------------------------------------------------------
	if (profile == "trendMonthYear") {
		ylab <- c("actual occupancy % / month")
	}



#-------------------------------------------------------------------------------------------
#
#	Reshape data to suit ggplot function
#
#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------
# 	Trim to only those occupancy variables requested
#-------------------------------------------------------------------------------------------
	dfOccupancy <- as.data.frame(occupancy)
	columnSubset <- c(summariseBy)
	for (i in 1:nrow(dfOccupancy)) {
#	    columnName <- paste0(dfOccupancy[i,1], metric)
	    columnName <- paste0(dfOccupancy[i,1])
  		columnSubset <- cbind(columnSubset, columnName)
	}
#   dfWide <- dfSummary[ ,names(dfSummary) %in% columnSubset]
    dfWide <- subset(dfSummary, select=columnSubset)

#-------------------------------------------------------------------------------------------
# 	Remove rows where all columns bar "summariseBy" are set to NA
#-------------------------------------------------------------------------------------------
	dfWide <- dfWide[rowSums(is.na(dfWide)) != (ncol(dfWide)-1), ]

#-------------------------------------------------------------------------------------------
# 	For the partially or full rows that remain set NA to 0, thereby extending the analysis.
#-------------------------------------------------------------------------------------------
#	dfWide[is.na(dfWide)] <- 0

#-------------------------------------------------------------------------------------------
# 	List of column headings list excluding first column (which is summarisBy)
#-------------------------------------------------------------------------------------------
	columnNames <- names(dfWide)[-1]
	dfLong <- reshape(dfWide, direction="long", timevar="variable",
		varying=list(columnNames))

#-------------------------------------------------------------------------------------------
# 	Rationalise column names
#-------------------------------------------------------------------------------------------
	names(dfLong)[1] <- "summariseBy"
	names(dfLong)[2] <- "variable"
  	names(dfLong)[3] <- "value"
  	dfLong$variable <- factor(dfLong$variable, labels=columnNames)
  	
#-------------------------------------------------------------------------------------------
# 	Remove column name suffix introduced under ddply. Circumvented. 
#-------------------------------------------------------------------------------------------
#  	names(dfLong) <- gsub(metric, "", names(dfLong))
#  	dfLong$variable <- gsub(metric, "", dfLong$variable)
  	
#-------------------------------------------------------------------------------------------
# 	When profile = (trendMonthYear or trendYear) we must amend the values stored within 
#	summariseBy so that we can take advantage of the enhanced x-axis major and minor tick
#	marks offered by ggplot2. e.g. "2016-12" becomes "2016-12-01".
#-------------------------------------------------------------------------------------------
  	if (profile == "trendMonthYear") {
  	    dfLong$summariseBy <- as.Date(paste0(dfLong$summariseBy,    "-01"),"%Y-%m-%d")
  	} else if (profile == "trendYear") {
  	    dfLong$summariseBy <- as.Date(paste0(dfLong$summariseBy, "-01-01"),"%Y-%m-%d")
  	}



#-------------------------------------------------------------------------------------------
#
#   Assemble and call ggplot
#
#-------------------------------------------------------------------------------------------
	library(ggplot2)
  	library(scales)

  	p <- ggplot(dfLong, aes(x=summariseBy, y=value, colour=variable, group=variable))

 	p <- p + theme(axis.text=element_text(size=14)) 				# change axis labels
	p <- p + theme(axis.text.x=element_text(angle=90, hjust=1)) 	# rotate text labels
	p <- p + theme(axis.title=element_text(size=14)) 				# change axis titles
	p <- p + theme(text=element_text(size=14)) 		  # change all text except geom_text
	p <- p + theme(legend.text=element_text(size=14)) 				  # legend text size
	p <- p + theme(legend.position="bottom")
	p <- p + theme(legend.title=element_blank())
	p <- p + theme(legend.key.width=unit(3,"line"))
	p <- p + xlab(xlab)
	p <- p + ylab(ylab)

	p <- p + geom_point(shape=21, size=1)
	p <- p + geom_line()
	
	if (percentageScale == "fixed") {
		p <- p + ylim(c(0,100))
	} else { 
		p <- p + scale_y_continuous(breaks=pretty_breaks(n=10))
	}

#-------------------------------------------------------------------------------------------
#   Configure x-axis according to profile and summariseBy parameters passed
#-------------------------------------------------------------------------------------------
	if 			(profile == "yearByMonth") {
	    p <- p + scale_x_discrete(
	    breaks=c("01","02","03","04","05","06","07","08","09","10","11","12"),
	    labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
	    
	} else if 	(profile == "trendMonthYear" || profile == "trendYear") {
	    p <- p + scale_x_date(date_breaks="1 year", date_minor_breaks="1 month",
	    		date_labels="%Y")
	}

#	p <- p + title to capture options ???

#-------------------------------------------------------------------------------------------
# 	Let's go...
#-------------------------------------------------------------------------------------------
	print(p)
}
