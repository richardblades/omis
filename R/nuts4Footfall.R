#'------------------------------------------------------------------------------------------
#'
#'  n u t s 4 F o o t f a l l . R
#'
#'  NUTS-4 | Plot footfall against time
#'
#'  @param 	excludeWeekends ("checked", "unchecked")
#'  @param 	footfall 		("newborough2East", "newborough2West", "westborough2East",
#' 								"westborough2West")
#'  @param 	geography 		("nuts4")
#'  @param 	metric 			("max", "mean", "p95", "sum")
#'  @param  profile         ("day", "week", "month", "yearByMonth", "yearByWeek", "trend")
#'  @param 	rangeFrom 		("2006-05-15") 		 	 First Spring Board camera installed  
#'  @param 	rangeTo 		("9999-12-31") 			 	 Sys.Date() would be more useful
#'  @param 	timeOfDay		("evening", "officeHours", "24Hours")
#'
#'------------------------------------------------------------------------------------------
nuts4Footfall <- function (
	excludeWeekends,
	footfall,
	geography,
	metric,
   	profile,
	rangeFrom,  
	rangeTo,
	timeOfDay
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
 	rdaFile <- paste0(systemFile, "/", geography, "HourFootfall.rda")
	load(rdaFile)
	dfName <- paste0(geography, "HourFootfall")
	df <- get(dfName)	



#-------------------------------------------------------------------------------------------
#
# 	Apply user specified selection criteria.
#
#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------
#   Select data within given date range
#-------------------------------------------------------------------------------------------
	dfRange <- subset(df, date >= rangeFrom & date <= rangeTo)

#-------------------------------------------------------------------------------------------
#   Select data according to time of day requested.
#-------------------------------------------------------------------------------------------
	if 			(timeOfDay == "officeHours") {
	    dfRange <- subset(dfRange, time >= "08:00:00" & time <= "17:00:00")
	} else if 	(timeOfDay == "evening") {
	    dfRange <- subset(dfRange, time >= "17:00:00" & time <= "23:00:00")
	}

#-------------------------------------------------------------------------------------------
#   If requested, excludeWeekends.
#-------------------------------------------------------------------------------------------
	if (excludeWeekends == "checked") {
	    dfRange$weekDayNum 	<- strftime(dfRange$date,"%u") # (Monday=1, Sunday=7)
	    dfRange <- subset(dfRange, weekDayNum >= "1" & weekDayNum <= "5")
	}



#-------------------------------------------------------------------------------------------
#
#	Set "summariseBy" according to "profile" and generate supporting summary variables
#
#	Note %W provides the UK version of week of the year as decimal number (00–53) using
#	Monday as the first day of week (and typically with the first Monday of the year as day
#	1 of week 1).
#
#   strptime() takes a character vector and makes a date-time object of class
#	POSIXlt/POSIXct.
#	strftime() takes a date-time object of class POSIXlt/POSIXct and makes a character
#	vector.
#-------------------------------------------------------------------------------------------
	if 			(profile == "day") {
	    summariseBy         <- c("hour")
	    dfRange$hour        <- as.POSIXct(paste(Sys.Date(), dfRange$time),
	                            format="%Y-%m-%d %H:%M:%S", tz="GMT")
	    xlab                <- c("summarised by hour of day")
	} else if 	(profile == "week") {
	    summariseBy         <- c("weekDayNum")
	    dfRange$weekDayNum 	<- strftime(dfRange$date,"%u") # (Monday=1, Sunday=7)
	    xlab                <- c("summarised by day of week")
	} else if 	(profile == "month") {
	    summariseBy         <- c("monthDayNum")
	    dfRange$monthDayNum	<- strftime(dfRange$date,"%d") # (01... 31)
	    xlab                <- c("summarised by day of month")
	} else if 	(profile == "yearByMonth") {
	    summariseBy         <- c("monthNum")
	    dfRange$monthNum	<- strftime(dfRange$date,"%m") # (01... 12)
	    xlab                <- c("summarised by month")
	} else if 	(profile == "yearByWeek") {
	    summariseBy         <- c("weekNum")
	    dfRange$weekNum 	<- strftime(dfRange$date,"%W") # (00... 53, UK Version)
	    xlab                <- c("summarised by week number")
	} else if 	(profile == "trend") {
	    summariseBy         <- c("yearMonthNum")
	    dfRange$yearMonthNum<- strftime(dfRange$date,"%Y-%m") # (2016-01... 2016-12)
	    xlab                <- c("summarised by year and month")
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
	        newborough2East  = max(newborough2East , na.rm=TRUE),
	        newborough2West  = max(newborough2West , na.rm=TRUE),
	        westborough2East = max(westborough2East, na.rm=TRUE),
	        westborough2West = max(westborough2West, na.rm=TRUE))
		ylab <- c("maximum footfall / hour")
	} else if 	(metric == "mean") 	{
		dfSummary <- ddply(dfRange, summariseBy, summarize,
	        newborough2East  = mean(newborough2East , na.rm=TRUE),
	        newborough2West  = mean(newborough2West , na.rm=TRUE),
	        westborough2East = mean(westborough2East, na.rm=TRUE),
	        westborough2West = mean(westborough2West, na.rm=TRUE))
		ylab <- c("mean footfall / hour")
	} else if 	(metric == "sum") 	{
		dfSummary <- ddply(dfRange, summariseBy, summarize,
	        newborough2East  = sum(newborough2East , na.rm=TRUE),
	        newborough2West  = sum(newborough2West , na.rm=TRUE),
	        westborough2East = sum(westborough2East, na.rm=TRUE),
	        westborough2West = sum(westborough2West, na.rm=TRUE))
		ylab <- c("sum footfall")
	} else if 	(metric == "p95") 	{
		dfSummary <- ddply(dfRange, summariseBy, summarize,
	        newborough2East  	= quantile(newborough2East , probs=0.95, na.rm=TRUE),
	        newborough2West  	= quantile(newborough2West , probs=0.95, na.rm=TRUE),
	        westborough2East 	= quantile(westborough2East, probs=0.95, na.rm=TRUE),
	        westborough2West 	= quantile(westborough2West, probs=0.95, na.rm=TRUE))
		ylab <- c("95th percentile footfall / hour")
	}



#-------------------------------------------------------------------------------------------
#
#	Reshape data to suit ggplot function
#
#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------
# 	Trim to only those footfall camera variables requested
#-------------------------------------------------------------------------------------------
	dfFootfall <- as.data.frame(footfall)
	columnSubset <- c(summariseBy)
	for (i in 1:nrow(dfFootfall)) {
#	    columnName <- paste0(dfFootfall[i,1], metric)
	    columnName <- paste0(dfFootfall[i,1])
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
# 	When profile = trend, we must amend the yearMonthNum value stored within summariseBy so
#   that we can take advantage of the enhance x-axis major and minor tick marks offered by
#   ggplot2. e.g. "2016-12" becomes "2016-12-01".
#-------------------------------------------------------------------------------------------
  	if (profile == "trend") {
  	    dfLong$summariseBy <- as.Date(paste0(dfLong$summariseBy, "-01"),"%Y-%m-%d")
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

	p <- p + scale_y_continuous(breaks=pretty_breaks(n=10))

#-------------------------------------------------------------------------------------------
#   Configure x-axis according to profile and summariseBy parameters passed
#-------------------------------------------------------------------------------------------
	if 			(profile == "day") 		{
	    start 	<- strptime(paste(Sys.Date(), "00:00"), "%Y-%m-%d %H:%M", tz="GMT")
	    end 	<- strptime(paste(Sys.Date(), "23:00"), "%Y-%m-%d %H:%M", tz="GMT")
	    xLimits <- as.POSIXct(c(start, end))
	    p <- p + scale_x_datetime(breaks=date_breaks("1 hour"), labels=date_format("%H:%M"),
	    		limits=xLimits)
	    	
	} else if 	(profile == "week") 	{
	    p <- p + scale_x_discrete(breaks=1:7,
	            labels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
	            
	} else if 	(profile == "month") 	{
	
	} else if 	(profile == "yearByMonth") {
	    p <- p + scale_x_discrete(
	    breaks=c("01","02","03","04","05","06","07","08","09","10","11","12"),
	    labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
	    
	} else if 	(profile == "yearByWeek") {
	
	} else if 	(profile == "trend") 	{
	    p <- p + scale_x_date(date_breaks="1 year", date_minor_breaks="1 month",
	    		date_labels="%Y")
	}

#	p <- p + title to capture options ???

#-------------------------------------------------------------------------------------------
# 	Let's go...
#-------------------------------------------------------------------------------------------
	print(p)
}
