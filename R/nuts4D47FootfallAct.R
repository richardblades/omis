#'------------------------------------------------------------------------------------------
#'
#'	n u t s 4 D 4 7 F o o t f a l l A c t . R
#'
#'	NUTS-4 | Division 47 | (turnover47s07 ~ newborough2East + westborough2West) | Actual
#'
#'  @param depVar1		:	("turnover47s07")
#'  @param depVar1Ttl	:	("turnover £m")
#'  @param factor1		:	("%","1"..."9")
#'  @param factor2		:	("%","1"..."9")
#'  @param geography	:	("nuts4")
#'  @param indVar1		: 	("newborough2East")
#'  @param indVar2		: 	("westborough2West")
#'  @param inputData	:	array of user entered footfall
#'  @param modelTo		:	("2012")
#'  @param options		:	("confidence", "modelAttr", "nonlinear")
#'  @param predictTo	:	("2020")
#'
#'------------------------------------------------------------------------------------------
nuts4D47FootfallAct <- function (
	depVar1 	,
	depVar1Ttl	,
	factor1		,
	factor2		,
	geography 	,
	indVar1 	,
	indVar2 	,
	inputData 	,
	options 	,
	modelTo		,
	predictTo	
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
#	Get independent variable data
#
#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------
#	For footfall I need a "unit" which is comparable across time. Days of the week vary
#   in terms of people's routine shopping habits. For example they may always meet a 
#	friend in town for a coffee on a Wednesday, go for drink after work on a Friday, do the
#	weekly shop on a Saturday morning and seldom if ever go into town on a Sunday. But they
#	will most likely repeat this pattern of activity each and every week. Hence I need to 
#	sum footfall by week and then remove the outliers for the year whilst still reflecting
#	school holidays and Christmas etc...
#-------------------------------------------------------------------------------------------
 	systemFile <- system.file("data", package="omis")
 	rdaFile <- paste0(systemFile, "/", geography, "HourFootfall.rda")
	load(rdaFile)
	dfName <- paste0(geography, "HourFootfall")
	footfall <- get(dfName)	
	
#-------------------------------------------------------------------------------------------
#   Select date range of interest. 
#-------------------------------------------------------------------------------------------
	from 	 <- paste0(modelTo, "-01-01")
	to 		 <- as.numeric(strftime(Sys.Date(), "%Y")) -1     		# last complete year
	to 		 <- paste0(to, "-12-31")
 	footfall <- subset(footfall, date >= from & date <= to)

#-------------------------------------------------------------------------------------------
#   Get weekly footfall figures
#-------------------------------------------------------------------------------------------
	library(plyr)

	footfall$yearWeekNum <- strftime(footfall$date,"%Y-%W") # (2016-00... 2016-53)
	footfallYWN <- ddply(footfall, .(yearWeekNum), summarize,
	    newborough2East  = sum(newborough2East,  na.rm=TRUE),
	    westborough2West = sum(westborough2West, na.rm=TRUE))
	    	
#-------------------------------------------------------------------------------------------
#   Get 1st and 99th percentile figures for weekly footfall within each year
#-------------------------------------------------------------------------------------------
	footfallYWN$year <- substr(footfallYWN$yearWeekNum, 1, 4)
	footfallPct <- ddply(footfallYWN, .(year), summarize,
	    newborough2EastLow   = quantile(newborough2East,  probs=0.01, na.rm=TRUE),
	    westborough2WestLow  = quantile(westborough2West, probs=0.01, na.rm=TRUE),
	    newborough2EastHigh  = quantile(newborough2East,  probs=0.99, na.rm=TRUE),
	    westborough2WestHigh = quantile(westborough2West, probs=0.99, na.rm=TRUE))
	
#-------------------------------------------------------------------------------------------
#   Clean footfallYWN by removing data (setting it to NA) that sits outside the 1st & 99th
#	percentiles. Must merge data with footfallPct to faciliate comparison by year.
#
#	Bipassed (commented out) as atypical behaviour still contributes to sales. Furthermore,
#	their removal pushes the VIF (between newborough2East & westborough2West) just beyond 
#	acceptable.
#-------------------------------------------------------------------------------------------
	footfallMrg <- merge(footfallYWN, footfallPct, by=c("year"), all=TRUE)
    
#	for (i in 1:nrow(footfallMrg)) {
#		if (footfallMrg[i, "newborough2East"]  <= footfallMrg[i, "newborough2EastLow"] ||
#			footfallMrg[i, "newborough2East"]  >= footfallMrg[i, "newborough2EastHigh"]) {
#			footfallMrg[i, "newborough2East"]  <- NA
#		}   
#		if (footfallMrg[i, "westborough2West"] <= footfallMrg[i, "westborough2WestLow"] ||
#			footfallMrg[i, "westborough2West"] >= footfallMrg[i, "westborough2WestHigh"]) {
#			footfallMrg[i, "westborough2West"] <- NA
#		}   
#	}   

#-------------------------------------------------------------------------------------------
#   Sum weekly footfall for the year
#-------------------------------------------------------------------------------------------
	footfallSum <- ddply(footfallMrg, .(year), summarize,
	   newborough2East  = sum(newborough2East,  na.rm=TRUE),
	   westborough2West = sum(westborough2West, na.rm=TRUE))

#-------------------------------------------------------------------------------------------
#   Re-scale for readability and align independent variable names with screen counterparts.
#-------------------------------------------------------------------------------------------
	indVars <- footfallSum
	indVars$newborough2East  <- indVars$newborough2East  / 1000000
	indVars$westborough2West <- indVars$westborough2West / 1000000
    colnames(indVars)[which(names(indVars) == "newborough2East" )] <- "indVar1"
    colnames(indVars)[which(names(indVars) == "westborough2West")] <- "indVar2"
	
	
	
#-------------------------------------------------------------------------------------------
#   
#	Get dependent variable data
#
#-------------------------------------------------------------------------------------------
	rdaFile <- paste0(systemFile, "/", geography, "Year.rda")
	load(rdaFile)
	dfName <- paste0(geography, "Year")
	depVars <- get(dfName)	
	
#-------------------------------------------------------------------------------------------
#   Select columns of interest
#-------------------------------------------------------------------------------------------
	depVars <- depVars[, c("turnover47s07", "year" )]
	
#-------------------------------------------------------------------------------------------
#   Remove incomplete obs (is.NA) and outliers (2013 value atypically high). Could omit
#   rows based on column setting e.g. depVars[!is.na(depVars$turnover47s07),].
#-------------------------------------------------------------------------------------------
	depVars <- na.omit(depVars)
	depVars <- subset(depVars, year != 2013)	
	
#-------------------------------------------------------------------------------------------
#   Re-scale for readability, then align dependent variable name with screen counterpart.
#-------------------------------------------------------------------------------------------
	depVars$turnover47s07 <- depVars$turnover47s07 / 1000000
    colnames(depVars)[which(names(depVars) == "turnover47s07")] <- "depVar1"
	
	
	
#-------------------------------------------------------------------------------------------
#
#   Produce graphical output
#
#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------
#	Combine dependent and independent variables as required for plot
#-------------------------------------------------------------------------------------------
	dfPlot <- depVars
	
#-------------------------------------------------------------------------------------------
#   Specify breaks as a Date vector. Create date variable of class date to facilitate date
#   based scale.
#-------------------------------------------------------------------------------------------
	dfPlot$date <- as.Date(paste("01Jan", dfPlot$year, sep=""),"%d%b%Y")
	library(scales)
	to <- paste0(modelTo, "-12-31")
	dateBreaks <- seq(as.Date("1998-01-01"), as.Date(to), by="1 year")

#-------------------------------------------------------------------------------------------
#   Rationalise axis
#-------------------------------------------------------------------------------------------
#	axisRange 	<- as.data.frame(range(dfPlot$depVar1))
#	axisFrom 	<- axisRange[1,1] - (axisRange[1,1] %% 50)
#	axisTo 		<- axisRange[2,1] - (axisRange[2,1] %% 50) +50
#	axisBreaks 	<- seq(axisFrom, axisTo, by=50)

#-------------------------------------------------------------------------------------------
#   Assemble ggplot
#-------------------------------------------------------------------------------------------
	library(ggplot2)
	library(plyr) 											   # For the desc() function
	
	p <- ggplot(dfPlot, aes(x=date, y=depVar1))
  	
	p <- p + theme(axis.text=element_text(size=14)) 				# change axis labels
	p <- p + theme(axis.text.x=element_text(angle=90, hjust=1)) 	# rotate text labels
	p <- p + theme(axis.title=element_text(size=14)) 				# change axis titles
	p <- p + theme(text=element_text(size=14)) 		  # change all text except geom_text
	p <- p + theme(legend.text=element_text(size=14)) 				  # legend text size
  	
    p <- p + geom_point(shape=22, size=4, colour="darkred", fill="pink")
    
   	p <- p + geom_smooth(alpha=0.2, colour="grey60", level=0.95, method=lm)
	
#-------------------------------------------------------------------------------------------
#	Note "limits" could have been used to constrain either axis to enable focus.
#  	p <- p + scale_y_continuous(breaks=axisBreaks, depVar1Ttl, limits=c(axisFrom, axisTo))
#-------------------------------------------------------------------------------------------
	p <- p + scale_y_continuous(breaks=pretty_breaks(n=10), depVar1Ttl) 
	p <- p + scale_x_date(breaks=dateBreaks, labels=date_format("%Y"))	
			
#-------------------------------------------------------------------------------------------
# 	Let's go...
#-------------------------------------------------------------------------------------------
	print(p)
	
	
	
#-------------------------------------------------------------------------------------------
#   
#	Produce tabular output
#
#-------------------------------------------------------------------------------------------
	
#-------------------------------------------------------------------------------------------
#	Generate years of interest
#-------------------------------------------------------------------------------------------
	year 	<- seq(as.numeric(modelTo), as.numeric(predictTo), 1)
	dfYears <- data.frame(year)
	
#-------------------------------------------------------------------------------------------
#	Combine dependent and independent variables with years on interest to enable
#	prepopulation of screen table.
#-------------------------------------------------------------------------------------------
	dfPrepop <- merge(depVars, indVars, by=c("year"), all=TRUE)
	dfPrepop <- subset(dfPrepop, year >= modelTo & year <= predictTo) 
	dfTable  <- merge(dfPrepop, dfYears, by=c("year"), all=TRUE)
																 
#-------------------------------------------------------------------------------------------
#	Format numbers for readability.
#-------------------------------------------------------------------------------------------
	dfTable$depVar1 <- formatNumber(dfTable$depVar1, 3)
	dfTable$indVar1 <- formatNumber(dfTable$indVar1, 3)
	dfTable$indVar2 <- formatNumber(dfTable$indVar2, 3)
	
#-------------------------------------------------------------------------------------------
#	Return data
#-------------------------------------------------------------------------------------------
	return(dfTable)
}