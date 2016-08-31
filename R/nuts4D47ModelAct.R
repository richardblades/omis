#'------------------------------------------------------------------------------------------
#'
#'	n u t s 4 D 4 7 M o d e l A c t . R 
#'
#'	NUTS-4 | Division 47 | (turnover47s07 ~ femalePop + femaleWorkGrossWeek) | Actual
#'
#'  @param depVar1		:	("turnover47s07")
#'  @param depVar1Ttl	:	("turnover £m")
#'  @param factor1		:	("%","1"..."9")
#'  @param factor2		:	("%","1"..."9")
#'  @param geography	:	("nuts4")
#'  @param indVar1		: 	("femalePop")
#'  @param indVar2		: 	("femaleWorkGrossWeek")
#'  @param inputData	:	array of user entered data
#'  @param modelTo		:	("2012")
#'  @param options		:	("confidence", "modelAttr", "nonlinear")
#'  @param predictTo	:	("2020")
#'
#'------------------------------------------------------------------------------------------
nuts4D47ModelAct <- function (
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
 	systemFile <- system.file("data", package="omis")
 	rdaFile <- paste0(systemFile, "/", geography, "Year.rda")
	load(rdaFile)
	dfName <- paste0(geography, "Year")
	indVars <- get(dfName)	
	
#-------------------------------------------------------------------------------------------
#   Select columns of interest
#-------------------------------------------------------------------------------------------
	indVars <- indVars [, c("femalePop", "femaleWorkGrossWeek", "year" )]
		
#-------------------------------------------------------------------------------------------
#   Re-scale for readability and align independent variable names with screen counterparts.
#-------------------------------------------------------------------------------------------
#	indVars$femalePop  			<- indVars$femalePop  / ???
#	indVars$femaleWorkGrossWeek <- indVars$femaleWorkGrossWeek / ???
    colnames(indVars)[which(names(indVars) == "femalePop" )] <- "indVar1"
    colnames(indVars)[which(names(indVars) == "femaleWorkGrossWeek")] <- "indVar2"
	
	
	
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
#   Re-scale for readability and align dependent variable name with screen counterpart.
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
	dfTable$indVar1 <- formatNumber(dfTable$indVar1, 0)
	dfTable$indVar2 <- formatNumber(dfTable$indVar2, 2)
	
#-------------------------------------------------------------------------------------------
#	Return data
#-------------------------------------------------------------------------------------------
 	return(dfTable)
#	message = paste0("dfTable... ", "<br>", dfTable)
#	stop(message)
}