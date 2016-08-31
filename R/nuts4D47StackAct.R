#'------------------------------------------------------------------------------------------
#'
#'	n u t s 4 D 4 7 S t a c k A c t . R
#'
#'	NUTS-4 | Division 47 | (turnover47s07 ~ femalePop + femaleWorkGrossWeek) | Stack Actual
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
#'  @param options		:	("modelAttr", "oneHundred")
#'  @param predictTo	:	("2020")
#'
#'------------------------------------------------------------------------------------------
nuts4D47StackAct <- function (
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
#	Get stack variable data
#
#-------------------------------------------------------------------------------------------
 	rdaFile <- paste0(systemFile, "/", geography, "Year.rda")
	load(rdaFile)
	dfName <- paste0(geography, "Year")
	stackVars <- get(dfName)	
					
#-------------------------------------------------------------------------------------------
#   Declare stack components
#-------------------------------------------------------------------------------------------
	stackComp <- c( "turnover471s07", "turnover472s07", "turnover473s07", "turnover474s07",
				    "turnover475s07", "turnover476s07", "turnover477s07", "turnover478s07",
				    "turnover479s07")
					
#-------------------------------------------------------------------------------------------
#   Select columns of interest. Note its vital that "year" is declared as the first.
#-------------------------------------------------------------------------------------------
	stackVars <- stackVars[, c("year", stackComp)]
	
#-------------------------------------------------------------------------------------------
#   Remove incomplete obs (is.NA) and outliers (2013 value atypically high). Could omit
#   rows based on specific column setting e.g. stackVars[!is.na(stackVars$column), ].
#-------------------------------------------------------------------------------------------
	stackVars <- na.omit(stackVars)
	stackVars <- subset(stackVars, year != 2013)	
	
#-------------------------------------------------------------------------------------------
#   Note, some items have been set to NA by the ONS suppression as a means of preventing
#  	disclosure. These are set to 0 so that the remaining columns in that observation may
#	be analysed. The following code essentially replaces NA with 0 throughout the object. 
#	X [is.na(X)] <- 0
#-------------------------------------------------------------------------------------------
	stackVars[is.na(stackVars)] <- 0 	
								  
#-------------------------------------------------------------------------------------------
#   Re-scale all bar "year" for readability.
#-------------------------------------------------------------------------------------------
	for (i in 1:nrow(stackVars)) {
		for (j in 1:ncol(stackVars)) {
		    if (names(stackVars) [j] != "year") {
  			    stackVars[i, j] <- stackVars[i, j] / 1000000
		    }
  		}
  	}
  		
#-------------------------------------------------------------------------------------------
#	Declare legend and labels
#-------------------------------------------------------------------------------------------
	legendBreaks <- stackComp
	legendLabels <- c(
		"non-specialised",
		"food, beverages & tobacco",
		"automative fuel",
		"info & comm equipment",
		"household equipment",
		"cultural & recreation goods",
		"other goods stores",
		"stalls & markets",
		"other")
	legendName <- c("Retail Group")
	
	
	
#-------------------------------------------------------------------------------------------
#
#   Produce graphical output
#
#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------
#   Reshape data to suit ggplot function
#-------------------------------------------------------------------------------------------
	dfWide <- stackVars
	
#-------------------------------------------------------------------------------------------
#   List of column headings excluding the first (which is the year).
#-------------------------------------------------------------------------------------------
	columnNames <- names(dfWide)[-1]

	dfLong <- reshape(dfWide, direction="long", timevar="variable",
		varying=list(columnNames))

#-------------------------------------------------------------------------------------------
#   Rationalise column names
#-------------------------------------------------------------------------------------------
  	names(dfLong)[2] <- "variable"
  	names(dfLong)[3] <- "value"
  	dfLong$variable <- factor(dfLong$variable, labels=columnNames)
  	
#-------------------------------------------------------------------------------------------
#   Create date variable of class date to facilitate date based scale
#-------------------------------------------------------------------------------------------
	dfLong$date <- as.Date(paste("01Jan",dfLong$year, sep=""),"%d%b%Y")
	
#-------------------------------------------------------------------------------------------
#   Specify breaks as a Date vector. Create date variable of class date to facilitate date
#   based scale.
#-------------------------------------------------------------------------------------------
	library(scales)
	to <- paste0(modelTo, "-12-31")
	dateBreaks <- seq(as.Date("1998-01-01"), as.Date(to), by="1 year")

#-------------------------------------------------------------------------------------------
#   Assemble ggplot
#-------------------------------------------------------------------------------------------
	library(ggplot2)
	library(plyr) 											# Used for the desc() function
	
  	p <- ggplot(dfLong, aes(x=date, y=value, order=desc(variable), fill=variable))
  	
	p <- p + theme(axis.text=element_text(size=14)) 				# change axis labels
	p <- p + theme(axis.text.x=element_text(angle=90, hjust=1)) 	# rotate text labels
	p <- p + theme(axis.title=element_text(size=14)) 				# change axis titles
	p <- p + theme(text=element_text(size=14)) 		  # change all text except geom_text
	p <- p + theme(legend.text=element_text(size=14)) 				  # legend text size
	
#-------------------------------------------------------------------------------------------
# 	Note, postion="stack" is the default for geom_area but added here for clarity. 
#-------------------------------------------------------------------------------------------
	p <- p + geom_area(alpha=0.5, colour="black", position = "stack", size=0.3,)
	
#-------------------------------------------------------------------------------------------
# 	Limits applied via expand_limits rather than scale_y_continuous as the latter will 
#	inadvertently exclude individual values rather than the combined stack.
#-------------------------------------------------------------------------------------------
#	p <- p + expand_limits(y=c(valueFrom, valueTo))
	p <- p + scale_y_continuous(breaks=pretty_breaks(n=10), depVar1Ttl) 
	p <- p + scale_x_date(breaks=dateBreaks, labels=date_format("%Y"))	
	p <- p + scale_fill_discrete(guide=guide_legend(reverse=TRUE), name=legendName,
		breaks=legendBreaks, labels=legendLabels)
			
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