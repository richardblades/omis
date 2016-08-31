#'------------------------------------------------------------------------------------------
#'
#'	n u t s 4 D 5 5 S t a c k P r d . R
#'
#'	NUTS-4 | Division 55 | (turnover55s07 ~ femaleWorkHourExOT + femalePop) | Stack Predict
#'
#'  @param depVar1		:	("turnover55s07")
#'  @param depVar1Ttl	:	("turnover £m")
#'  @param factor1		:	("%","1"..."9")
#'  @param factor2		:	("%","1"..."9")
#'  @param geography	:	("nuts4")
#'  @param indVar1		: 	("femaleWorkHourExOT")
#'  @param indVar2		: 	("femalePop")
#'  @param inputData	:	array of user entered data
#'  @param modelTo		:	("2012")
#'  @param options		:	("modelAttr", "oneHundred")
#'  @param predictTo	:	("2020")
#'
#'	Note, JavaScript returns the array "inputData" holding either numerics or NA. Hence
#'	specifying options("stringAsFactors = FALSE") as a session wide setting, though often
#'	prudent, is this case, unecessary.
#'
#'------------------------------------------------------------------------------------------
nuts4D55StackPrd <- function (
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
	indVars <- indVars [, c("femalePop", "femaleWorkHourExOT", "year" )]
	
#-------------------------------------------------------------------------------------------
#   Re-scale for readability and align independent variable names with screen counterparts.
#-------------------------------------------------------------------------------------------
#	indVars$femalePop  			<- indVars$femalePop  / ???
#	indVars$femaleWorkHourExOT <- indVars$femaleWorkHourExOT / ???
    colnames(indVars)[which(names(indVars) == "femaleWorkHourExOT")] <- "indVar1"
    colnames(indVars)[which(names(indVars) == "femalePop" )] <- "indVar2"
	
	
	
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
	depVars <- depVars[, c("turnover55s07", "year" )]
	
#-------------------------------------------------------------------------------------------
#   Remove incomplete obs (is.NA) and outliers (2013 value atypically high). Could omit
#   rows based on specific column setting e.g. depVars[!is.na(depVars$column), ].
#-------------------------------------------------------------------------------------------
	depVars <- na.omit(depVars)
	depVars <- subset(depVars, year != 2013)	
	
#-------------------------------------------------------------------------------------------
#   Re-scale for readability and align dependent variable name with screen counterpart.
#-------------------------------------------------------------------------------------------
	depVars$turnover55s07 <- depVars$turnover55s07 / 1000000
    colnames(depVars)[which(names(depVars) == "turnover55s07")] <- "depVar1"
	
	
	
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
	stackComp <- c( "turnover551s07", "turnover552s07", "turnover553s07", "turnover559s07")
					
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
			"Hotels",
			"Holiday & Short Stay",
			"Camping, recreational\n& trailer parks",
			"Other")
	legendName <- c("Accommodation Group")
	
	
	
#-------------------------------------------------------------------------------------------
#   
#	Combine dependent, independent and stack variable data. Note, "all.x=TRUE" retains only
#	matches where "x" in merge(x,y ...) is present.
#
#-------------------------------------------------------------------------------------------
	dfActual <- merge(depVars,  indVars,   by=c("year"), all.x=TRUE)
	dfActual <- merge(dfActual, stackVars, by=c("year"), all.x=TRUE)
		
	
	
#-------------------------------------------------------------------------------------------
#
#	Process screen input data.
#
#	Note, JavaScript returns the array "inputData" holding either numerics or NA. Hence
#	specifying options("stringAsFactors = FALSE") as a session wide setting, though often
#	prudent, is this case, unecessary.
#
#-------------------------------------------------------------------------------------------
	dfInput <- as.data.frame(inputData)
#	stopifnot("femalePop" %in% names(dfInput))
#	stopifnot("femaleWorkHourExOT" %in% names(dfInput))
	
#-------------------------------------------------------------------------------------------
#   If selected, impose cumulative growth factors from base year onwards.
#-------------------------------------------------------------------------------------------
	if (factor1 != "%") { factor1 <- as.numeric(factor1) }
	if (factor2 != "%") { factor2 <- as.numeric(factor2) }

	for (i in 1:nrow(dfInput)) {
	    if (factor1 != "%") {
	        if (!is.na(dfInput[i, "indVar1"])) {
	            indVar1Growth <- dfInput[i,"indVar1"] + (dfInput[i,"indVar1"] * factor1/100)
	        } else {
	            dfInput[i, "indVar1"] <- indVar1Growth
	            indVar1Growth <- indVar1Growth + (indVar1Growth * factor1/100)
	        }
	    }
	    if (factor2 != "%") {
	        if (!is.na(dfInput[i, "indVar2"])) {
	            indVar2Growth <- dfInput[i,"indVar2"] + (dfInput[i,"indVar2"] * factor2/100)
	        } else {
	            dfInput[i, "indVar2"] <- indVar2Growth
	            indVar2Growth <- indVar2Growth + (indVar2Growth * factor2/100)
	        }
	    }
	}

#-------------------------------------------------------------------------------------------
#   If we find that the user has not entered data for any given element then we need to
#   propergate data from previous years, starting with the base year.
#-------------------------------------------------------------------------------------------
	for (i in 1:nrow(dfInput)) {
  		if (!is.na(dfInput[i, "indVar1"])) {
    		indVar1Pass <- dfInput[i, "indVar1"]
  		} else {
    		dfInput[i, "indVar1"] <- indVar1Pass 
  		}
	    if (!is.na(dfInput[i, "indVar2"])) {
	        indVar2Pass <- dfInput[i, "indVar2"]
	    } else {
	        dfInput[i, "indVar2"] <- indVar2Pass 
	    }
	 }



#-------------------------------------------------------------------------------------------
#
#	Generate model
#
#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------
#	Combine dependent and independent variables to suit model. 
#-------------------------------------------------------------------------------------------
	dfModel <- subset(dfActual, year <= modelTo)
	
#-------------------------------------------------------------------------------------------
#   To capture latest data, build model on the fly rather than load a pre-built version.
#-------------------------------------------------------------------------------------------
 	model <- lm(depVar1 ~ indVar1 + indVar2, data=dfModel) 	

#-------------------------------------------------------------------------------------------
#   Extract model attributes
#-------------------------------------------------------------------------------------------
# 	str(summary(model))
	rSquared 	<- formatNumber(summary(model)$r.squared, 3)
	adjRSquared <- formatNumber(summary(model)$adj.r.squared, 3)
	interceptCf <- formatNumber(summary(model)$coef[[1,1]], 0)
	interceptPV <- formatNumber(summary(model)$coef[[1,4]], 3)
	indVar1Cf 	<- formatNumber(summary(model)$coef[[2,1]], 3)
    indVar1SE 	<- formatNumber(summary(model)$coef[[2,2]], 3)
    indVar1PV 	<- formatNumber(summary(model)$coef[[2,4]], 3)
    
	indVar2Cf 	<- formatNumber(summary(model)$coef[[3,1]], 3)
   	indVar2SE 	<- formatNumber(summary(model)$coef[[3,2]], 3)
	indVar2PV 	<- formatNumber(summary(model)$coef[[3,4]], 3)
	
    r2 <- paste0("r2: (", rSquared, ") adjusted (", adjRSquared, ")")
    SE <- paste0("std error: ", indVar1, " (", indVar1SE, ") ", indVar2, " (", indVar2SE,
     	  ")")
    PV <- paste0("p-value: ", indVar1, " (", indVar1PV, ") ", indVar2, " (",indVar2PV,
     	  ") intercept (", interceptPV, ")")
    	 		
    formula <- paste0(depVar1, " (£m) ~ ", indVar1Cf, " * ", indVar1, " (m) + ", indVar2Cf,
    		   " * ", indVar2, " (m) + ", interceptCf, " (m)")

#-------------------------------------------------------------------------------------------
#   Note, the summary function calculates the "overall p-value" at execution time, hence its
#	not available via extract. However, it can be obtained via the anova function but
#	cursory inspection suggests it differs.
#-------------------------------------------------------------------------------------------
# 	pValue <- anova(model)$'Pr(>F)'[1])



#-------------------------------------------------------------------------------------------
#   
#   Generate model predictions
#
#-------------------------------------------------------------------------------------------
	
#-------------------------------------------------------------------------------------------
#   Build new independent data from user input table
#-------------------------------------------------------------------------------------------
	year 			<- seq(as.numeric(modelTo)+1, as.numeric(predictTo), 1)
	dfPred 			<- data.frame(year)
	dfPred$indVar1 	<- as.numeric(dfInput$indVar1)
	dfPred$indVar2 	<- as.numeric(dfInput$indVar2)
	
#-------------------------------------------------------------------------------------------
#   Feed user supplied independent data into model to gain predicted dependent values
#-------------------------------------------------------------------------------------------
	dfPred$depVar1 	<- as.vector(predict(model, dfPred))
	
#-------------------------------------------------------------------------------------------
#	Subdivide Divisional data based on the Group's mean contribution to its parent division.
#-------------------------------------------------------------------------------------------
  	dfPred$turnover551s07 <- dfPred$depVar1*(55.22/100)
  	dfPred$turnover552s07 <- dfPred$depVar1*(13.55/100)
  	dfPred$turnover553s07 <- dfPred$depVar1*(29.79/100)
  	dfPred$turnover559s07 <- dfPred$depVar1*( 1.44/100)
	
	
	
#-------------------------------------------------------------------------------------------
#   
#   Produce graphical output
#
#-------------------------------------------------------------------------------------------
	
#-------------------------------------------------------------------------------------------
#   Combine predictions with actual data
#-------------------------------------------------------------------------------------------
	dfWide <- rbind(dfActual, dfPred)
	
#-------------------------------------------------------------------------------------------
#	Drop superfluous columns 
#-------------------------------------------------------------------------------------------
	dfWide <- subset(dfWide, select=-c(depVar1, indVar1, indVar2))	
		
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
	to <- paste0(predictTo, "-12-31")
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
  	if  ("oneHundred" %in% options) {
		p <- p + geom_area(alpha=0.5, colour="black", position = "fill", size=0.3)
  	} else {
		p <- p + geom_area(alpha=0.5, colour="black", position = "stack", size=0.3)
	}
	
#-------------------------------------------------------------------------------------------
# 	Limits applied via expand_limits rather than scale_y_continuous as the latter will 
#	inadvertently exclude individual values rather than the combined stack.
#-------------------------------------------------------------------------------------------
#	p <- p + expand_limits(y=c(valueFrom, valueTo))
	
#-------------------------------------------------------------------------------------------
#   If requested, Provide model attributes.
#-------------------------------------------------------------------------------------------
  	if  ("oneHundred" %in% options) {
		p <- p + scale_y_continuous(breaks=pretty_breaks(n=10), "turnover", labels=percent)
  	} else {
		p <- p + scale_y_continuous(breaks=pretty_breaks(n=10), depVar1Ttl) 
	}
	
	p <- p + scale_x_date(breaks=dateBreaks, labels=date_format("%Y"))	
	p <- p + scale_fill_discrete(guide=guide_legend(reverse=TRUE), name=legendName,
		breaks=legendBreaks, labels=legendLabels)
	
#-------------------------------------------------------------------------------------------
#   xintercept needs to be passed or calculated ???
#-------------------------------------------------------------------------------------------
 	p <- p + geom_vline(aes(xintercept=as.numeric(date[15])), linetype=2, colour="grey60")
	
#-------------------------------------------------------------------------------------------
#   If requested, Provide model attributes.
#-------------------------------------------------------------------------------------------
  	if  ("modelAttr" %in% options) {
		offset <- paste0(as.numeric(strftime(min(dfLong$date), "%Y")), "-07-01")
   		modelAttr <- data.frame(date=as.Date(offset))
   		if ("oneHundred" %in% options) {
   		  	modelAttr$value <- c(0.1)
   		} else {
   		  	modelAttr$value <- mean(dfLong$value)
   		}
  		modelAttr$text <- as.character(paste(r2, "\n", SE, "\n", PV, sep=""))
   		modelAttr$variable <- c("") 
    	p <- p + geom_text(data=modelAttr, aes(label=text), size=5, hjust="inward",
    				vjust="inward")
    	p <- p + ggtitle(formula)
  	}
 
#-------------------------------------------------------------------------------------------
#   Let's go...
#-------------------------------------------------------------------------------------------
	print(p)



#-------------------------------------------------------------------------------------------
#   
#   Produce tabular output
#
#-------------------------------------------------------------------------------------------
	dfBase <- tail(dfActual, 1)
	
#-------------------------------------------------------------------------------------------
#   Calculate changes in depVar1 between the years.
#-------------------------------------------------------------------------------------------
	dfPred[1, "depVar1Chg"] <-   dfPred[1, "depVar1"] - dfBase$depVar1
	dfPred[1, "depVar1Pct"] <- ((dfPred[1, "depVar1"] - dfBase$depVar1)       * 100) /
												        dfBase$depVar1
	dfPred[2, "depVar1Chg"] <-   dfPred[2, "depVar1"] - dfPred[1, "depVar1"]
	dfPred[2, "depVar1Pct"] <- ((dfPred[2, "depVar1"] - dfPred[1, "depVar1"]) * 100) /
												     	dfPred[1, "depVar1"]
	dfPred[3, "depVar1Chg"] <-   dfPred[3, "depVar1"] - dfPred[2, "depVar1"]
	dfPred[3, "depVar1Pct"] <- ((dfPred[3, "depVar1"] - dfPred[2, "depVar1"]) * 100) /
												     	dfPred[2, "depVar1"]
	dfPred[4, "depVar1Chg"] <-   dfPred[4, "depVar1"] - dfPred[3, "depVar1"]
	dfPred[4, "depVar1Pct"] <- ((dfPred[4, "depVar1"] - dfPred[3, "depVar1"]) * 100) /
												    	dfPred[3, "depVar1"]
	dfPred[5, "depVar1Chg"] <-   dfPred[5, "depVar1"] - dfPred[4, "depVar1"]
	dfPred[5, "depVar1Pct"] <- ((dfPred[5, "depVar1"] - dfPred[4, "depVar1"]) * 100) /
												     	dfPred[4, "depVar1"]
	dfPred[6, "depVar1Chg"] <-   dfPred[6, "depVar1"] - dfPred[5, "depVar1"]
	dfPred[6, "depVar1Pct"] <- ((dfPred[6, "depVar1"] - dfPred[5, "depVar1"]) * 100) /
												     	dfPred[5, "depVar1"]
	dfPred[7, "depVar1Chg"] <-   dfPred[7, "depVar1"] - dfPred[6, "depVar1"]
	dfPred[7, "depVar1Pct"] <- ((dfPred[7, "depVar1"] - dfPred[6, "depVar1"]) * 100) /
												     	dfPred[6, "depVar1"]
	dfPred[8, "depVar1Chg"] <-   dfPred[8, "depVar1"] - dfPred[7, "depVar1"]
	dfPred[8, "depVar1Pct"] <- ((dfPred[8, "depVar1"] - dfPred[7, "depVar1"]) * 100) /
												     	dfPred[7, "depVar1"]
												     	  												     	  
#-------------------------------------------------------------------------------------------
#   Calculate changes in indVar1 between the years.
#-------------------------------------------------------------------------------------------
	dfPred[1, "indVar1Chg"] <-   dfPred[1, "indVar1"] - dfBase$indVar1
	dfPred[1, "indVar1Pct"] <- ((dfPred[1, "indVar1"] - dfBase$indVar1)       * 100) /
													 	dfBase$indVar1
	dfPred[2, "indVar1Chg"] <-   dfPred[2, "indVar1"] - dfPred[1, "indVar1"]
	dfPred[2, "indVar1Pct"] <- ((dfPred[2, "indVar1"] - dfPred[1, "indVar1"]) * 100) /
													 	dfPred[1, "indVar1"]
	dfPred[3, "indVar1Chg"] <-   dfPred[3, "indVar1"] - dfPred[2, "indVar1"]
	dfPred[3, "indVar1Pct"] <- ((dfPred[3, "indVar1"] - dfPred[2, "indVar1"]) * 100) /
														dfPred[2, "indVar1"]
	dfPred[4, "indVar1Chg"] <-   dfPred[4, "indVar1"] - dfPred[3, "indVar1"]
	dfPred[4, "indVar1Pct"] <- ((dfPred[4, "indVar1"] - dfPred[3, "indVar1"]) * 100) /
														dfPred[3, "indVar1"]
	dfPred[5, "indVar1Chg"] <-   dfPred[5, "indVar1"] - dfPred[4, "indVar1"]
	dfPred[5, "indVar1Pct"] <- ((dfPred[5, "indVar1"] - dfPred[4, "indVar1"]) * 100) /
														dfPred[4, "indVar1"]
	dfPred[6, "indVar1Chg"] <-   dfPred[6, "indVar1"] - dfPred[5, "indVar1"]
	dfPred[6, "indVar1Pct"] <- ((dfPred[6, "indVar1"] - dfPred[5, "indVar1"]) * 100) /
													    dfPred[5, "indVar1"]
	dfPred[7, "indVar1Chg"] <-   dfPred[7, "indVar1"] - dfPred[6, "indVar1"]
	dfPred[7, "indVar1Pct"] <- ((dfPred[7, "indVar1"] - dfPred[6, "indVar1"]) * 100) /
														dfPred[6, "indVar1"]
	dfPred[8, "indVar1Chg"] <-   dfPred[8, "indVar1"] - dfPred[7, "indVar1"]
	dfPred[8, "indVar1Pct"] <- ((dfPred[8, "indVar1"] - dfPred[7, "indVar1"]) * 100) /
														dfPred[7, "indVar1"]

#-------------------------------------------------------------------------------------------
#   Calculate changes in indVar2 between the years.
#-------------------------------------------------------------------------------------------
	dfPred[1, "indVar2Chg"] <-   dfPred[1, "indVar2"] - dfBase$indVar2
	dfPred[1, "indVar2Pct"] <- ((dfPred[1, "indVar2"] - dfBase$indVar2)       * 100) /
													 	dfBase$indVar2
	dfPred[2, "indVar2Chg"] <-   dfPred[2, "indVar2"] - dfPred[1, "indVar2"]
	dfPred[2, "indVar2Pct"] <- ((dfPred[2, "indVar2"] - dfPred[1, "indVar2"]) * 100) /
														dfPred[1, "indVar2"]
	dfPred[3, "indVar2Chg"] <-   dfPred[3, "indVar2"] - dfPred[2, "indVar2"]
	dfPred[3, "indVar2Pct"] <- ((dfPred[3, "indVar2"] - dfPred[2, "indVar2"]) * 100) /
														dfPred[2, "indVar2"]
	dfPred[4, "indVar2Chg"] <-   dfPred[4, "indVar2"] - dfPred[3, "indVar2"] 
	dfPred[4, "indVar2Pct"] <- ((dfPred[4, "indVar2"] - dfPred[3, "indVar2"]) * 100) /
														dfPred[3, "indVar2"]
	dfPred[5, "indVar2Chg"] <-   dfPred[5, "indVar2"] - dfPred[4, "indVar2"]
	dfPred[5, "indVar2Pct"] <- ((dfPred[5, "indVar2"] - dfPred[4, "indVar2"]) * 100) /
													 	dfPred[4, "indVar2"]
	dfPred[6, "indVar2Chg"] <-   dfPred[6, "indVar2"] - dfPred[5, "indVar2"]
	dfPred[6, "indVar2Pct"] <- ((dfPred[6, "indVar2"] - dfPred[5, "indVar2"]) * 100) /
													    dfPred[5, "indVar2"]
	dfPred[7, "indVar2Chg"] <-   dfPred[7, "indVar2"] - dfPred[6, "indVar2"]
	dfPred[7, "indVar2Pct"] <- ((dfPred[7, "indVar2"] - dfPred[6, "indVar2"]) * 100) /
														dfPred[6, "indVar2"]
	dfPred[8, "indVar2Chg"] <-   dfPred[8, "indVar2"] - dfPred[7, "indVar2"]
	dfPred[8, "indVar2Pct"] <- ((dfPred[8, "indVar2"] - dfPred[7, "indVar2"]) * 100) /
														dfPred[7, "indVar2"]
																 
#-------------------------------------------------------------------------------------------
#	Format numbers for readability.
#-------------------------------------------------------------------------------------------
	dfPred$depVar1 		<- formatNumber(dfPred$depVar1,3)
	dfPred$depVar1Chg 	<- formatNumber(dfPred$depVar1Chg,3)
	dfPred$depVar1Pct	<- formatNumber(dfPred$depVar1Pct,3)
	dfPred$indVar1 		<- formatNumber(dfPred$indVar1,2)
	dfPred$indVar1Chg 	<- formatNumber(dfPred$indVar1Chg,2)
	dfPred$indVar1Pct 	<- formatNumber(dfPred$indVar1Pct,3)
	dfPred$indVar2 		<- formatNumber(dfPred$indVar2,0)
	dfPred$indVar2Chg	<- formatNumber(dfPred$indVar2Chg,0)
	dfPred$indVar2Pct	<- formatNumber(dfPred$indVar2Pct,3)
	
#-------------------------------------------------------------------------------------------
#	Return data for screen table. Note stackComp variables could be dropped.
#-------------------------------------------------------------------------------------------
	return(dfPred)
}