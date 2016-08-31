#'------------------------------------------------------------------------------------------
#'  
#'	n u t s 4 G 5 5 2 M o d e l P r d . R
#'
#'	NUTS-4 | Group 552 | (turnover552s07 ~ bedMean + malePop) | Predict
#'
#'  @param depVar1		:	("turnover552s07")
#'  @param depVar1Ttl	:	("turnover £m")
#'  @param factor1		:	("%","1"..."9")
#'  @param factor2		:	("%","1"..."9")
#'  @param geography	:	("nuts4")
#'  @param indVar1		: 	("bedMean")
#'  @param indVar2		: 	("malePop")
#'  @param inputData	:	array of user entered data
#'  @param modelTo		:	("2012")
#'  @param options		:	("confidence", "modelAttr", "nonlinear")
#'  @param predictTo	:	("2020")
#'
#'	Note, JavaScript returns the array "inputData" holding either numerics or NA. Hence
#'	specifying options("stringAsFactors = FALSE") as a session wide setting, though often
#'	prudent, is this case, unecessary.
#'
#'------------------------------------------------------------------------------------------
nuts4G552ModelPrd <- function (
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
#	Get independent variable data (Occupancy)
#
#-------------------------------------------------------------------------------------------
 	systemFile <- system.file("data", package="omis")
 	rdaFile <- paste0(systemFile, "/", geography, "MonthOccupancy.rda")
	load(rdaFile)
	dfName <- paste0(geography, "MonthOccupancy")
	occu <- get(dfName)	
	occu <- occu [, c("year", "srvBed", "scBed")]
	
#-------------------------------------------------------------------------------------------
#   Generate summary accommodation metrics
#-------------------------------------------------------------------------------------------
	for (i in 1:nrow(occu)) {
	    occu[i,"bed"] <- sum(occu[i,"srvBed"], occu[i,"scBed"], na.rm=TRUE) / 2
	}
	
#-------------------------------------------------------------------------------------------
#	Generate metrics for given period.
#	Note "na.rm=TRUE" excludes "NA" from analysis and ensures result is not set to NA.
#-------------------------------------------------------------------------------------------
	summariseBy <- c("year")

	library(plyr)

	occu <- ddply(occu, summariseBy, summarize, bedMean=mean(bed, na.rm=TRUE))
	
#-------------------------------------------------------------------------------------------
#   Re-scale for readability and align independent variable names with screen counterparts.
#-------------------------------------------------------------------------------------------
#	occu$bedMean <- occu$bedMean / ???
    colnames(occu)[which(names(occu) == "bedMean")] <- "indVar1"



#-------------------------------------------------------------------------------------------
#   
#	Get independent variable data (Demographic).
#
#-------------------------------------------------------------------------------------------
	rdaFile <- paste0(systemFile, "/", geography, "Year.rda")
	load(rdaFile)
	dfName <- paste0(geography, "Year")
	demo <- get(dfName)	
	
#-------------------------------------------------------------------------------------------
#   Select columns of interest
#-------------------------------------------------------------------------------------------
	demo <- demo [, c("malePop", "year" )]
		
#-------------------------------------------------------------------------------------------
#   Re-scale for readability and align independent variable names with screen counterparts.
#-------------------------------------------------------------------------------------------
#	demo$malePop <- demo$malePop / ???
    colnames(demo)[which(names(demo) == "malePop")] <- "indVar2"
	
	
	
#-------------------------------------------------------------------------------------------
#   
#	Combine independent variables
#
#-------------------------------------------------------------------------------------------
	indVars <- merge(occu, demo, by=c("year"), all=TRUE)
	
	
	
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
	depVars <- depVars[, c("turnover552s07", "year" )]
	
#-------------------------------------------------------------------------------------------
#   Remove incomplete obs (is.NA) and outliers (2013 value atypically high). Could omit
#   rows based on column setting e.g. depVars[!is.na(depVars$turnover552s07),].
#-------------------------------------------------------------------------------------------
	depVars <- na.omit(depVars)
	depVars <- subset(depVars, year != 2013)	
	
#-------------------------------------------------------------------------------------------
#   Re-scale for readability and align dependent variable name with screen counterpart.
#-------------------------------------------------------------------------------------------
	depVars$turnover552s07 <- depVars$turnover552s07 / 1000000
    colnames(depVars)[which(names(depVars) == "turnover552s07")] <- "depVar1"
	
	
	
#-------------------------------------------------------------------------------------------
#   
#	Combine dependent and independent data. "all.x=TRUE" retains only matches where depVars
#	is present.
#
#-------------------------------------------------------------------------------------------
	dfActual <- merge(depVars, indVars, by=c("year"), all.x=TRUE)

#-------------------------------------------------------------------------------------------
#   Add status to enable differentiation via "factor".
#-------------------------------------------------------------------------------------------
	dfActual$status <- c("actual")
	
	
	
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
#	stopifnot("bedMean" %in% names(dfInput))
#	stopifnot("malePop" %in% names(dfInput))
	
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
	dfPred$status  	<- c("predicted")


	
#-------------------------------------------------------------------------------------------
#   
#   Produce graphical output
#
#-------------------------------------------------------------------------------------------
	
#-------------------------------------------------------------------------------------------
#   Combine predictions with actual data
#-------------------------------------------------------------------------------------------
	dfPlot <- rbind(dfActual, dfPred)
		
#-------------------------------------------------------------------------------------------
#   Specify breaks as a Date vector. Create date variable of class date to facilitate date
#	based scale
#-------------------------------------------------------------------------------------------
	dfPlot$date <- as.Date(paste("01Jan", dfPlot$year, sep=""),"%d%b%Y")
	library(scales)
	to <- paste0(predictTo, "-12-31")
	dateBreaks <- seq(as.Date("1998-01-01"), as.Date(to), by="1 year")
  
#-------------------------------------------------------------------------------------------
#   Rationalise axis
#-------------------------------------------------------------------------------------------
 	axisRange 	<- as.data.frame(range(dfPlot$depVar1))
 	axisFrom 	<- axisRange[1,1] - (axisRange[1,1] %% 50)
 	axisTo 		<- axisRange[2,1] - (axisRange[2,1] %% 50) +50
 	axisBreaks 	<- seq(axisFrom, axisTo, by=50)

#-------------------------------------------------------------------------------------------
#   Assemble ggplot
#-------------------------------------------------------------------------------------------
	library(ggplot2)
	library(plyr) 											# Used for the desc() function

	p <- ggplot(dfPlot, aes(x=date, y=depVar1))

  	p <- p + theme(legend.position="none")
	p <- p + theme(axis.text=element_text(size=14)) 				# change axis labels
	p <- p + theme(axis.text.x=element_text(angle=90, hjust=1)) 	# rotate text labels
	p <- p + theme(axis.title=element_text(size=14)) 				# change axis titles
	p <- p + theme(text=element_text(size=14)) 		  # change all text except geom_text
	p <- p + theme(legend.text=element_text(size=14)) 				  # legend text size

	p <- p + geom_point(aes(fill=factor(dfPlot$status)), size=4, shape=22, colour="darkred")

#-------------------------------------------------------------------------------------------
#	Note "limits" could have been used to constrain either axis to enable focus.
#  	p <- p + scale_y_continuous(breaks=axisBreaks, depVar1Ttl, limits=c(axisFrom, axisTo))
#-------------------------------------------------------------------------------------------
	p <- p + scale_y_continuous(breaks=pretty_breaks(n=10), depVar1Ttl) 
	p <- p + scale_x_date(breaks=dateBreaks, labels=date_format("%Y"))
	p <- p + scale_fill_manual(values=c("pink", "white"))

#-------------------------------------------------------------------------------------------
#   xintercept needs to be passed or calculated ???
#-------------------------------------------------------------------------------------------
  	p <- p + geom_vline(aes(xintercept=as.numeric(date[15])), linetype=2, colour="grey60")

#-------------------------------------------------------------------------------------------
#   Honour user options
#-------------------------------------------------------------------------------------------
  	if  ("nonlinear" %in% options) {
  		if	("confidence" %in% options)	{
   			p <- p + geom_smooth(alpha=0.2, colour="grey60", level=0.95)
  		} else {
  			p <- p + geom_smooth(alpha=0.2, colour="grey60", se=FALSE)
  		}
  	} else {
		if	("confidence" %in% options)	{
   			p <- p + geom_smooth(alpha=0.2, colour="grey60", level=0.95, method=lm)
  		} else {
  			p <- p + geom_smooth(alpha=0.2, colour="grey60", method=lm, se=FALSE)
  		}
  	}

  	if  ("modelAttr" %in% options) {
#  		modelAttr <- data.frame(date=min(dfPlot$date), depVar1=max(dfPlot$depVar1))
   		modelAttr <- data.frame(date=min(dfPlot$date), depVar1=axisTo)
  		modelAttr$text <- as.character(paste(r2, "\n", SE, "\n", PV, sep=""))
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
	dfPred$indVar1 		<- formatNumber(dfPred$indVar1,3) 				   	 # bedMean %
	dfPred$indVar1Chg 	<- formatNumber(dfPred$indVar1Chg,3)
	dfPred$indVar1Pct 	<- formatNumber(dfPred$indVar1Pct,3)
	dfPred$indVar2 		<- formatNumber(dfPred$indVar2,0) 		 			   # malePop
	dfPred$indVar2Chg	<- formatNumber(dfPred$indVar2Chg,0)
	dfPred$indVar2Pct	<- formatNumber(dfPred$indVar2Pct,3)

#-------------------------------------------------------------------------------------------
#	Return data for screen table
#-------------------------------------------------------------------------------------------
	return(dfPred)
}