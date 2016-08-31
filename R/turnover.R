#'------------------------------------------------------------------------------------------
#'
#'	t u r n o v e r . R
#'
#'	Model of Turnover against selected independent variables
#'
#'  @param geography:	("nuts1", "nuts3", "nuts4")
#'  @param indVars: 	(selected independent variables)
#'  @param options:		("yAxisFrom0")
#'  @param turnover:	("")
#'   
#'------------------------------------------------------------------------------------------
turnover <- function (
	geography,
	indVars,
	options,
	turnover
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
#	Load data object for given geography.
#
#-------------------------------------------------------------------------------------------
 	systemFile <- system.file("data", package="omis")
 	rdaFile <- paste0(systemFile, "/", geography, "Year.rda")
	load(rdaFile)
	dfName <- paste0(geography, "Year")
	df <- get(dfName)	
	
#-------------------------------------------------------------------------------------------
#   Remove ONS outliers. 
#-------------------------------------------------------------------------------------------
	df <- subset(df, year != 2013)
	
#-------------------------------------------------------------------------------------------
#   Keep only columns of interest. 
#-------------------------------------------------------------------------------------------
	df <- df[, colnames(df) %in% c(indVars, turnover)] 
		
#-------------------------------------------------------------------------------------------
#	Remove those rows that have no turnover values.
#-------------------------------------------------------------------------------------------
  	df <- df[!is.na(df[turnover]), ]
	
#-------------------------------------------------------------------------------------------
#   Re-scale turnover to enhance readability. Note turnover resolved to actual name passed.
#-------------------------------------------------------------------------------------------
	if 	(geography != "nuts1") {
		df[, turnover] <- df[, turnover] / 1000000
	}



#-------------------------------------------------------------------------------------------
#
#	If the user has selected a single independent variable then provide the model attributes
#
#-------------------------------------------------------------------------------------------
  	numIndVars <- as.data.frame(indVars)
  	if (nrow(numIndVars) == 1) {

#-------------------------------------------------------------------------------------------
#	Remove those rows that have no independent values. Note this can't be done where the
#   user has requested multiple independent variables as it will remove observations that 
#	hold full data for one but not another independent variable.
#-------------------------------------------------------------------------------------------
  	  	df <- df[!is.na(df[indVars]), ]

#-------------------------------------------------------------------------------------------
#   Build model
#-------------------------------------------------------------------------------------------
  	    formulaShort <- paste(turnover, " ~ ", indVars, sep="")
  	    model <- lm(formulaShort, data=df)

#-------------------------------------------------------------------------------------------
#   Extract model data
#-------------------------------------------------------------------------------------------
  	    rSquared 		<- formatNumber(summary(model)$r.squared, 3)
		adjRSquared 	<- formatNumber(summary(model)$adj.r.squared, 3)
		interceptCf 	<- formatNumber(summary(model)$coef[[1,1]], 0)
		interceptPV 	<- formatNumber(summary(model)$coef[[1,4]], 3)
		independentCf 	<- formatNumber(summary(model)$coef[[2,1]], 3)
		independentSE 	<- formatNumber(summary(model)$coef[[2,2]], 3)
		independentPV 	<- formatNumber(summary(model)$coef[[2,4]], 3)

		r2 		<- paste("r2: (", rSquared, ") adjusted (", adjRSquared, ")", sep="")
		SE 		<- paste("se: ", indVars, " (", independentSE, ")", sep="")
		PV 		<- paste("pv: ", indVars, " (", independentPV, ")", sep="")

		formula <- paste(turnover, " = ", independentCf, " * ", indVars, " + ",
			interceptCf, sep="")
	}



#-------------------------------------------------------------------------------------------
#
# 	Reshape data to suit ggplot function
#
#-------------------------------------------------------------------------------------------
	columnSubset <- c(turnover, indVars)
	dfWide <- subset(df, select=columnSubset)

#-------------------------------------------------------------------------------------------
# 	List of column headings list excluding first which is turnover
#-------------------------------------------------------------------------------------------
	columnNames <- names(dfWide)[-1]

	dfLong <- reshape(dfWide, direction="long", timevar="variable",
		varying=list(columnNames))

#-------------------------------------------------------------------------------------------
# 	Rationalise column names
#-------------------------------------------------------------------------------------------
  	names(dfLong)[1] <- "turnover" 	 	 # Need to do this as ggplot demands actual name
  	names(dfLong)[2] <- "variable"
  	names(dfLong)[3] <- "value"
  	dfLong$variable <- factor(dfLong$variable, labels = columnNames)



#-------------------------------------------------------------------------------------------
#
#   Assemble ggplot
#
#-------------------------------------------------------------------------------------------
	library(ggplot2)
	library(scales)												# Supports pretty_breaks

	p <- ggplot(dfLong, aes(x=value, y=turnover, colour=variable))

	p <- p + theme(axis.text=element_text(size=14)) 				# change axis labels
#	p <- p + theme(axis.text.x=element_text(angle=90, hjust=1)) 	# rotate text labels
	p <- p + theme(axis.title=element_text(size=14)) 				# change axis titles
	p <- p + theme(text=element_text(size=14)) 		  # change all text except geom_text
	p <- p + theme(legend.text=element_text(size=14)) 				  	   # legend text

	p <- p + geom_point(shape=22, size=4, colour="darkred", fill="pink")

  	p <- p + geom_smooth(method=lm, level=0.95, colour="grey60", alpha=0.2)
#	p <- p + stat_smooth(method=lm, fullrange=TRUE)

	if	("yAxisFrom0" %in% options) {
		p <- p + scale_y_continuous(breaks=pretty_breaks(n=10), limits=c(0,
			max(df$turnover)*1.05), "turnover £m") 		# 1.05=5% caters for geom_smooth 
	} else {
		p <- p + scale_y_continuous(breaks=pretty_breaks(n=10), "turnover £m") 
	}

	if (nrow(numIndVars) == 1) {
		p <- p + scale_x_continuous(breaks=pretty_breaks(n=10))     
	    if 	("yAxisFrom0" %in% options) {
	        modelAttr <- data.frame(variable=indVars, value=min(dfLong$value),
	    	    			turnover=c(0))
	    } else {
	        modelAttr <- data.frame(variable=indVars, value=min(dfLong$value),
	                    	turnover=max(dfLong$turnover))
	    }
  		modelAttr$text <- as.character(paste(r2, "\n", SE, "\n", PV, sep=""))
    	p <- p + geom_text(data=modelAttr, aes(label=text), size=6, colour="black",
    		hjust="inward",	vjust="inward")
   		p <- p + ggtitle(formula)
	} else {
		p <- p + scale_x_continuous(breaks=pretty_breaks(n=8))      
	    p <- p + theme(strip.text.x = element_text(size=14)) 				# facet text
	    p <- p + facet_wrap( ~ variable, scales="free", nrow=2)
  	}

#-------------------------------------------------------------------------------------------
# 	Let's go...
#-------------------------------------------------------------------------------------------
	print(p)
}
