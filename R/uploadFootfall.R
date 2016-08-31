#'------------------------------------------------------------------------------------------
#'
#'	u p l o a d F o o t f a l l . R
#'
#'	Upload Spring Board Footfall data via their JSON API
#'
#'  @param geography:
#'  @param rangeFrom:
#'  @param rangeTo:
#'
#-------------------------------------------------------------------------------------------
uploadFootfall <- function (   
	geography 	= c("nuts1", "nuts3", "nuts4"),
	rangeFrom 	= c("2006-05-15"), 		# ccyy-mm-dd First Spring Board camera installed  
	rangeTo 	= c("9999-12-31") 				  
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
#   Establish environment
# 	
#-------------------------------------------------------------------------------------------
	library(jsonlite)
	library(httr)								   # Simplifies URL and HTTP interaction
	
	
	
#-------------------------------------------------------------------------------------------
# 	
#   Load JSON from URL directly into R data frame. 
#
#-------------------------------------------------------------------------------------------
	rangeFromSB <- format(as.Date(rangeFrom), "%Y%m%d")
	rangeToSB 	<- format(as.Date(rangeTo), "%Y%m%d")
	urlStub   	<- "https://api.spring-board.info/api/footfalloutputinfo/"
	security  	<- "omis.scarborough@gmail.com/DGEYKsXa9TQX//"
	
	url <- paste0(urlStub, security, rangeFromSB, "/", rangeToSB, "/19970101/20971231")
 	
 	springboard <- fromJSON(url)
	df <- springboard$data
	  	
  	
  	
#-------------------------------------------------------------------------------------------
#   Select columns of interest
#-------------------------------------------------------------------------------------------
	df <- df[, c("LocationName", "ProcessDate", "AcceptedIn", "AcceptedOut")]

#-------------------------------------------------------------------------------------------
#   Replace ProcessDate with date and time
#-------------------------------------------------------------------------------------------
    df$date <- as.Date(df$ProcessDate, "%Y-%m-%dT%H:%M:%S")
    df$time <- format(strptime(df$ProcessDate, "%Y-%m-%dT%H:%M:%S"), "%H:%M:%S")
    df <- subset(df, select= -c(ProcessDate))	
    
    

#-------------------------------------------------------------------------------------------
#	
#    Generate camera based data frames and their directional variables via column renames
#
#-------------------------------------------------------------------------------------------
    w      <- subset(df, df$LocationName %in% c("Westborough"))
    nNorth <- subset(df, df$LocationName %in% c("Newborough North"))
    nSouth <- subset(df, df$LocationName %in% c("Newborough South"))
    
    colnames(w)		[which(names(w) 	 == "AcceptedIn" )] <- "westborough2East"
    colnames(w)		[which(names(w) 	 == "AcceptedOut")] <- "westborough2West"
    colnames(nNorth)[which(names(nNorth) == "AcceptedIn" )] <- "newboroughNorth2West"
    colnames(nNorth)[which(names(nNorth) == "AcceptedOut")] <- "newboroughNorth2East"
    colnames(nSouth)[which(names(nSouth) == "AcceptedIn" )] <- "newboroughSouth2West"
    colnames(nSouth)[which(names(nSouth) == "AcceptedOut")] <- "newboroughSouth2East"
    
    w      <- subset(w,      select = -c(LocationName))	
    nNorth <- subset(nNorth, select = -c(LocationName))	
    nSouth <- subset(nSouth, select = -c(LocationName))	
    
    
    
#-------------------------------------------------------------------------------------------
#    
#    Re-combine data. Note, merges are set with "all=TRUE" to ensure that all unmatched 
#    obserations are also kept.
#
#-------------------------------------------------------------------------------------------
    
#-------------------------------------------------------------------------------------------
#	Build newborough(n) from newboroughNorth(nNorth) and newboroughSouth(nSouth)
#   Here camera figures are summed as they are capturing a share of the footfall on that
#	street.	Essentially, it little matters which side on the road (path) the pedestrian
#	took.	
#-------------------------------------------------------------------------------------------
    n <- merge(nNorth, nSouth, by=c("date","time"), all=TRUE)
#-------------------------------------------------------------------------------------------
#   Note, camera readings may be set to NA where it was either not installed or simply out
#	of operation. To enable summation these entries will be set to zero.
#-------------------------------------------------------------------------------------------
    n[is.na(n)] <- 0
    
    n$newborough2West <- n$newboroughNorth2West + n$newboroughSouth2West
    n$newborough2East <- n$newboroughNorth2East + n$newboroughSouth2East
    n <- subset(n, select = c(date, time, newborough2East, newborough2West))	
    
#-------------------------------------------------------------------------------------------
#   Combine westborough(w) with newborough(n)
#-------------------------------------------------------------------------------------------
    uploaded <- merge(n, w, by=c("date", "time"), all=TRUE)	
    nUploaded <- nrow(uploaded)

    
    
#-------------------------------------------------------------------------------------------
#   
#   Append upload to existing data before removing duplicates. The latter allows for reruns
#   and corrective processing.
#
#-------------------------------------------------------------------------------------------
    
#-------------------------------------------------------------------------------------------
#   Get existing data
#-------------------------------------------------------------------------------------------
 	systemFile <- system.file("data", package="omis")
	rdaFile <- paste0(systemFile, "/", geography, "HourFootfall.rda")
	load(rdaFile)
	dfName <- paste0(geography, "HourFootfall")
	existing <- get(dfName)	
    
#-------------------------------------------------------------------------------------------
#   Combine 
#-------------------------------------------------------------------------------------------
    nExisting <- nrow(existing)
    combined <- rbind(uploaded, existing)
    nCombined <- nrow(combined)
    
#-------------------------------------------------------------------------------------------
#   Remove any duplicates 
#-------------------------------------------------------------------------------------------
    resultant <- unique(combined) 	
    
#-------------------------------------------------------------------------------------------
#   Sort by date & time
#-------------------------------------------------------------------------------------------
    resultant <- resultant[order(resultant$date, resultant$time), ]		 		   
    nResultant <- nrow(resultant)
    nDuplicates <- nCombined - nResultant
    
    uploadFigures <- paste("Row Counts: Uploaded=", nUploaded, " Existing=",
    	nExisting, " Duplicates=", nDuplicates, " Resultant=", nResultant, sep="")
    
    
    
#-------------------------------------------------------------------------------------------
#   
#	Save resultant data frame to an external .rda file.
#
#-------------------------------------------------------------------------------------------
        
#-------------------------------------------------------------------------------------------
#	Assign effectively creates a pointer to the resultant data frame using the literal
# 	value held by dfName.
#-------------------------------------------------------------------------------------------
    assign(dfName, resultant)           
          
#-------------------------------------------------------------------------------------------
# 	Save objects referenced as literals within the list.
#-------------------------------------------------------------------------------------------
    save(list=dfName, file=rdaFile)
    
    
    
#-------------------------------------------------------------------------------------------
#   
#	Provide graphical summary of successful upload via "trend" profile. Refer to
#	nuts4Footfall.R for explanation of code.
#
#-------------------------------------------------------------------------------------------
	load(rdaFile)
	df <- get(dfName)	
	
	df$yearMonthNum <- strftime(df$date,"%Y-%m") # (2016-01... 2016-12)
	library(plyr)
	dfWide <- ddply(df, .(yearMonthNum), summarize,
		newborough2East  = quantile(newborough2East , probs=0.95, na.rm=TRUE),
		newborough2West  = quantile(newborough2West , probs=0.95, na.rm=TRUE),
		westborough2East = quantile(westborough2East, probs=0.95, na.rm=TRUE),
		westborough2West = quantile(westborough2West, probs=0.95, na.rm=TRUE))
		
	columnNames <- names(dfWide)[-1]
	dfLong <- reshape(dfWide, direction="long", timevar="variable",
		varying=list(columnNames))
	names(dfLong)[1] <- "yearMonthNum"
	names(dfLong)[2] <- "variable"
  	names(dfLong)[3] <- "value"
  	dfLong$variable <- factor(dfLong$variable, labels=columnNames)
  	dfLong$yearMonthNum <- as.Date(paste0(dfLong$yearMonthNum, "-01"),"%Y-%m-%d")



#-------------------------------------------------------------------------------------------
#
#   Assemble and call ggplot
#
#-------------------------------------------------------------------------------------------
	library(ggplot2)
  	library(scales)

  	p <- ggplot(dfLong, aes(x=yearMonthNum, y=value, colour=variable, group=variable))

 	p <- p + theme(axis.text=element_text(size=14)) 				# change axis labels
	p <- p + theme(axis.text.x=element_text(angle=90, hjust=1)) 	# rotate text labels
	p <- p + theme(axis.title=element_text(size=14)) 				# change axis titles
	p <- p + theme(text=element_text(size=14)) 		  # change all text except geom_text
	p <- p + theme(legend.text=element_text(size=14)) 				  # legend text size
	p <- p + theme(legend.position="bottom")
	p <- p + theme(legend.title=element_blank())
	p <- p + theme(legend.key.width=unit(3,"line"))
	p <- p + theme(plot.title=element_text(size=14))
	p <- p + ggtitle(uploadFigures)
	p <- p + xlab("summarised by year and month")
	p <- p + ylab("95th percentile footfall / hour")
	p <- p + geom_point(shape=21, size=1)
	p <- p + geom_line()
	p <- p + scale_y_continuous(breaks=pretty_breaks(n=10))
	p <- p + scale_x_date(date_breaks="1 year", date_minor_breaks="1 month",
	    	date_labels="%Y")

#-------------------------------------------------------------------------------------------
# 	Let's go...
#-------------------------------------------------------------------------------------------
	print(p)
}