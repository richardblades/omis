#'------------------------------------------------------------------------------------------
#'
#'  u p l o a d S p a t i a l . R
#'
#' 	Upload and process spatial data. 
#'
#'  @param 	geography 		("nuts4")
#'  @param 	options
#'	@param  shpBuilding			 
#'  @param 	shp
#'  @param 	shpFunctionalSite
#'  @param 	shpImportantBuilding
#'  @param 	shpOAC
#'
#'------------------------------------------------------------------------------------------
uploadSpatial <- function (
	geography,
	options,
	shpBuilding,
	shpFunctionalSite,
	shpImportantBuilding,
	shpOAC
	)
  	{



#-------------------------------------------------------------------------------------------
#
#	Establish working environment
#
#-------------------------------------------------------------------------------------------
    options(StringsAsFactors=FALSE)
	library(sp)																	# over()
	library(maptools)
	library(raster) 													   # intersect()
	library(rgdal)
	library(rgeos)
	library(tmaptools) 											# append_data(), sbind()
    library(spdplyr) 													 # Spatial dplyr
    library(dplyr) 	  # dplyr functions take precidence as last load. Else dplyr::select



#-------------------------------------------------------------------------------------------	
# 	
#	Read data covering 
#		specific OS Open Map Local shapefiles (within the TA sector) and 
#		the CDRC OAC GeoDemographics for the Scarborough LAD. 
#
#-------------------------------------------------------------------------------------------	
	systemFile <- system.file("data", package="omis")
	
	Building 			<- read_shape(file.path(paste0(shpBuilding)))
	FunctionalSite 		<- read_shape(file.path(paste0(shpFunctionalSite )))
	ImportantBuilding 	<- read_shape(file.path(paste0(shpImportantBuilding)))
	OAC 				<- read_shape(file.path(paste0(shpOAC)))
	
	
	
#-------------------------------------------------------------------------------------------	
#	
#	Align OAC with OSGB36 CRS (as differs slightly from the more accurate OSGB36).
#
#-------------------------------------------------------------------------------------------	
 	OAC	<- spTransform(OAC, CRS(proj4string(Building))) 

	
		 	
#-------------------------------------------------------------------------------------------	
# 	
#	Clip data according to respective OAC boundaries.	
#	
#   Utilise spatial subsetting. Note, neither dplyr nor spdplyr can	achieve this. The
#	following is by far the superior method as target data sat outside the origin's polygon
#	is removed.
#
#-------------------------------------------------------------------------------------------	
	Building 			<- Building[OAC, ]
	FunctionalSite 		<- FunctionalSite[OAC, ]
	ImportantBuilding 	<- ImportantBuilding[OAC, ]
	
	
	
#-------------------------------------------------------------------------------------------
#
#	Recalculate intersections between OAC and OS Open Map Local Building data
#
#-------------------------------------------------------------------------------------------
  	
#-------------------------------------------------------------------------------------------
#	Acquire new attributes via spatial join and intersections.
#
#	over(base, comparitor[c("attribute1", "attribute2")])
#		Performs a spatial join. base is compared with comparitor.
#		If spatial match found then comparitor attributes are captured.
#		if (returnList=FALSE) then the attributes of the LAST match are stored in a new data
#		 	frame whose row number is dictated by the base.
#		if (returnList=TRUE) then ALL spatial matches are stored in a new list (of data
#			frame elements)
#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------
#	Determine now many OACs a building is split across.
#-------------------------------------------------------------------------------------------
	overLength <- over(Building, OAC[, "grpcode"], fn=length)  	   # length=split number
	overLength <- rename(overLength, overLength=grpcode)
	BuildingOver <- append_data(Building, overLength)

#-------------------------------------------------------------------------------------------
#	Split Building data into those that sit wholely within one OAC and those that straddle
#	several.
#-------------------------------------------------------------------------------------------
	BuildingWhole <- subset(BuildingOver, BuildingOver@data$overLength <= 1,
						select=-c(overLength))
	BuildingSplit <- subset(BuildingOver, BuildingOver@data$overLength > 1,
						select=-c(overLength))

#-------------------------------------------------------------------------------------------
#	BuildingWhole - Acquire OAC attributes via spatial join.
#-------------------------------------------------------------------------------------------
 	BuildingWholeOver 			<- over(BuildingWhole, OAC["grpcode"])
  	BuildingWholeOAC 			<- append_data(BuildingWhole, BuildingWholeOver)
## 	BuildingWholeOAC$grpcode 	<- as.factor(BuildingWholeOAC$grpcode)

#-------------------------------------------------------------------------------------------
#   BuildingSplit - Acquire OAC attributes (per newly formed split) via raster::intersect
#
#   raster::insersect preferred to rgeos::gInersection as it copies accross all relevant
#	attribute data thereby cutting down on further merges.
#-------------------------------------------------------------------------------------------
 	BuildingSplitOAC <- raster::intersect(BuildingSplit, OAC["grpcode"])

#-------------------------------------------------------------------------------------------
#	Combine all Building data
#-------------------------------------------------------------------------------------------
 	BuildingOAC <- sbind(BuildingSplitOAC, BuildingWholeOAC)

#-------------------------------------------------------------------------------------------
#	Re-project data to a CRS that is acceptable to Leaflet. Note, entering Leaflet via tmap
# 	would make this step superfluous.
#		EPSG:3395	= 	WGS84 "World Mercator"
#		EPSG:3857	= 	"Spherical Mercator" aka "Web Mercator" (Google, Leaflet, OSM...)
#						EPSG:900913 is OpenLayer's equivalent.
#		EPSG:4326	= 	WGS84 (GPS, NATO...)
#
#	The Proj4Leaflet plugin may be used to employ other CRSs.
#-------------------------------------------------------------------------------------------
	Building    	  <- spTransform(Building,    	  	CRS("+init=epsg:4326"))
	BuildingOAC       <- spTransform(BuildingOAC,       CRS("+init=epsg:4326"))
	FunctionalSite    <- spTransform(FunctionalSite,    CRS("+init=epsg:4326"))
	ImportantBuilding <- spTransform(ImportantBuilding, CRS("+init=epsg:4326"))
	OAC         	  <- spTransform(OAC,         		CRS("+init=epsg:4326"))

#-------------------------------------------------------------------------------------------
#	Preserve data for future recall.
#
# 	R ".rda" used rather than "rds". rds is by far the most convenient to use but alas
#  	R packages do not recognise them as a fully-fledged component. Consequently, OpenCPU
# 	API ignores their presence.
#	
#	Overwrite files held within the data system folder of the omis ("compiled") package.
#------------------------------------------------------------------------------------------
	systemFile <- system.file("data", package="omis")
	save(Building, 			file=paste0(systemFile, "/", geography, "Building.rda"))
	save(BuildingOAC, 		file=paste0(systemFile, "/", geography, "BuildingOAC.rda"))
	save(FunctionalSite, 	file=paste0(systemFile, "/", geography, "FunctionalSite.rda"))
	save(ImportantBuilding, file=paste0(systemFile, "/", geography,
							"ImportantBuilding.rda"))
	save(OAC, 				file=paste0(systemFile, "/", geography, "OAC.rda"))
		
#------------------------------------------------------------------------------------------
#	If we're going to rebuild ("compile") the omis package then we must update the 
#	"development" source copy so that its contents are propagated
#------------------------------------------------------------------------------------------
##	save(Building, 			file=paste0("data/", geography, "Building.rda"))
##	save(BuildingOAC, 		file=paste0("data/", geography, "BuildingOAC.rda"))
##	save(FunctionalSite, 	file=paste0("data/", geography,	"FunctionalSite.rda"))
##	save(ImportantBuilding, file=paste0("data/", geography, "ImportantBuilding.rda"))
##	save(OAC, 				file=paste0("data/", geography, "OAC.rda"))
}
