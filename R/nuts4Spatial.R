#'------------------------------------------------------------------------------------------
#'
#'  n u t s 4 S p a t i a l . R
#'
#'  NUTS-4 | Map spatial data
#'
#'  @param 	aggregation ("oac", "postcode", "workplaceZone")
#'  @param 	data 		("carParking", "footfall", "oacDemographic", "occupancy")
#'  @param 	geography 	("nuts4")
#'  @param 	options 	()
#'
#'
#'  Ensure latest releases are in play:
#'  	devtools::install_github("rstudio/leaflet")
#'		devtools::install_github('bhaskarvk/leaflet.extras')
#'
#'------------------------------------------------------------------------------------------
nuts4Spatial <- function (
	aggregation,
	data,
	geography,
	options
	)
  	{



#-------------------------------------------------------------------------------------------
#
#	Establish working environment
#
#-------------------------------------------------------------------------------------------
	library(sp)																	# over()
	library(leaflet)
	library(maptools)
	library(raster) 													   # intersect()
	library(rgdal)
	library(rgeos)
##	library(tmaptools) 											# append_data(), sbind()
##  library(spdplyr) 													 # Spatial dplyr
    library(dplyr) 	  # dplyr functions take precidence as last load. Else dplyr::select



#-------------------------------------------------------------------------------------------
#
#	OAC Palette FULL
#
#-------------------------------------------------------------------------------------------
#    											subdued pallette
#	supgrp 	1	Rural Residents
#		grp 1a	Farming Communities				#4b5715
#		grp 1b	Rural Tenants					#697144	Olive
#		grp 1c	Ageing Rural Dwellers			#869663
#	supgrp 	2	Cosmopolitans
#		grp 2a	Students Around Campus			#50545c
#		grp 2b	Inner-City Students 			#64728f	Blue Grey
#		grp 2c	Comfortable Cosmopolitans		#8791a5
#		grp 2d	Aspiring & Affluent 			#adb9d1
#	supgrp 	3	Ethnicity Central
#		grp 3a	Ethnic Family Life				#5e0a0f
#		grp 3b	Endeavouring Ethnic Mix			#94181f	Maroon
#		grp 3c	Ethnic Dynamics					#913337
#		grp 3d	Aspirational Techies			#bf8386
#	supgrp 	4	Multicultural Metropolitans
#		grp 4a	Rented Family Living			#49405c
#		grp 4b	Challenged Asian Terraces		#7866a0
#		grp 4c	Asian Traits					#957aca	Purple
#	supgrp 	5	Urbanites
#		grp 5a	Urban Professionals & Families 	#695843
#		grp 5b	Ageing Urban Living				#897358	Brown
#	supgrp 	6	Suburbanites
#		grp 6a	Suburban Achievers				#c3a858	Gold
#		grp 6b	Semi-Detached Suburbia			#d6b552
#	supgrp 	7	Constrained City Dwellers
#		grp 7a	Challenged Diversity			#aa7e7e
#		grp 7b	Constrained Flat Dwellers		#bc8f8f	Salmon
#		grp 7c	White Communities				#dea3a3
#		grp 7d	Ageing City Dwellers			#fab8b8
#	supgrp 	8	Hard-Pressed Living
#		grp 8a	Industrious Communities			#4e6794
#		grp 8b	Challenged Terraced Workers		#6385c3	Blue
#		grp 8c	Hard-Pressed Ageing Workers		#719ae2
#		grp 8d	Migration & Churn				#acc7f8

	subdued <- c(
		"#4b5715", 	"#697144",	"#869663",	"#546078",	"#64728f",	"#8791a5",	"#adb9d1",
		"#6a1015",	"#932e34",	"#a85e61",	"#bf9193",	"#5b5071",	"#7866a0",	"#957aca",
		"#695843",	"#897358",	"#c3a858",	"#d6b552", 	"#aa7e7e",	"#bc8f8f",	"#dea3a3",
		"#fab8b8",	"#4e6794",	"#6080b9",	"#719ae2",	"#acc7f8")

    grpcodeOAC <- c(
        "1a", 		"1b", 	   	"1c",   	"2a", 		"2b", 		"2c", 		"2d",
        "3a", 		"3b", 		"3c", 		"3d",       "4a", 		"4b", 		"4c",
        "5a", 		"5b", 		"6a", 		"6b", 		"7a", 		"7b", 		"7c",
        "7d",     	"8a", 		"8b", 		"8c", 		"8d")

#-------------------------------------------------------------------------------------------
#	Generate OAC colour palette function. Note, this is not a lookup type function, instead
#	its a relative factor...
#-------------------------------------------------------------------------------------------
  	paletteOAC <- colorFactor(subdued, grpcodeOAC)



#-------------------------------------------------------------------------------------------
#
# 	Recall data.
#
#	Loads data from the data system folder within the omis ("compiled") package.
#
#-------------------------------------------------------------------------------------------
 	systemFile <- system.file("data", package="omis")
	load(paste0(systemFile, "/", geography, "BuildingOAC.rda"))
	load(paste0(systemFile, "/", geography, "FunctionalSite.rda"))
	load(paste0(systemFile, "/", geography, "ImportantBuilding.rda"))
	load(paste0(systemFile, "/", geography, "OAC.rda"))



#-------------------------------------------------------------------------------------------
#
#	Establish urlTemplates for Mapbox styles.
#
#	MapboxLight is provided free by Mapbox. This Mapbox "style" may be referenced without
#	limit. ??? Can the buildings layer be switched off via JavaScript???
#
#	MapboxLightEdit is a copy of the Mapbox Light style. It has had its "building" hidden
#	from view. This style is limited to 50,000 views/month as per the free account.
#
#-------------------------------------------------------------------------------------------
	accessToken <- "pk.eyJ1IjoicmljaGFyZGJsYWRlcyIsImEiOiJjaXhycDlyZ2EwMDVlMnFwbzNlOWt3MmV3In0.lQSyDE4fN8xbpgmskdKDkQ"

	urlTemplateLight <- "https://api.mapbox.com/styles/v1/mapbox/light-v9/tiles/256/{z}/{x}/{y}?access_token="
	mapboxLight <- paste0(urlTemplateLight, accessToken)

	urlTemplateLightEdit <-
"https://api.mapbox.com/styles/v1/richardblades/ciyn3zm6b00as2spj8t8mfe1z/tiles/256/{z}/{x}/{y}?access_token="
	mapboxLightEdit <- paste0(urlTemplateLightEdit, accessToken)



#-------------------------------------------------------------------------------------------
#
#	Utilise Font Awesome with marker icons.
#	Library argument, ‘glyphicon’ (default), 'fa' (Font Awesome) or 'ion' (Ionicons).
#
#-------------------------------------------------------------------------------------------
	marketIon <- makeAwesomeIcon(
	    icon="pizza", 						 # beer, coffee, ios-cart-outline, wineglass
	    iconColor="white",
	    library="ion",
	    markerColor="gray"
	)

	marketFa <- makeAwesomeIcon(
 	    icon="shopping-basket", 							 # bus, car, coffee, cutlery
	    iconColor="white",
	    library="fa",
	    markerColor="gray",
 	    spin=TRUE 										# Only available to library "fa"
	)

	marketGlyphicon <- makeAwesomeIcon(
	    icon="shopping-cart", 												   # cutlery
	    iconColor="white",
	    library="glyphicon",
	    markerColor="gray"
	)



#-------------------------------------------------------------------------------------------
#
#	Create test data that covers entire palette
#
#-------------------------------------------------------------------------------------------
## 	for (i in 1:nrow(BuildingOAC@data)) {
## 		if (i >=   1 && i <= 500) { BuildingOAC@data$grpcode[i] <- "2b" }
## 		if (i >= 501 && i <=1000) { BuildingOAC@data$grpcode[i] <- "2c" }
## 		if (i >=1001 && i <=1500) { BuildingOAC@data$grpcode[i] <- "2d" }
## 		if (i >=1501 && i <=2000) { BuildingOAC@data$grpcode[i] <- "3a" }
## 		if (i >=2001 && i <=2500) { BuildingOAC@data$grpcode[i] <- "3b" }
## 		if (i >=2501 && i <=3000) { BuildingOAC@data$grpcode[i] <- "3c" }
## 		if (i >=3001 && i <=3500) { BuildingOAC@data$grpcode[i] <- "3d" }
## 		if (i >=3501 && i <=4000) { BuildingOAC@data$grpcode[i] <- "4a" }
## 		if (i >=4001 && i <=4500) { BuildingOAC@data$grpcode[i] <- "4b" }
## 		if (i >=4501 && i <=5000) { BuildingOAC@data$grpcode[i] <- "4c" }
## 	}



#-------------------------------------------------------------------------------------------
#
#	Build Interactive leaflet Map - Utilising Ordnance Survey data.
#
#	Output from htmlwidgets has to be stored in an <iframe> as it constitutes an entire
#	independent page content, including components such as <script>
#
#-------------------------------------------------------------------------------------------
	widget <- leaflet() %>%
	    addAwesomeMarkers(lng=-0.39650828, lat=54.283761, icon=marketGlyphicon,
	       label="Beer, coffee, pizza and wine are available...",
	       popup="Market Hall & Vaults") %>%
	    addPolygons(data=FunctionalSite, fill=TRUE,
            fillColor="#f0eeed", fillOpacity=0.7,
            group="FunctionalSite", stroke=FALSE) %>%
	    addPolygons(data=BuildingOAC, fill=TRUE,
	       fillColor= ~paletteOAC(BuildingOAC$grpcode), fillOpacity=1.0,
	       group="BuildingIntersect", stroke=FALSE) %>%
	    addPolygons(data=ImportantBuilding, color="gray", fill=TRUE,
            fillColor="#ffffff", fillOpacity=1.0, group="ImportantBuilding",
            opacity=0.5, stroke=TRUE, weight=0.5) %>%
	    addPolylines(data=OAC, color="gray", dashArray="1,2", group="OAC",
	       opacity=0.5, weight=0.5) %>%
    	addLayersControl(
##         	baseGroups=c("Mapbox Light", "Mapbox Light Edit"),
 	        overlayGroups=c("OAC"),
         	options=layersControlOptions(autoZIndex=TRUE, collapsed=TRUE)) %>%
##      Full legend better provided from within HTML.
##	    addLegend(opacity=1.0, pal=paletteOAC, position="bottomright",
##	        values=BuildingOAC$grpcode) %>%
	    addTiles(mapboxLightEdit,
	    attribution='<a href="http://mapbox.com/" target="_blank">Mapbox</a> | OS Open Data',
	       	group="Mapbox Light Edit", options=tileOptions(minZoom=13, maxZoom=17)) %>%
##	    addTiles(mapboxLight, group="Mapbox Light") %>%
	    fitBounds(-1.064683, 54.132420, -0.2123534, 54.5621437) %>%
	    setMaxBounds(-1.064683, 54.132420, -0.2123534, 54.5621437) %>%
	    setView(-0.39650828, 54.283761, zoom=15)
	    
#-------------------------------------------------------------------------------------------
#	Note, "selfcontained=FALSE" is vital with Ubuntu environment. Else Pandoc fails with
#	OpenCPU server message "strsplit(version_info....", which under RStudio-Server equates
#	to "Segmentation fault... Error: pandoc document conversion failed with error 139"
#-------------------------------------------------------------------------------------------
	htmlwidgets::saveWidget(widget, "leafletMap.html", selfcontained=FALSE)
}
