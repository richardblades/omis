#'------------------------------------------------------------------------------------------
#'
#' 	u s e r s C u r r e n t . R
#'
#' 	Provide current view of .users.rda.
#'
#'------------------------------------------------------------------------------------------
usersCurrent <- function(
	)
  	{



#-------------------------------------------------------------------------------------------
#
#	Establish filePath to /extdata/users.rda and load.
#
#-------------------------------------------------------------------------------------------
 	pathBinary <- system.file("extdata", ".users.rda", package="omis")
	load(pathBinary)                        			 	  # Load /extdata/.users.rda

	

#-------------------------------------------------------------------------------------------
#
# 	Return relevant columns within .users.rda. (Pre-sorted within login.R by email).
#
#-------------------------------------------------------------------------------------------
	usersCurrent <- users[, c("email", "userType")]
	return(usersCurrent)
    }
