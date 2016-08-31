#'------------------------------------------------------------------------------------------
#'
#' 	u s e r s U p d a t e . R
#'
#' 	Update users accounts within /extdata/.users.rda.
#'
#' 	@param :	usersUpdates
#'
#'	OpenCPU Server note regarding "rJava" dependent packages.
#'	When OpenCPU is accessed via the single localhost it will quite happily load rJava
#'	dependent packages, such as mailR, via > library(mailR). However, according GitHub
#'	discussion #85 and #125 the OpenCPU Server edition has an issue with a "JVM fork". 
#'	Consequently, such packages must be "lazy-loaded" e.g. package::function().   
#'
#'------------------------------------------------------------------------------------------
usersUpdate <- function(
	usersUpdates
	)
  	{



#-------------------------------------------------------------------------------------------
#
#	Establish package libraries
#
#	Note, mailR must be loaded via "lazy load", see above.
#
#-------------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------------
#
#	Send email from omis.scarborough@gmail.com via Google's gmail relay server.
#
#-------------------------------------------------------------------------------------------
	gmail <- function(emailBody, emailSubject, emailTo) {

		pathMailR <- system.file("extdata", ".mailR", package="omis")
		command <- paste0("cat ", pathMailR)
		emailPassword <- system(command, intern=TRUE)

		mailR::send.mail(
			authenticate	= TRUE,
			body			= emailBody,
# 		   	debug			= TRUE,
			from			= "omis.scarborough@gmail.com",
			send			= TRUE,
			smtp			= list(
				host.name 	= "smtp.gmail.com",
				port		= 465,
				user.name 	= "omis.scarborough@gmail.com",
				passwd 		= emailPassword,
				ssl 		= TRUE),
			subject			= emailSubject,
			to				= emailTo
		)
	}



#-------------------------------------------------------------------------------------------
#
#	Establish filePath to /extdata/.users.rda and load.
#
#-------------------------------------------------------------------------------------------
 	pathBinary <- system.file("extdata", ".users.rda", package="omis")
	load(pathBinary)                        			 	  # Load /extdata/.users.rda
	usersCurrent <- users 							   # Take working copy of .users.rda


#-------------------------------------------------------------------------------------------
#
#	Reformat usersUpdates table passed by users.html.
#
#-------------------------------------------------------------------------------------------
	usersUpdates <- as.data.frame(usersUpdates) 	 # Convert input array to data frame



#-------------------------------------------------------------------------------------------
#
#	Determine which user accounts have been altered and notify them via email. Note, tables
#	"current" and "updates" will have the same length and key order.
#
#-------------------------------------------------------------------------------------------
	current <- usersCurrent[, c("email", "userType")]
	updates <- usersUpdates[, c("email", "userType")]
	iterations <- nrow(current)

	for (i in 1:iterations) {

		if (current$email[i] == updates$email[i] &&
			current$userType[i] != updates$userType[i]) {

			#-------------------------------------------------------------------------------
			#	Determine nature of the alteration.
			#-------------------------------------------------------------------------------
		  	if (current$userType[i] %in% c("admin", "base")) {
				if (updates$userType[i] == "denied") {
					emailBody <-
						"Please accept our apologies, your OMIS access has been removed."
				} else {
					emailBody <- paste0("Your OMIS user type has been altered to '",
					    updates$userType[i], "'.")
				}
			}

			if (current$userType[i] == "denied") {
				emailBody <- paste0("Welcome to OMIS, your user type is '",
					    updates$userType[i], "'.")
			}

			if (current$userType[i] == "pending") {
				if (updates$userType[i] == "denied") {
					emailBody <-
						"Please accept our apologies, your OMIS request has been denied."
				} else {
					emailBody <- paste0("Welcome to OMIS, your user type is ",
					    updates$userType[i], ".")
				}
			}

			#-------------------------------------------------------------------------------
			#	Notify user via email
			#-------------------------------------------------------------------------------
			emailSubject <- "OMIS User Account Update"
			emailTo <- updates$email[i]
			gmail(emailBody, emailSubject, emailTo)
		}
	}



#-------------------------------------------------------------------------------------------
#
#	Apply usersUpdates to current user table /extdata/users.rda.
#
#-------------------------------------------------------------------------------------------
	usersCurrent <- users[, c("email", "password", "passwordAttempts")]

	users <- merge(usersCurrent, usersUpdates, by=c("email"), all=TRUE)
	users <- users[order(users$email), ] 				# Sort by email, prior to saving

	save(users, file=pathBinary)

    }
