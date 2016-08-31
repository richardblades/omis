#'------------------------------------------------------------------------------------------
#'
#' 	l o g i n . R
#'
#' 	omis login.
#'
#'	User account data held in /extdata/.users.rda
#'
#'	To ensure an email address is recognised by its recipient mail server, OMIS must
#'	preserve case-sensitivity. However, to ease subsequent logins, comparisons made
#'	agaist the users.rda table will be performed under lower-case transformations.
#'
#'	Valid userType:
#'		admin	: 	Access includes Admin menu.
#'		base	:	Access excludes Admin menu.
#'		denied	:	Request for access denied.
#'		pending	:	Request for access pending.
#'
#'  @param email    	:
#'  @param password 	:
#'  @param passwordNew 	:
#'  @param request  	:
#'		forgot	: 	User has fogotten password - email reset.
#'		login	:	Process login request.
#'		register:	Process OMIS registration request.
#'		update	:	Update user's password.
#'
#'	For-loop Technical Note.
#'	"break" breaks out of a for, while or repeat loop; control is transferred to the first
#'	statement outside the inner-most loop.
#'	"next" halts the processing of the current iteration and advances the looping index.
#'	Both "break" and "next" apply only to the innermost of nested loops.
#'
#'	OpenCPU Server note regarding "rJava" dependent packages.
#'	When OpenCPU is accessed via the single localhost it will quite happily load rJava
#'	dependent packages, such as mailR, via > library(mailR). However, according GitHub
#'	discussion #85 and #125 the OpenCPU Server edition has an issue with a "JVM fork". 
#'	Consequently, such packages must be "lazy-loaded" e.g. package::function().   
#'
#'------------------------------------------------------------------------------------------
login <- function(
	email,
	password,
	passwordNew,
	request
	)
  	{



#-------------------------------------------------------------------------------------------
#
#	Establish package libraries
#
#	Note, mailR must be loaded via "lazy load", see above.
#
#-------------------------------------------------------------------------------------------
  	library(digest)



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
# 	Process parameters
#
#-------------------------------------------------------------------------------------------
	password 	<- digest(password, algo="md5", serialize=FALSE)
	passwordNew <- digest(passwordNew, algo="md5", serialize=FALSE)



#-------------------------------------------------------------------------------------------
#
#	Examine R Session Data
#		.Platform
#		R.version
#		sessionInfo()
#		Sys.info()                             # Login, user & effective_user = "510147"
#-------------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------------
#
#	Establish filePath to /extdata/users.rda and load.
#
#-------------------------------------------------------------------------------------------
 	pathBinary <- system.file("extdata", ".users.rda", package="omis")
	load(pathBinary)                        			 	  # Load /extdata/.users.rda



#-------------------------------------------------------------------------------------------
#
# 	Initialise findings table.
#
#-------------------------------------------------------------------------------------------
	findings <- data.frame(
		emailExists    		=FALSE,
		passwordAttempts	=c(0),
		passwordMatch 		=FALSE,
		requestSuccessful	=FALSE,
		userType	 		=c(""),
		stringsAsFactors	=FALSE
	)



#-------------------------------------------------------------------------------------------
#
# 	f o r g o t
#
#	Process forgotten password
#
#-------------------------------------------------------------------------------------------
	if (request == "forgot") {

		for (i in 1:nrow(users)) {

			if (tolower(email) == tolower(users$email[i]) &&
				users$userType[i] %in% c("admin", "base")) {
				findings$emailExists <- TRUE
				#---------------------------------------------------------------------------
				#	Reset Password
				#---------------------------------------------------------------------------
				characters <- c(1:9, letters, LETTERS)
				passwordReset <- paste(sample(characters, 6), collapse="")
				users$password[i] <- digest(passwordReset, algo="md5", serialize=FALSE)
				#---------------------------------------------------------------------------
				#	Notify user via email
				#---------------------------------------------------------------------------
				emailBody <- paste("Your password has been reset to '", passwordReset, "'. You may alter it through the Login panel.")
				emailSubject <- c("OMIS Password Reset Request")
				emailTo <- users$email[i]
				gmail(emailBody, emailSubject, emailTo)
				findings$requestSuccessful <- TRUE
				break							  # Exit for-loop as no need to continue
			}
		}
	}															 # (request == "forgot")



#-------------------------------------------------------------------------------------------
#
# 	l o g i n
#
#	Process login request
#
#-------------------------------------------------------------------------------------------
	if (request == "login") {

		for (i in 1:nrow(users)) {

			if (tolower(email) == tolower(users$email[i]) &&
				users$userType[i] %in% c("admin", "base")) {
				findings$emailExists <- TRUE

				if (password == users$password[i]) {
					#-----------------------------------------------------------------------
					#	Valid login
					#-----------------------------------------------------------------------
					findings$passwordMatch <- TRUE
					findings$userType <- users$userType[i]
					users$passwordAttempts[i] <- c(0) 			# Reset passwordAttempts
					findings$requestSuccessful <- TRUE

				} else {
					#-----------------------------------------------------------------------
					#	Take note of failed login attempt.
					#-----------------------------------------------------------------------
					users$passwordAttempts[i] <- users$passwordAttempts[i] + 1
					findings$passwordAttempts <- users$passwordAttempts[i]

					if (users$passwordAttempts[i] >= 3) {
						#-------------------------------------------------------------------
						#	Reset Password
						#-------------------------------------------------------------------
						characters <- c(1:9, letters, LETTERS)
						passwordReset <- paste(sample(characters, 6), collapse="")
						users$password[i] <- digest(passwordReset, algo="md5",
							serialize=FALSE)
						#-------------------------------------------------------------------
						#	Notify user via email
						#-------------------------------------------------------------------
						emailBody <- paste("You are only permitted 3 password attempts. Your password has been reset to '", passwordReset, "'. You may alter it through the Login panel.")
						emailSubject <- c("OMIS Password Reset Enforced")
						emailTo <- users$email[i]
						gmail(emailBody, emailSubject, emailTo)
					}
				}
				break							  # Exit for-loop as no need to continue
			}
		}
	}														      # (request == "login")



#-------------------------------------------------------------------------------------------
#
# 	r e g i s t e r
#
# 	Process registration request
#
#-------------------------------------------------------------------------------------------
	if (request == "register") {

		userRequest <- data.frame(
			email			=email,
			passwordAttempts 	=c(0),
			password		=password,
			userType 		=c("pending"),
			stringsAsFactors=FALSE
		)

#-------------------------------------------------------------------------------------------
# 	Look to see if this user is already registered.
#-------------------------------------------------------------------------------------------
		for (i in 1:nrow(users)) {
			if (tolower(email) == tolower(users$email[i])) {
				findings$emailExists <- TRUE
				break							  # Exit for-loop as no need to continue
			}
		}

#-------------------------------------------------------------------------------------------
# 	If this is a new user request then append it
#-------------------------------------------------------------------------------------------
		if (findings$emailExists == FALSE) { 							   # Add request
			users <- rbind(users, userRequest)
			findings$userType <- c("pending")
			findings$requestSuccessful <- TRUE
		}
	}														   # (request == "register")



#-------------------------------------------------------------------------------------------
#
# 	u p d a t e
#
# 	Process update password request
#
#-------------------------------------------------------------------------------------------
	if (request == "update") {
		for (i in 1:nrow(users)) {

			if (tolower(email) == tolower(users$email[i]) &&
				users$userType[i] %in% c("admin", "base")) {
				findings$emailExists <- TRUE

				if (password == users$password[i]) {
					#-----------------------------------------------------------------------
					#	Valid login
					#-----------------------------------------------------------------------
					findings$passwordMatch <- TRUE
					findings$userType <- users$userType[i]
					users$passwordAttempts[i] <- c(0) 			# Reset passwordAttempts
					findings$requestSuccessful <- TRUE
					#-----------------------------------------------------------------------
					#	Update password
					#-----------------------------------------------------------------------
					users$password[i] <- passwordNew
					findings$requestSuccessful <- TRUE

				} else {
					#-----------------------------------------------------------------------
					#	Take note of failed login attempt.
					#-----------------------------------------------------------------------
					users$passwordAttempts[i] <- users$passwordAttempts[i] + 1
					findings$passwordAttempts <- users$passwordAttempts[i]

					if (users$passwordAttempts[i] >= 3) {
						#-------------------------------------------------------------------
						#	Reset Password
						#-------------------------------------------------------------------
						characters <- c(1:9, letters, LETTERS)
						passwordReset <- paste(sample(characters, 6), collapse="")
						users$password[i] <- digest(passwordReset, algo="md5",
							serialize=FALSE)
						#-------------------------------------------------------------------
						#	Notify user via email
						#-------------------------------------------------------------------
						emailBody <- paste("You are only permitted 3 password attempts. Your password has been reset to '", passwordReset, "'. You may alter it through the Login panel.")
						emailSubject <- c("OMIS Password Reset Enforced")
						emailTo <- users$email[i]
						gmail(emailBody, emailSubject, emailTo)
					}
				}
				break							  # Exit for-loop as no need to continue
			}
		}
	}														     # (request == "update")



#-------------------------------------------------------------------------------------------
#
# 	Save any changes made
#
#-------------------------------------------------------------------------------------------
	users <- users[order(users$email), ] 						 # Sort by email address
	save(users, file=pathBinary)



#-------------------------------------------------------------------------------------------
#
# 	Return findings, these will govern appropriate response.
#
#-------------------------------------------------------------------------------------------
	return(findings)
    }
