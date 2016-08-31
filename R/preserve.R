#'------------------------------------------------------------------------------------------
#'
#' 	p r e s e r v e . R 
#'
#' 	Take snapshot of OMIS data and store it in a location (externalDir) that is outside of
#'	OMIS thereby ensuring it is not overwritten upon package upgrade.
#'
#' 		/data/ 			holds OMIS application data.
#' 		/extdata/ 		holds OMIS user account data and mailR password.
#'		/R/sysdata.* 	maybe used in future.
#'
#'	Following AppArmor profiles added to "/etc/apparmor.d/opencpu.d/custom"
#'		## Permit preservation of omis application data
#'		/var/lib/omis/asterixasterix rw,
#'
#'	Creation of /var/lib/omis/ directory as follows:
#'		$ sudo mkdir -m 777 /var/lib/omis
#'
#'	Following file permissions also needed
#'		$ sudo chmod o+w -R /var/lib/omis   <= CARE specifying -Recursive path
#'
#' 	@param externalDir : 	Path name of directory external to OMIS package. Reasonable
#'							candidates are /var/lib/omis/ and /srv/omis/.
#'
#' 	cp -af <from/.> <to> 
#' 		'-af'	: 	'-a' is equivalent to -dpR. Archive and keep attributes.
#'					'-f' remove existing destinations, never prompt
#'		'/.'	: adjoining <from> stipulates contents only
#'
#'------------------------------------------------------------------------------------------
preserve <- function(
	externalDir
	)
  	{



#-------------------------------------------------------------------------------------------
#
#	Create target folders if they don't already exist.
#
#-------------------------------------------------------------------------------------------
  	toData 		<- paste0(externalDir, "data/")
  	toExtdata 	<- paste0(externalDir, "extdata/")
  	toR 		<- paste0(externalDir, "R/")

  	if (! dir.exists(externalDir)) {
		dir.create(externalDir, showWarnings=TRUE, recursive=FALSE)
	}
  	if (! dir.exists(toData)) {
		dir.create(toData, showWarnings=TRUE, recursive=FALSE)
	}
  	if (! dir.exists(toExtdata)) {
		dir.create(toExtdata, showWarnings=TRUE, recursive=FALSE)
	}
  	if (! dir.exists(toR)) {
		dir.create(toR, showWarnings=TRUE, recursive=FALSE)
	}



#-------------------------------------------------------------------------------------------
#
#	Backup entire contents of /data/ directory within omis package.
#
#-------------------------------------------------------------------------------------------
	path <- system.file("data", package="omis")
	from <- paste0(path, "/.")	   				  
	shellCommand <- paste("cp -af", from, toData) 			  
	system(shellCommand)



#-------------------------------------------------------------------------------------------
#
#	Backup entire contents of /extdata/ directory within omis package.
#
#-------------------------------------------------------------------------------------------
	path <- system.file("extdata", package="omis")
	from <- paste0(path, "/.")	   				  
	shellCommand <- paste("cp -af", from, toExtdata) 		 
	system(shellCommand)



#-------------------------------------------------------------------------------------------
#
#	Backup sysdata.rdb and sysdata.rdx of /R/ directory within omis package. These are the
#	production versions of sysdata.rda.
#
#-------------------------------------------------------------------------------------------
	path <- system.file("R", package="omis")
  	from <- paste0(path, "/sysdata.*")					 	
	shellCommand <- paste("cp -af", from, toR) 				 
	system(shellCommand)
}
