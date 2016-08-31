#'------------------------------------------------------------------------------------------
#'
#' 	r e s t o r e . R 
#'
#'	Use snapshot produced by the preserve function to restore omis database to its
#'	pre-package upgrade position.
#'
#'	omis package data is overwritten as follows: 
#'
#'		FROM							TO
#'		/var/lib/omis/data/.			.../omis/data/
#'		/var/lib/omis/extdata/.			.../omis/extdata/
#'		/var/lib/omis/R/sysdata.*		.../omis/R/
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
restore <- function(
	externalDir
	)
  	{



#-------------------------------------------------------------------------------------------
#
#	Restore entire contents of .../omis/data/ directory from snapshot.
#
#-------------------------------------------------------------------------------------------
	from <- paste0(externalDir, "data/.")		 
	to <- system.file("data", package="omis")
	shellCommand <- paste("cp -af", from, to) 				 
	system(shellCommand)



#-------------------------------------------------------------------------------------------
#
#	Restore entire contents of .../omis/extdata/ directory from snapshot.
#
#-------------------------------------------------------------------------------------------
	from <- paste0(externalDir, "extdata/.")	  
	to <- system.file("extdata", package="omis")
	shellCommand <- paste("cp -af", from, to) 				  
	system(shellCommand)



#-------------------------------------------------------------------------------------------
#
#	Restore sysdata.rdb and sysdata.rdx of /R/ directory from snapshot. 
#
#-------------------------------------------------------------------------------------------
	from <- paste0(externalDir, "R/sysdata.*")		  		
	to <- system.file("R", package="omis")
	shellCommand <- paste("cp -af", from, to) 				  
	system(shellCommand)
}
