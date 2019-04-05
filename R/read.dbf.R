################################################################################
# Function: read.dbf
# Programmer: Tom Kincaid
# Date: March 1, 2005
# Last Revised: August 18, 2016
#
#' Read the dbf File of an ESRI Shapefile
#'
#' This function reads either a single dbf file or multiple dbf files.  For
#' multiple dbf files, all of the dbf files must have the same variable names.
#'
#' @param filename  Name of the dbf file without any extension.  If filename
#'   equals a dbf file name, then that dbf file is read.  If filename equals
#'   NULL, then all of the dbf files in the working directory are read.  The
#'   default is NULL.
#'
#' @return Data frame composed of either the contents of the single dbf file,
#'   when filename is provided, or the contents of the dbf file(s) in the
#'   working directory, when filename is NULL.
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{readDbfFile}}{C function to read a single dbf file or
#'       multiple dbf files}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @export
################################################################################

read.dbf <- function(filename = NULL) {

# Ensure that the processor is little-endian

   if(.Platform$endian == "big")
      stop("\nA little-endian processor is required for the read.dbf function.")

# If necessary, strip the file extension from the file name

   if(!is.null(filename)) {
      nc <- nchar(filename)
      if(substr(filename, nc-3, nc) == ".dbf") {
         filename <- substr(filename, 1, nc-4)
      }
   }

# Read the dbf file

   dbffile <- .Call("readDbfFile", filename)
   if(is.null(dbffile[[1]]))
      stop("\nAn error occurred while reading the dbf file(s) in the working directory.")

# Convert character vectors to factors

   ind <- sapply(dbffile, is.character)
   if(any(ind)) {
      for(i in (1:length(dbffile))[ind])
         dbffile[,i] <- as.factor(dbffile[,i])
   }

# Return the data frame

   dbffile
}
