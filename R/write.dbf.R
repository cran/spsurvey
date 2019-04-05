################################################################################
# Function: write.dbf
# Programmer: Tom Kincaid
# Date: September 30, 2009
# Revised: February 17, 2016
#
#' Write a Data Frame as the dbf File of an ESRI Shapefile
#'
#' This function writes a data frame to a dbf file.
#'
#' @param dframe Data frame to be written to the dbf file
#'
#' @param filename Name of the dbf file without any extension.
#'
#' @return  Data frame composed of either the contents of the single dbf file,
#'   when filename is provided, or the contents of the dbf file(s) in the
#'   working directory, when filename is NULL.
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{writeDbfFile}}{C function to write a single dbf file
#'       or multiple dbf files}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @export
################################################################################

write.dbf <- function(dframe, filename) {

# If necessary, strip the file extension from the file name
   if(!is.null(filename)) {
      nc <- nchar(filename)
      if(substr(filename, nc-3, nc) == ".dbf") {
         filename <- substr(filename, 1, nc-4)
      }
   }

# Convert character vectors to factors
   temp <- sapply(dframe, is.character)
   if(any(temp)) {
      for(i in seq(ncol(dframe))[temp]) {
         dframe[,i] <- as.factor(dframe[,i])
      }
   }

# Ensure correct handling of missing values for factors
   temp <- sapply(dframe, is.factor)
   if(any(temp)) {
      for(i in seq(ncol(dframe))[temp]) {
         dframe[,i] <- as.character(dframe[,i])
         temp <- dframe[,i] == "" | is.na(dframe[,i])
         if(any(temp)) {
            dframe[temp,i] <- " "
         }
      }
      temp <- .Call("writeDbfFile", names(dframe), dframe, filename)
   } else {
      temp <- .Call("writeDbfFile", names(dframe), dframe, filename)
   }

# Return NULL
   invisible(NULL)
}
