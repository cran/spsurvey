\name{read.sas}
\alias{read.sas}
\title{Read SAS Datasets or a SAS XPORT (Transport) File}
\description{
  This function reads either a SAS dataset or a SAS XPORT (transport) file and 
  creates a data frame.
}
\usage{
read.sas(filename, libname=NULL, xport=FALSE,
   sascmd="C:/Program Files/SASHome/SASFoundation/9.4/sas.exe")
}
\arguments{
  \item{filename}{if xport equals TRUE, a character string giving the full path 
    to the SAS XPORT file, which must include the file extension.  If xport  
    equals FALSE, either a character string giving the the name of a dataset
    in the SAS library or a vector of character strings giving the names of
    datasets in the SAS library, where the dataset names cannot exceed eight
    characters in length and do not include the file extension.}
  \item{libname}{a character string defining the SAS library, which is usually a
    directory reference.   If xport equals FALSE and the dataset(s) named in
    argument filename do not reside in the working directory, then this
    argument is required.  The default value is NULL.}
  \item{xport}{a logical value indicating whether the input file is a SAS XPORT
    file.  The default value is FALSE.}
  \item{sascmd}{a character string giving the full path to SAS executable.  This
    argument is required only when xport equals FALSE.  The default value is
    "C:/Program Files/SASHome/SASFoundation/9.4/sas.exe".}
}
\value{
  Either a single data frame or a list of data frames.
}
\author{Tom Kincaid \email{Kincaid.Tom@epa.gov}}
\examples{
  \dontrun{
    MySasFile <- read.sas("mysasfil", "C:/Documents and Settings/auser/My Documents/My Project")
  }
}
\keyword{IO}
