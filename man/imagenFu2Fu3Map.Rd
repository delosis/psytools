\name{imagenFu2Fu3Map}
\alias{imagenFu2Fu3Map}
\docType{data}
\title{
 Imagen map between FU2 and FU3 variable codes
}
\description{
 a Map between FU2 and FU3 variable codes where there is overlap. And a definition of what "taskID" to save them under for the FU3 split file format
}
\usage{data("imagenFu2Fu3Map")}
\format{
  A data frame with 2255 observations on the following 4 variables.
  \describe{
    \item{\code{fu3Column}}{fu3 Variable Name }
    \item{\code{fu2Column}}{fu2 Variable Name where there is a match, or an fu2 format variable name to use if this is a new variable }
    \item{\code{Instrument}}{fake taskID to allow FU3 data to be stored in seperate task files } 
    \item{\code{fu3Core}}{indicating in which FU3 core the variables is found}
  }
}
\source{
  fuzzy string matching between FU2 and FU3 datasets followed by hand curration
}

\examples{
data(imagenFu2Fu3Map)
}
\keyword{datasets}
