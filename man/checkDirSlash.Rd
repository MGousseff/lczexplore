% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkDirSlash.R
\name{checkDirSlash}
\alias{checkDirSlash}
\title{Simply adds a slash at then end of a directory path if there is none}
\usage{
checkDirSlash(dirPath)
}
\arguments{
\item{dirPath}{is a directory pas as a string}
}
\value{
the same string if it ended with a slash, else the string with a slash at its end
}
\description{
Simply adds a slash at then end of a directory path if there is none
}
\examples{
dirPathTest<-"test"
checkDirSlash(dirPathTest)
}
