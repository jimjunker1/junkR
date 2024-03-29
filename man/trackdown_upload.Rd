% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trackdown_addins.R
\name{trackdown_upload}
\alias{trackdown_upload}
\title{trackdown_upload}
\usage{
trackdown_upload(ul_filePattern = "upload_trackdown.R", force = FALSE, ...)
}
\arguments{
\item{ul_filePattern}{the file pattern for the script used to automate the upload}

\item{force}{logical. should the function continue if multiple scripts are identified}

\item{...}{additional argument passed to function}
}
\value{
nothing
}
\description{
This function automates a trackdown upload to google drive based on the script of \code{upload_trackdown.R}
}
