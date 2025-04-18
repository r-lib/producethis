% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/connect.R
\name{use_github_connect}
\alias{use_github_connect}
\title{Publish to Posit Connect from a github repository}
\usage{
use_github_connect(from = c("main", "master"))
}
\arguments{
\item{from}{A character vector giving which branch(es) to publish from}
}
\value{
This function is called for its side effects
}
\description{
This function sets up the \code{connect.yaml} action that powers the connect
deployment from github. Further, it takes the local \code{CONNECT_SERVER} and
\code{CONNECT_API_KEY} environment variables and, if they exist, sets them as
secrets on the remote, for the action to use.
}
