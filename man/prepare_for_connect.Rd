% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/connect.R
\name{prepare_for_connect}
\alias{prepare_for_connect}
\title{Prepares a production project for deployment on connect}
\usage{
prepare_for_connect()
}
\value{
This function is called for its sideeffects
}
\description{
This function generates all the necessary files for your project to be
deployed on connect, most importantly the \code{manifest.json} file, as well as
the orchestration file that runs your code. It is expected that this function
is not called interactively, but rather as part of an automation using e.g.
GitHub Actions.
}
