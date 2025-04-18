% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/try_connect_run.R
\name{try_connect_run}
\alias{try_connect_run}
\title{Mimick a Connect deployment locally}
\usage{
try_connect_run(clean = TRUE)
}
\arguments{
\item{clean}{Should the temporary folder with the deployment be deleted once
the function returns}
}
\value{
This function is called for its side effects
}
\description{
This function allows you to make a dry run locally of your production code in
a setting that mimicks how it will run on Posit Connect. This is not a
complete replica of the environment in which your code will run on Connect,
so lack of issues here does not equal lack of issues with deployment. However,
it is a good indicator.
}
