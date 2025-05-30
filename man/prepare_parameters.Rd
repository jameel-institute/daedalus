% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_country.R, R/class_infection.R,
%   R/class_vaccination.R, R/pkg_generics.R
\name{prepare_parameters}
\alias{prepare_parameters}
\alias{prepare_parameters.daedalus_country}
\alias{prepare_parameters.daedalus_infection}
\alias{prepare_parameters.daedalus_vaccination}
\title{Prepare country parameters for model}
\usage{
\method{prepare_parameters}{daedalus_country}(x, ...)

\method{prepare_parameters}{daedalus_infection}(x, ...)

\method{prepare_parameters}{daedalus_vaccination}(x, ...)

prepare_parameters(x, ...)
}
\arguments{
\item{x}{An S3 object with an appropriate method.}

\item{...}{Not used; included for compatibility with methods.}
}
\value{
A list of parameters suitable for the DAEDALUS model.
\code{\link[=prepare_parameters.daedalus_country]{prepare_parameters.daedalus_country()}} returns the country parameters, while
\code{\link[=prepare_parameters.daedalus_infection]{prepare_parameters.daedalus_infection()}} returns infection parameters.
}
\description{
Generic for the \link{prepare_parameters} methods associated with
\pkg{daedalus} classes.
}
\details{
\subsection{Country parameters}{

Country contact data is processed as follows:
\itemize{
\item \code{contact_matrix}: scaled by its leading eigenvalue, and with each column
\code{j} scaled by the \code{j}-th element of the country demography vector (i.e.,
scaling contacts from each age group by the size of that group).
}

The returned parameter list consists of:
\itemize{
\item \code{demography}: the demography vector;
\item \code{contact_matrix}: the contact matrix;
\item \code{contacts_workplace}: the contacts in workplaces scaled by the number of
workers in each sector;
\item \code{contacts_consumer_worker}: contacts in workplaces distributed in
proportion to the demography distribution, and scaled by the largest singular
value (similar to eigenvalue for non-square matrices).
}
}

\subsection{Infection parameters}{

Infection parameters are returned from \verb{<daedalus_infection>} objects without
modification and only the name removed.
}
}
\keyword{internal}
