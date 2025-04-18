% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/costs.R
\name{get_costs}
\alias{get_costs}
\title{Get epidemic costs from a DAEDALUS model run}
\usage{
get_costs(x, summarise_as = c("none", "total", "domain"))
}
\arguments{
\item{x}{A \verb{<daedalus_output>} object from a call to \code{\link[=daedalus]{daedalus()}}.}

\item{summarise_as}{A string from among "none", "total", or "domain", for how
the costs should be returned. Select "none", the default, for the raw costs
along with overall and domain-specific totals; "total" for the overall cost,
and "domain" for the total costs per domain; the domains are 'economic',
'education', and 'life years'.}
}
\value{
A list of different cost values, including the total cost. See
\strong{Details} for more information.
}
\description{
Get epidemic costs from a DAEDALUS model run
}
\details{
The total cost in million dollars is returned as \code{total_cost}. This is
comprised of the following costs.
\subsection{Economic costs}{

A three element list of \code{economic_cost_total}, the total costs from pandemic
impacts on economic sectors, including both costs of lost gross value added
(GVA) due to pandemic-control restrictions or closures
(\code{economic_cost_closures}), and pandemic-related absences due to illness and
death (\code{economic_cost_absences}).
}

\subsection{Educational costs}{

A three element list of \code{education_cost_total}, the total costs from pandemic
impacts on education due to pandemic-control restrictions or closures
(\code{education_cost_closures}), and pandemic-related absences due to illness and
death (\code{education_cost_absences}).
}

\subsection{Life-value lost}{

A four-element vector (for the number of age groups) giving the value of
life-years lost per age group. This is calculated as the life-expectancy of
each age group times the value of a statistical life, with all years assumed
to have the same value.
}

\subsection{Life-years lost}{

A four-element vector (for the number of age groups) giving the value of
life-years lost per age group. This is calculated as the life-expectancy of
each age group times the number of deaths in that age group. No quality
adjustment is applied.
}
}
\examples{
output <- daedalus("Canada", "influenza_1918")

get_costs(output)
}
