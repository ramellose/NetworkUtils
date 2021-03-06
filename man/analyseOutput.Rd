% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyseOutput.R
\name{analyseOutput}
\alias{analyseOutput}
\title{Compute network statistics}
\usage{
analyseOutput(network, klemmadj, tool, setname, x, n, absolute = TRUE,
  alldata = NULL)
}
\arguments{
\item{network}{list of networks for one tool}

\item{klemmadj}{list of Klemm matrices}

\item{tool}{toolname}

\item{setname}{name of simulation}

\item{x}{vector specificying environmental strength or removed species}

\item{n}{number of replicates}

\item{absolute}{if true: only check for edge presence, not matching sign}

\item{alldata}{list of datasets with simulated counts}

\item{mode}{"env" or "abundance", env takes environmental strength into account while "abundance" includes species removal}
}
\value{
List of dataframes with results per point of x
}
\description{
Compute multiple statistics for tool outputs.
}
\details{
Calculates precision, sensitivity, specificity, assortativity, degree, transitivity, AUC and scale-freeness for tool output.
}
