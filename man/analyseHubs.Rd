% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyseHubs.R
\name{analyseHubs}
\alias{analyseHubs}
\title{Return hubs or central species}
\usage{
analyseHubs(networks, toolnames, n, mode = "hub")
}
\arguments{
\item{networks}{list of list of networks per tool}

\item{toolnames}{list of toolnames}

\item{n}{number of replicates}

\item{mode}{"hub" or "central": for hub, the degree is returned, for central, the centrality scores}
}
\value{
Dataframe specifying tool ID and degree
}
\description{
Calculate the top 5 hub species or central species.
}
\details{
The top 5 hub species or betweenness centrality species are calculated for a list with including networks from different tools.
}
