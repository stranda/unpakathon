% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find-outliers.R
\name{findOutlier}
\alias{findOutlier}
\title{Identify Outliers}
\usage{
findOutlier(pl, pheno, q = 0.01, z.score = F)
}
\arguments{
\item{pl}{phenotype file (long)}

\item{pheno}{is a phenotype}

\item{q}{the quantile}

\item{z.score}{if true converts the phenotypes to z-scores scaled by the sd in their own growth-chamber}
}
\description{
Identify Outliers
}
