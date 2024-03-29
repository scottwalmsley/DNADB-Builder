% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_algo.R
\name{spectrum_similarity_score}
\alias{spectrum_similarity_score}
\title{Compare and score 2 msn spectra using the MetaboDIA similarity score}
\usage{
spectrum_similarity_score(spectrum, ref_spectrum, mzTol, ppmTol)
}
\arguments{
\item{spectrum}{matrix spectrum}

\item{ref_spectrum}{matrix reference spectrum}

\item{mzTol}{numeric mz tolerance (Da)}

\item{ppmTol}{numeric ppm mz tolerance}
}
\value{
numeric similarity score (range 0.0 - 1.0)
}
\description{
Compare and score 2 msn spectra using the MetaboDIA similarity score
}
