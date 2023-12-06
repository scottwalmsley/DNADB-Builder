
#' Compare and score 2 msn spectra using the MetaboDIA similarity score
#'
#' @param spectrum matrix spectrum
#' @param ref_spectrum matrix reference spectrum
#' @param mzTol numeric mz tolerance (Da)
#' @param ppmTol  numeric ppm mz tolerance
#'
#' @return numeric similarity score (range 0.0 - 1.0)
#' @export
#'
spectrum_similarity_score <- function(spectrum, ref_spectrum, mzTol, ppmTol){

  spectrum[,2] <- spectrum[,2] / max(spectrum[,2])

  ref_spectrum[,2] <- ref_spectrum[,2] / max(ref_spectrum[,2])

  de <- sqrt(sqrt(sum(spectrum[,2]^2)) * sqrt(sum(ref_spectrum[,2]^2))) * max(sum(spectrum[,2]), sum(ref_spectrum[,2]))

  temp <- lapply(as.list(1:nrow(spectrum)), function(y) {
    mz.dif <- abs(spectrum[y,1] - ref_spectrum[,1])
    if(!is.null(ppmTol)) mzTol = spectrum[y,1] * ppmTol / 1000000
    if(min(mz.dif) > mzTol) {
      return (c(NA,NA,NA))
    } else {
      id <- which.min(mz.dif)
      delt.mass <- (spectrum[y,1] - ref_spectrum[id,1]) / spectrum[y,1] * 1000000
      return(c(delt.mass, spectrum[y,2], ref_spectrum[id,2]))
    }
  })
  spectrum2 <- do.call(rbind,temp)
  nu <- sqrt(sum(spectrum2[,2] * spectrum2[,3], na.rm = T)) * (sum(spectrum2[,2], na.rm=T) + sum(spectrum2[,3], na.rm=T)) / 2
  nu / de

}




#'  normalize the spectrum to 0-100% of the base peak intensity
#'
#' @param spectrum matrix 2 columns
#'
#' @return matrix
#' @export
#'
normalize.spectrum <- function(spectrum){

  spectrum[,2] <- spectrum[,2] / max(spectrum[,2])
  spectrum

}




#' Title
#'
#' @param spectra matrix 2 column
#'
#' @return list of spectra
#' @export
#'
normalize.spectra <- function(spectra){

  lapply(spectra, function(x) normalize.spectrum(x))

}



#' Search the library for a spectral match
#'
#' @param spectrum matrix 2 column
#' @param ref_spectra matrix 2 column
#' @param N integer N
#'
#' @export
search_library <- function(spectrum, ref_spectra, N = 10){


    # spectrum = m2.spectra$consensus_spectrum$`490.2_hcd_80`
     #nm = names(m2.spectra$title)[2]
     #spectra = normalize.spectra(spectra)
     #ref_spectrum = m2.spectra$consensus_spectrum$`490.2_hcd_90`
     #spectrum
     #ref_spectrum

     spectrum_similarity_score(spectrum,ref_spectrum, 0.1,5)
     plot.spectrum_match(spectrum,ref_spectrum, xlim = c(100,500))



}



