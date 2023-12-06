#' Build a consensus spectrum from several input spectra.  For spectra derived from the same compound.
#'
#' @param spectra a list of spectra in matrix format (mz, intensities)
#' @param mzTol numeric tolerance in Daltons
#' @param ppmTol numeric tolerance in part per million of mz
#' @param min_rel_int numeric cutoff of the relative intensity of the consensus spectrum
#' @param min_rel_rep numeric cutoff of the reproducibility of the consensus peak across the spectra
#' @param mz_IQR inter-quartile-range of differences of the consensus peak and the corresponding peaks of each spectrum
#' @param clean boolean default TRUE  clean the final spectgrum of noise?
#' @param min_noise numeric the relative intensity noise floor
#'
#' @export
build_consensus_spectrum <- function(spectra, mzTol,ppmTol, min_rel_int, min_rel_rep, mz_IQR, clean = T, min_noise = NULL){

  n_spectrum <- length(spectra)
  spectrum_all <- matrix(do.call(rbind,spectra), ncol=2)
  spectrum_all <- matrix(spectrum_all[order(spectrum_all[ ,1]), ], ncol=2)

  if(clean){

    spectrum_all = clean_spectrum(spectrum_all,min_basepeak = 0,min_rel_int = min_rel_int, min_noise = min_noise)$spectrum
  }


  ###Group ms2 spectra
  #difference between adjacent peaks
  if(nrow(spectrum_all)>1){
    mz_group <- list()
    mz_group[[1]] <- c(1)
    for(i in 2:nrow(spectrum_all)){
      mz_dif <- spectrum_all[i,1]-spectrum_all[(i-1),1]
      if(!is.null(ppmTol)) tol <- spectrum_all[(i-1),1] * ppmTol/ 1000000
      if(mz_dif < mzTol){
        mz_group[[length(mz_group)]] <- c(mz_group[[length(mz_group)]],i)
      } else {
        mz_group[[length(mz_group)+1]] <- i
      }
    }
    temp <- lapply(mz_group, function(x) {
      tmp <- matrix(spectrum_all[x,], ncol=2)
      if(length(x) > n_spectrum * min_rel_rep){
        mz <- median(tmp[,1])
        mz_diff.i <- (tmp[,1] - mz) * 1000000 / mz

        if(IQR(mz_diff.i) <= mz_IQR){

          inten <- mean(tmp[,2])
          list(lib.i=c(mz,inten), mz_diff.i=mz_diff.i)

        }
      }
    })
    id <- do.call("c", lapply(as.list(c(1:length(temp))), function(z) {
      temp2 <- temp[[z]]
      if(!is.null(temp2)){
        if(temp2$lib.i[2] >= min_rel_int){
          return(z)
        }
      }
    }) )
    if(length(id) > 0){
      mz_var <- do.call(c,lapply(as.list(id), function(y) { temp[[y]]$mz_diff.i }))
      consensus_lib <- do.call(rbind,lapply(as.list(id), function(y){ temp[[y]]$lib.i }))
      consensus_lib[,2] <- consensus_lib[,2] / max(consensus_lib[,2])



      return(list(lib=matrix(consensus_lib, ncol=2), mz_diff=mz_var))
    }
    else {
      return(list(lib=NULL, mz_diff=NULL))
    }

  }
  else {
    return(list(lib=NULL, mz_diff=NULL))
  }

}




#' Clean spectra of noise
#'
#' @param spectrum matrix the spectrum to be cleaned with mz and intensities (2 columns)
#' @param min_basepeak decimal the minimum base peak
#' @param min_rel_int  the minimum relative intensity to leave in the spectrum
#' @param min_noise the min relative intensity noise level (eg .1)
#'
#' @export
clean_spectrum <- function(spectrum,  min_basepeak, min_rel_int, min_noise){



  if(nrow(spectrum) > 0){

    noise <- min_noise
    basepeak <- max(spectrum[,2])

    if(basepeak > min_basepeak) {
      noise <- max(noise, basepeak*min_rel_int)
      spectrum <- matrix(spectrum[which(spectrum[,2]>noise), ], ncol=2)
      spectrum[,2] <- spectrum[,2] / basepeak
      return(list(basepeak=basepeak, spectrum=spectrum))
    }
    else {
      return(list(basepeak=NULL, spectrum=NULL))
    }
  }
  else {
    return(list(basepeak=NULL, spectrum=NULL))
  }

}
