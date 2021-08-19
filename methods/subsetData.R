subsetData <- function(minScan = NULL, maxScan = NULL){
     
   if(is.null(minScan) | is.null(maxScan)){
        print('You must inspect your TIC plots and determine the minimum and maximum scan range!')
        return()
   }
    
   
   w = which(meta$acquisitionNum > minScan & meta$acquisitionNum < maxScan)

   #assign('mz', openMSfile(adduct_ID),envir = .GlobalEnv)
   assign('spectra', spectra[w],envir = .GlobalEnv)
   assign('meta', meta[w,],envir = .GlobalEnv)


}