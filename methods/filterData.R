filterData <- function(qtof = FALSE){
  
  
  contam = 173.52815
  
  ISO  = 1.003355
  
  filtered.ms2.spectra = list();
  
  NUMPEAKS  = array(dim = length(ms2.spectra),0)
  
  k = 1;
  if(!qtof){
    for(s in ms2.spectra){
      
      mz = s[,1]
      
      intensity = s[,2]
      
      err = tol(contam,5)
      
      w = which(mz > err[1]  & mz < err[2])
      
      if(length(w>0)){
        
        mz = mz[-w]
        
        intensity = intensity[-w]
      }
      
      err = tol(contam+ISO,5)
      
      w = which(mz > err[1]  & mz < err[2])
      
      if(length(w>0)){
        
        mz = mz[-w]
        
        intensity = intensity[-w]
        
      }
      
      is.isotope = is.peak = array(dim = length(mz),FALSE)
      
      
      # scan to leave M+1 peaks (if any)
      i = 1;
      for(m in mz){
        
        err = tol(m+ISO,7)
        
        w = which(mz > err[1]  & mz < err[2])
        
        if(length(w>0)){
          is.isotope[w] = TRUE
          is.peak[i] = TRUE
        }
        i = i+1
      }
      
      is.peak = c(which(is.isotope), which(is.peak))
      
      is.peak = unique(is.peak[order(is.peak)])
      
      s = as.matrix(cbind(mz[is.peak], intensity[is.peak])) 
      
      NUMPEAKS[k] = length(is.peak)
      
      
      
      #Now filter by abundance, > relative 3-5
      
      filtered.ms2.spectra[[k]] = s
      
      k=k+1
      
    }
  }
  if(qtof){
    
    filtered.ms2.spectra = list()
    NUMPEAKS  = tempCE = tempCT =  tempComment = tempMSlevel = tempPpmErr = tempPrecursorInt = tempPrecursorMZ = tempScanNo = 
      tempTIC = NULL#array(dim = length(ms2.spectra),0)
    
    k = i = 1;
    
    for(s in ms2.spectra){
      
      w = which(s[,1] < (adduct_mz + 1.1) & s[,2] > (0.05 * max(s[,2])) & s[,2] > 50)
      
      if(length(w)>0){
        filtered.ms2.spectra[[k]] = s[w,]
        NUMPEAKS[k] = length(w)
        tempMSlevel[k] = MSLEVEL[i]
        tempCE[k] = CE[i]
        tempCT[k] = CT[i]
        tempScanNo[k] = SCANNO[i]
        tempMSlevel[k] = MSLEVEL[i]
        tempPpmErr[k] = ppmErr[i]
        tempPrecursorInt[k] = precursorIntensity[i]
        tempPrecursorMZ[k] = precursorMZ[i]
        tempTIC[k] = TIC[i]
        tempComment[k] = comment[i]
        k=k+1
      }
      i=i+1
      
    }
  }
  if(qtof){
    
    
    assign('CE',tempCE, envir = .GlobalEnv)
    assign('CT', tempCT ,envir = .GlobalEnv)
    assign('SCANNO', tempScanNo, envir = .GlobalEnv)
    assign('comment', tempComment ,envir = .GlobalEnv)
    assign('MSLEVEL', tempMSlevel ,envir = .GlobalEnv)
    assign('ppmErr',tempPpmErr, envir = .GlobalEnv)
    assign('precursorMZ', tempPrecursorMZ,envir = .GlobalEnv)
    assign('precursorIntensity',tempPrecursorInt, envir = .GlobalEnv)
    assign('TIC',tempTIC, envir = .GlobalEnv)
  }
  assign('filtered.ms2.spectra', filtered.ms2.spectra, envir = .GlobalEnv)
  
  assign('NUMPEAKS',NUMPEAKS, envir = .GlobalEnv)
  
}
