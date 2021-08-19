extractMSnData <- function(qtof = FALSE){
    
    
    w = which(meta$msLevel == 2)
    
    assign('ms2.spectra', spectra[w],envir = .GlobalEnv)
    assign('CE', meta$collisionEnergy[w],envir = .GlobalEnv)
    CT = meta$filterString
    if(!qtof){
        CT[grep("hcd",CT)] = "HCD"
        CT[grep("cid",CT)] = "CID"
    }
    if(qtof){
        CT[which(is.na(CT))] = 'CID'
    }
    assign('CT',CT ,envir = .GlobalEnv)
    
    assign('MSLEVEL',    meta$msLevel,envir = .GlobalEnv)
    assign('TIC',        meta$totIonCurrent,envir = .GlobalEnv)
    assign('SCANNO',     meta$acquisitionNum,envir = .GlobalEnv) ## check here.
    assign('precursorMZ',meta$precursorMZ,envir = .GlobalEnv)
    assign('ppmErr',     wSIMCity::ppmErr(precursorMZ, adduct_mz),envir = .GlobalEnv)
    assign('precursorIntensity', meta$precursorIntensity,envir = .GlobalEnv)
    assign('comment',   meta$filterString,envir = .GlobalEnv)
    
}



