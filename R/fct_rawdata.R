#' Calculate the mean precursor mz from all detected precursor mz in the file. Relevant only for msn data.
#' @param header mzR header dataframe
#' @param msLevel integer ms level (eg, 2, or 3)
#'
#' @return numeric mz value
#'
#' @export
#'
meanPrecursorMZ <- function(header, msLevel){

  w = which(header$msLevel == msLevel)
  mean(header$precursorMZ[w])


}


#'  Calculate the part per million error between a theoretical and real mz value
#'
#' @param mt numeric theoretical or alternatively measured mz value
#' @param me numeric measured mz value
#'
#' @return numeric ppm error
#'
#' @export
#'
computePPMerror <- function(mt,me){

  (me-mt)/mt*1E6

}




#' Title
#'
#' @param mass numeric exact mass / mz
#' @param ppm numeric ppm tol
#'
#' @return numeric
#' @export
getMassTolRange <- function(mass,ppm){
  dM <- ppm*mass/1e6
  return(c(mass-dM,mass+dM))
}




#' Title
#'
#' @param mz numeric mz
#' @param spectrum matrix spectrum
#' @param ppmTol numeric ppm tolerance
#'
#' @return matrix
#' @export
removeMass <- function(spectrum, mz, ppmTol){

  tol = getMassTolRange(mz, ppmTol)
  w = which(spectrum[,1] < tol[1] | spectrum[,1] > tol[2])

  spectrum[w,]

}





#' Load mz data using the mzR library.  Returns data in ms levels.
#'
#' @param mzML_file character vector file name
#' @param ppmTol numeic ppm tolerance
#' @param mzToRemove numeric mz contam to remove
#' @return list of mzML data segregated into ms levels (eg 1,2,3)  Up to maximum of 3 ms levels.
#' @export
getMzData <- function(mzML_file, mzToRemove = 173.52815, ppmTol){

  requireNamespace("mzR")
  if (!requireNamespace("mzR", quietly = TRUE)) {
    stop(
      "Package \"mzR\" must be installed to use this function.",
      call. = FALSE
    )
  }

  mzData = mzR::openMSfile(mzML_file)
  header_data = mzR::header(mzData)
  temp = mzR::spectra(mzData)
  mzR::close(mzData)

  spectra = lapply(temp, function(x) removeMass(x,mzToRemove,10))

  mz = meanPrecursorMZ(header_data, 2)

  # levels
  mslevels =  header_data$msLevel

  #filters
  filters = header_data$filterString
  ms1_filters = (header_data$filterString[which(header_data$msLevel == 1)])
  ms2_filters = (header_data$filterString[which(header_data$msLevel == 2)])
  ms3_filters = (header_data$filterString[which(header_data$msLevel == 3)])

  #mslevel indices
  ms1_idx = which(header_data$msLevel == 1)
  ms2_idx = which(header_data$msLevel == 2)
  ms3_idx = which(header_data$msLevel == 3)

  #mslevel spectra
  ms1_spectra = spectra[ms1_idx]
  ms2_spectra = spectra[ms2_idx]
  ms3_spectra = spectra[ms3_idx]

 # cleaned_ms1_spectra = clean_spectrum(spectra[[paste(m,t,e,sep = "_")]], min_basepeak=min_basepeak, min_int=min_int, min_noise=min_noise)



  # ms2 precursors
  ms2_precursorMZ = (round(header_data$precursorMZ[ms2_idx],1))

  # Unique  ms3 precursors
  ms3_precursorMZ = (round(header_data$precursorMZ[ms3_idx],1))

  # collision energies
  ms2_ce = (header_data$collisionEnergy[ms2_idx])
  ms3_ce = (header_data$collisionEnergy[ms3_idx])

  # Collision types
  ct = array(NA,nrow(header_data))
  #ct[grep("cid",filters)] = "cid"
  #ct[grep("hcd",filters)] = "hcd"

  #QTOF fix
  ct = rep("cid",nrow(header_data))

  #ct = ct
  ms2_ct = ct[ms2_idx]
  ms3_ct = ct[ms3_idx]

  #print(ms2_ct)


  # unique precursor ct ce combinations
  ms2_ct_ce = unique(data.frame(ms2_precursorMZ,ms2_ct,ms2_ce))
  ms3_ct_ce = unique(data.frame(ms3_precursorMZ,ms3_ct,ms3_ce))

  ms2_ct_ce = ms2_ct_ce[order(ms2_ct_ce$ms2_ct,as.numeric(ms2_ct_ce$ms2_precursorMZ) ,as.numeric(ms2_ct_ce$ms2_ce)),]
  ms3_ct_ce = ms3_ct_ce[order(ms3_ct_ce$ms3_ct,as.numeric(ms3_ct_ce$ms3_precursorMZ), as.numeric(ms3_ct_ce$ms3_ce)),]

  # ms2_precursor intensity
  cid_prec_intensity = header_data$basePeakIntensity[ms2_idx[which(ms2_ct == "cid")]]
  hcd_prec_intensity = header_data$basePeakIntensity[ms2_idx[which(ms2_ct == "hcd")]]


  list(mz = mz,
       ms1_idx = ms1_idx,
       ms1_filters = ms1_filters,
       ms1_spectra = ms1_spectra,

       cid_prec_intensity = cid_prec_intensity,
       hcd_prec_intensity = hcd_prec_intensity,

       ms2_idx = ms2_idx,
       ms2_filters = ms2_filters,
       ms2_spectra = ms2_spectra,
       ms2_precursorMZ = ms2_precursorMZ,
       ms2_ce = ms2_ce,
       ms2_ct = ms2_ct,
       ms2_ionTable = ms2_ct_ce,



       ms3_idx = ms3_idx,
       ms3_filters = ms3_filters,
       ms3_spectra = ms3_spectra,
       ms3_precursorMZ = ms3_precursorMZ,
       ms3_ce = ms3_ce,
       ms3_ct = ms3_ct,
       ms3_ionTable = ms3_ct_ce

  )
}

#' Get ms level data from loaded mz Data using getMzData.
#'
#' @param mzData list from getMzData function.
#' @param mslevel integer ms level to extract
#'
#' @return list of data containing index, filter strings, ion tables, collision energies and collision types
#' @export
getMSLevelData <- function(mzData, mslevel){

  if(mslevel == 1){
    cat(paste("\nGetting MS1 level data...\n"))
    return(
      list(
        ms1_idx = ms1_idx,
        ms1_filters = ms1_filters,
        ms1_spectra = ms1_spectra,

        cid_prec_intensity = cid_prec_intensity,
        hcd_prec_intensity = hcd_prec_intensity



      )
    )
  }

  if(mslevel == 2){
    cat(paste("\nGetting MS2 level data... \n"))
    return(
      list(
        ms2_idx = mzData$ms2_idx,
        ms2_filters = mzData$ms2_filters,
        ms2_spectra = mzData$ms2_spectra,
        ms2_precursorMZ = mzData$ms2_precursorMZ,
        ms2_ce = mzData$ms2_ce,
        ms2_ct = mzData$ms2_ct,
        #QTOF fix
        #ms2_ct = rep("CID",times = length(mzData$ms2_ct)),
        ms2_ionTable = mzData$ms2_ionTable

      )
    )
  }
  if(mslevel == 3){
    cat(paste("\nGetting MS3 level data... \n"))
    list(
      ms3_idx = mzData$ms3_idx,
      ms3_filters = mzData$ms3_filters,
      ms3_spectra = mzData$ms3_spectra,
      ms3_precursorMZ = mzData$ms3_precursorMZ,
      ms3_ce = mzData$ms3_ce,
      ms3_ct = mzData$ms3_ct,
      ms3_ionTable = mzData$ms3_ionTable

    )

  }

}


#' Get MS level spectra
#'
#' @param msLevelData list ms Level data from getMzData.  Build consensus spectra and de noises spectra
#' @param msLevel integer ms level
#' @param mzTol numeric tolerance in Daltons for building consensus spectra
#' @param ppmTol numeric tolerance in part per million of mz
#' @param min_rel_int numeric cutoff of the relative intensity of the consensus spectrum
#' @param min_rel_rep numeric cutoff of the reproducibility of the consensus peak across the spectra
#' @param mz_IQR inter-quartile-range of differences of the consensus peak and the corresponding peaks of each spectrum in ppm
#' @param clean boolean
#' @param min_noise numeric
#' @export
#'
#' @return list of raw and consensus spectra
getMSLevel_Spectra <- function(msLevelData,msLevel,mzTol,ppmTol,min_rel_int,min_rel_rep, mz_IQR, clean = TRUE, min_noise = 0.01){

  #msLevelData = m2.data
  #mslevel = 2
  #mzTol = 0.005
  #ppmTol = 7
  #min_rel_int = 0.1
  #min_rel_rep = 0.5
  #mz_IQR = 10
  #clean = TRUE
  #min_noise = 0.1


  spectra = consensus_spectrum = title = nPeaks = basePeak = ct.a = ce.a = nm = list();

  if(msLevel ==2){
    cat(paste("\nDenoising and building MS2 consensus spectra...\n"))
    ionTable = msLevelData$ms2_ionTable
    mz = unique(ionTable$ms2_precursorMZ)

    for(m in mz){

      s.ionTable = ionTable[which(ionTable$ms2_precursorMZ == m),]
      #print(s.ionTable)
      for(i in nrow(s.ionTable)){

        u.ct = unique(s.ionTable$ms2_ct)

        for(tu in u.ct){

            for(e in s.ionTable$ms2_ce[which(s.ionTable$ms2_ct == tu)]){

                  w = which(msLevelData$ms2_ct == tu & msLevelData$ms2_ce == e)

                  idx = paste(m,tu,e,sep = "_")


                  spectra[[idx]] = msLevelData$ms2_spectra[w];

                  consensus_spectrum[[idx]] = build_consensus_spectrum(spectra[[idx]] ,mzTol=mzTol,ppmTol=ppmTol,min_rel_int = min_rel_int,min_rel_rep = min_rel_rep, mz_IQR = mz_IQR, clean = clean, min_noise = min_noise)$lib
                  if(is.null(consensus_spectrum[[idx]])){
                    consensus_spectrum[[idx]] = build_consensus_spectrum(spectra[[idx]] ,mzTol=mzTol,ppmTol=ppmTol,min_rel_int = 0.1,min_rel_rep = min_rel_rep, mz_IQR = mz_IQR, clean = clean, min_noise = min_noise)$lib

                  }


                  #cleaned[[idx]] =  clean_spectrum( consensus_spectrum[[idx]] , min_basepeak=min_basepeak, min_int=min_int, min_noise=min_noise)$spectrum
                  title[[idx]] = idx

                  nPeaks[[idx]] = nrow(consensus_spectrum[[idx]])

                  nm[[idx]] = idx

                  ce.a[[idx]] = e

                  ct.a[[idx]] = tu

                  basePeak[[idx]] =     consensus_spectrum[[idx]][which.max(consensus_spectrum[[idx]][,2]),1]
                  #print(length(w))
                  print(paste(m,tu,e,nPeaks[[idx]]))
                  #print(t)
             }
        }

      }

    }

  }
  if(msLevel ==3){
    cat(paste("\nDenoising and building MS3 consensus spectra...\n"))

    ionTable = msLevelData$ms3_ionTable
    mz = unique(ionTable$ms3_precursorMZ)

    for(m in mz){

      s.ionTable = ionTable[which(ionTable$ms3_precursorMZ == m),]
      for(i in nrow(s.ionTable)){

        u.ct = unique(unique(s.ionTable$ms3_ct))
        for(tu in u.ct){

          for(e in s.ionTable$ms3_ce[which(s.ionTable$ms3_ct == tu)]){

            w = which(msLevelData$ms3_ct == tu & msLevelData$ms3_ce == e)
            idx = paste(m,tu,e,sep = "_")
            spectra[[idx]] = msLevelData$ms3_spectra[w];

            consensus_spectrum[[idx]] = build_consensus_spectrum(spectra[[idx]] ,mzTol=mzTol,ppmTol=ppmTol,min_rel_int = min_rel_int,min_rel_rep = min_rel_rep, mz_IQR = mz_IQR, clean = clean, min_noise = min_noise)$lib
            if(is.null(consensus_spectrum[[idx]])){
              consensus_spectrum[[idx]] = build_consensus_spectrum(spectra[[idx]] ,mzTol=mzTol,ppmTol=ppmTol,min_rel_int = 0.1,min_rel_rep = min_rel_rep, mz_IQR = mz_IQR, clean = clean, min_noise = min_noise)$lib
              if(is.null(consensus_spectrum[[idx]])){
                consensus_spectrum[[idx]] = build_consensus_spectrum(spectra[[idx]] ,mzTol=mzTol,ppmTol=ppmTol,min_rel_int = min_rel_int,min_rel_rep = 0.2, mz_IQR = mz_IQR, clean = clean, min_noise = min_noise)$lib
              }
            }


            title[[idx]] = idx
            nPeaks[[idx]] = nrow(consensus_spectrum[[idx]])
            basePeak[[idx]] =     consensus_spectrum[[idx]][which.max(consensus_spectrum[[idx]][,2]),1]

            nm[[idx]] = idx

            ce.a[[idx]] = e

            ct.a[[idx]] = tu
            print(paste(m,tu,e,nPeaks[[idx]]))
          }
        }

      }

    }

  }
  ID = unlist(basePeak)
  ID = names(ID)
  str = lapply(ID,function(x) strsplit(split = "_",x))

  #for QTOF
  #CT = rep("cid",times = length(ID))
  CT = unlist(lapply(str, function(x) x[[1]][2]))
  CE =  unlist(lapply(str, function(x) x[[1]][3]))

  dt =  data.frame(ID = ID,basePeak = unlist(basePeak),nPeaks = unlist(nPeaks), CT = CT, CE = CE)
  #dt =  data.frame(ID = unlist(nm),basePeak = unlist(basePeak),nPeaks = unlist(nPeaks), CT = unlist(ct.a), CE = unlist(ce.a))
# print(dt)

 #--> TODO
  list(title = title, spectra = spectra, consensus_spectrum = consensus_spectrum, peakData =dt)

}

