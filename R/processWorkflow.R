#'  process the DNA adduct lbrary building workflow
#'
#' @param mzML_file character mzML file
#' @param metadata character array of metadata
#' @wd character working directory
#' @param mzTol numeric mzTol (Da)
#' @param ppmTol numeric ppm tol
#' @param min_rel_int numeric rel int
#' @param min_rel_rep numeric rel freaction (x/N samples required)
#' @param mz_IQR numeric mz IQR of ppm tol to strip outliers by mass error
#' @param clean boolean clean the spectra
#' @param min_noise noise floor (relative to base peak in consensus)
#' @param contamMZ numeric mz value
## @return
#' @export
#'
#@examples
processWorkflow <- function(mzML_file,metadata, wd = NULL, mzTol=0.005,ppmTol=7,min_rel_int=0.01,min_rel_rep=0.25, mz_IQR = 14, clean = T, min_noise = 0.01, contamMZ = 173.52815){


  setVars(metadata, wd = wd)#, plotdir = "plots", outdir = "frag")

  assign("rawData",getMzData(mzML_file, mzToRemove = contamMZ, ppmTol = 10),envir = .GlobalEnv)
  assign("m2.data",getMSLevelData(rawData,2),envir = .GlobalEnv)

  assign("m2.spectra", getMSLevel_Spectra(m2.data,msLevel = 2,mzTol=mzTol,ppmTol=ppmTol,min_rel_int=min_rel_int,min_rel_rep=min_rel_rep, mz_IQR = mz_IQR, clean = clean, min_noise = min_noise),envir = .GlobalEnv)


  # Get the ms2 ion table
  iontable = m2.spectra$peakData

  cat(paste("\nGenerating MS2 msp files for fragment analysis...\n"))

  assign("msp_folder", paste(wd, paste(adduct_ID,"msp", sep = "_"),sep = "/"),envir = .GlobalEnv)
  if(dir.exists(msp_folder)){
    unlink(msp_folder, recursive = T)
  }
  dir.create(msp_folder)

  for(i in 1:length(m2.spectra$consensus_spectrum)){

    spectrum = m2.spectra$consensus_spectrum[[i]]
    generateMSPFile(spectrum,ct = iontable$CT[i], ce = iontable$CE[i], mz = adduct_mz)

    #assign('ct', iontable$CT[i], envir= .GlobalEnv)
    #assign('ce', iontable$CE[i], envir= .GlobalEnv)
  }


  assign("m3.data", getMSLevelData(rawData,3),envir = .GlobalEnv)
  assign("m3.spectra", getMSLevel_Spectra(m3.data,msLevel = 3,mzTol=mzTol,ppmTol=ppmTol,min_rel_int=min_rel_int,min_rel_rep=min_rel_rep, mz_IQR = mz_IQR, clean = clean, min_noise = min_noise),envir = .GlobalEnv)
  assign("msp3_folder", paste(wd, paste(adduct_ID,"msp3", sep = "_"),sep = '/'),envir = .GlobalEnv)

  if(dir.exists(msp3_folder)){
    unlink(msp3_folder, recursive = T)
  }
  dir.create(msp3_folder)

  ###### NEW catch 20221026
  if(nrow(m3.spectra$peakData)>0){
    cat(paste("\nGenerating MS3 msp files for fragment analysis...\n"))

    assign("m3.iontable",  m3.spectra$peakData, envir = .GlobalEnv)
    #m3.iontable = m3.spectra$peakTable

    #print(m3.iontable)
    for(i in 1:length(m3.spectra$consensus_spectrum)){

      spectrum = m3.spectra$consensus_spectrum[[i]]
      #print(names(spectrum))
      generateMS3MSPFile(spectrum,ct = m3.iontable$CT[i], ce = m3.iontable$CE[i], mz = m3.iontable$basePeak[i])
      #assign('ct', iontable$CT[i], envir= .GlobalEnv)
      #assign('ce', iontable$CE[i], envir= .GlobalEnv)
    }
  }
  cat(paste("\nAnnotating MS2 fragment ions using MSFinder...\n"))
  runMSFinder()

  cat(paste("\nWriting product ion and neutral loss files...\n"))

  #find sfd files
  lf = list.files(path = msp_folder, recursive = T, pattern = "sfd", full.names = T)
  #assign('lf', lf, envir = .GlobalEnv)
  #print(lf)
  ##break

  sd = lapply(lf, function(x) read.SGDfile(x))
  #print(sd)
  d = do.call("rbind",sd)
  #print(d)

  fh = paste(msp_folder,"/",adduct,"_structures.csv", sep = "")
  #fileConn = file(fh)
  #print(fh)
  write.csv(d, file = fh)#,sep="\n")
  #close(fileConn)



  lf = list.files(path = msp_folder, recursive = T, pattern = "fgt", full.names = T)
  #lf

  fd = lapply(lf, function(x)read.FgtFile(x)$product_ions)
  print(fd)
  d = NULL
  d = do.call("rbind",fd)

  #print(fh)
  fh = paste(msp_folder,"/",adduct,"_fragments.csv", sep = "")
  #fileConn = file(fh)
  write.csv(d, file = fh)#,sep="\n")
  #close(fileConn)



  fd = lapply(lf, function(x)read.FgtFile(x)$neutral_losses)
  d = NULL
  d= do.call("rbind",fd)
  #print(d)

  fh = paste(msp_folder,"/",adduct,"_neutral_losses.csv", sep = "")
  #fileConn = file(fh)
  write.csv(d, file = fh)#,sep="\n")
  #close(fileConn)
#fd

  ####################
  ## Write the library spectra as one file


  #find sfd files
  lf = list.files(path = msp_folder, recursive = T, pattern = "sfd")
  ld = list.dirs.depth.n(msp_folder , n=1)

  # get the ion table for the output
  ld = list.dirs.depth.n(msp_folder,n=1)
  ld = sub(paste(msp_folder,"/",sep = ""),"",ld)
  ct = lapply(ld,function(x) strsplit(x,split = "_")[[1]][[3]])
  ce = lapply(ld,function(x) strsplit(x,split = "_")[[1]][[4]])

  #print(ld)

  #outIT = data.frame(ct = unlist(ct),ce = unlist(ce))

  #print(outIT)







}
