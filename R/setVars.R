

#' Set variables for a new ion
#'
#' @param metadata character vector of adduct info read from file
#' @param wd character user wpecified working dir
### @param plotdir characterd plot directory full path
### @param outdir character directory for raw ms finder results
#' @export
#'
setVars <- function(metadata,wd = NULL){

  if(dir.exists(wd)){
    unlink(wd, recursive = T)
  }
  #if(dir.exists(plotdir)){
  #  unlink(plotdir, recursive = T)
  #}

  #if(dir.exists(outdir)){
  #  unlink(outdir, recursive = T)
  #}

  assign('adduct_ID', metadata$Data_File_Name,envir = .GlobalEnv)

  if(dir.exists(adduct_ID)){
    unlink(adduct_ID, recursive = T)
  }

  if(!is.null(wd)){
    assign('wd', wd,envir = .GlobalEnv)
  }else{
    assign('wd', adduct_ID,envir = .GlobalEnv)
  }

  assign('adduct', adduct_ID,envir = .GlobalEnv)


  mol = Rdisop::getMolecule(metadata$FORMULA)
  mz = mol$exactmass + 1.00728

  dir.create(wd)
  #dir.create(plotdir)

  dir.create(paste(wd,'data',sep = '/'))
  #dir.create(outdir)


  assign("rawData", NULL, envir = .GlobalEnv)

  assign("m1.data", NULL, envir = .GlobalEnv)
  assign("m2.data", NULL, envir = .GlobalEnv)
  assign("m3.data", NULL, envir = .GlobalEnv)

  assign("m1.spectra", NULL, envir = .GlobalEnv)
  assign("m2.spectra", NULL, envir = .GlobalEnv)
  assign("m3.spectra", NULL, envir = .GlobalEnv)


  #assign('plot_dir', paste(wd,plotdir),envir = .GlobalEnv)
  #assign('raw_dir', outdir,envir = .GlobalEnv)

  assign('adduct_name',metadata$NAME,envir = .GlobalEnv)

  assign('form', metadata$FORMULA,envir = .GlobalEnv)
  assign('adduct_mz',mz,envir = .GlobalEnv)

  assign('inchi',metadata$inchi,envir = .GlobalEnv)
  assign('smiles',metadata$SMILES,envir = .GlobalEnv)

  assign('MSFINDER_PATH', 'c:/MSTOOLS/MSFINDERv3.60/MsfinderConsoleApp.exe', envir = .GlobalEnv)
  assign('MSFINDER_INI', 'c:/MSTOOLS/MSFINDERv3.60/MSFINDER.INI', envir = .GlobalEnv)


}










