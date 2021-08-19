setVars <- function(adduct_name,adduct_ID,adduct_mz,form,smiles){
  if(dir.exists(adduct_ID)){
    unlink(adduct_ID, recursive = T)
  }
  
  source('software/fct_algo.R')
  source('software/plotMSnTIC.R')
  source('software/plotMSnPPMErr.R')
  source('software/subsetData.R')
  source('software/extractMSnData.R')
  source('software/filterData.R')
  source('software/generateMATFiles.R')
  source('software/runMSFinder.R')
  source('software/processStructureFiles.R')
  source('software/mergeStructureFiles.R')
  source('software/computeConsensusSpectra.R')
  source('software/summarySpectrum.R')
  source('software/plotConsensusSpectra.R')
  source('software/computeMS1Purity.R')
  source('software/writeMSPfile.R')
  source('software/writeSDFile.R')
  source('software/write_massbank_file.R')
  source('software/fct_algo.R')
  require(webchem)
  library(mzR)
  
  assign('wd', adduct_ID,envir = .GlobalEnv)	
  assign('adduct', adduct_ID,envir = .GlobalEnv)	
  
  
  dir.create(wd)
  dir.create(paste(wd,'plots',sep = '/'))
  dir.create(paste(wd,'data',sep = '/'))
  dir.create(paste(wd,'raw',sep = '/'))
  dir.create(paste(wd,'raw','ms2',sep = '/'))
  
  assign('plot_dir', paste(wd,'plots/',sep = '/'),envir = .GlobalEnv)
  assign('raw_dir', paste(wd,'raw/ms2/',sep = '/'),envir = .GlobalEnv)
  assign('data_dir', paste(wd,'data/',sep = '/'),envir = .GlobalEnv)
  
  assign('adduct_name', adduct_name,envir = .GlobalEnv)	
  assign('adduct_ID', paste(adduct_ID,'.mzML', sep = ''),envir = .GlobalEnv)
  assign('form', form,envir = .GlobalEnv)
  assign('smiles', smiles,envir = .GlobalEnv)
  assign('adduct_mz', adduct_mz,envir = .GlobalEnv)
  
  assign('inchi',webchem::cs_convert(smiles,from = 'smiles',to = 'inchi')[[1]],envir = .GlobalEnv)
  assign('inchikey', webchem::cs_convert(inchi,from = 'inchi',to = 'inchikey')[[1]],envir = .GlobalEnv)
  
  
  
  pth = paste(getwd(),'/',adduct_ID,'.mzML',sep = '')
  #pth = paste(getwd(),'/',adduct_ID,sep = '')
  print(pth)
  mzd = openMSfile(pth, backend = NULL)
  
  assign('mz',mzd ,envir = .GlobalEnv)
  assign('spectra', spectra(mzd),envir = .GlobalEnv)
  assign('meta', header(mzd),envir = .GlobalEnv)
  assign('InstrumentInfo', instrumentInfo(mz),envir = .GlobalEnv)
  close(mzd)
  
  
  assign('MSFINDER_PATH', 'c:/MSTOOLS/MSFINDERv3.30/MsfinderConsoleApp.exe', envir = .GlobalEnv)
  assign('MSFINDER_INI', 'c:/MSTOOLS/MSFINDERv3.30/MSFINDER.INI', envir = .GlobalEnv)
  
  
  
}


