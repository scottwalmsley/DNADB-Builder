writeMSPfile <- function(){
  MSPFILE = NULL
  uct = as.character(unique(consensusSpectra$collisionType))
  
  if(length(uct) > 0 ){
    pmz = mean(consensusSpectra$precursorMZ)
    
    for(ut in uct){
      # ut = 'HCD'
      w = which(consensusSpectra$collisionType == ut)
      
      subdata = consensusSpectra[w,]
      uce = (unique(subdata$collisionEnergy))
      
      for(ue in uce){
        # ue = 0
        w =  which(subdata$collisionEnergy == ue)
        
        
        
        headline = c(
          paste("NAME:",adduct_name),
          'RETENTIONTIME: 1',
          paste("PRECURSORMZ:", pmz),
          'PRECURSORTYPE: [M+H]+',
          'IONMODE: Positive',
          'SPECTRUMTYPE: Centroid',
          paste('FORMULA:',form),
          paste('INCHIKEY',inchikey),
          paste('INCHI:',inchi),
          paste('SMILES:',smiles),
          paste('AUTHORS:',''),
          paste('COLLISIONENERGY:',ue),
          paste('INSTRUMENT:',paste(InstrumentInfo$model, InstrumentInfo$manufacturer,sep = ', ')),
          'INSTRUMENTTYPE: ESI-ITFT',
          paste('IONIZATION:',InstrumentInfo$ionisation),
          'LICENCE: CC 4.0',
          'COMMENT: ',
          paste("Num Peaks:",length(w)) )
        
        
        
        peaks = headline;
        for(j in 1:length(w)){
          line = subdata[w[j],]
          peaks = c(peaks,paste(round(line$meanMZ,4), line$relIntensity,sep=" ") )
          
        }
        
        peaks = c(peaks, "\n")
        
        MSPFILE = c(MSPFILE,peaks)
        
      }  
    } 
    MSPFILE = c(MSPFILE,"\n")
    fh = paste(data_dir,adduct,'_',adduct_name,'.msp',sep="")
    fileConn = file(fh)
    writeLines(MSPFILE, fileConn,sep="\n")
    close(fileConn)
  }
  
}
#writeMSPfile()
