generateMATFiles <- function(){
  
  #dir.create('raw')
  #dir.create('raw/ms2')
  
  MSPFILE = TMPFILE = NULL;
  
  for(i in 1:length(filtered.ms2.spectra)){
   
    
    # if(is.matrix(filtered.ms2.spectra[[i]])){
    if(nrow(filtered.ms2.spectra[[i]]) > 0 ){#} || is.numeric(filtered.ms2.spectra[[i]]) ){
      if(MSLEVEL[i] == 2){
        headline = 
          
          c(paste(paste("NAME:",adduct_name),CT[i],CE[i],SCANNO[i], sep="_"),
            paste("SCANNUMBER:",SCANNO[i]),
            paste("PRECURSORMZ:", precursorMZ[i]),
            paste("PRECURSORINTENSITY: ", precursorIntensity[i]),
            "PRECURSORTYPE: [M+H]+",
            "INSTRUMENTTYPE: ESI-ITFT",
            "INSTRUMENT: LTQ Orbitrap Fusion Tribrid, Thermo Scientfic",
           # "INSTRUMENTTYPE: QTOF",
          #  "INSTRUMENT: QTOF 6530, Agilent Technologies",
            paste("SMILES:",smiles),
            paste("INCHI:",inchi),
            paste("INCHIKEY:",inchikey),
            paste("COLLISIONENERGY: ",CE[i],CT[i],sep=""),
            paste("FORMULA:",form),
            "RETENTIONTIME: 1",
            "IONMODE: Positive",
            "Links: ",
            paste("Comment:",comment[i]),
            "MSTYPE: MS2")#,
        #paste("Num Peaks:",NUMPEAKS[i]) )#, sep="",
        
        
        #TMPFILE = c(TMPFILE,headline,'\n')
        
        peaks = c(headline,paste("Num Peaks:",NUMPEAKS[i]));
        
        for(j in 1:NUMPEAKS[i]){
          if(is.matrix(filtered.ms2.spectra[[i]])){
            line = filtered.ms2.spectra[[i]][j,]
            
          }
          if(!is.matrix(filtered.ms2.spectra[[i]])){
            line = filtered.ms2.spectra[[i]]
          }
          
          peaks = c(peaks,paste(line[1], line[2],sep=" ") )
          #print(line)
          
        }
        
        peaks = c(peaks, "\n")
        
        #MSPFILE = c(MSPFILE,peaks)
        
        
        fh = paste(raw_dir,adduct,'_',CT[i],'_',CE[i],'_',SCANNO[i],sep="")
        fileConn = file(paste(fh,".mat",sep=""))
        writeLines(peaks, fileConn,sep="\n")
        close(fileConn)
      }
    }
    
    # }
    
    
  }
  
  #fh = paste(wd,'/','tmp_',adduct,'.msp',sep='')
  #  fileConn = file(fh)
  # writeLines(TMPFILE, fileConn)#,sep="\n")
  # close(fileConn)
  
  
}
