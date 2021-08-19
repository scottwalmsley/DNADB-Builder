write_massbank_file <- function(){
  
  
  #library()
  uct = as.character(unique(consensusSpectra$collisionType))
  
  if(length(uct) > 0 ){
    
    pmz = round(mean(consensusSpectra$precursorMZ),5)
    
    for(ut in uct){
      # ut = 'HCD'
      w = which(consensusSpectra$collisionType == ut)
      
      subdata = consensusSpectra[w,]
      uce = (unique(subdata$collisionEnergy))
      
      for(ue in uce){
        FILE = NULL
        # ue = 0
        w =  which(subdata$collisionEnergy == ue)
        
        # get peak information
        int  = mz = relint = fm =  NULL
        for(j in 1:length(w)){
          
          line = subdata[w[j],]
          int  = c(int,round(line$relIntensity,3))#line$productIon,sep=" ") )
          mz   = c(mz, round(line$meanMZ,5))
          fm = c(fm, as.character(line$productIon))
          
        }
        relint = round(999*int,0)
        
        splash = splashR::getSplash(cbind(mz = mz,intensity = relint))
        
        headline = c(
          #paste('ACCESSION:',adduct),
          
          
          paste('RECORD_TITLE: ',adduct_name,'; ESI-FT; MS2; ',ut,' ',ue,' V; [M+H]+]',sep = ''),
          #paste('DATE: 2020.05.09'),
          #paste('AUTHORS: Guo J, MCC-UMN; Villatla P, ABSR-MCC-UMN; Walmsley S, IHI-MCC-UMN'),
          paste('AUTHORS: Jingshu Guo PhD, Peter Villalta PhD, Scott Walmsley PhD, Masonic Cancer Center, Univ. Of Minnesota and Marcus Cooke PhD, Anthony DeCaprio PhD, Anamary Tarifa PhD, Florida Int. Univ.'),
          paste('LICENSE: CC BY 4.0'),
          paste('COPYRIGHT: Florida International University and Masonic Cancer Center, University of Minnesota'),
          paste('PROJECT: Development of a DNA-adductome database, NIEHS: 1R03ES03118801 (PI: Guo, J.)'),
          paste('PUBLICATION: Guo J, Turesky RJ, et al., Chem. Res. Toxicol. 33, 852-854. [PMID: 32223224]'),
          paste('Comment: consensus spectrum'),
          paste("COMPOUND_CLASS: DNA adduct"),
          paste("FORMULA:",form),
          
          #paste('PRECURSOR_TYPE: [M+H]+'),
          #paste('PRECURSOR_MZ:',pmz),
          #paste("MW:", (pmz-1.00728)),
          
          paste("EXACT_MASS:", round((pmz-1.00728),5)),
          paste("CH$NAME:",adduct_name),
          paste("CH$IUPAC:",inchi),
          paste("CH$SMILES:",smiles),
          paste('SP$SAMPLE: Synthetic standard'),
          paste("AC$INSTRUMENT: Orbitrap Fusion, Thermo Scientific."),     
          paste('AC$INSTRUMENT_TYPE: ESI-FT'),
          #paste("AC$INSTRUMENT: QTOF 6530, Agilent Technologies."),     
          #paste('AC$INSTRUMENT_TYPE: QTOF'),
          #paste('AC$MASS_SPECTROMETRY: CAPILLARY_VOLTAGE 2.2 kV'),
          paste('AC$MASS_SPECTROMETRY: MS_TYPE MS2'),
          paste('AC$MASS_SPECTROMETRY: ION_MODE POSITIVE'),
          paste('AC$CHROMATOGRAPHY: FLOW_RATE 0.5 uL/min'),
          paste('AC$CHROMATOGRAPHY: RETENTION_TIME 0.5 min'),
          #paste('AC$CHROMATOGRAPHY: INLET_TYPE nanospray inlet'),
          #paste('AC$CHROMATOGRAPHY: SOLVENT acetonitrile-water (1:1) with 0.05% formic acid'),
          paste('AC$MASS_SPECTROMETRY: AUTOMATIC_GAIN_CONTROL 50000'),
          paste("AC$MASS_SPECTROMETRY: COLLISION_ENERGY ",ue,'% (nominal)',sep=""),
          paste("AC$MASS_SPECTROMETRY: COLLISION_ENERGY ",ue,'',sep=""),
          paste('AC$MASS_SPECTROMETRY: COLLISION_GAS N2'),
          paste('AC$MASS_SPECTROMETRY: FRAGMENTATION_MODE',ut),
          paste('AC$MASS_SPECTROMETRY: IONIZATION ESI'),
          paste('AC$MASS_SPECTROMETRY: RESOLUTION 120000'),
          #paste('AC$MASS_SPECTROMETRY: SOURCE_TEMPERATURE 300 C'),
          paste('MS$FOCUSED_ION: PRECURSOR_TYPE [M+H]+'),
          paste('MS$FOCUSED_ION: PRECURSOR_MZ',pmz),
          #paste('MS$FOCUSED_ION: BASE_PEAK',round(pmz,5)),
          paste('MS$DATA_PROCESSING: COMMENT Raw spectra filtered to include peaks > 0.01% of base peak intensity'),
          paste('MS$DATA_PROCESSING: COMMENT Intensity normalized to base peak and set to 999'),
          paste('MS$DATA_PROCESSING: COMMENT MSFinder 3.30 used to identify product ions'),
          #paste('MS$DATA_PROCESSING: COMMENT Calculated formulae are theoretical and for neutral uncharged predicted structure'),
          paste('MS$DATA_PROCESSING: COMMENT Consensus product ion mzs and intensities calculated from minimum 3 recurrent product ions / spectra'),
          paste('MS$DATA_PROCESSING: DEPROFILE Proteowizard 3.0.19098'))
        # paste('PK$SPLASH:',splash)
        #paste('PK$ANNOTATION: m/z tentative_formula'))
        
        
        
        
        
        
        #peaks = headline
        
        #for(j in 1:length(w)){
        
        # line = subdata[w[j],]
        
        #peaks = c(peaks,paste('',mz[j],fm[j],sep = ' '))#line$productIon,sep=" ") )
        
        #}
        
        
        
        peaks = c(headline,
                  paste('PK$NUM_PEAK:', length(w)),
                  paste('PK$PEAK: m/z int. rel.int.'))
        
        for(j in 1:length(w)){
          
          line = subdata[w[j],]
          
          peaks = c(peaks,paste(mz[j], int[j], relint[j],sep = ' '))#line$productIon,sep=" ") )
          
        }
        
        
        peaks = c(peaks, "\n")
        
        FILE = c(FILE,peaks)
        fh = paste(data_dir,adduct,'_',adduct_name,'_',ut,'_',ue,'.txt',sep="")
        fileConn = file(fh)
        writeLines(FILE, fileConn,sep="\n")
        close(fileConn)
        
      }  
    } 
    
    
  }
}
