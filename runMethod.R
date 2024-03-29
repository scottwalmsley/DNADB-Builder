#setwd('D:/rdev/DNAAdductDBBuilder/20200324')

rm(list=ls())
source('get_atom_counts.R')

d = read.csv('20210331.csv', stringsAsFactors = F, check.names = F)


qtof = F

for(i in 6:nrow(d)){  #1
  
 #i=2
  source('software/setVars.R')
  setVars(adduct_name = d$NAME[i],
          adduct_ID = d$ID[i],
          adduct_mz = get_atom_counts(f = d$FORMULA[i], phos = d$P[i], Cl = d$Cl[i])$exact_mass + 1.007825,
          form = d$FORMULA[i],
          smiles = d$SMILES[i]
  )
  
  
  if(length(which(meta$msLevel ==2)) > 0){#2
    
    g = grep(1,meta$msLevel)
    
    if(length(g) > 3){#3
      plotMSnTIC(type = 'png')
      
      plotMSnPPMError(ppmTol = 10, type = 'png')
      
      
      #subsetData(minScan = 0, maxScan =200000)
      #subsetData(minScan = d$start[i], maxScan =d$end[i])
      if(qtof){
        extractMSnData(qtof=TRUE)
      }else{
        extractMSnData()
      }
      
      
      if(qtof){
        computeMS1Purity(qtof = TRUE)
      }else{    
         computeMS1Purity(collision_type = 'HCD')
         computeMS1Purity(collision_type = 'CID')
      }
      
      if(qtof){
        filterData(qtof = TRUE)
      }else{
        filterData()
      }
      
      generateMATFiles()
      lm = list.files(path = raw_dir, pattern = '.mat', recursive = T)   
      
      if(length(lm) > 4){#4
        runMSFinder()
        Sys.sleep(3)
        
        ds = processStructureFiles()
        
        if(length(ds)>0){#5
          assign('productIons',ds,envir = .GlobalEnv)
          
          computeConsensus()
          
          writeMSPfile()
          writeSDFile()
          write_massbank_file()
          
        }#5
        
      }#4
    }#3
  }else{ #2/2e
    if(dir.exists(wd)){#2s
      unlink(wd, recursive = T)
    }#2s
  }#2e
} #1


library('rjson')


# Give the input file name to the function.
library(rjson)
result <- fromJSON(file = 'mnDNA.json')
str(result[[1]])
