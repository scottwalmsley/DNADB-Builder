computeConsensus <- function(rel.n = 0.25){
   
   ct =  unlist(lapply(productIons, function(x) x$ct))
   ce =  unlist(lapply(productIons, function(x) x$ce))

   rel.n = 0.25
   out.d = NULL

   u.ct =unique(ct)

   for(ut in u.ct){
	
	u.ce = unique(ce[which(ct == ut)])
	
	u.ce = u.ce[order(u.ce)]
	
	
	for(ue in u.ce){
         
		w = which(ct == ut & ce == ue)
		
		## start function here
		s.spectra = productIons[w]
		
		#TODO: ## remove empty spectra
		
		
		uf = table(unlist(lapply(s.spectra, function(x) x$form)))
		
		uf = uf[which(uf > 2)]
		
		
		# get the count of occupied spectra
		n = unlist(lapply(s.spectra, function(x) x$int))
		
            w = which(n >0)
		
           if(length(w)> 2 & length(uf)>0){
			
			
			i = 1
			
			cons = list()# vector(mode = 'list', length=length(uf))
			
			# get the spectra as arrays
			for(f in names(uf)){
				
				e.mz  = e.int =  NULL
				#e.smiles = NULL
			      
								
				for(s in s.spectra){
					w = which(s$form == f)
					
					if(length(w)>0){
						
						if(length(w)> 1){
                                          
                                          w = w[which.max(s$int[w])]
                                          
						}
						
						e.mz = c(e.mz,s$mz[w])
						
						e.int = c(e.int,s$int[w])
						#print(s$smiles[w])
						#e.smiles = c(e.smiles,s$smiles[w])
                                    
					}
					
					
				}
				
				
				if(!is.null(e.mz)){
					
					cons[[f]] = list( 'p.mz' = s$pmz,
								'collision_type' = ut,
								'collision_energy' = ue,
								'fragment' = f,
                                                #'smiles' = unique(e.smiles),
								'u.mz' = mean(e.mz),
								'u.int' = mean(e.int), 
								'rsd.int' = sd(e.int)/mean(e.int)*100,
								'mz' = e.mz, 
								'int' = e.int)			
					
				}else{
					cons[[f]] = NULL
				}
				
				
				i=i+1
				
			}
			
			
			
                  d = NULL
		      d = summary.spectrum(cons)
			## summarize energy series
			if(is.null(out.d)){

				out.d = d

			}else{
                  
                      
				out.d = rbind(out.d, d)
                        plotConsensusSpectra(d,main = paste(ut,ue, sep = '_'), type = 'png',max.range = T)
                        plotConsensusSpectra(d,main = paste(ut,ue, sep = '_'), type = 'pdf',max.range = T)
			
                  }
			
			
			#plotConsensusSpectra(d,main = paste(ut,ue, sep = '_'), type = '',max.range = T)
			
			
		}
	}
  }


 if(!is.null(out.d)){
 	write.csv(file = paste(data_dir,adduct,'_spectrumData','.csv',sep = ''),out.d, row.names = F)
 }
 assign('consensusSpectra',out.d,envir = .GlobalEnv)
}