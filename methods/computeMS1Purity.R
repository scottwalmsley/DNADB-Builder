computeMS1Purity <- function(type = 'png', collision_type = NA, qtof = FALSE){
  
  
  
 
  if(qtof){
    w = which(meta$msLevel==2 & meta$collisionEnergy == 0)
  }else{
    w = which(meta$msLevel==2 & meta$collisionEnergy == 0 & CT == collision_type)
  }
  
  
  if(length(w) > 4){
    if(qtof){
      pmz = adduct_mz
    }else{
      id = meta$precursorScanNum[w]
      id = unique(id)
      pmz = mean(meta$precursorMZ[w])
    }
    
    
    s = spectra[w]
    
    w = which(unlist(lapply(s, function(x) !is.null(x))))
    
    s = s[w]
    
    out = NULL
    for(i in s){
      out = rbind(out,i)
    }
    out = out[order(out[,1]),]
    w = which(out[,1] > (pmz-.8)  & out[,1] < (pmz+0.8))
    
    if(!is.null(out) & length(w)>0){
      
      if(type == 'png'){
        
        png(width = 1200, height = 600, res = 300,pointsize = 6, file = paste(plot_dir,adduct,'_',collision_type,'_MS1_purity.png', sep = ''))
        par(mar = c(5,5,3,3))
        plot(out[w,],bty = 'n', type = "h",  xlab ="m/z", ylab = "intensity", main = paste(adduct, collision_type))
        
        
        
      }
      
      t.pmz = adduct_mz
      
      s.out = out[w,]
      if(qtof){
        mr = getMassTolRange(t.pmz,25)
      }else{
        mr = getMassTolRange(t.pmz,7)
      }
      w = which(s.out[,1] > mr[1] & s.out[,1] < mr[2])
      
      lines(s.out[w,],type = 'h', col=4)
      
      rat = sum(s.out[-w,2], na.rm = T) / sum(s.out[w,2], na.rm = T)
      rat2 = max(s.out[-w,2])/max(s.out[w,2])
      text(min(s.out[,1]),max(s.out[,2]*0.9), pos = 4,
           paste('MS1 purity:',round(rat,2),'\nRel. next peak: ',round(rat2,2))  )
      
      assign(paste(collision_type,'MS1Purity',sep='_'),rat,envir = .GlobalEnv)
      assign(paste(collision_type,'MS1RelNextContam',sep='_'),rat2,envir = .GlobalEnv)
      
      if(type == 'png'){
        dev.off()
      }    
    }
    
  }
}
