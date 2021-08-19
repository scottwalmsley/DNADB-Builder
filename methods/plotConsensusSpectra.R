plotConsensusSpectra = function(d,main = '', type = 'png',max.range = T){
  #print(main)
  f = as.character(d$productIon)
  
  x = d$meanMZ
  
  y = d$relIntensity
  #y = y/max(y)
  
  
  ys = d$sdRelIntensity
  #ys = sd(y)/mean(y)
  
  #y = mean(y)
  names(x) = f
  
  if(type == 'pdf'){
    pdf(file = paste(plot_dir,adduct,'_spectrum_',main,'.pdf',sep = ''), width = 3.9, height = 3, pointsize = 6)
  }
  if(type == 'png'){
    
    png(file = paste(plot_dir,adduct,'_spectrum_',main,'.png',sep = ''), width = 1200, height = 700, pointsize = 6, res = 300)
    
  }
  
  par(mar = c(4,5,3,3))
  
  if(max.range){
    xl = c(25,d$precursorMZ[1] * 1.2)
  }else{
    xl = xlim = c(min(x) * 0.95, max(x)*1.2 )
  }
  #if(!is.na(ys))
  #  yl = c(0,max(y)+ys[which.max(y)])
  #else
  yl = c(0,(max(y)*2))
  
  plot(x,y,main = main, 
       type = 'h',
       bty = 'n' , 
       las = 2,
       xlim = xl,
       ylim = yl, 
       ylab = '',#'rel intensity' , 
       xlab = 'mz',
       col = 'grey20', 
       lwd = 1.5)
  
  segments(x,y-ys,x,y+ys, col=rgb(0.8,0.1,0.1,0.45), lwd = 4)
  points(x,y+ys, col = 4, pch = 19, cex = 1)
  
  points(x,y-ys, col = 4, pch = 1,cex = 1)
  
  points(x,y, col = 1, pch = 19, cex = 1)
  
  
  
  text(x = x ,y=(y+ys+.05),labels = names(x), adj = 0,srt = 40, cex = 0.75)
  
  abline(h = 0, lwd = 2)
  
  dev.off()
  
}
