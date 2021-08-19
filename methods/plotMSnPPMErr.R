plotMSnPPMError <- function(type = '',ppmTol){

   dM = tol(adduct_mz,ppmTol)

   w = which(meta$msLevel == 1)

   x = meta$acquisitionNum[w]
   
     # Plot the data
      
	if(type == 'pdf'){
		pdf(file = paste(plot_dir,adduct_ID,'_ppmErrPlot.pdf',sep = ''), width = 6, height = 3, pointsize = 6)
	}

	if(type == 'png'){
		
		png(file = paste(plot_dir,adduct_ID,'_ppmErrPlot.png',sep = ''),width = 1200, height = 600, pointsize = 6, res = 300)
	}
      if(type == '' ){
          dev.new()
      }



      precursorMZ = unlist(lapply(spectra[w], function(k) {m = k[which(k[,1] > dM[1] & k[,1] < dM[2]),1]; ifelse(is.null(m), NA,m)}))
         

      ppmErr= (precursorMZ-adduct_mz) / adduct_mz * 1e6

     
      yl = c(-ppmTol,ppmTol)
      
      plot(x, ppmErr, pch = 18, col = rgb(0.15,0.15,.015,0.5), 
           xlab = "Scan", bty = 'n', ylim = yl, main = paste(adduct_name, adduct_ID, sep = ' / '),cex.main = 0.75)
                                           

       w = which(!is.na(ppmErr))

       if(length(w) > 5){
          sm.ppm = smooth.spline(x[w], ppmErr[w], spar = 0.55)

          lines(sm.ppm$x,sm.ppm$y, col= 2, lwd=2)
       }
       abline(h = 0, lty = 2)

       if(type !=''){

            dev.off()   
       }

}