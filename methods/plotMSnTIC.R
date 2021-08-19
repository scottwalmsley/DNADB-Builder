
plotMSnTIC <- function(type = ''){

      #TODO:   precursor or aglycone plots + SIM windows
      # Extract scan event #
	x  = meta$acquisitionNum

	# Extract TICs
	y  = meta$totIonCurrent
     
      # Extract only ms1 data for evaluation
      w = which(meta$msLevel == 1)
      if(length(w) > 5){
	  # Plot the data
      
	  if(type == 'pdf'){
		pdf(file = paste(plot_dir,adduct,'_ticPlot.pdf',sep = ''), width = 6, height = 3, pointsize = 6)
	  }
        if(type == 'png'){
		
		png(file = paste(plot_dir,adduct,'_ticPlot.png',sep = ''), width = 1200, height = 700, pointsize = 6, res = 300)
	  }
        if(type == '' ){
          dev.new()
        }
	  plot(x[w],y[w],pch = 19, col= "grey50", xlab = "Scan",ylab = "Total Ion Current", bty = 'n', main = paste(adduct_name, adduct_ID, sep = ' / '),cex.main = 0.75)

	  # Compute the mean tic to a smooth curve fit to the TICs
	  sm.tic = smooth.spline(x[w],y[w], spar = 0.85)
	  lines(sm.tic$x,sm.tic$y, col= 2, lwd=2)
	  mid = mean(sm.tic$yin)

	  abline( h = mid, col=4, lty = 2)
      
        if(type != '' ){
           dev.off()
	  }

     } 
   
}