#' Plot Spectrum
#'
#' @param spectrum matrix or numeric 2 columns
#' @param name character name for plot title
#' @param xlim vector x axis limit
#'
#' @export
plot_spectrum <-function(spectrum,name, xlim = c(100,500)){


  plot(spectrum,
       type = 'h',
       main = name,
       xlab = "m/z",
       ylab = "Relative Intensity",
       ylim = c(0,1),
       xlim = xlim
  )

}



#' Plot a match between 2 spectra
#'
#' @param spectrum matrix or numeric 2 columns
#' @param ref_spectrum matrix or numeric 2 columns
#' @param xlim vector x axis limit
#'
#' @export
plot_spectrum_match <- function(spectrum, ref_spectrum, xlim = c(100,500)){

  importFrom("graphics", "abline", "lines")
  plot(spectrum[,1],
       spectrum[,2],
    type = 'h',
    main = name,
    xlab = "m/z",
    ylab = "Relative Intensity",
    xlim = xlim,
    ylim  = c(-1,1)

  )

  lines(
    ref_spectrum[,1],
    ref_spectrum[,2]* -1,

    type = 'h'
  )


  abline(h=0)


}

#' Plot ppm error of ms1 data
#'
#' @param type type 'pdf','png' or device (monitor) plot
#' @param ppmTol float ppm tolerance
#'
#' @export
#'
plotMSnPPMError <- function(type = 'pdf',ppmTol){

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



#' Plot a consensus spectrum
#'
#' @param d
#' @param main text label for title
#' @param type char png or pdf for plot type
#' @param max.range bool plot the max range
#'
#' @export
#'
plotConsensusSpectra = function(d,main = '', type = 'png',max.range = T){

  f = as.character(d$productIon)

  x = d$meanMZ

  y = d$relIntensity


  ys = d$sdRelIntensity

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
