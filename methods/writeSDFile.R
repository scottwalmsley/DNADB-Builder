writeSDFile <- function(){
   require(ChemmineR)
   
   sd = smiles2sdf(smiles)


   png(file = paste(plot_dir,'structure_',adduct,'.png',sep = ''))#,res = 300)#, height = 600)#, width = 600)

   openBabelPlot(sd[1],regenCoords=F)
    dev.off()

   write.SDF(sd,paste(wd,'/',adduct,'.sd',sep = '') )
   detach("package:ChemmineR")
   detach("package:ChemmineOB")
}