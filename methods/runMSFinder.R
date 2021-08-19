
runMSFinder <- function(){
   command = paste('START /W /min ', 
                MSFINDER_PATH,
                'annotate',
                '-i',
                paste(getwd(),raw_dir,sep = '/'),
                '-m',
                MSFINDER_INI
                )
      write(file = paste(wd,'/',adduct,'.bat',sep = ''),command)

      shell(paste(wd,'/',adduct,'.bat',sep = ''),wait = T, translate=T)
  
}

