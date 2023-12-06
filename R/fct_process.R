#' Run MSFinder from the command line.
#'
#' @export
#'
runMSFinder <- function(){


  #msp_file = `TUR-UMN-001_hcd_100_msp`
  command = paste('START /W /min ',
                  MSFINDER_PATH,
                  'annotate',
                  '-i',
                  paste(getwd(),msp_folder,sep = '/'),
                  '-m',
                  MSFINDER_INI
  )

  print(command)
  bat_file = paste(getwd(),"/",msp_folder,"/",adduct_ID,'.bat',sep = '')
  bat_file

  write(file = bat_file,command)

  shell(bat_file,wait = T, translate=T)








}




#' List directories in folder
#'
#' @param p string path of directory.  Default = '.'
#' @param n integer value indicating the folder depth to search
#'
#' @return vector of strings containing the paths to the directories
#' @export
list.dirs.depth.n <- function(p, n) {
  res <- list.dirs(p, recursive = FALSE)
  if (n > 1) {
    add <- list.dirs.depth.n(res, n-1)
    c(res, add)
  } else {
    res
  }
}
