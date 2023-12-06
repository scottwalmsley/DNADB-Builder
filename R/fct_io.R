#' Generate a MSP file for MS2 data
#'
#' @param spectrum matrix or numeric 2 columns
#@param msLevel integer mslevel
#@param nm character name for the adduct
#' @param ct numeric collision type
#' @param ce numeric collision energy
#' @param mz numeric m/z
#'
#' @export
generateMSPFile <- function(spectrum, ct,ce,mz){


  NUMPEAKS = nrow(spectrum)


  msp =
    c(paste("NAME:",adduct_name, sep=" "),

      paste("PRECURSORMZ:", mz),

      "PRECURSORTYPE: [M+H]+",
      "INSTRUMENTTYPE: ESI-ITFT",
      #"INSTRUMENT: LTQ Orbitrap Fusion Tribrid, Thermo Scientfic",
       "INSTRUMENTTYPE: QTOF",
       "INSTRUMENT: QTOF 6530, Agilent Technologies",
      paste("SMILES:",smiles),
      paste("INCHI:",inchi),
      #paste("INCHIKEY:",inchikey),
      paste("COLLISIONTYPE: ",ct ,sep=""),
      paste("COLLISIONENERGY: ",ce ,sep=""),
      paste("FORMULA:",form),
      "RETENTIONTIME: ",
      "IONMODE: Positive",
      "Links: ",
      "MSTYPE: 2",
      # paste("MSTYPE:",msLevel),
      paste("Num Peaks:",NUMPEAKS) )#, sep="",




  #peaks = c(headline,paste("Num Peaks:",NUMPEAKS));


  for(i in 1:NUMPEAKS){

    line = spectrum[i,]
    msp = c(msp,paste(line[1], line[2],sep=" ") )


  }

  #msp = c(msp, "\n")

  nm = paste(adduct,'_',round(mz,4),'_',ct,'_',ce, sep = "")

  fh = paste(msp_folder,"/",nm,sep="")

  fh = paste(fh,".msp",sep="")

  assign(paste(adduct_ID,"msp",sep = "_"),paste(getwd(),fh,sep = "/"),envir = .GlobalEnv)

  print(fh)

  fileConn = file(fh)

  writeLines(msp, fileConn,sep="\n")

  close(fileConn)

}

#' Generate a MSP file for MS3 data
#'
#' @param spectrum matrix or numeric 2 columns
#@param msLevel integer mslevel
#@param nm character name for the adduct
#' @param ct numeric collision type
#' @param ce numeric collision energy
#' @param mz numeric m/z
#'
#' @export
generateMS3MSPFile <- function(spectrum, ct,ce,mz){


  #spectrum = m3.spectra$peakData
  #m3.spectra$peakData
  #print(ct)
  NUMPEAKS = nrow(spectrum)
  #MSPFILE =  NULL;

  msp =

    c(paste("NAME:",adduct_name, sep=" "),

      paste("PRECURSORMZ:", mz),

      "PRECURSORTYPE: [M+H]+",
      "INSTRUMENTTYPE: ESI-ITFT",
      "INSTRUMENT: LTQ Orbitrap Fusion Tribrid, Thermo Scientfic",
      # "INSTRUMENTTYPE: QTOF",
      # "INSTRUMENT: QTOF 6530, Agilent Technologies",
      "SMILES:",
      "INCHI:",
      #paste("INCHIKEY:",inchikey),
      paste("COLLISIONTYPE:", ct, sep = ""),
      paste("COLLISIONENERGY: ",ce ,sep=""),
      "FORMULA:",
      "RETENTIONTIME: ",
      "IONMODE: Positive",
      "Links: ",
      "MSTYPE: 3",
      paste("Num Peaks:",NUMPEAKS) )#, sep="",




  #peaks = c(headline,paste("Num Peaks:",NUMPEAKS));


  for(i in 1:NUMPEAKS){

    line = spectrum[i,]
    msp = c(msp,paste(line[1], line[2],sep=" ") )


  }

  #msp = c(msp, "\n")

  nm = paste(adduct,'_',round(mz,4),'_',ct,'_',ce, sep = "")
  #print(nm)
  fh = paste(msp3_folder,"/",nm,sep="")

  fh = paste(fh,".msp",sep="")

  assign(paste(adduct_ID,"msp3",sep = "_"),paste(getwd(),fh,sep = "/"),envir = .GlobalEnv)

  print(fh)

  fileConn = file(fh)

  writeLines(msp, fileConn,sep="\n")

  close(fileConn)

}


#' Read sgd file
#'
#' @param fh input sgd file
#'
#' @return data.frame of product ion data and structures
#' @export
#'
# @examples
read.SGDfile <- function(fh){

  tryCatch(
    expr = {
      res = read.delim(fh, skip = 26, header = F)
    },
    error = function(err){
      print("No SGD file")
    }
  )



}



#' Read fgt file
#'
#' @param fh input fgt file
#'
#' @return list of product and neutral loosses
#' @export
#'
# @examples
read.FgtFile = function(fh){

  res = scan(fh, what = "raw", sep = "\n")
  g = grep("^Num Product", res)
  g2 = grep("^Num Neutral", res)

  n_productIons = n_NL = NULL

  if(length(g)>0){
    n_productIons = as.numeric(strsplit(res[g], split = ":")[[1]][2])
  }
  if(length(g2)>0){
    n_NL = as.numeric(strsplit(res[g2], split = ":")[[1]][2])
  }

  if(n_productIons > 0){
    i = g+1
    j = g+n_productIons
    write(file = "tmp_frag.txt",res[i:j])
    fi =  fragment.fgt( read.delim(file = "tmp_frag.txt",header = F))

  }

  if(n_NL > 0){
    i = g2
    j = g2+n_NL

    write(file = "tmp_nl.txt",res[i:j])
    nl = neutralLoss.fgt(read.delim("tmp_nl.txt", header = F))

  }
  ###############################
  #
  #  if(length(g)>0 & g2 < length(res)){
  #   i = g+1
  #    j = g2-1

  #    write(file = "tmp_frag.txt",res[i:j])
  #    fi =  fragment.fgt( read.delim(file = "tmp_frag.txt",header = F))

  #    i = g2+1
  #    j = length(res)

  #    write(file = "tmp_nl.txt",res[i:j])
  #    nl = neutralLoss.fgt(read.delim("tmp_nl.txt", header = F))

  #    return(list(
  #      "product_ions" = fi,
  #      "neutral_losses" = nl
  #    ))

  #  }


  # if(length(g)>0 & g2 == length(res)){
  #    i = g+1
  #    j = g2-1

  #    write(file = "tmp_frag.txt",res[i:j])
  #    fi =  fragment.fgt( read.delim(file = "tmp_frag.txt",header = F))
  #    return(list(
  #      "product_ions" = fi,
  #      "neutral_losses" = NULL
  #    ))


  #  }
}



#read.FgtFile(file.choose())







