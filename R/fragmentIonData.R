#' Sgd file dta store
#'
#' @param df data frame from sgd file
#' @param collision_type collision type eg 'hcd' or 'cid'
#' @param collision_energy collision energy (meV) eg '80'
#'
#' @return data. frame
#' @export
#'
# @examples
fragment.sgd <- function(df,collision_type = NULL, collision_energy = NULL){

  mz = df[,1]
  exactMass = df[,3]
  rel.int = df[,2]
  form = df[,5]
  smile = df[,16]

  data.frame(
    mz,
    exactMass,
    rel.int,
    form,
    smile

  )

}



#' Fragment ion data store
#'
#' @param df fgt data. frame
#' @param collision_type collision type eg 'hcd' or 'cid'
#' @param collision_energy collision energy (meV) eg '80'
#'
#' @return data frame
#' @export
#'
# @examples
fragment.fgt <- function(df){

    mz = round(df[,3],4)
    exactMass = round(df[,2],4)
    rel.int = df[,4]
    rel.int.err = df[,5]
    form = df[,1]

    data.frame(
     # collision_type,
     # collision_energy,
      mz,
      exactMass,
      rel.int,
      rel.int.err,
      formula = form

    )



}



#' Neutral loss data store
#'
#' @param df input fgt data frame
#' @param collision_type collision type eg 'hcd' or 'cid'
#' @param collision_energy collision energy (meV) eg '80'
#'
#' @return dataframe
#' @export
#'
# @examples
neutralLoss.fgt <- function(df, collision_type = NULL, collision_energy = NULL){


    mz = round(df[,7],4)
    exactMass = round(df[,2],4)
    prec.mz =  df[,3]
    prod.mz = df[,4]
    prec.int = df[,5]
    prod.int = df[,6]
    form = df[,1]

    data.frame(
      #collision_type,
      #collision_energy,
      mz,
      exactMass,
      prec.mz,
      prod.mz,
      prec.int,
      prod.int,
      formula = form

    )




}
