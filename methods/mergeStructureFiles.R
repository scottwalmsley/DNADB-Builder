
#### get the array of data from each file pair
mergeStructureFiles <- function(pmz,sfd,ct,ce){

    

    form = mz = int = smiles = NULL

    
   g = grep('Num Fragment',sfd)
   if(g != length(sfd)){

    if(length(g)>0){

       for(i in (g+1):length(sfd)){
 
          arr = strsplit(sfd[i], split = '\t')[[1]]
          form = c(form,arr[5])
          mz = c(mz,arr[1])
          int = c(int,arr[2])
          #smiles = c(smiles,arr[16])
      }
   
   }
 
   mz = as.numeric(mz)
   int = as.numeric(int)

   return(
   list('ct' = ct,
        'ce' = ce,
        'pmz' = pmz,
        'mz' = mz,
        'int' = int,
        #'smiles' = smiles,
        'form' = form))
   }else{
      return(NA)
   }

}

