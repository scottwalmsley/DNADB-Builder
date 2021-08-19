
processStructureFiles <- function(){

   lm = list.files(pattern = '.mat',recursive = TRUE, path = gsub('/$','',raw_dir), full.names =T)
   mat = lapply(lm, function(x) scan(x,what = 'character', sep = '\n',quiet = T))
   lm = basename(lm)
   lm = sub('.mat','',lm)


   ld = list.dirs(recursive = TRUE, path =gsub('/$','',raw_dir))
   ld = basename(ld)

   g = grep('ms2', basename(ld))
   if(length(g)>0){
      ld = ld[-g]
   } 

   s.nm = ld
   s.ct = unlist(lapply(ld, function(x) strsplit(x, split='_')[[1]][2]))
   s.ce = as.numeric(unlist(lapply(ld, function(x) strsplit(x, split='_')[[1]][3])))

   lsd = list.files(pattern = '.sfd',recursive = TRUE, path = gsub('/$','',raw_dir), full.names =T)
   sfd = lapply(lsd, function(x) scan(x,what = 'character', sep = '\n',quiet = T))

   out = list()#vector(mode = 'list', length = length(sfd))
   k=1
   for(i in 1:length(s.nm)){

      w = which(lm == s.nm[i])
      if(length(w) > 0){

        
         m.arr = mat[[i]]
         g = grep('PRECURSORMZ',m.arr)
         pmz = as.numeric(strsplit(m.arr[g],split = ' ')[[1]][2])
         
         msf = mergeStructureFiles(pmz, sfd[[i]],s.ct[i],s.ce[i])
        if(length(msf) > 1 ){
           out[[k]] = msf
           k=k+1
        }

     }

   }
   out
}
