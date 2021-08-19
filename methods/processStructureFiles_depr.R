
processStructureFiles <- function(){

   lm = list.files(pattern = '.mat',recursive = TRUE, path = gsub('/$','',raw_dir), full.names =T)
   mat = lapply(lm, function(x) scan(x,what = 'character', sep = '\n',quiet = T))
   lm = basename(lm)
   lm = sub('.mat','',lm)


   
   lf = list.files(pattern = '.fgt',recursive = TRUE, path = gsub('/$','',raw_dir), full.names =T)
   fgt = lapply(lf, function(x) scan(x,what = 'character', sep = '\n',quiet = T))
   lf = basename(lf)

   lf = sub('.fgt','',lf)
   f.nm = lf
   f.ct = unlist(lapply(lf, function(x) strsplit(x, split='_')[[1]][2]))
   f.ce = unlist(lapply(lf, function(x) strsplit(x, split='_')[[1]][3]))


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

   out = list()
   k = 1
   if(length(s.nm)>0){
    for(i in 1:length(s.nm)){

      w = which(f.nm == s.nm[i])
      if(length(w) > 0){

         w2 = which(lm == s.nm[i])
         m.arr = mat[[i]]
         g = grep('PRECURSORMZ',m.arr)
         pmz = as.numeric(strsplit(m.arr[g],split = ' ')[[1]][2])
         msf = mergeStructureFiles(pmz, sfd[[i]],s.ct[i],s.ce[i])
        
         if(length(msf) > 1){
           print(msf)
           out[[k]] = msf
           k = k+1
         }
      }

     
    }
   }
   out
}
