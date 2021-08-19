summary.spectrum <- function(cons){
     	

	p.mz =  unlist(lapply(cons,function(x) x$p.mz))
      #smiles =  unlist(lapply(cons,function(x) x$smiles))

	ct = unlist(lapply(cons,function(x) x$collision_type))
	ce = unlist(lapply(cons,function(x) x$collision_energy))
	pi = unlist(lapply(cons,function(x) x$fragment))
	u.mz = unlist(lapply(cons,function(x) x$u.mz))
	u.int = unlist(lapply(cons,function(x) x$u.int))
	rsd.int = unlist(lapply(cons,function(x) x$rsd.int))
	
    
	t.mz = lapply(cons,function(x) x$mz)
	
	int = lapply(cons,function(x) x$int)

	rel.int =lapply(int, function(x) x/max(unlist(int)))
	
	u.rel.int = unlist(lapply(rel.int, function(x) mean(x)))
	
	rsd.rel.int = unlist(lapply(rel.int, function(x) sd(x) / mean(x)))
	
	

	d = data.frame(
		'name' = rep(adduct_name,length(p.mz)),
		'ID' = rep(basename(adduct_ID),length(p.mz)),
		'precursorMZ' = p.mz,
		'collisionType' = ct,
		'collisionEnergy' = ce,
		'productIon' = pi,
            #'smiles' = smiles,
		'meanMZ' = u.mz,
		'meanIntensity' = u.int,
		'RSDIntensity' = rsd.int,
		'relMeanIntensity' = u.rel.int,
		'RSDRelMeanIntensity' = rsd.rel.int,
		'relIntensity' = u.rel.int /max(u.rel.int),
		'sdRelIntensity' = unlist(lapply(rel.int,function(x) sd(x)))
	)
	
	d
	
}