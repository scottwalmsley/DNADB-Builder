# DNADB-Builder
Code to build a consensus spectrum library for DNA adductomics

To build a consunsus spectrum from our acquiored data, the following simple code lines are used:

```{r}
library(DnaAdductBuilder)


processWorkflow(mzMLFile, # the mzML file converted from the raw or .d file for a single compound.
				metadata, # a data from of compound metadata (se below)
				wd = metadata$ID, # an identifier for the compound
				mzTol=0.05, # mz tolerance for spectrum binning during the consensus building stage
				ppmTol=7, # ppm tolerance for grouping of same product ions
				min_rel_int=0.05, # minimum relative intensity of the peaks in the spectra
				min_rel_rep=0.25, # minimum % repllicates of the product ions
				mz_IQR = 14, # minimmum interquartile range of deviation of production ion intensities
				clean = T, # do you want to clean the spectra of noise?  Following 2 parameters apply:
				min_noise = 0.01, # noise floor of which to strip peaks (remove from spectra, relative to max intensity
				contamMZ = 173.52815) # any individual identified contaminant peak to remove
```