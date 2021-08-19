rm(list=ls())
#setwd('D:/rdev/DNAAdductDBBuilder/20200324/')
#meta  = read.csv('20200401_list.csv', as.is = T)
#meta  = read.csv('20200411_list.csv', as.is = T)
meta = read.csv('20210331.csv',as.is = T)

ID = meta$ID
NAME = meta$NAME
SMILES = meta$SMILES
FORMULA = meta$FORMULA

MZ = unlist(lapply(1:nrow(meta),function(x) get_atom_counts(f= meta$FORMULA[x], phos = meta$P[x],Cl=meta$Cl[x])$exact_mass + 1.007825))
MZ = MZ - 1.007825
MZ = round(MZ,4)

 
INCHI    = unlist(lapply(1:nrow(meta),function(x)  webchem::cs_convert(meta$SMILES[x],from = 'smiles',to = 'inchi')[[1]]))
INCHIKEY = unlist(lapply(1:nrow(meta),function(x)  webchem::cs_convert(INCHI[x],      from = 'inchi' ,to = 'inchikey')[[1]]))



library(officer)

out = NULL
### start loop here

for(z in 2:length(ID)){

  
  cs = list.files(pattern = 'csv',path = paste(ID[z]), full.names = T,recursive = T)
  

  doc  = read_docx()
  cursor_begin(doc)
  
  if(length(cs)>0){
    
    out = rbind(out,read.csv(cs[1], as.is = T))
    library(officer)
    

    pl = list.files(pattern = '.png', path = ID[z], full.names = T, recursive = T)
    
    if(length(pl) > 0){
      #Header
      prop =  fp_text(font.size = 18, bold = TRUE, font.family = "Times")
      body_add_fpar(doc,fpar(ftext("DNA Adduct Card", prop)), style = 'Normal',pos = 'before')
      body_add_par(doc,"")
      prop =  fp_text(font.size = 14, bold = TRUE, font.family = "Times")
      body_add_fpar(doc,fpar(ftext(ID[z], prop)))
      body_add_par(doc,"")
      #body_add_par(doc,"")

      body_end_section_continuous(doc)
      
      
      prop =  fp_text(font.size = 18, bold = TRUE, font.family = "Times")
      
      #body_add_par(doc,"")
      #body_add_par(doc,"")
      
      
      prop.bold =  fp_text(font.size = 11, bold = TRUE, font.family = "Times")
      prop.bold_text =  fp_text(font.size = 9, bold = TRUE, font.family = "Times")
      prop.normal =  fp_text(font.size = 8, bold = FALSE, font.family = "Times")
      
  
      body_add_par(doc,"")
      body_add_par(doc,"")
      body_add_fpar(doc,fpar(ftext("UMN ID: ", prop.bold),ftext(ID[z], prop.normal)))
      body_add_par(doc,"")
      body_add_fpar(doc,fpar(ftext("Name: "  , prop.bold),ftext(NAME[z], prop.bold_text)))
      body_add_par(doc,"")
      body_add_fpar(doc,fpar(ftext("Exact Mass: ", prop.bold),ftext(as.character(MZ[z]), prop.normal)))
      body_add_par(doc,"")
      body_add_fpar(doc,fpar(ftext("FORMULA: ", prop.bold),ftext(FORMULA[z], prop.normal)))
      body_add_par(doc,"")
      body_add_fpar(doc,fpar(ftext("SMILES: ", prop.bold),ftext(SMILES[z], prop.normal)))
      body_add_par(doc,"")
      body_add_fpar(doc,fpar(ftext("InChI: ", prop.bold),ftext(INCHI[z], prop.normal)))
      body_add_par(doc,"")
      body_add_fpar(doc,fpar(ftext("InChIKey: ", prop.bold),ftext(INCHIKEY[z], prop.normal)))
      body_add_par(doc,"")
      body_add_fpar(doc,fpar(ftext("PubChem ID: ", prop.bold),ftext('TBD', prop.normal)))
      body_add_par(doc,"")
      body_add_fpar(doc,fpar(ftext("HMDB ID: ", prop.bold),ftext('TBD', prop.normal)))
      slip_in_column_break(doc, pos = 'after')
      
      body_add_img(doc,src = pl[grep('structure',pl)], width = 3, height = 3)
      body_end_section_columns(doc,widths = c(3,3), sep = F, space = 0.5) 
      body_end_section_continuous(doc)
      
      body_add(doc,fpar(ftext('Acquisition date: ',prop.bold),ftext('2019/12/13', prop.normal)))
      body_add(doc,fpar(ftext('Instrument: ',prop.bold),ftext('Thermo Orbitrap Fusion Tribrid', prop.normal)))
      body_add(doc,fpar(ftext('Instrument handlers: ',prop.bold),ftext('Peter Villalta, PhD and Jingshu Guo, PhD', prop.normal)))
      body_add(doc,fpar(ftext("Data processed by: ", prop.bold),ftext('Scott J Walmsley, PhD', prop.normal)))
      body_end_section_continuous(doc)
     
    
      ##############################
      body_add_par(doc,"")
      
      g = grep('CID_MS1_purity',pl)
      if(length(g) > 0){
        body_add_img(doc,src = pl[grep('CID_MS1_purity',pl)], width = 3, height = 1.5, style = 'centered')
      }
      
      g = grep('HCD_MS1_purity',pl)
      if(length(g) > 0){
        body_add_img(doc,src = pl[grep('HCD_MS1_purity',pl)], width = 3, height = 1.5, style = 'centered')
      }
      
      slip_in_column_break(doc, pos = 'after')
      
      
      g = grep('ticPlot',pl)
      if(length(g) > 0){
        body_add_img(doc,src = pl[grep('ticPlot',pl)], width = 3, height = 1.5, style = 'centered')
      }
      
      g = grep('ppmErr',pl)
      if(length(g) > 0){
        body_add_img(doc,src = pl[grep('ppmErr',pl)], width = 3, height = 1.5, style = 'centered')
      }
      
      
      body_end_section_columns(doc,widths = c(3,3), sep = F, space = 0.5) 
      #body_add_break(doc, pos = "after")
      
      #body_add_par(doc,"")
      body_end_section_portrait(doc, w = 8.5, h = 11)
      
      ################################
      ## HCD Spectra
      body_add(doc,fpar(ftext('HCD Spectra',prop.bold)))
      
      g = grep('HCD_[0-9]',pl)
      if(length(g) > 0){
        for(i in g){
          body_add_img(doc,src = pl[i], width = 3, height = 1.5, style = 'centered')
          
          
        }
      }
      
      body_end_section_columns(doc,widths = c(3,3), sep = F, space = 0.5) 
      body_end_section_portrait(doc, w = 8.5, h = 11)
      #body_add_break(doc, pos = "after")
      ################################
      ## CID Spectra
      body_add(doc,fpar(ftext('CID Spectra',prop.bold)))
      
      g = grep('CID_[0-9]',pl)
      if(length(g) > 0){
        for(i in g){
          if(i == 6){
              body_add_fpar(doc,fpar(ftext("", prop.normal),ftext(ID[z], prop.normal)))

          }
          body_add_img(doc,src = pl[i], width = 3, height = 1.5, style = 'centered')
          
          
        }
      }
      
      body_end_section_columns(doc,widths = c(3,3), sep = F, space = 0.5)
      #body_add_break(doc, pos = "after")
      
      print(doc, target = paste(ID[z],".docx", sep = ''))
      doc = NULL
    }
  }
}


write.csv(file = '20210401_ms2_DNA_Adduct_annotated_productIons.csv',out)


