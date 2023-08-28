addLinks_oncokb = function(snv){
  snv$oncokb = paste0("https://www.oncokb.org/gene/", snv$gene,"/", gsub("p\\.",'', snv$protein))
  snv$link_oncokb <- paste0("<a href='", snv$oncokb, "' target='_blank'>", snv$protein, "</a>")
  return(snv)
}


addLinks_clinvar = function(snv, clinvar_hits){
  snv_clinvar = dplyr::left_join(snv, dplyr::select(clinvar_hits, -protein), by = c("gene",'rowid', 'coding'))
  snv_clinvar$link_clinvar = NA
  for (i in 1:nrow(snv_clinvar)){
    if(!is.na(snv_clinvar$ClinVar_VariationID[i])){
      snv_clinvar$link_clinvar[i] = paste0("https://www.ncbi.nlm.nih.gov/clinvar/variation/",
                                           snv_clinvar$ClinVar_VariationID[i])
      snv_clinvar$link_clinvar[i] <- paste0("<a href='", snv_clinvar$link_clinvar[i], "' target='_blank'>", snv_clinvar$ClinVar_Significance[i], "</a>")
    }
  }
  return(snv_clinvar)
}
