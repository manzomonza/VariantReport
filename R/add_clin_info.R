# ##
# files = list.files(path = "/Users/manzo/USB/USB_Diagnostics/genie/testfiles/Y537_B2023.6661_OcaV3_output/annotation_output", full.names = TRUE)
#
# annotation_fp = VariantAnnotationModules::annotation_filepaths('/Users/manzo/USB/USB_Diagnostics/genie/testfiles/Y537_B2023.6661_OcaV3_output')
# #lapply(annotation_fp[-7], readr::read_tsv)

#' read in annotation FP cancerHotspot
#'
#' @param cancerHotspot_path
#'
#' @return
#' @export
#'
#' @examples
read_cancerHotspot = function(cancerHotspot_path){
  if(file.exists(cancerHotspot_path)){
    cancerHotspot = readr::read_tsv(cancerHotspot_path)
    cancerHotspot = dplyr::select(cancerHotspot, rowid, gene, mutation_position_count, mutation_count)
    cancerHotspot = dplyr::rename(cancerHotspot,
                                  cancerHotspot_position_count = mutation_position_count,
                                  cancerHotspot_variant_count = mutation_count)
    return(cancerHotspot)
  }
}


#' read in annotation FP  TSGinfo
#'
#' @param tsg_path
#'
#' @return
#' @export
#'
#' @examples
read_TSG = function(tsg_path){
  if(file.exists(tsg_path)){
    tsg = readr::read_tsv(tsg_path)
    tsg = dplyr::select(tsg, rowid, gene, tsgInfo)
    return(tsg)
  }
}

#' read in annotation FP COSMIC info
#'
#' @param cosmic_path
#'
#' @return
#' @export
#'
#' @examples
read_COSMIC = function(cosmic_path){
  if(file.exists(cosmic_path)){
    cosmic = readr::read_tsv(cosmic_path)
    cosmic = dplyr::select(cosmic, rowid, gene, contains("COSMIC"))
    return(cosmic)
  }
}

#' read in annotation FP gnomad Info
#'
#' @param gnomad_path
#'
#' @return
#' @export
#'
#' @examples
read_Gnomad = function(gnomad_path){
  if(file.exists(gnomad_path)){
    cosmic = readr::read_tsv(gnomad_path)
    cosmic = dplyr::select(cosmic, rowid, gene, gnomad_MAF)
    return(cosmic)
  }
}

#' read in annotation FP Horak scores
#'
#' @param horak_path
#'
#' @return
#' @export
#'
#' @examples
read_Horak = function(horak_path){
  if(file.exists(horak_path)){
    horak = readr::read_tsv(horak_path)
    horak = dplyr::select(horak, rowid, gene, Horak_score, classification)
    horak = dplyr::rename(horak, Horak_classification = classification)
    return(horak)
  }
}

#' Add Clinical interpretation columns
#'
#' @param snv
#'
#' @return
#' @export
#'
#' @examples
add_interpretation_columns = function(snv){
    snv = addLinks_oncokb(snv)
    snv = addLinks_clinvar(snv, clinvar_hits)
    ## read in files
    horak = read_Horak(annotation_fp$Horak)
    cancerHotspot = read_cancerHotspot(annotation_fp$cancerHotspot)
    cosmic = read_COSMIC(annotation_fp$COSMIC)
    tsg = read_TSG(annotation_fp$TSG)
    gnomad = read_Gnomad(annotation_fp$gnomad)

    ## LEFT JOINS
    snv = dplyr::left_join(snv, horak)
    snv = dplyr::left_join(snv, cancerHotspot)
    snv = dplyr::left_join(snv, cosmic)
    snv = dplyr::left_join(snv, tsg)
    snv = dplyr::left_join(snv, gnomad)
    # ## SELECT
    snv = dplyr::select(snv, rowid, gene, coding, protein
                        contains("Horak"),
                        contains("tsg"),
                        contains("link"),
                        contains("Hotspot"),
                        contains("COSMIC"),
                        contains("MAF"))
    snv = VariantAnnotationModules::amino_acid_code_3_to_1(snv)
    return(snv)
  }
