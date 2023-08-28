
#' Add columns containing forward reverse strand coverage info
#'
#' @param snv
#'
#' @return
#' @export
#'
#' @examples
add_strand_ratios = function(snv){
  snv$log2_read_ratio_Alt = log2((snv$FSAF+1)/(snv$FSAR+1))
  snv$log2_read_ratio_Ref = log2((snv$FSRF+1)/(snv$FSRR+1))
  snv$log2_read_ratio_Alt = round(  snv$log2_read_ratio_Alt, digits = 2)
  snv$log2_read_ratio_Ref = round(  snv$log2_read_ratio_Alt, digits = 2)
  return(snv)
}



#' Select few columns for variant inspection
#'
#' @param snv
#'
#' @return
#' @export
#'
#' @examples
display_variants = function(snv){
  snv = dplyr::select(snv, rowid, gene, transcript, coding, protein, AF)
  return(snv)
}


#' Select few columns for metrics inspection
#'
#' @param snv
#'
#' @return
#' @export
#'
#' @examples
display_std_metrics = function(snv){
  snv = dplyr::select(snv, rowid, gene, protein, AF, QUAL, totalDepth )
  return(snv)
}
