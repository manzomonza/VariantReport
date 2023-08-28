#' Generate list of filepaths for reporting folder
#'
#' @param output_path
#'
#' @return
#' @export
#'
#' @examples
reporting_filepaths = function(output_path){
  report_dir = paste0(output_path, "/reporting_output")
  if(!dir.exists(report_dir)){
    dir.create(report_dir)
  }
  filepaths = list(translations_clinvar = paste0(report_dir,'/reporting_translations.tsv'),
                   )
  return(filepaths)
}
