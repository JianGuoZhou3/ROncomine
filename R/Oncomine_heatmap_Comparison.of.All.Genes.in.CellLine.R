#' To get data from All Genes in CellLine
#'
#' @param filename filename can be one or more, or missing if only data files in your working directory.
#'
#' @return a dataframe
#' @export
#'
#' @examples Oncomine_heatmap_Comparison.of.All.Genes.in.CellLine()
Oncomine_heatmap_Comparison.of.All.Genes.in.CellLine<-function(filename){
  if (missing(filename)){
    filename=list.files()
  }
  if (length(filename)==1){
    finaldata=Oncomine_heatmap_Comparison.of.Concept_onepage_notempty(filename)
  }else if(length(filename)>1){
    finaldata=Oncomine_heatmap_Comparison.of.Concept_manypages_notempty(filename)
  }
  return(finaldata)
}
