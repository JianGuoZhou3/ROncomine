#' To combine two datasets following command Oncomine_Survival.Data
#'
#' @param data1 data1
#' @param data2 data2
#'
#' @return a data frame
#' @export
#'
#' @examples Oncomine_SurvAnalysis.DataCombine(data1,data2)
Oncomine_SurvAnalysis.DataCombine <- function(data1,data2) {
  data2.1=data2[,c("Sample Name","Expression")]
  colnames(data2.1)[2]="gene2"
  data3=merge(data1,data2.1,"Sample Name")
  data3$Gene1Gene2=paste0(data3$Expression,data3$gene2)
  data4=data3[,c("Sample Name", "Time", "Outcome", "Gene1Gene2")]
  colnames(data4)[4]="Expression"
  return(data4)
}