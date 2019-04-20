#' To convert data to GraphPad Prism data, after command Oncomine_SurvAnalysis.Data
#'
#' @param data data
#'
#' @return a data frame
#' @export
#'
#' @examples Oncomine_ToGraphPad(data)
Oncomine_ToGraphPad<-function(data){
  data$high=""
  for (i in 1:nrow(data)) {
    if (data$Expression[i]=="High"){
      data$high[i]=data$Outcome[i]
    }else{
      data$high[i]=""
    }
  }
  data$low=""
  for (i in 1:nrow(data)) {
    if (data$Expression[i]=="Low"){
      data$low[i]=data$Outcome[i]
    }else{
      data$low[i]=""
    }
  }
  dataf=data[order(data$Expression),]
  colnames(dataf)[grep("high",colnames(dataf))]=tmcn::toUTF8("\u9AD8\u8868\u8FBE")
  colnames(dataf)[grep("low",colnames(dataf))]=tmcn::toUTF8("\u4F4E\u8868\u8FBE")
  return(dataf)
}
