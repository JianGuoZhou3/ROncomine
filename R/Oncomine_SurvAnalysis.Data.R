#' To do get data for survival analysis from Oncomine
#'
#' @param data data name
#' @param Time The complete variable name of following time
#' @param Outcome The complete variable name of outcome
#' @param OutcomeCode1 The primary ending of outcome, which will be coded as 1. String is needed
#' @param x Complete Expression Value variable name. String is needed. Please use original Expression Value, this function will divide it into 1,as high expression and 0, as low expression.
#'
#' @description Please use original Expression Value, this function will divide it into 1,as high expression and 0, as low expression.
#' @return a Dataframe
#' @export
#' @author Jing Zhang
#' @examples Oncomine_SurvAnalysis.Data(data=data,Time="Time",Outcome="Outcome",OutcomeCode1="Dead",x="Expression Value")
Oncomine_SurvAnalysis.Data<-function(
  data,
  Time,
  Outcome,
  OutcomeCode1,
  x
){
  #tran to matrix
  colnameD=c("Sample Name",Time,Outcome,x)
  data=na.omit(data)
  data1=as.matrix(data[,colnameD])
  #missing value
  data1=ifelse(data1=="No value",NA,data1)
  data1=ifelse(data1=="NA",NA,data1)
  data2=na.omit(data1)
  #change outcomecode to 1
  data2[,Outcome]=ifelse(data2[,Outcome]==OutcomeCode1,1,0)
  #tran expression to 1 and 0
  data2[,x]=ifelse(grepl("-",data2[,x]),"Low","High")
  dataf=data.frame(na.omit(data2))
  colnames(dataf)=c("Sample Name","Time","Outcome","Expression")
  dataf[,"Time"]=as.numeric(as.character(dataf[,"Time"]))
  dataf[,"Outcome"]=as.numeric(as.character(dataf[,"Outcome"]))
  return(dataf)
}
