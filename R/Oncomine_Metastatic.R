#' To clear Oncomine data for Metastatic Event Status
#'
#' @param filenames The filenames that your data stored in. if ONLY data files in your working directory this object can be omitted.
#' @param NoValue a logistic value
#' @return a dataframe
#' @export
#'
#' @examples Oncomine_Metastatic()
Oncomine_Metastatic<-function(filenames,NoValue=TRUE){
  if (missing(filenames)){
    filenames=list.files()
  }
  txtcheck=filenames[grepl(pattern = ".txt",x = filenames)==FALSE]
  docxcheck=txtcheck[grepl(pattern = ".docx",x = txtcheck)==FALSE]
  doccheck=docxcheck[grepl(pattern = ".doc",x = docxcheck)==FALSE]
  if (length(doccheck)==0){
    survival.i=data.frame()
    for ( i in 1:length(filenames)){
      ONAME=sub(pattern = "\\..*",replacement = "",x = filenames[i])
      if (i==1){
        Osurvival.1=Oncomine_bar(filenames[i])
        if (NoValue==FALSE){
          survival.i=Osurvival.1[!(Osurvival.1$`Legend Value`=="No value"),]
        }else if (NoValue==TRUE){
          survival.i=Osurvival.1
        }
        colnames(survival.i)[grep(pattern = "Legend Value",x = colnames(survival.i))]=ONAME
      }else if (i>1){
        Osurvival.i=Oncomine_bar(filenames[i])
        if (NoValue==FALSE){
          Osurvival.i2=Osurvival.i[!(Osurvival.i$`Legend Value`=="No value"),]
        }else if(NoValue==TRUE){
          Osurvival.i2=Osurvival.i
        }
        Osurvival.i4=Osurvival.i2[,c(grep(pattern = "Sample Name",x = colnames(Osurvival.i2)),
                                     grep(pattern = "Legend Value",x = colnames(Osurvival.i2)))]
        colnames(Osurvival.i4)[grep("Legend Value",colnames(Osurvival.i4))]=ONAME
        survival.i=merge(survival.i,Osurvival.i4,"Sample Name")
      }
    }
    return(survival.i)
  }else{
    doccheck1=paste(doccheck,collapse="\n")
    warning("The following files shoul be removed","\n",doccheck1)
    stop("Please make sure ONLY data file end with .txt .doc .docx including in the working directory. Or give object to this function")
  }
}
