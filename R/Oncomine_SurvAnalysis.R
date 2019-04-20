#' To plot Survival Curve after command Oncomine_SurvAnalysis.Data
#'
#' @param data data name
#' @param direction the curve direction,"low" or "l" means pct methord as defaulted, "high" or "h" means cumhaz methord, "event" or "e" means cumhaz methord in command ggsurvplot
#'
#' @description To plot Survival Curve
#' @export
#' @author Jing Zhang
#' @examples Oncomine_SurvAnalysis(data)
Oncomine_SurvAnalysis<-function(data,direction="low"){
    fit=survival::survfit(survival::Surv(Time,Outcome)~Expression,data=data)
    library(survminer)
    if (any((direction=="low"),(direction=="l"))){
      fund="pct"
    }else if (any((direction=="high"),(direction=="h"))){
      fund="cumhaz"
    }else if (any((direction=="event"),(direction=="e"))){
      fund="event"
    }
    p=list(fit,data=data,
           legend.title="Expression",pval = TRUE,pval.method = TRUE,
           risk.table = TRUE,tables.height = 0.2,
                          tables.theme = theme_classic(),fun=fund)
    ggsurv <- do.call(ggsurvplot, p)
    return(ggsurv)
}
