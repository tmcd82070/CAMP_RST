#' @export
#' 
#' @title checkMissingCovars
#'   
#' @description Identify potential covariates that fail to have data for at
#'   least 90% of available efficiency trials.
#'   
#' @param tmp.df The reduced data frame originating from \code{df} containing 
#'   only efficiency-trial dates; i.e., those with non-\code{NA}
#'   \code{nReleased} values.
#' 
#' @param m.i An integer containing the number of rows in data frame 
#'   \code{tmp.df}.
#' 
#' @param df The data frame for a specific \code{TrapPositionID} containing 
#'   efficiency-trial information and covariates, if available, at the time of 
#'   fitting enhanced efficiency trials in \code{eff_model.r} (or 
#'   \code{F.efficiency.model }).
#' 
#' @param trap A trap for which efficiency data are available. 
#' 
#' @param plot.file The name of the file prefix under which output is to be 
#'   saved.  Set to \code{NA} to plot to the plot window.
#'   
#' @details Function \code{checkMissingCovars}, surprisingly, checks for
#'   covariates with missing data. It outputs a \code{csv} or data rows with
#'   missing data.  Generally, the function applies a 90\% rule, meaning that
#'   90\% of the efficency trials in \code{tmp.df} must have covariate data
#'   available for ultimate consideration in enhanced efficiency models.  
#' 
#' @return A list containing several objects.  
#' 
#' \describe{
#'   \item{tmp.df}{A copy of the \code{tmp.df} initially provided to the function with rows (efficiency trials) missing covariate data possibly removed.}
#'   \item{df}{A copy of the \code{df} initially provided to the function.}
#'   \item{m.i}{The number of efficiency trials with full rows of data.  Necessarily less than or equal to the \code{m.i} provided to the function.}
#'   \item{dataDeficient}{The dataframe identifying the e-trial rows removed from \code{tmp.df}.  Output as a \code{csv}.}
#'   \item{atLeast}{The threshold indicating the number of e-trial rows that must be non-missing for covariate inclusion.  Equal to \eqn{floor(0.90*m.i) + 1}.}
#' 
#' @examples
#' \dontrun{
#' ans <- checkMissingCovars(tmp.df,m.i,df,trap,plot.file)
#' }
checkMissingCovars <- function(tmp.df,m.i,df,trap,plot.file){
  
  # tmp.df <- tmp.df
  # m.i <- m.i
  # df <- df
  
  cat(paste0("I'm starting with ",m.i," data rows.  Let me see if there are any I need to remove.\n"))
  
  #   ---- With the inclusion of all CAMP covariates, the probability increases by a lot that we don't have all 
  #   ---- the values over all time.  Chuck those that don't have at least ... 90% of the data rows, given a 
  #   ---- trap.  Note that this considers NA WITHIN a column.  If we delete, we have to update tmp.df$covar.
  # write.csv(tmp.df,"L:/PSMFC_CampRST/ThePlatform/CAMP_RST20161212-campR1.0.0/Outputs/Mokelumne River--Golf RST Main Site/2006/Enhanced_Eff_Get_Betas/tmp.df.csv",row.names=FALSE)
  atLeast <- floor(0.90*m.i) + 1
  vars <- c("bdMeanNightProp","bdMeanMoonProp","bdMeanForkLength",colnames(tmp.df[,(which(colnames(tmp.df) == "covar") + 1):ncol(tmp.df)]))
  vars <- vars[!(vars %in% c("fishDay","batchDate2"))]
  for(i in 1:length(vars)){
    if(!(sum(!is.na(tmp.df[,vars[i]])) >= atLeast)){
      cat(paste0("Trap ",trap," variable ",vars[i]," has only ",sum(!is.na(tmp.df[,vars[i]]))," points but needs ",atLeast," for inclusion.  Deleting.\n"))
      tmp.df[,vars[i]] <- NULL
      
      #   ---- Have to now update the "+" situation.  Removed variable could be leading, in the middle, or 
      #   ---- trailing.  So, consider all these.  
      tmp.df$covar <- gsub(paste0(" + ",vars[i]),"",tmp.df$covar,fixed=TRUE)  # middle or trailing -- remove leading " + "
      tmp.df$covar <- gsub(vars[i],"",tmp.df$covar,fixed=TRUE)                # leading -- remove var[i] alone
    } else {
      cat(paste0("Trap ",trap," variable ",vars[i]," has ",sum(!is.na(tmp.df[,vars[i]]))," points and only need ",atLeast," for inclusion.  Keeping.\n"))
    }
  }
  
  #   ---- See if we have any missing covars --- assumes all other values are not NA.  Note that this considers 
  #   ---- NA OVER columns.  
  tmp.df$allCovars <- apply(tmp.df,1,function(x) as.numeric(!(sum(is.na(x)) > 0)))
  
  #   ---- Could be that we have efficiency trials performed at times for which we have no covariate information.
  #   ---- Delete out these trials in favor of data rows with all data present -- for now. Output when we do this.
  #   ---- Also identify the trials we delete so that the basis matrix gets the right number of rows (valid trials).
  #   ---- I do this via NA in efficiency, since that is the criterion used in fitSpline.R.  
  dataDeficient <- tmp.df[tmp.df$allCovars == 0,]
  cat( paste0("Trap ",trap," has ",nrow(dataDeficient)," rows of data-covariate deficient, out of ",nrow(tmp.df)," originally present.\n") )
  write.csv(dataDeficient,paste0(plot.file,"-",trap,"dataDeficient.csv"),row.names=FALSE)
  if(length(tmp.df[tmp.df$allCovars == 0,]$batchDate) > 0){
    df[df$batchDate %in% tmp.df[tmp.df$allCovars == 0,]$batchDate,]$efficiency <- NA
  }
  tmp.df <- tmp.df[tmp.df$allCovars == 1,]
  tmp.df$allCovars <- NULL     # This has served its purpose.  
  
  cat(paste0("I finished with ",nrow(tmp.df)," data rows.  So, I removed ",m.i - nrow(tmp.df),".\n"))
  
  return(list(tmp.df=tmp.df,df=df,m.i=nrow(tmp.df),dataDeficient=dataDeficient,atLeast=atLeast))
}  