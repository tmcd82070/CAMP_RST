
backEnhFit <- function(tmp.df,df,initialVars,possibleVars,m.i,eff.ind.inside,max.df.spline,eff.inside.dates,trap,model.info,fits,plot.file,option,bsplBegDt,bsplEndDt){
  

  
  
  
  eff.min.spline.samp.size <- get("eff.min.spline.samp.size", pos=.GlobalEnv)
  
  #   ---- Fit the full model with all possible covariates that made it.  Object fit.tmp.bs is from the last kept run.
  method <- "likeType3SS"    # "pVal"    
  distn <- "binomial"   # "binomial"
  pCutOff <- 0.10
  covarString <- tmp.df$covar[1]
  
  #   ---- Clean up the duplicate variable situation. 
  
  #   ---- If we have water temperature from both EnvCovDB and CAMP, keep EnvCovDB.
  one <- sum(initialVars %in% "temp_c")
  two <- as.numeric(sum(initialVars %in% c("waterTemp_C")) >= 1)
  interimVars1 <- initialVars
  interimVars1Num <- c(3,as.numeric(possibleVars %in% interimVars1))
  
  #   ---- If both conditions are true, we have both vars.  Get rid of CAMP.  
  if( (one == two) & (one == 1) ){
    interimVars1 <- interimVars1[!(interimVars1 %in% c("waterTemp_C"))]
    interimVars1Num <- c(3,as.numeric(possibleVars %in% interimVars1))
    
    #   ---- Have to now update the "+" situation.  Removed var could be leading, middle, or trailing. 
    covarString <- gsub(paste0(" + ","waterTemp_C"),"",covarString,fixed=TRUE)  # middle or trailing -- remove leading " + "
    covarString <- gsub("waterTemp_C","",covarString,fixed=TRUE)                # leading -- remove var[i] alone
    cat("I have removed variable waterTemp_C because temp_c is already present.\n\n")
    
    covarString <- gsub(paste0(" + ","waterTemp_F"),"",covarString,fixed=TRUE)  # middle or trailing -- remove leading " + "
    covarString <- gsub("waterTemp_F","",covarString,fixed=TRUE)                # leading -- remove var[i] alone
    cat("I have removed variable waterTemp_F because temp_c is already present.\n\n")
  }
  
  
  #   ---- If we have water flow from both EnvCovDB and CAMP, keep EnvCovDB.
  one <- sum(initialVars %in% "flow_cfs")
  two <- as.numeric(sum(initialVars %in% c("discharge_cfs")) >= 1)
  interimVars2 <- initialVars
  interimVars2Num <- c(4,as.numeric(possibleVars %in% interimVars2))
  
  #   ---- If both conditions are true, we have both vars.  Get rid of CAMP.  
  if( (one == two) & (one == 1) ){
    interimVars2 <- interimVars2[!(interimVars2 %in% c("discharge_cfs"))]
    interimVars2Num <- c(4,as.numeric(possibleVars %in% interimVars2))
    
    #   ---- Have to now update the "+" situation.  Removed var could be leading, middle, or trailing. 
    covarString <- gsub(paste0(" + ","discharge_cfs"),"",covarString,fixed=TRUE)  # middle or trailing -- remove leading " + "
    covarString <- gsub("discharge_cfs","",covarString,fixed=TRUE)                # leading -- remove var[i] alone
    cat("I have removed variable discharge_cfs because flow_cfs is already present.\n\n")
  }
  
  #   ---- Save a record of the variables we care about for plotting below.  
  covarStringPlot <- covarString
  
  #   ---- If we only ended up with an intercept-only model, skip straight to the end.
  if( !(m.i < eff.min.spline.samp.size) | (sum(tmp.df$nCaught) == 0) ){
    
    
    m0 <- fitSpline(covarString,df,eff.ind.inside,tmp.df,distn,max.df.spline,eff.inside.dates)
    fit0 <- m0$fit
    tmp.bs <- m0$bspl[!is.na(df$efficiency[eff.ind.inside]),]
    disp <- m0$disp
    new.bspl <- m0$bspl
    
    #   ---- Send results to console.  Note that this applies the value of disp in the summary.  
    #   ---- In other words, this is 'quasibinomial' by definition, but in a backwards way.  
    cat("\n\n\nTemporal & initial covariate efficiency model for trap: ", trap, "\n")
    print(summary(m0$fit, disp=sum(residuals(m0$fit, type="pearson")^2)/m0$fit$df.residual))
    
    #   ---- Plot the spline.  
    png(filename=paste0(plot.file,"-EnhEff-O",option,"-",trap,"-f",1,"m0.png"),width=7,height=7,units="in",res=600)
    model.info <- plot.bs.spline(m0$bspl,m0$fit,bsplBegDt,bsplEndDt,tmp.df,option,df$batchDate2[eff.ind.inside])
    dev.off()  
    
    
    model.i <- 2
    repeat{
      
      #   ---- Identify the worst-performing variable.  I use the highest p-value.  
      if(method == "pVal"){
        theHighestp <- max(coefficients(summary(fit0))[row.names(coefficients(summary(fit0))) != "(Intercept)" & !(sapply(row.names(coefficients(summary(fit0))),function(x) grepl("fit.tmp",x,fixed=TRUE))),4])
        varToTestForDeletion <- row.names(coefficients(summary(fit0)))[coefficients(summary(fit0))[,4] == theHighestp]
        
      } else if(method == "likeType3SS"){
        
        #   ---- We need to calculate a X2 test for each variable, one-by-one.  
        thePossibleCovarString <- as.data.frame(coefficients(summary(fit0)))[row.names(coefficients(summary(fit0))) != "(Intercept)" & !(sapply(row.names(coefficients(summary(fit0))),function(x) grepl("tmp.bs",x,fixed=TRUE))),]
        temporalSplinePresent <- ifelse(sum(grepl("tmp.bs",row.names(coefficients(summary(fit0))))) > 0,1,0) 
        
        #   ---- Loop through, creating a formula string with the ith row's coefficient deleted. 
        checkThisMany <- nrow(thePossibleCovarString)
        holdEm <- vector("list",checkThisMany)
        df.anova <- NULL
        for(v in 1:checkThisMany){
          
          if( checkThisMany == 1 ){
            vthCovarString <- paste0("nCaught / nReleased ~ tmp.bs")
            if(temporalSplinePresent == 0){
              vthCovarString <- paste0("nCaught / nReleased ~ 1")
            }
          } else {
            vthCovarString <- paste0("nCaught / nReleased ~ tmp.bs + ",paste0(rownames(thePossibleCovarString)[-v],collapse=" + "))
            if(temporalSplinePresent == 0){   # Can this ever happen?
              vthCovarString <- paste0("nCaught / nReleased ~ 1 + ",paste0(rownames(thePossibleCovarString)[-v],collapse=" + "))
            }
          }
          holdEm[[v]] <- glm( as.formula(vthCovarString), family=distn, data=tmp.df, weights=tmp.df$nReleased )
          
          #   ---- Assuming all variables to be tested are of 1 degree of freedom, could just find the set that results in the lowest
          #   ---- residual deviance.  But, we should make it smarter for the eventual factors we could have.  
          tmp.anova <- as.data.frame(anova(holdEm[[v]]))
          if(nrow(tmp.anova) == 1){
            rownames(tmp.anova) <- rownames(thePossibleCovarString)[v]
          }
          tmp.anova$testingCovarDF <- anova(fit0)[row.names(anova(fit0)) == rownames(thePossibleCovarString)[v],]$Df
          tmp.anova$model <- v
          tmp.anova$testingCovar <- rownames(thePossibleCovarString)[v]
          df.anova <- rbind(df.anova,tmp.anova)
        }
        df.anova$biggerModelDev <- anova(fit0)$`Resid. Dev`[checkThisMany + 1 + temporalSplinePresent]   # + 1 Int + 1 spline
        df.chi <- aggregate(df.anova,list(df.anova$testingCovar),function(x) tail(x,1))
        df.chi$Group.1 <- df.chi$`Resid. Df` <- NULL
        df.chi$ChiSq <- df.chi$`Resid. Dev` - df.chi$biggerModelDev
        #df.chi$pVal <- pchisq(df.chi$ChiSq/summary(fit0)$dispersion,df.chi$testingCovarDF,lower.tail=FALSE)
        df.chi$pVal <- pchisq(df.chi$ChiSq/disp,df.chi$testingCovarDF,lower.tail=FALSE)
        
        #   ---- Now, identify the one with the highest Chi-square p-value.  
        theHighestp <- max(df.chi$pVal) 
        varToTestForDeletion <- df.chi[df.chi$pVal == theHighestp,]$testingCovar
        
      }
      
      if(theHighestp > pCutOff & covarString != ""){
        
        cat(paste0("Variable ",varToTestForDeletion," has a p-value of ",round(theHighestp,4),", which is greater than ",pCutOff,".  Removing.\n\n"))
        
        #   ---- Have to now update the "+" situation.  Removed var could be leading, middle, or trailing. If this is the last covariate, 
        #   ---- we have nothing left.  
        if(checkThisMany == 1){
          covarString <- ""
        } else {
          covarString <- gsub(paste0(" + ",varToTestForDeletion),"",covarString,fixed=TRUE)  # middle or trailing -- remove leading " + "
          covarString <- gsub(paste0(varToTestForDeletion," + "),"",covarString,fixed=TRUE)  # leading -- remove var[i] alone and next " + "
        }
        
        #   ---- Fit updated model.  
        if(covarString == ""){  # We could NOW have a blank covarString.  Note the lack of the '+' below. 
          if(temporalSplinePresent == 1){
            fit1 <- glm( as.formula(paste0("nCaught / nReleased ~ tmp.bs ",covarString)), family=distn, data=tmp.df, weights=tmp.df$nReleased ) 
          } else if(temporalSplinePresent == 0){
            fit1 <- glm( as.formula(paste0("nCaught / nReleased ~ ",covarString)), family=distn, data=tmp.df, weights=tmp.df$nReleased ) 
          }
        } else {
          if(temporalSplinePresent == 1){   # Assumes this was calculated above in evaluation of covariate.
            fit1 <- glm( as.formula(paste0("nCaught / nReleased ~ tmp.bs + ",covarString)), family=distn, data=tmp.df, weights=tmp.df$nReleased ) 
          } else if(temporalSplinePresent == 0){
            fit1 <- glm( as.formula(paste0("nCaught / nReleased ~ ",covarString)), family=distn, data=tmp.df, weights=tmp.df$nReleased ) 
          }
        }
        
        #   ---- Output visual impact of variable deletion.  Object new.bspl possibly has a bigger spline from below. 
        png(filename=paste0(plot.file,"-EnhEff-O",option,"-",trap,"-f",model.i,"mc.png"),width=7,height=7,units="in",res=600)
        model.info <- plot.bs.spline(new.bspl,fit1,bsplBegDt,bsplEndDt,tmp.df,option,df$batchDate2[eff.ind.inside])
        dev.off()
        
        #   ---- We've now removed a covariate.  Reconsider the temporal spline.
        fit0 <- fit1    
        
        if(covarString != ""){
          
          
          m2 <- fitSpline(covarString,df,eff.ind.inside,tmp.df,distn,max.df.spline,eff.inside.dates)
          fit2 <- m2$fit
          tmp.bs <- m2$bspl[!is.na(df$efficiency[eff.ind.inside]),]
          disp <- m2$disp
          
          #   ---- Send results to console.  Note that this applies the value of disp in the summary.  
          #   ---- In other words, this is 'quasibinomial' by definition, but in a backwards way.  
          cat("\nFinal Efficiency model for trap: ", trap, "\n")
          print(summary(m2$fit, disp=sum(residuals(m2$fit, type="pearson")^2)/m2$fit$df.residual))
          
          #   ---- Plot the spline.  
          png(filename=paste0(plot.file,"-EnhEff-O",option,"-",trap,"-f",model.i,"mt.png"),width=7,height=7,units="in",res=600)
          model.info <- plot.bs.spline(m2$bspl,m2$fit,bsplBegDt,bsplEndDt,tmp.df,option,df$batchDate2[eff.ind.inside])
          dev.off()  
          
          #   ---- Report.  fit1 is from the covariate fit, while fit2 is from the subsequent temporal spline fit.
          before.bs.Len <- length(coef(fit1)[substr(names(coef(fit1)),1,6) == "tmp.bs"])
          after.bs.Len <- length(coef(fit2)[substr(names(coef(fit2)),1,6) == "tmp.bs"])
          cat(paste0("After last variable removable, I went from a ",before.bs.Len," spline to a ",after.bs.Len," spline.\n\n"))
          if(after.bs.Len != before.bs.Len){
            new.bspl <- m2$bspl
          } 
          fit0 <- fit2
        } else if(covarString == ""){
          
          #   ---- If we are here, we are back to the temporal spline-only model.  No covariate works.  
          covarString <- "1"
          m0 <- fitSpline(covarString,df,eff.ind.inside,tmp.df,dist="binomial",max.df.spline,eff.inside.dates)
          
          cat("\nTemporal-only (after consideration of covariates) efficiency model for trap: ", trap, "\n")
          print(summary(m0$fit, disp=sum(residuals(m0$fit, type="pearson")^2)/m0$fit$df.residual)  )
          
          #   ---- Plot the spline.  
          png(filename=paste0(plot.file,"-EnhEff-O",option,"-",trap,"-f",model.i + 1,"mc.png"),width=7,height=7,units="in",res=600)
          model.info <- plot.bs.spline(m0$bspl,m0$fit,bsplBegDt,bsplEndDt,tmp.df,option,df$batchDate2[eff.ind.inside])
          dev.off() 
          break
        }
        
      } else {
        cat(paste0("Variable ",varToTestForDeletion," has a p-value of ",round(theHighestp,8),", which is less than ",pCutOff,".  Keeping and exiting.\n\n"))
        cat("Final model identified.\n")
        break
      }
      model.i <- model.i + 1
      varToTestForDeletion <- NULL
      fits[[trap]] <- fit0
    }
  } else {
    #   ---- Intercept-only model.  Just report what we already have.  
    fit0 <- fits[[trap]]
  } 
  
  cat(paste0("The final fit with covariates and spline together is:  \n"))
  print(summary(fit0, disp=sum(residuals(fit0, type="pearson")^2)/fit0$df.residual))
  cat(paste0("\n\n\n"))
  
  fit <- fit0
  
  return(list(fit=fit,model.info=model.info,fits=fits,covarStringPlot=covarStringPlot,interimVars1Num=interimVars1Num,interimVars2Num=interimVars2Num))

}