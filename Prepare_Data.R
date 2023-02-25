# Predicting complications in paediatric PH paper
# Code to load up and organise the data


# Libraries

    cat("\n Loading libraries...")
    invisible(lapply(c("readxl", "JointAI", "RColorBrewer", "ggplot2", "circlize", "rms", "viridis", "partykit",
    "stringr", "pROC", "yardstick", "ROCR", "OptimalCutpoints", "mixOmics", "lme4", "scales", "funModeling", "plsRglm", "plotly"),
    function(x) suppressMessages(require(x, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))))
    
    

# Functions
    
    cat("\n Defining functions...")
    
    
    quiet <- function(x) { 
      sink(tempfile()) 
      on.exit(sink()) 
      invisible(force(x)) 
    } 
    
    convert.likaert.text<- function(a,name)
          {
            n<- which(colnames(a)==name)
            new.name<- paste(name,"No",sep="")
            new.col<- unlist(a[,n])
            
            new.col[which(new.col=="1")]<- "Mod"
            new.col[which(new.col=="0")]<- "None"
            new.col[which(new.col=="Moderate")]<- "Mod"
            new.col[which(new.col=="Trace")]<- "Trivial"
            
            # Converting language to the Likert scale
                  new.col[which(new.col=="Slightly")]<- "Mild"
                  new.col[which(new.col=="Significantly")]<- "Mod"
                  new.col[which(new.col=="Very")]<- "Severe"
                  new.col[which(new.col=="++")]<- "Severe"
                  new.col[which(new.col=="Mild+")]<- "Mild/Mod"
                  new.col[which(new.col=="Mod+")]<- "Mod/Severe"
                  new.col[which(new.col=="Hypocontractile")]<- "Severe"
                  new.col[which(new.col=="Preserved")]<- "None"
                  new.col[which(new.col=="Reasonable")]<- "None"
                  new.col[which(new.col=="Gross")]<- "Severe"
                  new.col[which(new.col=="Trace")]<- "Trivial"
                  
                  new.colNo<- matrix(match(new.col, c("None","Trivial","Mild","Mild/Mod","Mod","Mod/Severe","Severe")), ncol=1)
                  
            # Filter to check all the non-standard adjectival terms have been picked up
                  nas<- which(is.na(new.col)==TRUE)
                  nas2<- which(is.na(new.colNo)==TRUE)
                  missed.terms<- setdiff(nas2, nas)
                  if (length(missed.terms)!=0) {cat("\n Non-standard term used for",name,"in",d2$Demo_HRN[missed.terms])}
                  
                  
            a<- cbind(a, new.colNo)
            colnames(a)[ncol(a)]<- new.name
            
            return(a)
          }
    
    
    
  
    convert.yesno.numbers<- function(a,name)
          {
            n<- which(colnames(a)==name)
            new.name<- paste(name,"No",sep="")
            new.colNo<- matrix(match(a[,n], c("No","Yes")) - 1, ncol=1)
            a<- cbind(a, new.colNo)
            colnames(a)[ncol(a)]<- new.name
          }
            
      
    
    define.complications<- function(HRdrop, BPdrop, SFdrop, d2)
          {
            d2$Complication_HRcont<- as.numeric(d2$Anaes_Change_in_HR_percent < HRdrop)
            d2$Complication_BPcont<- as.numeric(d2$Anaes_Change_in_MBP_percent < BPdrop)
            d2$Complication_SFcont<- as.numeric(d2$Anaes_Change_in_SF_percent < SFdrop)
            
            complications<- d2[, c("Complication_HRcont","Complication_BPcont","Complication_SFcont", "Complication_RespCVOther",
                                   "Complication_ECG","Complication_Cardiacarrest")]
            
            escalations<- d2[, c("Complication_PeriopiNO", "Complication_HDUunplanned","Complication_CICUunplanned",
                                   "Complication_Postopintubated","Complication_Postop_respsupport", "Complication_Inotropes")]
            
            adverseevents<- cbind(complications, escalations)
            
            # Complications
                      fatal<- which(d2$Complication_DiedPeriOp == 1)
                      nonfatal<- which(is.na(d2$Complication_DiedPeriOp) == TRUE)
                      
                # Non-Fatal
                      complication.df.nonfatal<- cbind(complications[nonfatal,],
                                                        Complication_Patients = as.numeric(apply(complications[nonfatal,],1,sum, na.rm=TRUE)!=0), # How many patients had a complication?
                                                        Complication_Events = apply(complications[nonfatal,],1,sum, na.rm=TRUE)) # How many complications were there in total?
                      complication.summary.nonfatal<- data.frame(Type = gsub("Complication_","", colnames(complication.df.nonfatal)),
                                                                  Number = apply(complication.df.nonfatal,2,sum, na.rm=TRUE),
                                                                  Percent = round(100 * apply(complication.df.nonfatal,2,sum, na.rm=TRUE) / nrow(complication.df.nonfatal),1))
                      rownames(complication.summary.nonfatal)<- rep(1:nrow(complication.summary.nonfatal))
                      
                
                # Fatal
                      complication.df.fatal<- cbind(complications[fatal,],
                                                       Complication_Patients = as.numeric(apply(complications[fatal,],1,sum, na.rm=TRUE)!=0), # How many patients had a complication?
                                                       Complication_Events = apply(complications[fatal,],1,sum, na.rm=TRUE)) # How many complications were there in total?
                      complication.summary.fatal<- data.frame(Type = gsub("Complication_","", colnames(complication.df.fatal)),
                                                                 Number = apply(complication.df.fatal,2,sum, na.rm=TRUE),
                                                                 Percent = round(100 * apply(complication.df.fatal,2,sum, na.rm=TRUE) / nrow(complication.df.fatal),1))
                      rownames(complication.summary.fatal)<- rep(1:nrow(complication.summary.fatal))
                      
                      
                # All
                      complication.df<- cbind(complications,
                                              Complication_Patients = as.numeric(apply(complications,1,sum, na.rm=TRUE)!=0), # How many patients had a complication?
                                              Complication_Events = apply(complications,1,sum, na.rm=TRUE)) # How many complications were there in total?
                      complication.summary<- data.frame(Type = gsub("Complication_","", colnames(complication.df)),
                                                        Number = apply(complication.df,2,sum, na.rm=TRUE),
                                                        Percent = round(100 * apply(complication.df,2,sum, na.rm=TRUE) / nrow(d2),1))
                      rownames(complication.summary)<- rep(1:nrow(complication.summary))
                      
                                 
            # Escalations
                      escalation.df<- cbind(escalations,
                                            Escalation_Patients = as.numeric(apply(escalations,1,sum, na.rm=TRUE)!=0), # How many patients had a complication?
                                            Escalation_Events = apply(escalations,1,sum, na.rm=TRUE)) # How many complications were there in total?
                      
                      escalation.summary<- data.frame(Type = gsub("Complication_","", colnames(escalation.df)),
                                                      Number = apply(escalation.df,2,sum, na.rm=TRUE),
                                                      Percent = round(100 * apply(escalation.df,2,sum, na.rm=TRUE) / nrow(d2),1))
                      
                      rownames(escalation.summary)<- rep(1:nrow(escalation.summary))
                     
                       
            
            # Adverse Events
                
                      adverseevents.df<- cbind(adverseevents,
                                            AdverseEvents_Patients = as.numeric(apply(adverseevents,1,sum, na.rm=TRUE)!=0), # How many patients had a complication?
                                            AdverseEvents_Events = apply(adverseevents,1,sum, na.rm=TRUE)) # How many complications were there in total?
                      
                      adverseevents.summary<- data.frame(Type = gsub("Complication_","", colnames(adverseevents.df)),
                                                      Number = apply(adverseevents.df,2,sum, na.rm=TRUE),
                                                      Percent = round(100 * apply(adverseevents.df,2,sum, na.rm=TRUE) / nrow(d2),1))
                      
                      rownames(adverseevents.summary)<- rep(1:nrow(adverseevents.summary))
                      
                
                
            return (list(complications.all.nonfatal = complication.df.nonfatal,
                         complications.summary.nonfatal = complication.summary.nonfatal,
                         
                         complications.all.fatal = complication.df.fatal,
                         complications.summary.fatal = complication.summary.fatal,
                         
                         complications.all<- complication.df,
                         complications.summary<- complication.summary,
                         
                         escalations.all = escalation.df,
                         escalation.summary = escalation.summary,
                         
                         adverseevents.all = adverseevents.df,
                         adverseevents.summary = adverseevents.summary))
          }
    
    
    
    
    
    
    optimism.corrected.AUC<- function(x, y, no.reps)
          {
            x<- results.MV; y<- results.MVp
            seq.step1<- seq(1, nrow(x), no.reps)
            seq.step234<- setdiff(which(x[,1] == "-500"), seq.step1)
            
            results.500.step1<- x[seq.step1,]
            results.500.step234<- x[seq.step234,]
            
            # Find optimism adjusted 95% CIs
                OBrocmean<- round(sapply(unique(results.500.step234$Predictor), function(a) {quantile(as.numeric(results.500.step234[which(results.500.step234$Predictor == a),"OBroc"]), probs = c(0.025, 0.5, 0.975))}),3)
                OBroc_prmean<- round(sapply(unique(results.500.step234$Predictor), function(a) {quantile(as.numeric(results.500.step234[which(results.500.step234$Predictor == a),"OBroc_pr"]), probs = c(0.025, 0.5, 0.975))}),3)
                
                step1_auc<- round(sapply(unique(results.500.step1$Predictor), function(a) {mean(as.numeric(results.500.step1[which(results.500.step1$Predictor == a),]$AUCtrain))}),3)
                step1_aucpr<- round(sapply(unique(results.500.step1$Predictor), function(a) {mean(as.numeric(results.500.step1[which(results.500.step1$Predictor == a),]$AUCtrain_pr))}),3)
                
                names(step1_auc)<- names(step1_aucpr)<- unique(results.500.step1$Predictor)
                
                step5_auc<- rbind(upper95 = step1_auc - OBrocmean[1,], mean = step1_auc - OBrocmean[2,], lower95 = step1_auc - OBrocmean[3,])
                step5_aucpr<- rbind(upper95 = step1_aucpr - OBroc_prmean[1,], mean = step1_aucpr - OBroc_prmean[2,], lower95 = step1_aucpr - OBroc_prmean[3,])
            
            # P-values from the permuted data
                pvals_auc<- pvals_aucpr<- pvals<- NULL
                
                if (is.null(y)==FALSE) {
                  
                          seq.step1<- seq(1, nrow(y), no.reps)
                          seq.step234<- setdiff(which(y[,1] == "-500"), seq.step1)
                          results.500.step234<- y[seq.step234,]
                          
                          AUCroc.perm<- AUCpr.perm<- list()
                        
                          for (a in 1:length(unique(results.500.step234$Predictor)))
                            {
                            AUCroc.perm[[a]]<- as.numeric(results.500.step234[which(results.500.step234$Predictor == unique(results.500.step234$Predictor)[a]),"AUCval"])
                            AUCpr.perm[[a]]<- as.numeric(results.500.step234[which(results.500.step234$Predictor == unique(results.500.step234$Predictor)[a]),"AUCval_pr"])
                            }
                          
                          pvals_auc<- sapply(1:length(AUCroc.perm), function(a) {mean(AUCroc.perm[[a]] > step5_auc[2,a])})
                          pvals_aucpr<- sapply(1:length(AUCpr.perm), function(a) {mean(AUCpr.perm[[a]] > step5_aucpr[2,a])})
                          
                          names(pvals_aucpr)<- names(pvals_auc)<- unique(results.500.step1$Predictor)
                          
                          }
                
                  
                
            return(list(Step1_auc = step1_auc, Step5_auc = step5_auc, pvals_auc = pvals_auc, Step1_aucpr = step1_aucpr, Step5_aucpr = step5_aucpr, pvals_aucpr = pvals_aucpr))
    }
    
    
    
    
    optimism.corrected.AUCmv<- function(x, y, no.reps)
            {
                seq.step1<- seq(1, nrow(x), no.reps)
                seq.step234<- 2:no.reps
                  
                results.500.step1<- x[seq.step1,]
                results.500.step234<- x[seq.step234,]
                
              # Find optimism adjusted 95% CIs
                OBrocmean<- round(quantile(as.numeric(results.500.step234[not.nas,"OBroc"]), probs = c(0.025, 0.5, 0.975), na.rm=T),3)
                OBroc_prmean<- round(quantile(as.numeric(results.500.step234[, "OBroc_pr"]), probs = c(0.025, 0.5, 0.975), na.rm=T),3)
                
                step1_auc<- round(mean(as.numeric(results.500.step1$AUCtrain)),3)
                step1_aucpr<- round(mean(as.numeric(results.500.step1$AUCtrain_pr)),3)
                
                names(step1_auc)<- names(step1_aucpr)<- "All"
                
                step5_auc<- rbind(upper95 = step1_auc - OBrocmean[1], mean = step1_auc - OBrocmean[2], lower95 = step1_auc - OBrocmean[3])
                step5_aucpr<- rbind(upper95 = step1_aucpr - OBroc_prmean[1], mean = step1_aucpr - OBroc_prmean[2], lower95 = step1_aucpr - OBroc_prmean[3])
                
              
              # P-values from the permuted data
                    pvals_auc<- pvals_aucpr<- pvals<- NULL
              
              if (is.null(y)==FALSE) {
                no.perms<- (nrow(y) / no.reps)
                step5_auc.perm<- step5_aucpr.perm<- matrix(0, nrow=no.perms, ncol=3)
                
                for (i in 1:no.perms)
                    {
                    seq.step1<- (i-1)*no.reps + 1
                    seq.step234<- ((i-1)*no.reps + 2):(i*no.reps)
                    
                    results.500.step1<- y[seq.step1,]
                    results.500.step234<- y[seq.step234,]
                    
                    # Find optimism adjusted 95% CIs
                    OBrocmean.perm<- round(quantile(as.numeric(results.500.step234[,"OB"]), probs = c(0.025, 0.5, 0.975), na.rm=T),3)
                    OBroc_prmean.perm<- round(quantile(as.numeric(results.500.step234[, "OBroc_pr"]), probs = c(0.025, 0.5, 0.975), na.rm=T),3)
                    
                    step1_auc.perm<- round(mean(as.numeric(results.500.step1$AUCtrain)),3)
                    step1_aucpr.perm<- round(mean(as.numeric(results.500.step1$AUCtrain_pr)),3)
                    
                    AUCroc.perm<- as.numeric(results.500.step234[,"AUCval"])
                    AUCpr.perm<- as.numeric(results.500.step234[,"AUCval_pr"])
                    
                    step5_auc.perm[i,]<- rbind(upper95 = step1_auc.perm - OBrocmean.perm[1], mean = step1_auc.perm - OBrocmean.perm[2], lower95 = step1_auc.perm - OBrocmean.perm[3])
                    step5_aucpr.perm[i,]<- rbind(upper95 = step1_aucpr.perm - OBroc_prmean.perm[1], mean = step1_aucpr.perm - OBroc_prmean.perm[2], lower95 = step1_aucpr.perm - OBroc_prmean.perm[3])
                    }
                
                pvals_auc<- mean(step5_auc[2] < step5_auc.perm[,2])
                pvals_aucpr<- mean(step5_aucpr[2] < step5_aucpr.perm[,2])
                names(pvals_aucpr)<- names(pvals_auc)<- "All"
              }
              
              
              return(list(Step1_auc = step1_auc, Step5_auc = step5_auc, pvals_auc = pvals_auc, Step1_aucpr = step1_aucpr, Step5_aucpr = step5_aucpr, pvals_aucpr = pvals_aucpr))
            }
            
    
    
    create.cont.to.cat<- function(d2.cont, Proc_cut, labels, validate, need.to.resample)
    {
      cont.to.cat<- NULL
      
      for (i in 1:(ncol(d2.cont)-2))
      {
        var<- names(d2.cont)[i+2]
        n<- match(var, names(Proc_cut))
        cat.name.train<- as.numeric(cut(d2.cont[,var], breaks=Proc_cut[[n]], labels = labels[[n]], include.lowest = T))
        cat.name.validate<- as.numeric(cut(d2[validate,var], breaks=Proc_cut[[n]], labels = labels[[n]], include.lowest = T))
        
        cont.to.cat<- cbind(cont.to.cat, cat.name.train)
        colnames(cont.to.cat)[ncol(cont.to.cat)]<- var
        
        all.groups.in.this.sample<-  na.omit(unique(cat.name.train)) %in% na.omit(unique(cat.name.validate))
        need.to.resample<- (FALSE %in% all.groups.in.this.sample)
      }
      return(list(cont.to.cat, need.to.resample))
    }
    
    
    
    scores.to.link<- function(scores, predicted.link, scores.trialled)
            {
              c<- coef(lm(predicted.link$fit ~ scores.trialled))
              scores.converted<- c[1] + (c[2] * scores)
              return (scores.converted)
            }
    
    
    
    end.check<- function() {if (is.na(read.csv("stop.csv"))[1,2] == FALSE) {stop("Stop.")}}
    end.check.mv<- function() {if (is.na(read.csv("stopmv.csv"))[1,2] == FALSE) {stop("Stop.")}}
    
    
# Load the data
    
    cat("\n Loading data...")
    
    source("col_types.R")
    col_types = as.character(read.csv("col_types.csv")[,1])
    no.rows = as.numeric(read.csv("number_of_rows.csv")[,1])
    TAPSErefs<- read_excel("TAPSE.xlsx")
    d<- read_excel("Spreadsheet V4.xlsx", col_types=col_types, na = c("NA",""))[1:no.rows,]
    unique.IDs.1<- unique(d$Demo_HRN)
    audit.cohort.rows<- which(d$`Q_Include_DxPAH_&_PVRStudy`=="Yes")
    
    
    
    
    cat("\n Format the data...")
    
    
# Format the data for analysis
    d2<- d[audit.cohort.rows,]
    unique.IDs.2<- unique(d2$Demo_NHSNo)
    unique.IDs.row<- sapply(unique.IDs.2, function(x) {which.min(match(d2$Demo_NHSNo,x))})

    
    likert<- c("Echo_TRseverity", "Echo_RVdil", "Echo_RVhyp", "Echo_RVdys", "Echo_PRseverity", "Echo_RA.dilat", 
               "Echo_Septalflattening", "Echo_ASD", "Echo_VSD", "Echo_Effusion", "Echo_Rad.dysfunc", "Echo_Long.dysfunc", "Echo_PADilatation")
    
    for (i in 1:13) {
      x<- likert[i]
      d2<- convert.likaert.text(d2, x)
      }
    
      d2<- convert.yesno.numbers(d2,"Dx_T21")
   
# Convert TAPSE to TAPSE Z Scores (Ref: Koestenberger et al., 2009, Journal of the American Society of Echocardiography, RV function in Infants, Children and Adolescents: ..., )
    TAPSEref.rows<- sapply(d2$Proc_Age, function(x) {which(TAPSErefs$Age > x)[1]})
    for (i in 1:length(d2$Echo_TAPSE)) {d2$Echo_TAPSEZscore.calc[i]<- ((d2$Echo_TAPSE[i]/10) - TAPSErefs[TAPSEref.rows[i],]$Mean) / TAPSErefs[TAPSEref.rows[i],]$SD}
    
    
    d2$Demo_IDn<- match(d2$Demo_HRN, unique(d2$Demo_HRN))
    
    aux.vars<- c("Echo_RVdysNo","Echo_TRseverityNo","Echo_RVdilNo","Echo_RVSP","Echo_TRVel","Echo_RVhypNo","Echo_ASDNo","Echo_RVhypNo",
                 "Echo_PRseverityNo","Echo_SeptalflatteningNo","Echo_TAPSEZscore.calc","Echo_LVFS","Echo_PADilatation")
    
    

    cat("\n Define the complications...")
    
    
# Define the complications
    l<- define.complications(-500,-500, -100, d2)
    
    complications.all.nonfatal<- l[[1]]
    complications.summary.nonfatal<- l[[2]]
    
    complications.all.fatal<- l[[3]]
    complications.summary.fatal<- l[[4]]
    
    complications.all<- l[[5]]
    complications.summary<- l[[6]]
    
    escalations.all<- l[[7]]
    escalations.summary<- l[[8]]
    
    adverseevents.all<- l[[9]]
    adverseevents.summary<- l[[10]]
    
    
    
    
    
    
    cat("\n Define the groups of variables...")
    
    
    
# Define groups of variables
    
    predictors<- c("Proc_Age", "Echo_RVdysNo","Echo_TRseverityNo", "Echo_TRVel", "Echo_PRseverityNo","Echo_PRVmax", "Echo_PREDVmax", "Echo_RVdilNo","Echo_RVhypNo",
                   "Echo_RA.dilatNo","Echo_SeptalflatteningNo","Echo_ASDNo","Echo_VSDNo","Echo_EffusionNo","Echo_TAPSEZscore.calc")
    
    predictors.sig<- c("Proc_Age", "Echo_RVdysNo","Echo_TRseverityNo", "Echo_TRVel", "Echo_PRseverityNo","Echo_PRVmax", "Echo_RVdilNo") # FAC is significant but no readings in patients who had a complication so can't make groups
    
    
    dependent.variables<- c("RHC_mPAP", "Complication_Any", "Escalation_Any", "Adverse_Event_Any", "Anaes_Change_in_MBP_percent", "Anaes_Change_in_HR_percent","Anaes_Change_in_SF_percent", "Complication_Logistics")
    
    dependent.variables.type<- c("C","L","L","L","C","C","C","L")
    
    cath.variables<- c("Demo_IDn","Proc_Age", "RHC_mPAP", "RHC_PVRI1", "RHC_CO")
    
    echo.variables<- c("Echo_RVdilNo","Echo_SeptalflatteningNo","Echo_RVhypNo","Echo_RVdysNo",
                       "Echo_TRseverityNo", "Echo_TRVel","Echo_RVSP","Echo_PADilatationNo","Echo_EffusionNo",
                       "Echo_LVFS","Echo_ASDNo","Echo_VSDNo","Echo_PRseverityNo","Echo_PRVmax",
                       "Echo_PRgradient","Echo_PREDVmax","Echo_PREDgradient","Echo_FAC","Echo_TAPSEZscore","Echo_TAPSEZscore.calc", "Echo_TAPSE","Echo_RA.dilatNo","Echo_PAAT")
    
    anaes.variables<- c("Anaes_BIS_monitoring_binary", "Anaes_ASA", "Anaes_Induction_Gaseous", "Anaes_Premed") 
    
    independent.variables<- c(echo.variables, cath.variables)
    
    no.independent.variables<- length(independent.variables)
    
    columns.for.analysis<- na.omit(match(c(cath.variables, echo.variables), colnames(d2)))
    
    d3<- data.frame(cbind(d2[,columns.for.analysis], complications.all, escalations.all, adverseevents.all))
    
    cols<- brewer.pal(10,"Paired")
    
    
    cat("\n Done.")
    
    
    