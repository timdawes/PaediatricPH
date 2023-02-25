# Tim Dawes February 2023
# Paediatric IPAH risk stratification paper

# Code for developing a scoring system for pre-operative echocardiography to risk stratify children with IPAH
# undergoing general anaesthesia for cardiac catheterisation


    source("prepare_data.R")
    
# Which continuous variables are associated with the outcome?
            pval<- matrix(1, ncol=2, nrow=length(predictors))
    
            predictor.type<- rep("",length = length(predictors.sig))   
            set.seed(1000)
            for (i in 1:length(predictors))
            {
              cat(".")
                if (length(table(d2[,predictors[i]])) == 2) {b = as.factor(d2[,predictors[i]]); predictor.type[i]<- "cat"} else {b = d2[,predictors[i]]; predictor.type[i]<- "cont"}
                form<- as.formula(paste("AdverseEvents_Patients ~ ", predictors[i]))
                mod<- lme_imp(form, random = ~ 1 | Demo_IDn, data = d3, n.iter=1000, n.chains = 3, n.adapt = 500, family=binomial(),
                              auxvars = as.formula(paste(" ~ ", paste(paste(cath.variables, collapse =" + "), paste(setdiff(echo.variables, predictors[i]), collapse=" + "), sep=" + "))),
                              scale_vars = d2[,c(cath.variables, echo.variables)],
                              mess = FALSE)
                
                pval[i,]<- summary(mod)$res$AdverseEvents_Patients$regcoef[2,c(1,5)]
              
            }
    
            
                significant.predictor.cols<- match(predictors[which(pval[,2]<= 0.05)], colnames(d3))
                          
            # Loess plot to look at relationship of variables with outcome
                  predictors.sig<- colnames(d3)[significant.predictor.cols]
                  d3.cont<- d3[,c("Demo_IDn","AdverseEvents_Patients", predictors.sig)]
                  predictors.sig.n<- length(predictors.sig)
                  Proc_cut<- labels<- group.order<- vector(mode="list", length = predictors.sig.n)
                  
            names(Proc_cut)<- names(labels)<- names(group.order)<- predictors.sig
            titles<- c("Age","RV dysfunction", "TR severity", "TR maximal velocity", "PR severity", "PR maximal velocity", "RV dilatation")
            xlabs<- c("Years", "Likert scale", "Likert scale", "m/s", "Likert scale", "m/s", "Likert scale")
            range.max<- c(18,7,7,7,7,6,7)
            range.min<- c(0,1,1,1,1,1,1)
            units<- c(0.1,1,1,0.1,1,0.1,1)
            
            # For continuous variables
                foci<- 3
                par(mfrow=c(1,1), mar=c(4,4,4,4))
                layout.matrix <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 0), nrow = 3, ncol = 3, byrow=T)
                layout(mat = layout.matrix, heights = c(1,1), widths = c(1,1))
              
            # Optimised boundaries 
                      for (j in 1:length(predictors.sig)){
                        var=predictors.sig[j]
                        
                        a<- d3.cont[,var]
                        b<- as.numeric(d3.cont[,"AdverseEvents_Patients"])
                        
                        o<- order(a)
                        a<- a[o]
                        b<- b[o]
                        
                        these.are.not.nas<- which(is.na(a)==FALSE)
                        a<- a[these.are.not.nas]
                        b<- b[these.are.not.nas]
                        
                        lo<- loess(b ~ a, sp = 1, degree=1)
                        s<- seq(min(a, na.rm=T), max(a, na.rm=T), units[j])
                        pred<- predict(lo, newdata = data.frame(a = s), type="response")
                        pred[pred<0]<- 0
                        
                        cols<- c(brewer.pal(8,"Greens")[6], brewer.pal(6,"Blues")[5], brewer.pal(6, "Reds")[5])
                        
                        # KNN method
                            knn<- stats::kmeans(pred, centers=seq(min(pred), max(pred),length.out=3))
                            clusters<- knn$cluster
                            for (i in 1:foci) {clusters.knn[which(clusters.knn == unique(clusters.knn)[i])]<- i}
                            
                        
                        filename<- paste("Figures/Figure4/", titles[j], ".pdf",sep="")
                        
                        plot(s, pred, col=cols[clusters], type='b', lwd=2, main="", xlim=c(range.min[j],range.max[j]),
                             ylim=c(-0.1,0.7), bty='n', axes=F, xlab="", ylab="", cex.axis=2, pch=19)
                        
                        title(titles[j], line = -1, cex.main=2.5)
                        new.b<- 0.2 + (b / 5)
                        new.b<- new.b-runif(length(b),-0.03,0.03)
                        new.a<- a-runif(length(a),0,0.3)
                        
                        new.b[new.b<0]<- -new.b
                        new.a[new.a<0]<- -new.a
                        
                        points(new.a, new.b, pch=19, cex=2, col=alpha(cols.lab[b+1], 0.5))
                        points(s, pred, col=cols[clusters], type='p', lwd=4, main=titles[j], ylim=c(0,0.5), bty='n', axes=F, xlab=xlabs[j], ylab="")
                        
                        axis(side=1, lwd=4, xlab = xlabs[j], cex.axis=1.5)
                        mtext(xlabs[j], side=1, line=2.8, cex=1.5)
                        axis(side=2, lwd=4, xlab="Probability", cex.axis=1.5, las=2, line=-0.8)
                        mtext("Probability", side=2, line=2.2, cex=1.5)
                        
                        axis(side=4, lwd=0, xlab="Adverse Events", tick = TRUE, lwd.ticks = 6, at = c(0.2, 0.4), labels=c("No Event","Event"), las=3,
                             line=-0.5, cex.axis=2, padj=0.5)
                        
                        
                        group<- match(var, names(Proc_cut))
                        Proc_cut[[group]]<- sort(unique(c(min(a, na.rm=T), s[which(diff(clusters)!=0)], max(a, na.rm=T))))
                        boundaries<- c(which(diff(clusters)!=0), length(s))
                        labels[[group]]<- clusters[boundaries]
                        group.order[[group]]<- order(sapply(1:foci, function(x) {mean(pred[which(clusters==x)])}))
                        }
                      
            
                
                
              # Pragmatic boundaries
                      Proc_cut[[1]]<- c(0,1,6,18)
                      Proc_cut[[2]]<- Proc_cut[[3]]<- Proc_cut[[5]]<- Proc_cut[[7]]<- c(1,3,5,7)
                      Proc_cut[[4]]<- Proc_cut[[6]]<- c(0,3,5,7)
                      
                      labels[[1]]<- rev(1:3)
                      labels[[2]]<- labels[[3]]<- labels[[4]]<- labels[[5]]<- labels[[6]]<- labels[[7]]<- 1:3
                      

                      
                      
                     
                  
# Multivariable model
                  
            no.reps<- 1000
            perms<- 500
            results.MV<- results.MVp<- NULL
            HRBP.drop.sequence<- c(-500, rev(seq(-10,-50,-10)))
            SF.drop.sequence<- c(-500, rev(seq(-20,-60,-10)))
            score.names.of.interest<- sort(predictors.sig)
            score.cont.results<- matrix(0, nrow=no.reps, ncol = length(predictors.sig), dimnames = list(1:no.reps, score.names.of.interest))
            class.performance<- matrix(0, nrow=no.reps, ncol=152)
            auc.roc.matrix<- auc.pr.matrix<- list()
            
            r=1
              HRdrop<- HRBP.drop.sequence[r]
              BPdrop<- HRBP.drop.sequence[r]
              SFdrop<- SF.drop.sequence[r]
              l<- define.complications(HRdrop,BPdrop, SFdrop, d2)
              complications<- l[[1]]
              df.complications<- l[[2]]

              write.csv("", file="stopmv.csv")   

              

for (r in 1:perms)
    {
      for (q in 1:no.reps)
        {
        end.check.mv()   
        if (r==1) {cat("\n Original data")}
        if (r>1) {cat("\n Permuted data, iteration:", r)}
        par(mfrow=c(2,4))
        
        
                          # ***** Outer repeat loop *****
                            repeat{
                              
                              
                                                # ***** Inner repeat loop *****
                                                  repeat {    
                                                    
                                                            # Set training & validation groups            
                                                                  validate<- 1:nrow(d3)
                                                                  if (q == 1) {train<- 1:nrow(d3)} else {train<- sample(nrow(d3), nrow(d3), replace = TRUE)}
                                                                  d3.cont<- d3[train, c("AdverseEvents_Patients", "Demo_IDn", predictors.sig)]
                                                                  if (r>1) {for (i in 3:ncol(d3.cont)) {d3.cont[,i]<- d3.cont[sample(train, nrow(d3), replace=F),i]}}
                                                            
                                                            # Extract data necessary for analysis
                                                                  #cont.to.cat<- data.frame(Demo_IDn = d3.cont[, "Demo_IDn"])
                                                                  cont.to.cat<- NULL
                                                                  
                                                            # Default to 'don't need to resample'
                                                                  need.to.resample<- FALSE
                                                                  if (sum(d3.cont$AdverseEvents_Patients[train])<10 && sum(d3.cont$AdverseEvents_Patients[validate])<10) {need.to.resample<- TRUE}
                                                                  
                                                            # Bin the continuous variables
                                                                  ctc<- create.cont.to.cat(d3.cont, Proc_cut, labels, validate, need.to.resample)
                                                                  cont.to.cat<- ctc[[1]]
                                                                  need.to.resample<- ctc[[2]]
                                                                  rm(ctc)
                                                                  
                                                                  if (need.to.resample == FALSE) {break}
                                                          } # Inner repeat loop
                        
                                         
                                        y<- d3$AdverseEvents_Patients[train]
                                        
                                        X.impute <- impute.nipals(X = cont.to.cat, ncomp = 5)
                                        pls<- plsRglm(y ~ ., data.frame(cbind(X.impute, y)), 1, modele = "pls-glm-logistic")
                                        
                              
                                  # Extract the regression coefficients for each predictor
                                        score<- c(pls$pp)
                                        names(score)<- rownames(pls$pp)
                                        
                                        score.names.to.keep<- colnames(d3.cont)
                                        score.cont<- score[which(apply(sapply(score.names.to.keep, function (x) {grepl(x, names(score))}),1,sum)>0)]
                                        
            
                                  # Calculate the scores for numeric variables
                                        
                                        var.cont<- as.character(1)
                                        d3[,paste(predictors.sig,"points",sep=".")]<-as.numeric(NA)
                                        
                                        for (j in 1:length(Proc_cut))
                                          {
                                            for (i in 1:(length(Proc_cut[[j]])-1))
                                            {
                                              var.low<-Proc_cut[[j]][i]
                                              var.upper<-Proc_cut[[j]][i+1]
                                              var<- names(Proc_cut)[j]
                                              positive<- which(d3[,var] <= var.upper & d3[,var] >= var.low)
                                              group<- paste(var, labels[[j]][i], sep="")
                                              score.cont[var]
                                              d3[positive, paste(var,"points",sep=".")]<- score.cont[var] * i
                                            }
                                          }
                                        
                                        score.data.matrix<- data.matrix(d3[,grepl("\\.points",names(d3))])
                                        d3$score<- rowSums(score.data.matrix, na.rm=T)
                                        
                                  # Convert the scores to probability of mortality and comparing it to observed number of deaths
                                          glmod.lm<- glm(AdverseEvents_Patients ~ score, data = d3[train,], family=binomial(link = "logit"))
                                          
                                          glmod<- tryCatch({glmer(AdverseEvents_Patients ~ score + (1 | Demo_IDn), data = d3[train,], family=binomial(link = "logit"),
                                                        start=list(fixef=coef(glmod.lm)))},
                                                        error = function(e) {need.to.resample<- TRUE})
                                          if (class(glmod) != "glmerMod") {need.to.resample<- TRUE}
                                          
                                          
                                          if (need.to.resample == FALSE) {break}
                                          end.check.mv()
                                          
                              
                            }
                              
                              
                              newx<- seq(min(d3$score, na.rm=T), max(d3$score, na.rm=T), length=50)
                              prd<- predict(glmod, newdata = data.frame(score = newx, Demo_IDn = 1), type="response", re.form=NA)
                              
                            
                            br<- seq(min(d3$score, na.rm=T), max(d3$score, na.rm=T), length=20)
                            count<- as.matrix(table(cut(d3$score, breaks=br, include.lowest=T), d3$AdverseEvents_Patients))
                            
                      # Eighth section: visualisation of the relationship between scores and probability of outcome events
                      
                      
                      # Graphics
                            
                            if (r==1) {par(mar=c(8,2,2,2));
                              score.cont.results[q, match(names(score.cont), colnames(score.cont.results))]<- score.cont;
                              boxplot(score.cont.results[1:q,], las=2, ylim=c(-0.8,0.8), col=c("white", rainbow(length(predictors.sig))))
                              write.csv(score.cont.results, file="MVcoefficients.csv", row.names=rownames(score.cont.results), col.names = colnames(score.cont.results))
                            }
                            
                            
                            barplot(t(count), main="Scores versus probability of complication", xlab="Scores", ylab="Observed number of patients", space=0, col=c("yellow", "lightblue"))
                            legend("topright", fill=c("yellow","lightblue",NA), lty=c(NA,NA,1), lwd=c(NA,NA,2),legend=c("No complication","Complication","Predicted Prob."), col=c("Black"), border=c("black","black",NA))
                            par(new=TRUE)
                            plot(prd ~ newx, type="l", col="black", lwd=2, xaxt="n", yaxt="n", xlab="", ylab="")
                            mtext("Predicted probability of complication", side=4, line=3)
                            
                      
                      # Method 2: TRAINING group, other stats 
                            not.missing.train<- train[which(is.na(d3$score[train])==FALSE)]
                            not.missing.validate<- validate[which(is.na(d3$score[validate])==FALSE)]
                            
                            pr.train<- prediction(d3[not.missing.train,]$score, d3[not.missing.train,]$AdverseEvents_Patients)
                            pr.validate<- prediction(d3[not.missing.validate,]$score, d3[not.missing.validate,]$AdverseEvents_Patients)
                            
                            # Optimism bias for ROC
                                prf.pr.train <- performance(pr.train, measure = "tpr", x.measure = "fpr")
                                plot(prf.pr.train, ylim=c(0,1), lwd=4, col="blue", main="Normal ROC curve")
                                roc.train <- performance(pr.train, measure = "auc")@y.values[[1]]
                                
                                prf.pr.validate <- performance(pr.validate, measure = "tpr", x.measure = "fpr")
                                plot(prf.pr.validate, ylim=c(0,1), lwd=4, col="lightblue", add=T, type='b')
                                
                                auc.roc.matrix[[q]]<- data.frame(x = prf.pr.validate@x.values[[1]], y = prf.pr.validate@y.values[[1]])
                                
                                roc.validate <- performance(pr.validate, measure = "auc")@y.values[[1]]
                                optimism.bias_roc<- roc.train - roc.validate
                                
                                
                            # Optimism bias for precision-recall
                                prf.pr.train <- performance(pr.train, measure = "prec", x.measure = "rec")
                                plot(prf.pr.train, ylim=c(0,1), lwd=4, main="PR curve", avg="threshold", colorize=T)
                                auc.pr.train <- performance(pr.train, measure = "aucpr")@y.values[[1]]
                                
                                
                                prf.pr.validate <- performance(pr.validate, measure = "prec", x.measure = "rec")
                                plot(prf.pr.validate, ylim=c(0,1), lwd=4, avg="threshold", add=T, type='b', colorize=T)
                                
                                auc.pr.matrix[[q]]<- data.frame(x = prf.pr.validate@x.values[[1]], y = prf.pr.validate@y.values[[1]])
                                
                                auc.pr.validate <- performance(pr.validate, measure = "aucpr")@y.values[[1]]
                                optimism.bias_pr<- auc.pr.train - auc.pr.validate
                          
                                opt<-optimal.cutpoints("score", "AdverseEvents_Patients", methods="Youden", data=d3[not.missing.validate,], tag.healthy=0)
                                opt.cutoff<- opt$Youden$Global$optimal.cutoff$cutoff
                                class.performance[q, intersect(which(d3[not.missing.validate,]$score > opt.cutoff), which(d3[not.missing.validate,]$AdverseEvents_Patients == 1))]<- 1 # True positive
                                class.performance[q, intersect(which(d3[not.missing.validate,]$score <= opt.cutoff), which(d3[not.missing.validate,]$AdverseEvents_Patients == 1))]<- 4  # False negative
                                class.performance[q, intersect(which(d3[not.missing.validate,]$score > opt.cutoff), which(d3[not.missing.validate,]$AdverseEvents_Patients == 0))]<- 3 # False positive
                                class.performance[q, intersect(which(d3[not.missing.validate,]$score <= opt.cutoff), which(d3[not.missing.validate,]$AdverseEvents_Patients == 0))]<- 2 # True negative
                                
                                #cat("    ", d3$Demo_HRN[not.missing.validate][order(apply(class.performance,2,sum), decreasing=T)[1:10]])
                                
                                
                                
                                
                            if (r == 1){
                                        if (is.null(results.MV) == FALSE) {results.MV<- rbind(results.MV, c(HRdrop, "All", q,
                                                                                                   roc.train, roc.validate, optimism.bias_roc,
                                                                                                   auc.pr.train, auc.pr.validate, optimism.bias_pr
                                        ))}
                                        if (is.null(results.MV) == TRUE) {results.MV<- data.frame(HRdrop = HRdrop, Predictor = "All", Rep = q,
                                                                                            AUCtrain = roc.train, AUCval = roc.validate, OBroc = optimism.bias_roc,
                                                                                            AUCtrain_pr = auc.pr.train, AUCval_pr = auc.pr.validate, OBroc_pr = optimism.bias_pr
                                        )}}
                         
                            display.results<- results.MV
                            display.results$Predictor<- gsub("Proc_","", gsub("No", "", gsub("Echo_","", results.MV[,"Predictor"])))
                            
                            boxplot(as.numeric(AUCval) ~ Predictor, data = display.results, las=2, col="blue", ylab="AUCroc", xlab="", ylim=c(0,1), main="Original: AUCs for normal ROC")
                            boxplot(as.numeric(AUCval_pr) ~ Predictor, data = display.results, las=2, col="pink", ylab="AUCpr", xlab="", ylim=c(0,1), main="Original:AUCs for PR")
                            
                            
                            if (r>1) {
                              if (is.null(results.MVp) == FALSE) {results.MVp<- rbind(results.MVp, c(HRdrop, "All", q,
                                                                                            roc.train, roc.validate, optimism.bias_roc,
                                                                                            auc.pr.train, auc.pr.validate, optimism.bias_pr
                              ))}
                              if (is.null(results.MVp) == TRUE) {results.MVp<- data.frame(HRdrop = HRdrop, Predictor = "All", Rep = q,
                                                                                    AUCtrain = roc.train, AUCval = roc.validate, OB = optimism.bias_roc,
                                                                                    AUCtrain_pr = auc.pr.train, AUCval_pr = auc.pr.validate, OBroc_pr = optimism.bias_pr)}
                              
                              x.labels<- rev(gsub("Proc_","", gsub("No", "", gsub("Echo_","", unique(results.MVp[,"Predictor"])))))
                              
                              display.results.MVp<- results.MVp
                              display.results.MVp$Predictor<- gsub("Proc_","", gsub("No", "", gsub("Echo_","", unique(results.MVp[,"Predictor"]))))
                              
                              boxplot(as.numeric(AUCval) ~ Predictor, data = display.results.MVp, las=2, col="green", ylab="AUCroc", xlab="", ylim=c(0,1), main="Permuted: AUCs for normal ROC")
                              boxplot(as.numeric(AUCval_pr) ~ Predictor, data = display.results.MVp, las=2, col="lightgreen", ylab="AUCpr", xlab="", ylim=c(0,1), main="Permuted: AUCs for PR")
                              
                              
                            }
                            
      } # End of reps loop

} # End of perms loop
              
              
              
              
              
     
              
              
              # Build a scoring system
              
              # Define the model
                      d3.cont<- d3[, c("AdverseEvents_Patients", "Demo_IDn", predictors.sig)]
                      cont.to.cat<- NULL
                      ctc<- create.cont.to.cat(d3.cont, Proc_cut, labels, validate, need.to.resample)
                      cont.to.cat<- ctc[[1]]
                      need.to.resample<- ctc[[2]]
                      
                      y1<- d3$AdverseEvents_Patients
                      y2<- d3$Complication_Patients
                      
                      x <- impute.nipals(X = cont.to.cat, ncomp = 5)
                      pls1<- plsRglm(y1 ~ ., data.frame(cbind(x, y1)), 1, modele = "pls-glm-logistic")
                      pls2<- plsRglm(y2 ~ ., data.frame(cbind(x, y2)), 1, modele = "pls-glm-logistic")
                      
              
              # Set up the variables for names and coefficients 
                      score.cont.results<- read.csv("MVcoefficients.csv", header=TRUE, row.names=1, check.names=F)
                      scoring.system.coefficients<- scoring.system.bands.list<- cat.band.rounded<- vector(mode="list", length(Proc_cut))
                      names(scoring.system.coefficients)<- names(scoring.system.bands.list)<- names(cat.band.rounded)<- names(Proc_cut)
                      foci<- 3
                      foci.names<- c("Low Risk","Medium Risk", "High Risk")
                      
                    
              # Cycle through each variable and find the boundaries and coefficients
                        for (i in 1:length(Proc_cut))
                                {
                                  # Boundaries
                                        var<- names(Proc_cut)[[i]]
                                        cat.band.rounded[[i]]<- vector(mode="character", length=3)
                                        for (k in 1:foci)
                                            {
                                            t1<- which(d3[,significant.predictor.cols[i]] >= Proc_cut[[i]][k])
                                            if (k==foci) {t2<- which(d3[,significant.predictor.cols[i]] <= Proc_cut[[i]][k+1])} else {t2<- which(d3[,significant.predictor.cols[i]] < Proc_cut[[i]][k+1])}
                                            text<- paste("[",Proc_cut[[i]][k],"-",Proc_cut[[i]][k+1],"]",sep="")
                                            cat.band.rounded[[i]][k]<- text
                                            }
                                        
                                  # Coefficients
                                        for (j in 1:foci)
                                        {
                                          scoring.system.bands.list[[i]][[j]]<- cat.band.rounded[[i]][which(labels[[i]] == group.order[[i]][j])]
                                          
                                              col.from.pls.regression<- match(names(Proc_cut)[[i]], colnames(score.cont.results))#which(score.cont.results[,cols.for.each.variable[j]]!=0)
                                              row.from.pls.pca<- match(names(Proc_cut)[[i]], rownames(pls1$pp))
                                              
                                              non.zero.results<- score.cont.results[,col.from.pls.regression]
                                              non.zero.results<- non.zero.results[which(non.zero.results!=0)]
                                              
                                              if (length(non.zero.results)==0) {scoring.system.coefficients[[i]][[j]]<- NA} else
                                                {scoring.system.coefficients[[i]][[j]]<- median(non.zero.results) * pls1$CoeffC * labels[[i]][j]}
                                        
                                              names(scoring.system.bands.list[[i]])[j]<- foci.names[j]
                                              names(scoring.system.coefficients[[i]])[j]<- foci.names[j]
                                        }
                                        
                                }
                    
                    scoring.system.coefficients<- unlist(scoring.system.coefficients)
                    
                    score.if.all.low.risk<- sum(scoring.system.coefficients[seq(1,21,3)])
                    score.if.all.mod.risk<- sum(scoring.system.coefficients[seq(2,21,3)])
                    score.if.all.high.risk<- sum(scoring.system.coefficients[seq(3,21,3)])
                    
                    rescaling.multiplier<- 101 / (score.if.all.high.risk - score.if.all.low.risk)
                    rescaling.addition<- score.if.all.low.risk / 7
                    
                    rescaled.scoring.system.coefficients<- (scoring.system.coefficients - rescaling.addition) * rescaling.multiplier
                    
                    rescaled.scoring.system.coefficients<- round(apply(matrix(rescaled.scoring.system.coefficients,nrow=3), 2, function(x) {x - mean(x)}))
                    
                    rescaled.score.if.all.low.risk<- sum(rescaled.scoring.system.coefficients[seq(1,21,3)]) 
                    rescaled.score.if.all.mod.risk<- sum(rescaled.scoring.system.coefficients[seq(2,21,3)])
                    rescaled.score.if.all.high.risk<- sum(rescaled.scoring.system.coefficients[seq(3,21,3)])
                    
                    cat("\n Minimum score:", rescaled.score.if.all.low.risk, "  Maximal score:", rescaled.score.if.all.high.risk)
                    
                    scoring.system<- matrix(paste(paste(foci.names[1:foci], unlist(scoring.system.bands.list)),
                                                  round(unlist(rescaled.scoring.system.coefficients),0)),
                                            nrow=foci, dimnames=list(foci.names[1:foci], names(Proc_cut)))
                    
                    scoring.system   
                    
                    
                    
                    
        # Figure 3: Graph of scoring system total vs Risk
                    
                    # Build a prediction matrix for a score and the associated probability of an adverse event
                    
                         # Create a set of imaginary patients with scores between the lowest and the highest
                                scores.trialled<- seq(score.if.all.low.risk,score.if.all.high.risk, 0.01)
                                rescaled.scores.trialled<- (scores.trialled - rescaling.addition*7) * rescaling.multiplier - 50
                                
                                predict.matrix<- do.call(rbind, lapply(scores.trialled, function(x) {c(0,x,0,0,0,0,0)}))
                                
                          # Predict what their risk probabilities and their link values would be
                                predicted.responses1<- predict(pls1, newdata = data.frame(X.impute = predict.matrix), type="response", se.fit=F)
                                predicted.responses2<- predict(pls2, newdata = data.frame(X.impute = predict.matrix), type="response", se.fit=F)
                                
                                predicted.link1<- predict(pls1, newdata = data.frame(X.impute = predict.matrix), type="link", se.fit=TRUE)
                                predicted.link2<- predict(pls2, newdata = data.frame(X.impute = predict.matrix), type="link", se.fit=TRUE)
                                
                            
                          # Plot risk probabilities vs link
                                range.of.links<- range(c(predicted.link1$fit, predicted.link2$fit))
                                plot(predicted.link1$fit, predicted.responses1, type='l', lwd=3, col="red", xlim=range.of.links)
                                points(predicted.link2$fit, predicted.responses2, type='l', lwd=3, col="blue")
                                
                                links.trialled1<- scores.to.link(scores.trialled, predicted.link1, scores.trialled)
                                links.trialled2<- scores.to.link(scores.trialled, predicted.link2, scores.trialled)
                                
                          
                          # Convert to link values
                                fit1<- predicted.link1$fit
                                upr1<- predicted.link1$fit + 1.96 * predicted.link1$se.fit
                                lwr1<- predicted.link1$fit - 1.96 * predicted.link1$se.fit
                                
                                fit2<- predicted.link2$fit
                                upr2<- predicted.link2$fit + 1.96 * predicted.link2$se.fit
                                lwr2<- predicted.link2$fit - 1.96 * predicted.link2$se.fit
                                
                          # Convert to probabilities [p = e^link]
                                fit1.2 <- pls1$family$linkinv(fit1)
                                upr1.2 <- pls1$family$linkinv(upr1)
                                lwr1.2 <- pls1$family$linkinv(lwr1)
                                
                                fit2.2 <- pls2$family$linkinv(fit2)
                                upr2.2 <- pls2$family$linkinv(upr2)
                                lwr2.2 <- pls2$family$linkinv(lwr2)
                                
                                
                                preddata<- data.frame(x=scores.trialled,
                                                      x1=links.trialled1,
                                                      x2=links.trialled2,
                                                      graph.labels=rescaled.scores.trialled,
                                                      y1=predicted.responses1*100,
                                                      y2=predicted.responses2*100)
                                preddata$lwr1 <- lwr1.2 * 100
                                preddata$upr1 <- upr1.2 * 100
                                
                                preddata$lwr2 <- lwr2.2 * 100
                                preddata$upr2 <- upr2.2 * 100
                                
                                
                                graph.labels<- seq(-50,50,10)
                                graph.breaks<- preddata$x[sapply(graph.labels, function(x) {which.min(abs(x - preddata$graph.labels))})]
                                graph.breaks[length(graph.breaks)]<- score.if.all.high.risk
                                graph.breaks[1]<- score.if.all.low.risk
                                graph.breaks[(length(graph.breaks) + 1) / 2]<- score.if.all.mod.risk
                                
                                
                          
                          
                    p<- ggplot(data=preddata, aes(x=x)) +
                            geom_line(aes(y=y1), col=brewer.pal(8,"Blues")[8], lwd=2) +
                            geom_line(aes(y=y2), col=brewer.pal(8,"Reds")[8], lwd=2) +
                            geom_ribbon(fill=brewer.pal(8,"Blues")[6], aes(ymin = lwr1, ymax = upr1), alpha=0.3) +
                            geom_ribbon(fill=brewer.pal(8,"Reds")[6], aes(ymin = lwr2, ymax = upr2), alpha=0.3) +
                            scale_x_continuous(breaks = graph.breaks, labels = graph.labels) +
                            scale_y_continuous(breaks = seq(0,60,10), labels = seq(0,60,10)) +
                            labs(x = "Scoring system total", y = "Risk of Adverse Event or Complication (%)") +
                            annotate(geom = "vline",
                                     x = c(score.if.all.low.risk, score.if.all.mod.risk, score.if.all.high.risk),
                                     xintercept = c(score.if.all.low.risk, score.if.all.mod.risk, score.if.all.high.risk),
                                     linetype = c("dashed", "dashed","dashed")) +
                            annotate(geom = "text",
                                     label = c("All predictors are low risk", "All predictors are medium risk", "All predictors are high risk"),
                                     x = c(score.if.all.low.risk, score.if.all.mod.risk, score.if.all.high.risk),
                                     y = c(20,20,20),
                                     size=6,
                                     angle = 90, 
                                     vjust = 1) +
                            theme_bw(base_size=20) +
                            theme(axis.line = element_line(colour = "black"),
                                  panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  panel.border = element_blank(),
                                  panel.background = element_blank())
                            
                          
                    p
                    
                    
                    # Save the figure
                            pdf("Figure3.pdf", width=6, height=6)
                            print(p)
                            dev.off()
                    
                            
                    # What was the distribution of scores in the whole group?
                    
                    rescaled.scoring.system.coefficients.mat<- matrix(round(rescaled.scoring.system.coefficients,0), nrow=3)
                    rescaled.scoring.system.scores.mat<- matrix(0, nrow=nrow(cont.to.cat), ncol=ncol(cont.to.cat))
                    
                    for (i in 1:nrow(d3))
                    {
                      for (j in 1:ncol(cont.to.cat))
                      {
                        rescaled.scoring.system.scores.mat[i,j]<- rescaled.scoring.system.coefficients.mat[cont.to.cat[i,j],j]
                      }
                    }
                    
                    d3$rescaled.scoring.system.scores<- apply(rescaled.scoring.system.scores.mat,1,sum,na.rm=T)
                    br<- seq(-50,50, 10)
                    count_Complication_Patients<- as.matrix(table(cut(d3$rescaled.scoring.system.scores, breaks=br, include.lowest=T), d3$Complication_Patients))
                    count_AdverseEvents_Patients<- as.matrix(table(cut(d3$rescaled.scoring.system.scores, breaks=br, include.lowest=T), d3$AdverseEvents_Patients))
                    count_Escalation_Patients<- as.matrix(table(cut(d3$rescaled.scoring.system.scores, breaks=br, include.lowest=T), d3$Escalation_Patients))
                    count<- cbind(count_AdverseEvents_Patients[,1], count_Escalation_Patients[,2], count_Complication_Patients[,2])
                    
                    par(mfrow=c(1,1), mar=c(8,8,2,2), new=FALSE)
                    barplot(t(count), main="", xlab="", ylab="", space=0, col=c("lightblue", "darkblue","red"), axes=F, xaxt='n', ylim = c(0,40))
                    axis(side = 2, pos = -0.2, las=2, cex.axis=1.2, lwd=2)
                    mtext("Observed number of patients", side=2, line=3, cex=2)
                    axis(side = 1, at = seq_len(nrow(count) + 1) - 1, labels = br, cex.axis=1.2, lwd=2)
                    mtext("Scores", side=1, line=3, cex=2)
                    
                    legend("topright", fill=c("lightblue","darkblue","red"), lty=c(NA,NA,NA), lwd=c(NA,NA,NA),legend=c("No Adverse Event","Escalation","Complication"), col=c("Black"), border=c("black","black","black"), bty='n', x.intersp = -1.5)
                    par(new=TRUE)
                    
                    glmod.lm<- glm(AdverseEvents_Patients ~ rescaled.scoring.system.scores, data = d3, family=binomial(link = "logit"))
                    glmod<- glmer(AdverseEvents_Patients ~ rescaled.scoring.system.scores + (1 | Demo_IDn), data = d3, family=binomial(link = "logit"),
                                            start=list(fixef=coef(glmod.lm)))
                    summary(glmod)
                    
                    
                    
                    
                    
                        