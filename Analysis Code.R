# Paediatric Pulmonary Hypertension Risk Stratification
# May 2022
# Tim Dawes
# 
# To run this you will need the libraries readxl, JointAI and RColorBrewer pre-installed and the accompanying file col_types.R


# Load libraries
    library(readxl)
    library(JointAI)
    library(RColorBrewer)

# Define one new function for converting Likert scale data to numerics
    convert.likaert.text<- function(a,name)
              {
                n<- which(colnames(a)==name)
                new.name<- paste(name,"No",sep="")
                new.col<- unlist(a[,n])

                new.col[which(new.col=="1")]<- "Mod"
                new.col[which(new.col=="0")]<- "None"
                new.col[which(new.col=="Moderate")]<- "Mod"

                new.colNo<- matrix(match(new.col, c("None","Trivial","Mild","Mild/Mod","Mod","Mod/Severe","Severe")), ncol=1)
                table(new.colNo)
                a<- cbind(a, new.colNo)
                colnames(a)[ncol(a)]<- new.name

                return(a)
              }
              
# Define the colour scale
    cols<- brewer.pal(10,"Paired")

# Define your working folder here:
    working.dir<- c("")
    setwd(working.dir)

# Identify the column types in the data file and read them back in
    source("col_types.R")
    col_types = as.character(read.csv("Data/col_types.csv")[,1])
    no.rows = as.numeric(read.csv("Data/number_of_rows.csv")[,1])

# Read in your data
    d<- read_excel("xxx.xlsx", col_types=col_types)[1:no.rows,]
    unique.IDs.1<- unique(d$Demo_HRN)
    length(unique.IDs.1)

# PH diagnosis + undergoing a PVR study (the audit cohort)
    audit.cohort.rows<- which(d$`Q_Include_DxPAH_&_PVRStudy`=="Yes")
    length(audit.cohort.rows)
    
    d2<- d[audit.cohort.rows,]
    unique.IDs.2<- unique(d2$Demo_NHSNo)
    unique.IDs.row<- sapply(unique.IDs.2, function(x) {which.min(match(d2$Demo_NHSNo,x))})
    length(unique.IDs.2)
    
      
# Define a sub-cohort for a study
      
            d2$IDn<- match(d2$Demo_HRN, unique(d2$Demo_HRN))
            
            for (i in 1:11) 
              {
              x<- c("Echo_TRseverity", "Echo_RVdil", "Echo_RVhyp", "Echo_RVdys", "Echo_PRseverity", "Echo_TRseverity", "Echo_RA.dilat", 
                    "Echo_Septalflattening", "Echo_ASD", "Echo_VSD", "Echo_Effusion")[i]
              d2<- convert.likaert.text(d2, x)
            }
            
            d2$Demo_IDn<- match(d2$Demo_HRN, unique(d2$Demo_HRN))
            d2$Echo_Rad.dysfuncNo<- match(d2$Echo_Rad.dysfunc, c("None","1")) - 1
            d2$Echo_Long.dysfuncNo<- match(d2$Echo_Long.dysfunc, c("None","1")) - 1
            
            variables<- c("Echo_RVdysNo","Echo_RVhypNo","Echo_RVdilNo","Echo_TRseverityNo","Echo_TRVel","Echo_SeptalflatteningNo","Echo_PRseverityNo",
                              "Echo_PRVmax","Echo_ASDNo","Echo_EffusionNo", "Echo_TAPSE","Echo_RA.dilatNo","Echo_LVFS","Echo_PADilatation")
            
            rownames(all.coefs)<- variables
            colnames(all.coefs)<- c("Mean","SD","2.5%","97.5%","tail-prob.","GR-crit","MCE/SD")
            
            model<- list()
            d2$Complication_HRcont<- as.numeric(d2$Change_in_HR_percent < -50)
            d2$Complication_BPcont<- as.numeric(d2$Change_in_MBP_percent < -50)
            complications<- d2[, c("Complication_HRcont","Complication_BPcont","Complication_ECG","Complication_Cardiacarrest","Complication_IntraopiNO",
                   "Complication_HDUunplanned","Complication_CICUunplanned","Complication_DiedPeriOp","Complication_RespOther",
                   "Complication_Postopintubated","Complication_Postop_respsupport")]
            
            d2$Complication_Anycont<- as.numeric(apply(complications,1,sum, na.rm=TRUE)!=0)
            
            
            # Complication - All
            
                  for (i in 1:length(variables))
                  {
                    form<- as.formula(paste("Complication_Any ~ ",variables[i],sep=""))
                    cat(variables[i])
                    fit<- lme_imp(form, random = ~ 1 | Demo_IDn, data = d2, n.iter=1000, n.chains = 3, n.adapt = 200, family=binomial())
                    model[[i]]<- fit
                    coefficients[i]<- coef(summary(fit))[[1]][2,5]
                    all.coefs[i,]<- summary(fit)[[6]]$Complication_Any$regcoef[2,]
                  }
                  
            
            # Complication - Haemodynamic
            
                  for (i in 1:length(variables))
                  {
                    a<- scale(-d2$Change_in_MBP_percent[1:93])
                    b<- d2[1:93,match(variables[i], colnames(d2))]
                    data.reg<- data.frame(a=a, b=b, Demo_IDn = d2[1:93,]$Demo_IDn)
                    fit<- lme_imp(a ~ b, random = ~ 1 | Demo_IDn, data = data.reg, n.iter=1000, n.chains = 3, n.adapt = 200, family=gaussian())
                    model[[i]]<- fit
                    coefficients[i]<- coef(summary(fit))[[1]][2,5]
                    all.coefs[i,]<- summary(fit)[[6]]$a$regcoef[2,]
                  }
                  
            
            # Complication - Unplanned escalation
            
                  for (i in 1:length(variables))
                  {
                    a<- scale(d2$Complication_Logistics[1:93])
                    b<- d2[1:93,match(variables[i], colnames(d2))]
                    data.reg<- data.frame(a=a, b=b, Demo_IDn = d2[1:93,]$Demo_IDn)
                    fit<- lme_imp(a ~ b, random = ~ 1 | Demo_IDn, data = data.reg, n.iter=1000, n.chains = 3, n.adapt = 200, family=gaussian())
                    model[[i]]<- fit
                    coefficients[i]<- coef(summary(fit))[[1]][2,5]
                    all.coefs[i,]<- summary(fit)[[6]]$a$regcoef[2,]
                  }
            
            
            
            
            
            
            #Figure 1: Forest plot of coefficients
          
                remove.these.fields<- c("Echo_SeptalflatteningNo","Echo_EffusionNo","Echo_RA.dilatNo","Echo_PADilatation")
                remove.these.rows<- match(remove.these.fields, rownames(all.coefs))

                o<- order(all.coefs[,1])
                o<- setdiff(o, remove.these.rows)
            
                ns.survival<- rownames(all.coefs)[o]
                n.survival<- length(o)
                mean.cols<- 1:n.survival
            
                lower.cols<- mean.cols + 1
                upper.cols<- lower.cols + 1
                pvals.cols<- upper.cols + 1
            
                coefs.survival<- all.coefs[o,c("Mean","2.5%","97.5%","tail-prob.")]
            
                lower<- all.coefs[,"2.5%"]
                mean<- all.coefs[,"Mean"]
                upper<-  all.coefs[,"97.5%"]

                pvals<- all.coefs[,"tail-prob."]
                pvals<- sprintf("%1.2f", pvals)
                pvals[pvals=="1.00"]<- ">0.99"
                pvals[pvals=="0.00"]<- "<0.001"

            # Y height
                y.height<- 5
            
            # X-AXIS
                xmax<- 0.5
                xmin<- -xmax
                lwd<- 7
                
            # Change any coefficients which are out-of-range (oor) to the x-limit value
                oor.min<- which(lower[o]<xmin)
                lower[o][oor.min]<- xmin
                oor.max<- which(upper[o]>xmax)
                upper[o][oor.max]<- xmax
                
            
            
            # Set-up the diagram
              
                w<- 18
                h<- 8
                
                pdf(file="Complication.pdf", width=w, height=h)
                par(mfrow=c(1,1), mar=c(6, 12, 8, 36), xpd=TRUE)
                plot(1:n.survival,1:n.survival, type="n", xlim=c(xmin,xmax), ylim=c(1,y.height), yaxt="n", xaxt="n",xlab="BETA", cex.lab=1.5, font.lab=2, ylab="", bty="n", line=3)
                
            # Add ticks to x- and y-axes
              axis(1, seq(xmin,xmax,0.1), labels = FALSE, lwd=3)
            
            # Add labels to x-axis      
              text(seq(xmin,xmax,length.out=3), rep(0.65, 3), srt = 0, adj = 0.5, labels = sprintf("%+1.1f",seq(xmin,xmax,length.out=3)), xpd = TRUE, cex = 1.5, font=2)
            
            # Y-AXIS
                ys.bars<- seq(1,y.height,length.out=n.survival)
                z<- length(na.omit(all.coefs[,1]))
                y.counter<- (1:z)
                
            # Add segments  
                marker.size<- 0.05
            
            # Add vertical lines at 'zero'
                segments(0, .8, 0, y.height+0.2, lty=2, lwd=4)
                
            # Add the text around the plot
                text.cex<- 1.4
                lab.cex<- 1.4
                col.inc<- 0.15
                graph.inc<- xmax + 0.4
                top.header.inc<- 0.55
                
                # Better/Worse Survival
                    ns.labels<- c("No RV dysfunction", "No RV hypertrophy", "No RV dilatation", "No TR","Low TR velocity","No septal flattening","No PR",
                                  "Low PR Velocity","No ASD","No effusion","Low TAPSE","No RA dilatation","Low LVFS","No PA dilatation")
                    text(rep(xmin,n.survival)-0.01, ys.bars, adj = 1, labels = ns.labels[o], xpd = TRUE, cex = text.cex, font=2)
                    
                    ns.labels<- c("RV dysfunction", "RV hypertrophy", "RV dilatation", "Severe TR", "High TR velocity","Septal flattening","Severe PR",
                                  "High PR Velocity","Large ASD","Effusion", "High TAPSE", "RA dilatation","High LVFS","PA dilatation")
                    text(rep(xmax,n.survival)+0.01, ys.bars, adj = 0, labels = ns.labels[o], xpd = TRUE, cex = text.cex, font=2)
                    
                # Left of graph
                    text(xmin-0.01, y.height+top.header.inc, labels="Fewer Complications",cex=text.cex, font=2, adj=1)
                
                # Right of graph
                    text(xmax+0.01, y.height+top.header.inc, labels="More Complications",cex=text.cex, font=2, adj=0)
                
                # Beta
                    text(rep(graph.inc,20), c(ys.bars,y.height+top.header.inc), labels=c(sprintf("%+1.2f",mean[o]),"Beta"),xpd = TRUE, cex=text.cex, font=2)
                
                # Credible intervals for beta
                    text(rep(graph.inc+col.inc,length(ys.bars)+1), c(ys.bars,y.height+0.35), labels=c(sprintf("%+1.2f", c(all.coefs[o,3])), "Lower"),cex=lab.cex, font=2, adj=0.5)
                    text(rep(graph.inc+col.inc*2,length(ys.bars)+1), c(ys.bars,y.height+0.35), labels=c(sprintf("%+1.2f", c(all.coefs[o,4])), "Upper"),cex=lab.cex, font=2, adj=0.5)
                    text(graph.inc+col.inc*1.5, max(ys.bars)+top.header.inc, labels="Confidence Interval",cex=lab.cex, font=2, adj=0.5)
                    text(rep(graph.inc+col.inc*3,12), c(ys.bars,y.height+0.35, y.height+top.header.inc), labels=c(pvals[o],"","P-value"),cex=lab.cex, font=2, adj=0.5)
                    
              # Posterior samples
            
                    for (i in 1:z)
                    {
                      for (j in 1:3) # was 'imputations but the .pdf is massive
                      {
                        xs<- model[[o[i]]]$MCMC[[j]][,3]
                        xs[xs>xmax]<- NA
                        xs[xs<xmin]<- NA
                        random.y<- runif(length(xs),0,0.05) - 0.025
                        
                          ys<- rep(ys.bars[i],length(xs)) + random.y
                          points(xs, ys, cex=1, col=alpha(cols[5], 0.4), pch=20)
                        }
                      }
              
              # Bars
                    cols.bars<- rep(cols[6], each=n.survival)
                    segments(lower[o], ys.bars, upper[o], ys.bars, lwd=lwd, col=rgb(0.8,0,0,1))
                    
              # LEFT end markers
                    tips<- setdiff(1:length(lower), oor.min)
                    segments(lower[o][tips], ys.bars[tips]-marker.size, lower[o][tips], ys.bars[tips]+marker.size, lwd=lwd, col=rgb(0.8,0,0,1))
                    
              # RIGHT end markers
                    tips<- setdiff(1:length(upper), oor.max)
                    segments(upper[o][tips], ys.bars[tips]-marker.size, upper[o][tips], ys.bars[tips]+marker.size, lwd=lwd, col=rgb(0.8,0,0,1))
                    
              # OOR arrows
                    for (i in oor.min) {segments(lower[o][i]+marker.size, ys.bars[i]+marker.size, xmin, ys.bars[i], lwd=lwd, col=cols.bars[i]);
                      segments(lower[o][i]+marker.size, ys.bars[i]-marker.size, xmin, ys.bars[i], lwd=lwd, col=cols.bars[i])}
                    
                    for (i in oor.max) {segments(upper[o][i]-0.05, ys.bars[i]+marker.size, xmax, ys.bars[i], lwd=lwd, col=cols.bars);
                      segments(upper[o][i]-0.05, ys.bars[i]-marker.size, xmax, ys.bars[i], lwd=lwd, col=cols.bars[i])}
                    
              # Add point spheres
                    points(mean[o], ys.bars, lwd=10, cex=1, col=rgb(0.4,0,0,1), pch=19)
             
            dev.off()
            
            
            
           
            # Boxplot of drop in blood pressure for differing levels of RV dysfunction
                  a<- d2$Change_in_MBP_percent
                  b<- d2$Echo_TRVel

                  jpeg(file="BoxplotMAP_TR.jpeg", width=600, height=800)
                  
                  par(mar=c(6,6,2,2), mfrow=c(1,1))
                  boxplot(a ~ b, xlab = "Severity of RV dysfunction", ylab = "Drop in MAP during procedure (mmHg)", cex.lab = 2, frame.plot=FALSE, col = "lightblue", lwd=4, axes=FALSE)
                  axis(side=1, lwd=4, at=1:5, labels=c("None","","Moderate","","Severe"), cex.axis=1.2, cex.lab=2)
                  axis(side=2, lwd=4, at=seq(-50,0,10), labels=c("-50","-40","-30","-20","-10","0"), las=2, cex.axis=1.2)
                  
                  dev.off()

             # ANOVA of group differences
                  anova(lm(b ~ a))
            

            
            
            
            
            
            
