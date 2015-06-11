###################################################
########## functions and libraries ################
###################################################

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
	require(plyr)
	length2 <- function (x, na.rm=FALSE) {
		if (na.rm) sum(!is.na(x))
        else       length(x)
    		}
    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
	datac <- ddply(data, groupvars, .drop=.drop,
	.fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = round(mean   (xx[[col]], na.rm=na.rm), digits=2),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
     	},
      measurevar
    )
    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))
    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult
    return(datac)
}
superpose.eb <- function (x, y, ebl, ebu = ebl, length = 0.08, ...)
  arrows(x, y + ebu, x, y - ebl, angle = 90, code = 3,
  length = length, ...) # from http://users.fmg.uva.nl/rgrasman/rpages/2005/09/error-bars-in-plots.html

library("ggplot2")
library("reshape2")
library(plyr)


###################################################
################ read data ########################
###################################################

setwd("C:/Users/scheibha/Dropbox/04_Hannah/R")
setwd("/Users/hannahscheibner/.dropbox-two/Dropbox/04_Hannah/R")
setwd("
MyData <- read.table(
  "150529_AllData.dat",
  header=TRUE, na.strings="NA", dec=".")
MyData <- rename(MyData, c("ï..ID"="ID"))

origdata.wide = MyData[MyData$Ausschluss_Main<1,]
data.long = melt(origdata.wide, id.vars=c("ID","group", "Ausschluss_Main"))
dfc <- summarySE(data.long, measurevar="value", groupvars=c("variable", "group"))



png("Figure2.png", width=20, height=20, units="cm", res=300)
par(mfrow=c(3,2))



###################################################
################ Graph 1   ########################
###################################################

dfcPercentMF <- dfc[dfc$variable=="PercentMF",]
approachesA = (matrix( c(dfcPercentMF$value), 2, 1)) # create a 2-by-2 matrix, then transpose it
colnames(approachesA) = c("")
rownames(approachesA) = c("BPD", "HC")
semsA = (matrix(c(dfcPercentMF$se), 2, 1)) # do the same for the SEMs
rownames(sems) = c("BPD", "HC")
colnames(sems) = c("")


################plot
#png("2_MWMF.png", width=20, height=20, units="cm", res=200)
fillcolours = c("gray","white")
x.abscis <- barplot(
  approachesA, beside=TRUE,
  col=fillcolours,
  space=c(0.5,2), # spacing between bars in the same group, and then between groups
  width=1, # bar widths
  ylim=c(0,100),
  #yaxp=c(1,100,50),
  #coord_cartesian(ylim=c(1,3)),
  ylab="Percent %",
  xlim=c(1.5,5.5), # makes sense in the context of width and space parameters
  xlab="Mindful Episodes (Task 1)",
  #axis.lty=1, # enable tick marks on the X axis
  font.lab=2, # bold for axis labels
  las = 1,
  cex.axis=1.5, cex.names=1.5
)
box(bty="L")
superpose.eb(x.abscis, approachesA, ebl=0, ebu=semsA) # +1 SEM, no descending error bar
#legend(x=7, y=2.5, box.lty=0, legend=rownames(approachesA), fill=fillcolours, y.intersp=1)
segments(x0=2.5, y0=80, x1=4.0, y1=80) # add three line segments to make a "comparator" line
segments(x0=2.5, y0=80, x1=2.5, y1=78)
segments(x0=4.0, y0=80, x1=4.0, y1=78)
text(x=3.25, y=88, labels="**", family="sans", font=1, ps=8, cex=1.5) # add a "significance" label
text(x=5.5, y=93, labels="A", family="sans", font=2, ps=8, cex=1.5) # add a "significance" label


###################################################
################ Graph 2   ########################
###################################################

dfcPercentMW <- dfc[dfc$variable=="PercentMW",]
approachesB = (matrix( c(dfcPercentMW$value), 2, 1)) # create a 2-by-2 matrix, then transpose it
colnames(approachesB) = c("")
rownames(approachesB) = c("BPD", "HC")
semsB = (matrix(c(dfcPercentMW$se), 2, 1)) # do the same for the SEMs
rownames(semsB) = c("BPD", "HC")
colnames(semsB) = c("")


fillcolours = c("gray","white")
x.abscis <- barplot(
  approachesB, beside=TRUE,
  col=fillcolours,
  space=c(0.5,2), # spacing between bars in the same group, and then between groups
  width=1, # bar widths
  ylim=c(0,100),
  #yaxp=c(1,100,50),
  #coord_cartesian(ylim=c(1,3)),
  ylab="Percent %",
  xlim=c(1.5,5.5), # makes sense in the context of width and space parameters
  xlab="Mind-wandering Episodes (Task 1)",
  #axis.lty=1, # enable tick marks on the X axis
  font.lab=2, # bold for axis labels
  las = 1,
  cex.axis=1.5, cex.names=1.5
)
box(bty="L")
superpose.eb(x.abscis, approachesB, ebl=0, ebu=semsB) # +1 SEM, no descending error bar
#legend(x=7, y=2.5, box.lty=0, legend=rownames(approachesB), fill=fillcolours, y.intersp=1)
segments(x0=2.5, y0=80, x1=4.0, y1=80) # add three line segments to make a "comparator" line
segments(x0=2.5, y0=80, x1=2.5, y1=78)
segments(x0=4.0, y0=80, x1=4.0, y1=78)
text(x=3.25, y=88, labels="**", family="sans", font=1, ps=8, cex=1.5) # add a "significance" label
text(x=5.5, y=93, labels="B", family="sans", font=2, ps=8, cex=1.5) # add a "significance" label




###################################################
################ Graph 3   ########################
##################################################

dfcTimes <- dfc[dfc$variable=="Anzahl_Exp2",]
approachesC = (matrix( c(dfcTimes$value), 2, 1)) # create a 2-by-2 matrix, then transpose it
colnames(approachesC) = c("")
rownames(approachesC) = c("BPD", "HC")
semsC = (matrix(c(dfcTimes$se), 2, 1)) # do the same for the SEMs
rownames(semsC) = c("BPD", "HC")
colnames(semsC) = c("")

#png("3_Refocusing.png", width=10, height=20, units="cm", res=200)
fillcolours = c("gray","white")
x.abscis <- barplot(
  approachesC, beside=TRUE,
  col=fillcolours,
  space=c(0.5,2), # spacing between bars in the same group, and then between groups
  width=1, # bar widths
  ylim=c(0,40),
  ylab="Count (n)",
  xlim=c(1.5,5.5), # makes sense in the context of width and space parameters
  xlab="Refocusing Episodes (Task 2)",
  font.lab=2, 
  #axis.lty=1, # enable tick marks on the X axis
  las = 1,
  cex.axis=1.5, cex.names=1.5
)
box(bty="L")
superpose.eb(x.abscis, approachesC, ebl=0, ebu=semsC) # +1 SEM, no descending error bar
#legend(x=7, y=2.5, box.lty=0, legend=rownames(approaches), fill=fillcolours, y.intersp=1)
segments(x0=2.5, y0=32, x1=4.0, y1=32) # add three line segments to make a "comparator" line
segments(x0=2.5, y0=32, x1=2.5, y1=31)
segments(x0=4.0, y0=32, x1=4.0, y1=31)
text(x=3.25, y=36, labels="**", family="sans", font=1, ps=8 , cex=1.5) # add a "significance" label
text(x=5.5, y=36.5, labels="C", family="sans", font=2, ps=8, cex=1.5) # add a "significance" label


#dev.off()

###################################################
################ Graph 4   ########################
##################################################
dfcRefocusing <- dfc[dfc$variable=="PercentRefocusingNew",]
approachesD = (matrix( c(dfcRefocusing$value), 2, 1)) # create a 2-by-2 matrix, then transpose it
colnames(approachesD) = c("")
rownames(approachesD) = c("BPD", "HC")
semsD = (matrix(c(dfcTimes$se), 2, 1)) # do the same for the SEMs
rownames(semsD) = c("BPD", "HC")
colnames(semsD) = c("")

################Plot
#png("4_Refocusing.png", width=10, height=20, units="cm", res=200)
fillcolours = c("gray","white")
x.abscis <- barplot(
  approachesD, beside=TRUE,
  col=fillcolours,
  space=c(0.5,2), # spacing between bars in the same group, and then between groups
  width=1, # bar widths
  ylim=c(0,65),
las = 1,
#xpd = FALSE,
  ylab="Count / Percent",
  xlim=c(1.5,5.5), # makes sense in the context of width and space parameters
  xlab="Refocusing Episodes (Task 2) /\nMind-wandering Episodes (Task 1)",
  #axis.lty=1, # enable tick marks on the X axis
  font.lab=2, # bold for axis labels
	cex.axis=1.5, cex.names=1.5
)
box(bty="L")
superpose.eb(x.abscis, approachesD, ebl=0, ebu=semsD) # +1 SEM, no descending error bar
#legend(x=7, y=2.5, box.lty=0, fill=fillcolours, y.intersp=1)
segments(x0=2.5, y0=50, x1=4.0, y1=50) # add three line segments to make a "comparator" line
segments(x0=2.5, y0=50, x1=2.5, y1=49)
segments(x0=4.0, y0=50, x1=4.0, y1=49)
text(x=3.25, y=56, labels="ns.", family="sans", font=3, ps=8, cex=1.5) # add a "significance" label
text(x=5.5, y=56.5, labels="D", family="sans", font=2, ps=8, cex=1.5) # add a "significance" label


#dev.off()

###################################################
################ Graph 1   ########################
###################################################

dfcLength1and2 <- dfc[dfc$variable=="Mean_Q_EI_2" | dfc$variable=="Mean_Q_PI",]
approachesE = (matrix( c(dfcLength1and2$value), 2, 2)) # create a 2-by-2 matrix, then transpose it
colnames(approachesE) = c("", "")
rownames(approachesE) = c("BPD", "HC")
semsE = (matrix(c(dfcLength1and2$se), 2, 2)) # do the same for the SEMs
rownames(semsE) = c("BPD", "HC")
colnames(semsE) = c("","")

################plot
#png("5_LengthMW.png", width=20, height=20, units="cm", res=200)
fillcolours = c("gray","white")
x.abscis <- barplot(
  approachesE, beside=TRUE,
  col=fillcolours,
  space=c(0.5,2), # spacing between bars in the same group, and then between groups
  width=1, # bar widths
  ylim=c(1,3),
	xpd = FALSE,
  yaxp=c(1,3,2),
  #coord_cartesian(ylim=c(1,3)),
  ylab="Mean Response",
  xlim=c(1.5,9.5), # makes sense in the context of width and space parameters
  xlab="Length of Mind-wandering Episodes\n(Task 1 & 2)",
  #axis.lty=1, # enable tick marks on the X axis
  font.lab=2, # bold for axis labels
  
  las = 1,
  cex.axis=1.5, cex.names=1.5
)
box(bty="L")
superpose.eb(x.abscis, approachesE, ebl=0, ebu=semsE) # +1 SEM, no descending error bar
#legend(x=7, y=2.5, box.lty=0, legend=rownames(approachesE), fill=fillcolours, y.intersp=1)
segments(x0=2.5, y0=2.4, x1=4.0, y1=2.4) # add three line segments to make a "comparator" line
segments(x0=2.5, y0=2.4, x1=2.5, y1=2.35)
segments(x0=4.0, y0=2.4, x1=4.0, y1=2.35)
text(x=3.25, y=2.6, labels="*", family="sans", font=1, ps=8, cex=1.5) # add a "significance" label
segments(x0=7, y0=2.1, x1=8.5, y1=2.1) # add three line segments to make a "comparator" line
segments(x0=7, y0=2.1, x1=7, y1=2.05)
segments(x0=8.5, y0=2.1, x1=8.5, y1=2.05)
text(x=7.75, y=2.3, labels="ns.", family="sans", font=3, ps=8, cex=1.5) # add a "significance" label
text(x=9.2, y=2.8, labels="E", family="sans", font=2, ps=8, cex=1.5) # add a "significance" label

#dev.off()
plot(c(-2, 10), c(0, 10), type = "n",xlab="", ylab = "", xlim=c(0, 10), xaxt = "n", yaxt = "n", frame.plot=F,)
#box(bty="L")
rect(0.5, 0, 2, 4, col ="gray", border = "black")
rect(0.5,5,2,9, col= "white", border = "black")
text(x=3, y=2.5, labels="BPD", family="sans", font=1, ps=8, cex=1.5) # add a "significance" label
text(x=3, y=7.5, labels="HC", family="sans", font=1, ps=8, cex=1.5) # add a "significance" label

dev.off()



detach(fig23) # clean up
rm(fig23, approaches, sems, fillcolours, x.abscis)
