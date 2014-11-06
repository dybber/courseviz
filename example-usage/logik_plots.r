source("../courseviz.r")

####### Read data ########
data <- setup(default_dir="data-logik")
participants <- data$participants
handins <- data$handins
visits <- data$visits
assignments <- data$assignments

####### Print statistics ########
printStatistics(handins, participants)

####### Create plots ########
plotHandins <- handinsPerAssignment(handins)
plotAssessments <- assessmentPerAssignment(handins)
plotTotals <- totalPassedHistogram(handins)
plotStatus <- statusCounts(handins)
plotClassStatus <- statusPerClass(handins)
dropOutPlot <- dropOut(visits)

####### Save PDF ########
pdf(paste(data$directory,"/logik_allplots.pdf", sep=""), width=15, height=12)
grid.arrange(plotHandins,
             plotTotals,
             plotAssessments,
             plotStatus,
             plotClassStatus,
             dropOutPlot,
             ncol=2)
dev.off()
