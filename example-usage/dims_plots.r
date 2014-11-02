source("../courseviz.r")

####### Read data ########
data <- setup(default_dir="data-dims")
participants <- data$participants
handins <- data$handins
assignments <- data$assignments

# Udvælg øvelseshold
#handins <- selectClass(handins, 2)

# Opdel i Prøver og hjemmeogpaver
hjemmeopgaver <- chooseAssignments(handins, "Hjemmeopgave")
test1 <- chooseAssignments(handins, "Prøve 1")

####### Print statistics ########
printStatistics(handins, participants)

####### Create plots ########
plotHandins <- handinsPerAssignment(handins)
dotplotPoints <- dotplotPerAssignment(hjemmeopgaver)
boxplotPoints <- boxplotPerAssignment(hjemmeopgaver)
test1hist <- assignmentPointHistogram(test1) + ggtitle("Prøve 1")

# Fjern den ikke point-givende ugeopgave 1
handins <- removeAssignments(handins, "Hjemmeopgave 0")

plotAverages <- averagePointsHistogram(handins,binwidth=5) + scale_x_continuous(limits = c(0, 100))
plotStatus <- statusCounts(handins)
plotClassStatus <- statusPerClass(handins)

dropOutPlot <- dropOut(participants)


####### Save PDF ########
pdf(paste(data$directory,"/dims_allplots.pdf", sep=""), width=15, height=12)
grid.arrange(plotHandins,plotStatus,
             plotAverages, dotplotPoints,
             test1hist, boxplotPoints,
             plotClassStatus, dropOutPlot,
             ncol=2)
dev.off()
