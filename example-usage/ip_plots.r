source("../courseviz.r")

####### Read data ########
data <- setup(default_dir="data-ip")
participants <- data$participants
handins <- data$handins
assignments <- data$assignments

# Udvælg øvelseshold
# handins <- selectClass(handins, 1)

# Fjern Ugens nød
handins <- removeAssignments(handins, "Ugens nød")

# Separer eksamenskvalifikation fra afleveringer
examqual <- chooseAssignments(handins, "Er jeg eksamenskvalificeret?")
handins <- removeAssignments(handins, "Er jeg eksamenskvalificere")

# Specify sorting order for exam-qual plot
examqual$status <- factor(examqual$status, levels = c("Kvalificeret til eksamen", "Ikke kvalificeret til eksamen", "slacker!"))

####### Print statistics ########
printStatistics(handins, participants)


####### Create plots ########
plotHandins <- handinsPerAssignment(handins)
plotPoints <- pointsPerAssignment(handins)

# Fjern den ikke point-givende ugeopgave 1
handins <- removeAssignments(handins, "Individuel opgave 1")
handins <- removeAssignments(handins, "Gruppeopgave 1")

plotTotals <- totalPointsHistogram(handins)
plotTotalsCum <- totalPointsCumulative(handins)
plotStatus <- statusCounts(handins)
plotClassStatus <- statusPerClass(handins)
plotExamQual <- statusCounts(examqual)
plotClassExamQual <- statusPerClass(examqual) + ylab("# of students")


# Only show dropout's for class 1 through 6 (new DIKU students)
participants <- subset(participants, class >= 1 & class <= 6)
dropOutPlot <- dropOut(participants)

####### Save PDF ########
pdf(paste(data$directory,"/ip_allplots.pdf", sep=""), width=15, height=12)
grid.arrange(plotHandins, plotTotals,
             plotPoints, plotTotalsCum,
             plotStatus, plotClassStatus,
             plotExamQual, plotClassExamQual,
             dropOutPlot,
             ncol=2)
dev.off()
