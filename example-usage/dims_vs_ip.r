source("../courseviz.r")

####### Read data ########
dataIP <- setup(default_dir="data-ip")
participantsIP <- dataIP$participants

dataDiMS <- setup(default_dir="data-dims")
participantsDiMS <- dataDiMS$participants

# Only show dropout's for class 1 through 6 on IP (new DIKU students)
participantsIP <- subset(participantsIP, class >= 1 & class <= 6)

###### Create plots ######
dropOutPlotIP <- dropOut(participantsIP) + ylim(0, 5) + ggtitle("IP")
dropOutPlotDiMS <- dropOut(participantsDiMS) + ylim(0, 5) + ggtitle("DiMS")

####### Save PDF ########
pdf("dropout_comparison.pdf", width=11, height=9)
grid.arrange(dropOutPlotIP, dropOutPlotDiMS, ncol=1)
dev.off()
