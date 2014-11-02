library("ggplot2")
library("scales")
library("plyr")
library("gridExtra")

# Change color theme to colorbrewer2.org's "Set2" pallette
# It's just a nicer default :-)
scale_fill_discrete <- function(...) scale_fill_brewer(... , palette="Set2")


setup <- function(default_dir = "data") {
    args <- commandArgs(trailingOnly = TRUE)
    if (length(args) < 1) {
        warning(paste ("No argument given, using directory '", default_dir, "'", sep=""))
        directory <- default_dir
    } else {
        directory <- args[1]
    }
    
    participantsFilename = paste(directory, "/participants.csv", sep="")
    assignmentsFilename = paste(directory, "/assignments.csv", sep="")
    handinFilename = paste(directory, "/handins.csv", sep="")
    
    # Read CSV's
    participants <- read.csv(participantsFilename, stringsAsFactors=FALSE)
    assignments <- read.csv(assignmentsFilename, stringsAsFactors=FALSE)
    handins <- read.csv(handinFilename, stringsAsFactors=FALSE)
  
    handins[is.na(handins$score),]$score <- 0

    # Remove teachers, administrators etc.
    participants <- participants[participants$role == "Student",]
    
    # Shorten name for "resubmit" status
    handins[handins$status=="Not satisfactory, submit new answer",]$status <- "Resubmit"
  
    h <- merge(handins, participants)

    # Create "exercise class -1" for students in no class
    if (any(is.na(h$class))) {
        h[is.na(h$class),]$class <- -1
    }
    
    # Make factors
    h$class      <- factor(h$class)
    h$status     <- factor(h$status)
    h$assignment <- factor(h$assignment)
    h$name       <- factor(h$name)
    h$username   <- factor(h$username)

    # Reverse the order of assignment levels
    #  -- the order in which assignments are displayed
    h$assignment <- factor(factor(h$assignment),
                           levels=rev(levels(h$assignment)))


    ret <- list("participants" = participants,
                "assignments"  = assignments,
                "handins"      = h,
                "directory"    = directory)
    
    return (ret)
}

# Only keep hand-ins for a single exercise class
selectClass <- function(handins, classId) {
    if(!(classId %in% handins$class)) {
        stop(paste("No exercise class with id ", classId))
    }

    return(handins[handins$class==classId,])
}

printStatistics <- function(handins, participants) {
    print (paste("Students: ", (nrow(participants))))

    data <- removeNotSubmitted(handins)
    df <- as.data.frame(ftable(data$status))
    colnames(df) <- c("Status", "Count")
    print(df, row.names = FALSE)

    df2 <- as.data.frame(ftable(data$assignment))
    colnames(df2) <- c("Assignment", "Count")
    print(df2, row.names = FALSE)
}

# Hand-in selectors
removeNotSubmitted <- function(handins) {
    return(handins[handins$status != "Not submitted",])
}

removeSubmitted <- function(handins) {
    return(handins[handins$status != "Submitted",])
}

removeUnassessed <- function(handins) {
    return(removeSubmitted(removeNotSubmitted(handins)))
}

chooseAssignments <- function(handins, titlePrefix) {
    return(handins[substring(handins$assignment,0,nchar(titlePrefix)) == titlePrefix,])
}

removeAssignments <- function(handins, titlePrefix) {
    return(handins[substring(handins$assignment,0,nchar(titlePrefix)) != titlePrefix,])
}

# Assignment plots
handinsPerAssignment <- function(handins) {
    data <- removeNotSubmitted(handins)
    p <- ggplot(data, aes(assignment,fill=status))
    p <- p + geom_bar()
    p <- p + coord_flip()
    p <- p + xlab("")
    p <- p + ylab("# of hand-ins")

    # Hide legend title
    p <- p + guides(fill=guide_legend(title=NULL))
    return(p)
}

dotplotPerAssignment <- function(handins) {
    data <- removeNotSubmitted(handins)
    p <- ggplot(data, aes(x=assignment,y=score))
    p <- p + geom_dotplot(binaxis="y", binwidth=.5, stackdir="center")
    p <- p + coord_flip()
    p <- p + xlab("")
    p <- p + ylab("Point")
    return(p)
}

boxplotPerAssignment <- function(handins) {
    data <- removeNotSubmitted(handins)
    p <- ggplot(data, aes(x=assignment,y=score))
    p <- p + geom_boxplot()
    p <- p + coord_flip()
    p <- p + xlab("")
    p <- p + ylab("Point")

    # Add diamond at mean, box plot only shows median and quartiles
    p <- p + stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")
}

pointsPerAssignment <- function(handins) {
    data <- removeNotSubmitted(handins)

    p <- ggplot(data, aes(assignment,fill=as.factor(score)))
    p <- p + geom_bar()
    p <- p + coord_flip()
    p <- p + scale_fill_discrete(name = "Point")
    p <- p + xlab("")
    p <- p + ylab("# of hand-ins")
    return(p)
}

# Total / average score
assignmentPointHistogram <- function(handins) {
    p <- ggplot(handins, aes(as.factor(score)))
    p <- p + geom_bar()
    p <- p + xlab("Points")
    p <- p + ylab("# of students")
    return (p)
}

# Total / average score
totalPointsHistogram <- function(handins,binwidth=1) {
    summarized <- ddply(handins, .(username,class), summarize, totalscore = sum(score))

    p <- ggplot(summarized, aes(factor(totalscore),fill=as.factor(class)))
    p <- p + geom_histogram(binwidth=binwidth)
    p <- p + scale_fill_discrete(name = "Exercise class")
    p <- p + xlab("Points")
    p <- p + ylab("# of students")
    return (p)
}

totalPointsCumulative <- function(handins) {
    summarized <- ddply(handins, .(username), summarize, totalscore = sum(score))
    df <- as.data.frame(table(summarized$totalscore))
    colnames(df) <- c("score", "count")
    df[["count"]] <- rev(cumsum(rev(df[["count"]])))

    p <- ggplot(df, aes(factor(score), y=count))
    p <- p + geom_bar(stat="identity")
    p <- p + xlab("Points")
    p <- p + ylab("# of students")
    p <- p + ggtitle("# of students w. total score > x")
    return(p)
}

averagePointsHistogram <- function(handins,binwidth=5) {
    summarized <- ddply(handins, .(username,class), summarize, totalscore = mean(score))

    p <- ggplot(summarized, aes(totalscore, fill=as.factor(class)))
    p <- p + geom_histogram(binwidth=binwidth)
    p <- p + scale_fill_discrete(name = "Exercise class")
    p <- p + xlab(paste("Average score (bin size = ", binwidth, ")", sep=""))
    p <- p + ylab("# of students")

    return (p)
}

# Status
statusCounts <- function(handins) {
    p <- ggplot(handins, aes(status))
    p <- p + geom_bar()
    p <- p + coord_flip()
    p <- p + xlab("")
    p <- p + ylab("# of hand-ins")

    # Hide legend title
    p <- p + guides(fill=guide_legend(title=NULL))
    
    return (p)
}

statusPerClass <- function(handins) {
    p <- ggplot(handins, aes(class, fill=status))
    p <- p + geom_bar()
    p <- p + ylab("# of hand-ins")
    p <- p + xlab("Exercise class")

    # Hide legend title
    p <- p + guides(fill=guide_legend(title=NULL))

    return (p)
}

dropOut <- function(participants) {
    binwidth_days=7
    
    # Place those who didn't visit at an arbitrary date
    participants[participants$lastVisit == "Never visited",]$lastVisit <- "1/08/2014 00:00"

    # Remove recent visitors (to focus on the problems)
    today <- Sys.Date()
    y <- as.POSIXct(strptime(participants$lastVisit, "%d/%m/%Y %H:%M"))
    withoutRecent <- y[difftime(today,y,units="days") > 7]

    p <- qplot(withoutRecent,binwidth=binwidth_days*24*60*60)
    p <- p + scale_x_datetime(labels = date_format("%W"), breaks = date_breaks("2 weeks"))
    p <- p + ggtitle("Time of last Absalon visit")
    p <- p + ylab("# of students")
    p <- p + xlab("(Not counting those active the last 7 days)")
    p <- p + xlab(paste("Week number"))
}
