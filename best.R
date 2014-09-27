best <- function(state, outcome) {
    data <- read.csv("outcome-of-care-measures.csv")

    switch(outcome,
            "heart attack" = { col <- 13 },
            "heart failure" = { col <- 19 },
            "pneumonia" = { col <- 25 },
            { stop("invalid outcome") })

    m <- levels(data$State)

    if (as.character((state %in% m)) == "FALSE") {
        stop("invalid state")
    }

    is.na(data[,col]) <- data[,col] == "Not Available"

    data[,col] <- as.character(data[,col])
    data[,col] <- as.numeric(data[,col])

    byState <- subset(data, data$State == state)
    minInState <- min(byState[,col], na.rm=TRUE)

    hospitalName <- subset(byState[,2], byState[,col]==minInState)
    as.character(hospitalName[1])
}
