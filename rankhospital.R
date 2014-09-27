rankhospital <- function(state, outcome, num) {

    data <- read.csv("outcome-of-care-measures.csv")

    switch(outcome,
                "heart attack" = { col <- 11 },
                "heart failure" = { col <- 17 },
                "pneumonia" = { col <- 23 },
                { stop("invalid outcome") })

    m <- levels(data$State)

    if (as.character((state %in% m)) == "FALSE") {
        stop("invalid state")
    }

    is.na(data[,col]) <- data[,col] == "Not Available"

    data[,col] <- as.character(data[,col])
    data[,col] <- as.numeric(data[,col])

    sub <- subset(data, data$State==state)

    if (num == "best") {
        num <- 1
    }

    sorted <- sort(sub[,col])

    if (num == "worst") {
        num <- length(sorted)
    }

    n <- sorted[num]

    list <- subset(sub$Hospital.Name, sub[,col] == n)

    as.character(list[1])
}