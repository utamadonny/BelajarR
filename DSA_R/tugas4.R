dt <- read.csv("../data/outcome-of-care-measures.csv", colClasses = "character")
head(dt)
str(dt[,c(2,7,11,17,19,23)])

dt[, 11] <- as.numeric(dt[, 11])
hist(dt[, 11])


best <- function(state, outcome){
    file <- read.csv("../data/outcome-of-care-measures.csv", colClasses = "character")
    outcomes <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
    
	if( outcome %in% outcomes == FALSE ) {
    	stop("invalid outcome")
    }

  	file <- file[c(2, 7, 11, 17, 23)]
	names(file)[1] <- "name"
	names(file)[2] <- "state"
	names(file)[3] <- "heart attack"
	names(file)[4] <- "heart failure"
	names(file)[5] <- "pneumonia"

	states <- file[, 2]
    states <- unique(states)
    if( state %in% states == FALSE ) {
    	stop("invalid state")
	}
	file <- file[file$state==state & file[outcome] != 'Not Available', ]
    vals <- file[, outcome]

    ## RowNum = the index of the minimum value 
    rowNum <- which.min(vals)
	
	## Return hospital name in that state with lowest 30-day death rate
    file[rowNum, ]$name
}

# TEST CASE
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")


rankhospital <- function(state, outcome, num) {
    file <- read.csv("../data/outcome-of-care-measures.csv", colClasses = "character")
    file <- file[c(2, 7, 11, 17, 23)]
    names(file)[1] <- "name"
    names(file)[2] <- "state"
    names(file)[3] <- "heart attack"
    names(file)[4] <- "heart failure"
    names(file)[5] <- "pneumonia"


    outcomes = c("heart attack", "heart failure", "pneumonia")

    if( outcome %in% outcomes == FALSE ) {
    	stop("invalid outcome")
    }

    states <- file[, 2]
    states <- unique(states)
    if( state %in% states == FALSE ) {
    	stop("invalid state")
    }
    
    if( num != "best" && num != "worst" && num%%1 != 0 ) {
    	stop("invalid num")
    }

    file <- file[file$state==state & file[outcome] != 'Not Available', ]

    file[outcome] <- as.data.frame(sapply(file[outcome], as.numeric))
    file <- file[order(file$name, decreasing = FALSE), ]
    file <- file[order(file[outcome], decreasing = FALSE), ]

    vals <- file[, outcome]
    if( num == "best" ) {
        rowNum <- which.min(vals)
    } else if( num == "worst" ) {
        rowNum <- which.max(vals)
    } else {
        rowNum <- num
    }

    file[rowNum, ]$name
}

# TEST CASE
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)

rankall <- function(outcome, num) {
	file <- read.csv("../data/outcome-of-care-measures.csv", colClasses = "character")
	file <- file[c(2, 7, 11, 17, 23)]
	names(file)[1] <- "name"
	names(file)[2] <- "state"
	names(file)[3] <- "heart attack"
	names(file)[4] <- "heart failure"
	names(file)[5] <- "pneumonia"

	outcomes = c("heart attack", "heart failure", "pneumonia")

	if( outcome %in% outcomes == FALSE ) {
		stop("invalid outcome")
	}

	file[outcome] <- as.data.frame(sapply(file[outcome], as.numeric))
	file <- file[order(file$name, decreasing = FALSE), ]
	file <- file[order(file[outcome], decreasing = FALSE), ]

    ## Helper functiont to process the num argument
    getHospByRank <- function(df, s, n) {
        df <- df[df$state==s, ]
        vals <- df[, outcome]
        if( n == "best" ) {
            rowNum <- which.min(vals)
        } else if( n == "worst" ) {
            rowNum <- which.max(vals)
        } else {
            rowNum <- n
        }
        df[rowNum, ]$name
    }
    
    ## For each state, find the hospital of the given rank
    states <- file[, 2]
    states <- unique(states)
    newdata <- data.frame("hospital"=character(), "state"=character())
    for(st in states) {
        hosp <- getHospByRank(file, st, num)
        newdata <- rbind(newdata, data.frame(hospital=hosp, state=st))
    }

    ## Return a data frame with the hospital names and the (abbreviated) state name
    newdata <- newdata[order(newdata['state'], decreasing = FALSE), ]
    newdata
}

# TEST CASE
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)
r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)
r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
