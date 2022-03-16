# library(data.table)
# 
# pollutantmean <- function(directory, pollutant, id = 1:332) {
#   fileNames <- paste0(directory, '/', formatC(id, width=3, flag="0"), ".csv" )
# 
#   lst <- lapply(fileNames, data.table::fread)
#   dt <- rbindlist(lst)
# 
#   if (c(pollutant) %in% names(dt)){
#     return(dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = pollutant][[1]])
#   }
# }
# 
# #pollutantmean(directory ='../specdata', pollutant = 'sulfate', id=1)
# ##################
# 
# complete <- function(directory,  id = 1:332) {
#   fileNames <- paste0(directory, '/', formatC(id, width=3, flag="0"), ".csv" )
# 
#   lst <- lapply(fileNames, data.table::fread)
#   dt <- rbindlist(lst)
# 
#   return(dt[complete.cases(dt), .(nobs = .N), by = ID])
# }
# 
# # complete(directory = '../specdata', id = 20:25)
# ####################
# 
# corr <- function(directory, threshold = 0) {
#   lst <- lapply(file.path(directory, list.files(path = directory,pattern ="*.csv")), data.table::fread)
#   dt <- rbindlist(lst)
# 
#   dt <- dt[complete.cases(dt),]
#   dt <- dt[, .(nobs = .N, corr = cor(x = sulfate, y = nitrate)), by = ID][nobs > threshold]
# 
#   return(dt[, corr])
# }
# 
# # Example Usage
# corr(directory = '../specdata', 150)

pollutantmean <- function(directory, pollutant, id=1:332){
	pollutants <- c()
	for(i in id){
		files <- paste(getwd(),"/",directory,"/",sprintf("%03d",i),".csv",sep="")
		data <- read.csv(files)
		pollutants <- c(pollutants,data[pollutant][!is.na(data[pollutant])])
	}
	mean(pollutants)
}

# pollutantmean(directory ='../specdata', pollutant = 'sulfate', id=1:10)
pollutantmean("../specdata", "sulfate", 1:10)
pollutantmean("../specdata", "nitrate", 70:72)
pollutantmean("../specdata", "sulfate", 34)
pollutantmean("../specdata", "nitrate")

complete <- function (directory, id = 1:332) {
	for(i in id){
		files <- paste(getwd(),"/",directory,"/",sprintf("%03d",i),".csv",sep="")
		data <- read.csv(files)
		nobs <- data[complete.cases(data), ]
		results <- rbind(results, data.frame(id = i, nobs = nrow(nobs)))
	}
	results
}

# complete(directory = "../specdata", id=30:25)
cc <- complete("../specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)
cc <- complete("../specdata", 54)
print(cc$nobs)

corr <- function(directory, threshold = 0){
	correlations <- c()
	data <- list.files(directory, full.names = TRUE)
	for(i in 1:332){
		files <- read.csv(data[i], header = TRUE)[complete.cases(read.csv(data[i], header = TRUE))]
		if (nrow(files) > threshold){
			correlations <- c(correlations, cor(files$nitrate, files$sulfate))
		}
	}
	correlations
}

# corr(directory = "../specdata", threshold = 150)
# head(corr(directory = "../specdata", threshold = 150))
RNGversion("3.5.1")  
set.seed(42)
cc <- complete("../specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])
cr <- corr("../specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)
cr <- corr("../specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)
cr <- corr("../specdata", 2000)                
n <- length(cr)                
cr <- corr("../specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))