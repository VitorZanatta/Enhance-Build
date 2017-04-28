# Sys.setenv("R_LIBS_USER" = "C:/Users/vitor/Documents/IdeaProjects/rtest/R/win-library/3.3/library")
dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE)

neccesities <- c("timeSeries", "quantmod", "fPortfolio", "timeDate", "fBasics", "fAssets", "JavaGD")

for(val in neccesities){
  if(!require(val,character.only=TRUE)){
    install.packages(val, repos='http://cran.us.r-project.org', dependencies = TRUE)
    library(val,character.only=TRUE)
  }
}

#sDate <- as.Date("2012-09-04") #Start Date
#eDate <- as.Date("2014-09-02") #End   Date

options(download.file.method="libcurl")

get.symbol <- function(ticker) {  
  tryCatch(temp <- dailyReturn(Ad(getSymbols(ticker, auto.assign=FALSE, 
                                             from = sDate, to = eDate, warning = FALSE))),    
           error = function(e) {
             message("-ERROR-")
             message(paste("Error for ticker symbol  :", ticker, "\n"))
             message("Here's the original error message: ")
             message(e)
             message("")
             return(NULL)},
           finally = {
             message(paste("Data was processed for symbol:", "[", ticker, "]" ))
             message("\n","******************************************", "\n")  
           }) 
}
result <- do.call(cbind,lapply(ticker_symbol,get.symbol))
result[is.na(result)] <- 0
names(result) <- gsub("\\..+","",ticker_symbol)  # remove ".Adjusted" from names
head(result)

results <- as.timeSeries.xts(result)

#------------------
pfolioData  <-  results[,1:(ncol(results))]


#create portfolio specification
frontierSpec  <- portfolioSpec()

#optimization criteria - CVaR
setType(frontierSpec)  <- "CVAR"

#set optimization algorithm
setSolver(frontierSpec)  <- "solveRglpk.CVAR"

setOptimize(frontierSpec) <- 'maxReturn'

#set confidence level CVaR
setAlpha(frontierSpec)  <- cvar_level

#number of portfolios in efficient frontier
setNFrontierPoints(frontierSpec)  <- 100

#setting the weights
# maxWeights does not work
# minWeights does not work for all values
#box.min = paste("minW[1:", length(ticker_symbol), "]=",minw, sep = "")

#box.max = paste("maxW[1:", length(ticker_symbol), "]=","1.0", sep = "")
#cons = c(box.min, box.max)

#bothcons <- c(box.min, box.max)
bothcons = "LongOnly"

#optimize, without shortselling
frontier <- portfolioFrontier(data = pfolioData, spec = frontierSpec, constraints=bothcons);

#build efficient frontier graph
setwd("~/R")
png(filename='frontierplot.png')
tailoredFrontierPlot(object=frontier,mText="Mean-CVaR Frontier (Long only)",risk="CVaR");
dev.off()

#Start Minimal CVaR Efficient Portfolio
#efficient object
efficient <- efficientPortfolio(data = pfolioData, spec = frontierSpec, constraints=bothcons)
print(getPortfolio(efficient))

#portfolio weights
efficientPortfolioWeights <- getWeights(efficient)

#target return
efficientTargetReturn <- getTargetReturn(efficient)

#target risk
efficientTargetRisk <- getTargetRisk(efficient)
#End Minimal CVaR Efficient Portfolio


#Start Sharpe CVaR Tangency Portfolio
#tangency object
tangency <- tangencyPortfolio(data = pfolioData, spec = frontierSpec, constraints=bothcons)
print(getPortfolio(tangency))

#portfolio weights
tangencyPortfolioWeights <- getWeights(tangency)

#target return
tangencyTargetReturn <- getTargetReturn(tangency)

#target risk
tangencyTargetRisk <- getTargetRisk(tangency)
#End Sharpe CVaR Tangency Portfolio

# Start List assignment
frontierPortfolio <- getPortfolio(frontier)

#split lists into dataframes
frontierWeights <- frontierPortfolio[["weights"]]
frontierBudget <- frontierPortfolio[["covRiskBudgets"]]
frontierReturn <- frontierPortfolio[["targetReturn"]]
frontierRisk <- frontierPortfolio[["targetRisk"]]

#assign dataframes into individual lists
fWeights <- setNames(split(frontierWeights, rep(1:ncol(frontierWeights), each = nrow(frontierWeights))), colnames(frontierWeights))
fBudget <- setNames(split(frontierBudget, rep(1:ncol(frontierBudget), each = nrow(frontierBudget))), colnames(frontierBudget))
fReturn <- setNames(split(frontierReturn, rep(1:ncol(frontierReturn), each = nrow(frontierReturn))), colnames(frontierReturn))
fRisk <- setNames(split(frontierRisk, rep(1:ncol(frontierRisk), each = nrow(frontierRisk))), colnames(frontierRisk))

rm(frontierWeights, frontierBudget, frontierReturn, frontierRisk)