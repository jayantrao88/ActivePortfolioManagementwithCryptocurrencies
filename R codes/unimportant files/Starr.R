
library(fBasics)
library(readxl)
library(gdata)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(ROI)
library(flextable)
library(officer)
library(dplyr)
library(gdata)
library(profvis)
library(tictoc)
library(rlist)
library(parallel)
library(DEoptim)
library(pso)
library(GenSA)
library(pbmcapply)




if (length(PortfolioWeights[["Starr"]]) != 0 ){PortfolioWeights[["Starr"]] <- list()}
if (length(PortfolioOptimization[["Starr"]]) != 0 ){PortfolioOptimization[["Starr"]] <- list()}

simreturns <- Sim.Returns
Pweights <- list()
Popt <- list()  

for (CopulaType in 1:4){
  tic(sprintf("CopulaType %d", CopulaType))  
  AssetsforOptimization = simreturns[[CopulaType]]
  NoofRebalPeriod = length(AssetsforOptimization)
  weights <-  opt <- list()
  
  Staropt <- function(AssetOptim){
    init.portfolio <- portfolio.spec(assets = colnames(AssetOptim))
    #init.portfolio <- add.constraint(portfolio = init.portfolio, type = "full_investment") or
    init.portfolio <- add.constraint(portfolio = init.portfolio, type = "weight_sum", 
                                     min_sum = 0.99, max_sum = 1.01) 
    init.portfolio <- add.constraint(portfolio = init.portfolio, type = "long_only") 
    
    
    init.portfolio <- add.objective(portfolio=init.portfolio, type="risk", name="ES")
    init.portfolio <- add.objective(portfolio=init.portfolio, type="return", name="mean")
    optimized <- optimize.portfolio(R = AssetOptim, portfolio = init.portfolio, 
                                    optimize_method = "ROI",trace = TRUE, maxSR = TRUE,message = T)
    out <- optimized
    return(out)
  }
  numCores <- detectCores() 
  opt <- pbmcapply::pbmclapply(AssetsforOptimization,Staropt, mc.cores = numCores)
  weights <- lapply(opt, function(x){x$weights})
  
  
  weights <- Unequal_df(weights)
  Pweights <- list.append(Pweights, weights)
  Popt <- list.append(Popt, opt)
  
  toc()
}
names(Pweights) <- c("P.R.Starr.Weights","P.C.Starr.Weights","E.R.Starr.Weights","E.C.Starr.Weights")
names(Popt) <- c("P.R.Starr.Optimization","P.C.Starr.Optimization","E.R.Starr.Optimization","E.C.Starr.Optimization")

PortfolioWeights[["Starr"]] <- Pweights
PortfolioOptimization[["Starr"]] <- Popt


rm(opt, weights, CopulaType, NoofRebalPeriod, AssetsforOptimization, simreturns,Pweights,Popt,Staropt,numCores)
toc()