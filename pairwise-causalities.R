library(xts);library("quantmod")
library(Rpdb);library(dplyr)

get_aggregated_returns <- function(sampling_period,vol){
  returns <- xts()
  per<- sampling_period
  for(i in seq_along(markets)) {
    sym <- markets[i]
    adj_returns <- periodReturn(Ad(get(sym,envir=data.env)),period=per,type = "log")
    returns <- merge(returns, EMA(adj_returns, n = 1, ratio = 0.06))
  }
  returns[is.na(returns)]<-0 ##imputation of 0
  colnames(returns) <- paste(markets,".ret",sep="")
  returns
}

#Extract the epoch to analysis
dI="2004-01-01"; dF="2005-12-31"
weekly_retp <- get_aggregated_returns('weekly')[paste(dI,"/",dF,sep=""),]
monthly_retp <- get_aggregated_returns('monthly')[paste(dI,"/",dF,sep=""),]

library(lmtest)

granger_4lags <- function(s1,s2){
  bools = c()
  if (all(s1 == s2)){
    return("Self-comparison")
  }
  for (i in 1:4){
    gtest <- grangertest(s1, s2, order = i, na.action = na.omit)
    p_val <- gtest$`Pr(>F)`[2]

    if (p_val < 0.05){
      bools[i] <- 1
    }else{
      bools[i] <- 0
    }
  }
  return(bools)
}

print(granger_4lags(weekly_retp$IBEX.ret, weekly_retp$HSCE.ret))


pairwise_causality <- function(retp){
  all_causalities <- list()
  for(i in 1:ncol(retp)) {
    current_causality <- list()
    for(j in 1:ncol(retp)){
      retp[ , i] <- retp[ , i]
      current_causality[[j]] <- granger_4lags(retp[ , i], retp[ , j])
    }
    all_causalities[[i]] <- current_causality
  }
  df <- as.data.frame(do.call(rbind, all_causalities))
  colnames(df) <- colnames(retp)
  df$ticker<- colnames(retp)
  df <- df %>% select(ticker, everything())

  return(df)
}

weekly_causalities = pairwise_causality(weekly_retp)
monthly_causalities = pairwise_causality(monthly_retp)
