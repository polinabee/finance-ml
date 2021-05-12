get_aggregated_returns <- function(sampling_period, vol=FALSE){
  returns <- xts()
  for(i in seq_along(markets)) {
    sym <- markets[i]
    adj_returns <- periodReturn(Ad(get(sym,envir=data.env)),period=sampling_period,type = "log")
    if (vol==TRUE){
      returns <- merge(returns, EMA(adj_returns,  ratio = 0.06))
    }else(
      returns <- merge(returns, adj_returns)
    )
    
  }
  returns <- na.omit(returns) # na removal
  colnames(returns) <- paste(markets,".ret",sep="")
  
  # remove VLIC and VIX
  returns$VLIC.ret=NULL
  returns$VIX.ret=NULL
  
  returns 
}

#Extract the epoch to analysis
dI="2004-01-01"; dF="2005-12-31"
weekly_retp <- get_aggregated_returns('weekly')[paste(dI,"/",dF,sep=""),]
monthly_retp <- get_aggregated_returns('monthly')[paste(dI,"/",dF,sep=""),]
weekly_retp_vol <- get_aggregated_returns('weekly',vol=TRUE)[paste(dI,"/",dF,sep=""),]
monthly_retp_vol <- get_aggregated_returns('monthly',vol=TRUE)[paste(dI,"/",dF,sep=""),]

granger_4lags <- function(s1,s2){
  bools = c()
  if (all(na.omit(s1) == na.omit(s2))){
    return("--")
  }
  for (i in 1:4){
    gtest <- grangertest(s1, s2, order = i, na.action = na.omit)
    p_val <- gtest$`Pr(>F)`[2]

    if (p_val <= 0.05){
      bools[i] <- 1
    }else{
      bools[i] <- 0
    }
  }
  return(bools)
}

pairwise_causality <- function(retp){
  all_causalities <- list()
  for(i in 1:ncol(retp)) {
    current_causality <- list()
    for(j in 1:ncol(retp)){
      current_causality[[j]] <- granger_4lags(retp[ , i], retp[ , j])
    }
    all_causalities[[i]] <- current_causality
  }
  df <- as.data.frame(do.call(rbind, all_causalities))
  colnames(df) <- colnames(retp)
  df$ticker<- colnames(retp)
  df <- df %>% dplyr::select(ticker, everything())

  return(df)
}

weekly_causalities = pairwise_causality(weekly_retp)
monthly_causalities = pairwise_causality(monthly_retp)
weekly_causalities_vol = pairwise_causality(weekly_retp_vol)
monthly_causalities_vol = pairwise_causality(monthly_retp_vol)


### PRETTY PRINT ####
kable(weekly_causalities, caption="Weekly Returns Causalities")
kable(monthly_causalities, caption="Monthly Returns Causalities")
kable(weekly_causalities_vol, caption="Weekly Volatility Causalities")
kable(monthly_causalities_vol, caption="Monthly Volatility Causalities")
