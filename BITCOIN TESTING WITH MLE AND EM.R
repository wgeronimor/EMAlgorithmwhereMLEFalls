# Define the ticker symbol for Bitcoin (BTC-USD)
ticker <- "BTC-USD"

# Fetch historical Bitcoin price data from Yahoo Finance
#btc_data <- quantmod::getSymbols(ticker, src = "yahoo", from = "2009-01-01", to = Sys.Date(), auto.assign = FALSE)
#btc_df <- data.frame(Date = index(btc_data), coredata(btc_data))

# View the first few rows of the Bitcoin price data
head(btc_data)
head(btc_df)

btc_df_filtered <- btc_df|>
  dplyr::filter(Date>=as.Date('2021-04-01') & Date<=as.Date('2021-07-31'))


heavy_tails_data = btc_df_filtered[,which(colnames(btc_df_filtered) %in% "BTC.USD.Close")]
mean(heavy_tails_data)

mle_result <- optim(par = c(0, 1), fn = neg_log_likelihood_cauchy, data = heavy_tails_data)
#mle_result <- optim(par = c(0, 1), fn = neg_log_likelihood_cauchy, data = heavy_tails_data, method = "L-BFGS-B")

# Perform EM using normalmixEM() function from mixtools package
em_result <- normalmixEM(heavy_tails_data, k = 2, maxit = 3000)
#print(em_result)

# Print MLE and EM parameter estimates
cat("MLE Parameter Estimates:\n")
cat("Mean:", mle_result$par[1], "\n")
cat("Scale:", mle_result$par[2], "\n\n")

cat("EM Parameter Estimates:\n")
print(summary(em_result))