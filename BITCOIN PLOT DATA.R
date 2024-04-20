# Load required libraries
library(magrittr)  # For data manipulation
library(dplyr)     # For data manipulation
library(ggplot2)   # For data visualization

# Function to calculate daily percentage change
calculate_daily_returns <- function(data) {
  data <- data %>% mutate(Date = as.Date(Date),
                          Return = (BTC.USD.Close - lag(BTC.USD.Close)) / lag(BTC.USD.Close))
  return(data[-1, ])  # Remove first row (NA due to lag)
}

# Function to plot Bitcoin price data and daily returns
plot_price_and_returns <- function(data) {
  data <- data %>% mutate(Date = as.Date(Date))
  ggplot(data, aes(x = Date)) +
    geom_line(aes(y = BTC.USD.Close), color = "blue", linetype = "solid") +
    geom_line(aes(y = Return), color = "red", linetype = "dashed") +
    labs(title = "Bitcoin Price and Daily Returns",
         y = "Price / Return") +
    theme_minimal()
}

# Calculate daily returns
btc_returns <- calculate_daily_returns(
  btc_df|>
    dplyr::filter(Date>=as.Date('2021-04-13') & Date<=as.Date('2021-05-20'))
)

# Plot Bitcoin price and daily returns
plot_price_and_returns(btc_returns)