
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```{r load-data}

# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

amd_df <- amd_df[, c("date", "close")]
```


# Plotting the Data
- Plot the closing prices over time to visualize the price movement.
```{r plot}
plot(amd_df$date, amd_df$close,'l')
```


## Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```{r Algorithm}
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

# Fill your code here
for (i in 1:nrow(amd_df)) {  # Loop through rows
  if (previous_price == 0) {  # Initial buy condition
    amd_df$trade_type[i] <- "buy"  # Set trade type to buy
    amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size  # Calculate cost
    accumulated_shares <- accumulated_shares + share_size  # Update shares
  } else if (amd_df$close[i] < previous_price) {  # Price drop condition
    amd_df$trade_type[i] <- "buy"  # Set trade type to buy
    amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size  # Calculate cost
    accumulated_shares <- accumulated_shares + share_size  # Update shares
  }
  if (i == nrow(amd_df)) {  # Last day condition
    amd_df$trade_type[i] <- "sell"  # Set trade type to sell
    amd_df$costs_proceeds[i] <- accumulated_shares * amd_df$close[i]  # Calculate proceeds
    accumulated_shares <- 0  # Reset accumulated shares to 0
  }
  amd_df$accumulated_shares[i] <- accumulated_shares  # Track accumulated shares
  previous_price <- amd_df$close[i]  # Update previous price
}
```

## Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 

```{r period}
# Fill your code here
# Define the start and end dates for the trading period
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2020-07-27")

# Filter the dataframe to include only the chosen trading period
amd_df <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date,]

# Set trade type to 'sell' on the last day of the trading period
amd_df$trade_type[nrow(amd_df)] <- 'sell'
amd_df$costs_proceeds[nrow(amd_df)] <- amd_df$close[nrow(amd_df)]*amd_df$accumulated_shares[nrow(amd_df)]
```   

## Step 4: Run Your Algorithm and Analyze Results
- After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.
 
- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$
```
```{r Results}
# Calculate Total Profit/Loss
total_profit_loss <- sum(amd_df$costs_proceeds, na.rm = TRUE)

# Calculate Total Invested Capital
total_invested_capital <- -sum(amd_df$costs_proceeds[amd_df$trade_type == "buy"], na.rm = TRUE)

# Calculate ROI
roi <- (total_profit_loss / total_invested_capital) * 100

# Print the results
cat("Total Profit/Loss:", total_profit_loss, "\n")
cat("Total Capital Invested:", total_invested_capital, "\n")
cat("ROI:", roi, "%\n")
```

## Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.

```{r Profit Taking Strategy}
# Profit Taking Strategy
# Re-initialize columns and variables
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA
amd_df$accumulated_shares <- 0
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
profit_threshold <- 0.20  # 20% profit-taking threshold
for (i in 1:nrow(amd_df)) {  # Loop through rows
  if (is.na(previous_price) || previous_price == 0) {  # Initial buy condition
    amd_df$trade_type[i] <- "buy"  # Set trade type to buy
    amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size  # Calculate cost
    accumulated_shares <- accumulated_shares + share_size  # Update shares
  } else if (amd_df$close[i] < previous_price) {  # Price drop condition
    amd_df$trade_type[i] <- "buy"  # Set trade type to buy
    amd_df$costs_proceeds[i] <- -amd_df$close[i] * share_size  # Calculate cost
    accumulated_shares <- accumulated_shares + share_size  # Update shares
  }
  if (sum(!is.na(amd_df$trade_type) & amd_df$trade_type == "buy") > 0) {  # Check for buy trades
    avg_purchase_price <- mean(amd_df$close[amd_df$trade_type == "buy"], na.rm = TRUE)  # Calculate avg price
    if (amd_df$close[i] >= avg_purchase_price * (1 + profit_threshold) && accumulated_shares > 0) {  # Profit condition
      shares_to_sell <- accumulated_shares / 2  # Calculate shares to sell
      amd_df$trade_type[i] <- "sell"  # Set trade type to sell
      amd_df$costs_proceeds[i] <- shares_to_sell * amd_df$close[i]  # Calculate proceeds
      accumulated_shares <- accumulated_shares - shares_to_sell  # Update shares
    }}
  if (i == nrow(amd_df)) {  # Last row condition
    amd_df$trade_type[i] <- "sell"  # Set trade type to sell
    amd_df$costs_proceeds[i] <- accumulated_shares * amd_df$close[i]  # Calculate proceeds
    accumulated_shares <- 0  # Reset shares
  }
  amd_df$accumulated_shares[i] <- accumulated_shares  # Track shares
  previous_price <- amd_df$close[i]}  # Update previous price
```

## Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.

Discussion
----------
The customised trading period of 02/01/2020 - 27/07/2020 was chosen to assess the implications of the first COVID-19 lockdown on AMD stocks. The ROI from the simple trading algorithm is 38.9%, with a total profit of $133,263. When implimenting the profit taking margin, the ROI and total profit drop significantly to 16.0% and $53,856 respectively. This is likely due to the volatility of the market during the lockdown period, with prices variating from the average purchase price by more than 20% over short periods. This can lead to premature selling of shares, reduing the total profit.
