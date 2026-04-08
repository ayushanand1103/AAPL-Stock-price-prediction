library(dplyr)
library(TTR)

# Load
AAPL <- read.csv("C:/R/AAPL Stock price prediction/aapl_us_d.csv")

# Correct format: DD-MM-YYYY
AAPL$Date <- as.Date(AAPL$Date, format = "%d-%m-%Y")

# Filter last 10 years
AAPL <- filter(AAPL, Date >= Sys.Date() - 365 * 10)

# Sort old to new
AAPL <- arrange(AAPL, Date)

# Check
head(AAPL)
tail(AAPL)


## ---------------------------------------------------------
## Calculating Monthly Mean Close Price
## ---------------------------------------------------------

AAPL$YearMonth <- format(AAPL$Date, "%Y-%m")

monthly <- summarise(group_by(AAPL, YearMonth), 
                     Mean_Close = mean(Close))

monthly <- arrange(monthly, YearMonth)

monthly_vector <- monthly$Mean_Close

plot(monthly_vector,
     type = "l",
     col  = "darkblue",
     lwd  = 2,
     main = "AAPL Monthly Average Closing Price (Last 10 Years)",
     xlab = "Time (Months)",
     ylab = "Average Closing Price (USD)")


# ---------------------------------------------------------
# Simple Moving Average (SMA) & Exponential Moving Average (EMA)
# ---------------------------------------------------------

ma3  <- SMA(monthly_vector, n=3)
ma6  <- SMA(monthly_vector, n=6)
ma12 <- SMA(monthly_vector, n=12)

ema3  <- EMA(monthly_vector, n=3)
ema6  <- EMA(monthly_vector, n=6)
ema12 <- EMA(monthly_vector, n=12)


# ---------------------------------------------------------
# Plotting Simple Moving Averages
# ---------------------------------------------------------

plot(monthly_vector,
     type = "l",
     col = "grey60",
     lwd = 1,
     main = "AAPL Monthly Closing Price with Simple Moving Averages",
     xlab = "Time (Months)",
     ylab = "Closing Price (USD)",
     xaxt = "n")

lines(ma3,  col = "dodgerblue",  lwd = 2)
lines(ma6,  col = "darkgreen",   lwd = 2)
lines(ma12, col = "firebrick",   lwd = 2)

legend("topleft",
       legend = c("Actual Price", "SMA (3 Months)", "SMA (6 Months)", "SMA (12 Months)"),
       col    = c("grey60", "dodgerblue", "darkgreen", "firebrick"),
       lwd    = 2,
       cex = 0.8)


# ---------------------------------------------------------
# Plotting Exponential Moving Averages
# ---------------------------------------------------------

plot(monthly_vector,
     type = "l",
     col = "grey60",
     lwd = 1,
     main = "AAPL Monthly Closing Price with Exponential Moving Averages",
     xlab = "Time (Months)",
     ylab = "Closing Price (USD)",
     xaxt = "n")

lines(ema3,  col = "dodgerblue",  lwd = 2)
lines(ema6,  col = "darkgreen",   lwd = 2)
lines(ema12, col = "firebrick",   lwd = 2)

legend("topleft",
       legend = c("Actual Price", "EMA (3 Months)", "EMA (6 Months)", "EMA (12 Months)"),
       col    = c("grey60", "dodgerblue", "darkgreen", "firebrick"),
       lwd    = 2,
       cex = 0.8)


# ---------------------------------------------------------
# Stationarity Check
# ---------------------------------------------------------

plot(monthly_vector,
     type = "l",
     col  = "purple",
     lwd  = 2,
     main = "Trend of AAPL Monthly Average Closing Price",
     xlab = "Time (Months)",
     ylab = "Average Closing Price (USD)")


# ACF & PACF before transformation
acf(monthly_vector, lag.max = 10,
    main = "Autocorrelation Function (ACF) - Original Series")
# ACF is showing random walk beheviour (Decaying over period of time)
#Red flag for stationarity

pacf(monthly_vector, lag.max = 10,
     main = "Partial Autocorrelation Function (PACF) - Original Series")


# ---------------------------------------------------------
# Checking Constant Mean (Block Means)
# ---------------------------------------------------------

monthly_vec_10_intervals_mean <- c()

x <- 1
y <- 10

while (y <= length(monthly_vector)) {
  
  monthly_vec_10_intervals_mean <- append(
    monthly_vec_10_intervals_mean,
    mean(monthly_vector[x:y])
  )
  
  x <- x + 10
  y <- y + 10
}

barplot(monthly_vec_10_intervals_mean,
        col  = "royalblue",
        main = "Block Mean of AAPL Prices (10-Month Intervals)",
        xlab = "10-Month Time Blocks",
        ylab = "Average Price (USD)")

#Mean is also not constant 
#Red flag for stationarity
# ---------------------------------------------------------
# Checking Constant Variance (Block Variance)
# ---------------------------------------------------------

monthly_vec_10_intervals_variance <- c()

xx <- 1
yy <- 10

while (yy <= length(monthly_vector)) {
  
  monthly_vec_10_intervals_variance <- append(
    monthly_vec_10_intervals_variance,
    var(monthly_vector[xx:yy])
  )
  
  xx <- xx + 10
  yy <- yy + 10
}

barplot(monthly_vec_10_intervals_variance,
        col  = "darkorange",
        main = "Block Variance of AAPL Prices (10-Month Intervals)",
        xlab = "10-Month Time Blocks",
        ylab = "Variance of Price")

#Variance is also not constant 
#red flag for stationarity




# Now lets deal with non stationarity
# ---------------------------------------------------------
# Log Transformation
# ---------------------------------------------------------

log_vec <- log(monthly_vector)

plot(log_vec,
     type = "l",
     col  = "darkcyan",
     lwd  = 2,
     main = "Log Transformation of AAPL Monthly Prices",
     xlab = "Time (Months)",
     ylab = "Log of Average Closing Price")


# ---------------------------------------------------------
# First Order Differencing
# ---------------------------------------------------------

diff_vec <- diff(log_vec)

plot(diff_vec,
     type = "l",
     col = "darkred",
     lwd = 2,
     main = "First Difference of Log-Transformed AAPL Prices",
     xlab = "Time (Months)",
     ylab = "Differenced Log Price")


# ---------------------------------------------------------
# Checking Mean After Transformation
# ---------------------------------------------------------

monthly_vec_10_intervals_mean_after_trans_diff <- c()

x <- 1
y <- 10

while (y <= length(diff_vec)) {
  
  monthly_vec_10_intervals_mean_after_trans_diff <- append(
    monthly_vec_10_intervals_mean_after_trans_diff,
    mean(diff_vec[x:y])
  )
  
  x <- x + 10
  y <- y + 10
}

barplot(monthly_vec_10_intervals_mean_after_trans_diff,
        col  = "seagreen",
        main = "Block Mean After Log Transformation & Differencing",
        xlab = "10-Month Time Blocks",
        ylab = "Mean of Differenced Series")
# Mean is constant 
# Green flag for stationarity


# ---------------------------------------------------------
# Checking Variance After Transformation
# ---------------------------------------------------------

monthly_vec_10_intervals_variance_after_trans_diff <- c()

xx <- 1
yy <- 10

while (yy <= length(diff_vec)) {
  
  monthly_vec_10_intervals_variance_after_trans_diff <- append(
    monthly_vec_10_intervals_variance_after_trans_diff,
    var(diff_vec[xx:yy])
  )
  
  xx <- xx + 10
  yy <- yy + 10
}
monthly_vec_10_intervals_variance_after_trans_diff
barplot(monthly_vec_10_intervals_variance_after_trans_diff,
        col  = "goldenrod",
        main = "Block Variance After Log Transformation & Differencing",
        xlab = "10-Month Time Blocks",
        ylab = "Variance of Differenced Series")
# Variance is constant
# Green flag for stationarity


# ---------------------------------------------------------
# Final ACF & PACF (Stationary Series)
# ---------------------------------------------------------

acf(diff_vec, lag.max = 10,
    main = "ACF After Log Transformation and Differencing")

# ACF is now showing white noice 
# Which means it does not makes sense to fit in auto regressive models like AR,ARIMA,SARIMA,etc 

pacf(diff_vec, lag.max = 10,
     main = "PACF After Log Transformation and Differencing")

#----------------------------------------------------------
#Linear regression model 
#----------------------------------------------------------

time_index <- c(1:length(monthly_vector))
lr_model <- lm(monthly_vector ~ time_index)
summary(lr_model)
plot(monthly_vector,
     type="l",
     col="grey",
     main="Linear Regression Trend in AAPL Prices")

lines(predict(lr_model), col="blue", lwd=2)

#----------------------------------------------------------
# Auto regression model (AR1)
#----------------------------------------------------------
# create lag variable
lag1 <- diff_vec[1:(length(diff_vec)-1)]

# dependent variable
y <- diff_vec[2:length(diff_vec)]

# AR(1) model
ar1_model <- lm(y ~ lag1)

summary(ar1_model)

plot(y,
     type="l",
     col="grey",
     main="AR Model Fit")

lines(predict(ar1_model),
      col="blue",
      lwd=2)

#-----------------------------------------------------------
# Auto regression (AR2)
#-----------------------------------------------------------
lag1 <- diff_vec[2:(length(diff_vec)-1)]
lag2 <- diff_vec[1:(length(diff_vec)-2)]

y <- diff_vec[3:length(diff_vec)]

ar2_model <- lm(y ~ lag1 + lag2)

summary(ar2_model)

plot(y,
     type="l",
     col="grey",
     main="AR Model Fit")

lines(predict(ar2_model),
      col="blue",
      lwd=2)
