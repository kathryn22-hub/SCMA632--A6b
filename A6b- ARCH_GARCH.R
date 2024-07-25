options(repos = c(CRAN = "https://cran.rstudio.com/")) 
install.packages("rugarch")
# Load required packages
library(quantmod)
library(rugarch)

# Get the data for Infosys
ticker <- "INFY.NS"
start_date <- "2021-04-01"
end_date <- "2024-03-31"

# Download the data
getSymbols(ticker, from=start_date, to=end_date, src="yahoo")
data <- get(ticker)
market <- Cl(data)
returns <- na.omit(Delt(market)) * 100

# Fit an ARCH model
spec_arch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                        distribution.model = "norm")
arch_fit <- ugarchfit(spec = spec_arch, data = returns)
print(arch_fit)

# Plot the conditional volatility
plot(arch_fit, which = "all")

# Fit a GARCH model
spec_garch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                         distribution.model = "norm")
garch_fit <- ugarchfit(spec = spec_garch, data = returns)
print(garch_fit)

# Plot the conditional volatility
plot(garch_fit, which = "all")

# Forecasts
forecasts <- ugarchforecast(garch_fit, n.ahead = 90)
print(forecasts@forecast$seriesFor)
print(forecasts@forecast$sigmaFor)
