  # ========================================
  # This program uses a GAM model to forecast existing home sales
  #========================================

  require(mgcv)
  require(nlme)
  require(quantmod)
  require(splines)
  require(forecast)
  require(ggplot2)

  ExistingHomeSales <- read.csv("~/ExistingHomeSales/EXHOSLUSM495N.txt", sep = "\t", header = TRUE)
  ExistingHomeSales$Date <- rownames(ExistingHomeSales)

  # Create monthly and yearly time series variables 
  ExistingHomeSales[,3] <- as.numeric(format(as.Date(ExistingHomeSales[,2]), "%m"))
  ExistingHomeSales[,4] <- as.numeric(format(as.Date(ExistingHomeSales[,2]), "%Y"))

  # Create time index for gam model
  ExistingHomeSales[,5] <- 
  as.numeric(c(seq(from = 1, to = as.numeric(length(ExistingHomeSales[,2]), by = 1))))

  # Assign names to the columns
  colnames(ExistingHomeSales) <- c("HomeSales", "Date", "Month", "Year", "Time")
  
  # Convert Exisiting Home Sales Data to ,000s
  ExistingHomeSales[,1] <- ExistingHomeSales[,1]/1000

  # Set control for the gam model AR fits
  ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod = "L-BFGS-B")

  # Fit the gam model
  XHS <- gamm(HomeSales ~ s(Month, bs = "cc") + s(Time, bs = "cr"),
            data = ExistingHomeSales,
            correlation = corARMA(form = ~1|Year, p = 1),
            knots = list(Month = c(1,12)),
            control = ctrl)

  # Residual Analysis
  #layout(matrix(1:2, ncol = 2))
  #acf(resid(XHS$lme), lag.max = 11, main = "ACF")
  #pacf(resid(XHS$lme), lag.max = 11, main = "PACF")
  #layout(1)

  # Fit auto correlated GAM Models 2, 3 and 6 months
  XHS.2 <- gamm(HomeSales ~ s(Month, bs = "cc") + s(Time, bs = "cr"),
              data = ExistingHomeSales,
              correlation = corARMA(form = ~1|Year, p = 2),
              knots = list(Month = c(1,12)),
              control = ctrl)

  XHS.3 <- gamm(HomeSales ~ s(Month, bs = "cc") + s(Time, bs = "cr"),
              data = ExistingHomeSales,
              correlation = corARMA(form = ~1|Year, p = 3),
              knots = list(Month = c(1,12)),
              control = ctrl)

  XHS.6 <- gamm(HomeSales ~ s(Month, bs = "cc") + s(Time, bs = "cr"),
              data = ExistingHomeSales,
              correlation = corARMA(form = ~1|Year, p = 6),
              knots = list(Month = c(1,12)),
              control = ctrl)
  
  # Use analysis of variance to select the best model
  anova(XHS$lme, XHS.2$lme, XHS.3$lme, XHS.6$lme)
  
  # Looks like either 2 months or 6 months auto correlation

  # Plot gam model
  #layout(matrix(1:2, ncol = 2))
  #plot(XHS.6$gam, scale = 0)
  #layout(1)

  # Normalized Residual Analysis
  #layout(matrix(1:2, ncol = 2))
  #res <- resid(XHS.6$lme, type = "normalized")
  #acf(res, lag.max = 11, main = "ACF - AR(3)errors")
  #pacf(res, lag.max = 11, main = "PACF - AR(3) errors")
  #layout(1)

  #create new data for prediction six months forward
  NewData <- data.frame(c(2,3,4,5,6,7))   # These are months forward
  NewData[,2] <- c(seq(218, 223,1))         # Time index     
  colnames(NewData) <- c("Month", "Time")

  #plot the trend and seasonal factors
  Trend <- predict(XHS.6$gam, newdata = ExistingHomeSales, type = "terms")
  #plot(x = as.Date(ExistingHomeSales[,2]), y = Trend[,1], type = "l")
  #plot(x = as.Date(ExistingHomeSales[,2]), y = Trend[,2], type = "l",
  #     main = "Trend GAMM Model")

  HomeSales.Term <- predict(XHS.6$gam, newdata = NewData, type = "terms", se.fit = TRUE)
  HomeSales <- predict(XHS.6$gam, newdata = NewData, type = "response", se.fit = TRUE)


  # =======================================================================================
  # Fit a seasonal model to the data using R decompose
  # =======================================================================================
  # Create a Time Series object for existing home sales
  XHomeSales <-data.frame(cbind(ExistingHomeSales[,"Date"]),ExistingHomeSales[,"HomeSales"])
  colnames(XHomeSales) <- c("Date", "Sales")

  TS.ExistingHomeSales <- ts(XHomeSales, start = c(1999, 1), end = c(2017, 01), frequency = 12)
  Seasonal <- decompose(x = TS.ExistingHomeSales[,2], type = c("multiplicative"))
  Stl.Seasonal <- stl(TS.ExistingHomeSales[,2], s.window = "periodic", s.degree = 0)
  stl.predict <- forecast(Stl.Seasonal, method = c("arima"), h = 12)
  
  #plot(stl.predict, 
  #     main = "Seasonal Decompostion",
  #     ylab = "Existing Home Sales (,000s)",
  #     xlab = "Year")
  #plot(Stl.Seasonal)
  
  # Convert Exisiting Home Sales Data to back to raw and save
  ExistingHomeSales[,1] <- ExistingHomeSales[,1] * 1000
  write.csv(ExistingHomeSales, file = "ExistHomeSales.csv")

 # Plots
 
  ggplot(subset(ExistingHomeSales, Month ==2), 
         aes(y = HomeSales/1000, x = as.Date(Date, format = "%Y-%m-%d"))) +
    geom_bar(stat = "identity", aes(fill = as.factor(Year))) + 
    labs(y = "Existing Home Sales (,000s)", x = "Year",
         caption = "Source: National Assoc. Realtors") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_blank())
  
  autoplot(stl.predict,
           main = "",
           xlab = "Date",
           ylab = "Existing Home Sales (,000s)") +
    labs(aesthetic='custom text') +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  
  