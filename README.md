# Advanced-Capstone-Forecasting-Project

**Independent Studies Forecasting Project**


**Independently**

learned proficiency in implementing and interpreting various forecasting models including Exponential Smoothing (ETS, Brown, Holt, Winters), Decomposition (STL, Classical, X-11, X12, X11 seats), ARIMA (Seasonal and Non Seasonal) Time Series Regression Models (Regression with seasonality, dynamic regression w/ARIMA, Dynamic Harmonic Regression), Hierarchical forecasting (top-down and bottom-up), and Profit model.

**Project:**

Build and evaluate various forecasting models on a 10-million-record dataset for a department store in Ecuador.


**Challenges:**

How to build 1800 models to capture department variation and regional variation for 54 store locations. Had to learn how to account for complex daily seasonality using harmonics and Fourier terms to account for all different types of seasonality associated with fine-grain time series data. How to develop a forecasting model that would work with hierarchical data and could also account for various seasonality associated with daily sales data. How to further improve the model by incorporating regional holiday data to help capture holiday seasonality.

**Solution:** 

Implemented various forecasting models and evaluated them based on multiple forecast error measures such as RMSE, MAE, MAPE,MSE.

**Outcome:**

Developed a localized, bottom-up hierarchical forecasting model using harmonics and a dynamic regressions model with Arima using R studios fable package that was 96% Acuraate with a MAPE of 4.06.

***For your information it takes 2 days to run the whole data set with the profit model and dynamic regression using hierarchal data which captures local and department sentiment
