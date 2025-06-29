# forecasting_Abbott_for_next_30_days

This project presents a 30-day price forecast for Abbott India Ltd., visualized through a time series model using R. The chart captures not just the predicted trend, but also the uncertainty range around those predictions.

🛠️ Methodology

Data Source: Historical prices of Abbott India

Model Used: Time series forecasting model (ARIMA)

Forecast Length: 30 days ahead

Visualization: Built in R using ggplot2 with:-

geom_ribbon() to show the uncertainty (upper & lower bounds)

geom_point() to highlight predicted price points.

📦 File Description
File Name	Description
Rplot14.jpeg	Main forecast graph
README.md	Explanation of the project.
Abbott_india.R

📘 Interpretation
The model predicts an upward trend in stock price over the next month.
The confidence interval expands over time, reflecting more uncertainty the further we forecast.
This kind of visualization is useful for investors, traders, and analysts to assess both expected direction and risk.

## Author 
Anshu Kumar 
