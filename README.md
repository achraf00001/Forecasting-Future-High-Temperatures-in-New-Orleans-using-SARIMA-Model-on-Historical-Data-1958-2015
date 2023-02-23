#TIME SERISE/Forecasting-Future-High-Temperatures-in-New-Orleans-using-SARIMA-Model-on-Historical-Data-1958-2015 

Objective:

We are looking at the New Orleans weather high temperature from 1958 to 2015, we want to 
fit a good SARIMA (Autoregressive integrated moving average) model in order to predict the future 
observation. 

Model selection procedure: 

after changing the high temperature variable to be a time series object 
using the ts function. We plot the data (Figure 1.). The best model selection should have the smallest 
AICc compared to other fitted models. Also, it should have the smallest ME, RMSE …, obtained from the 
model accuracy metrics. We will look at the Ljung- Box-lack as well to calculate the p-value for the fitted 
models, the bigger the p-value the better the model is. Moreover, Analyzing the residuals. There should 
not be any trend in the residuals. Based on ACF and PACF plot of the residuals should look like the white 
noise.
