rm(list=ls()) #clear the workspace

#loading of the libraries
library(quantmod)
library(urca)
library(R6)
library(tseries)
library(fBasics)
library(ggplot2)
library(forecast)
library(xts)
library(rugarch)
library(shiny)
library(patchwork)
library(vars)
library(DT)
library(purrr)



#creation of class referred to a stock


# definition of class in R6 -----------------------------------------------------

stock<- R6Class("stock",
                public = list(
                  ticker=NULL, #short name for company
                  n_obs=NULL, #number of observations 
                  date=NULL, #dates of the observations
                  adjPrice=NULL, #adjusted price time series
                  logRet=NULL, #logarithmic return of prices
                  
                  
                  initialize = function(ticker,n_obs,date,adjPrice,logRet){
                    #some defensive programming:
                    #ticker must be a character with capital letters
                    stopifnot(all(is.character(ticker)), all(grepl("^[A-Z]+$", ticker)))
                    #n_obs must be numeric and longer than 0
                    stopifnot(all(is.numeric(n_obs)), length(n_obs) > 0)
                    #format date must be yyyy/mm/dd
                    stopifnot(all(grepl("\\d{4}-\\d{2}-\\d{2}", date)),length(date)== n_obs)
                    stopifnot(all(is.numeric(adjPrice)))
                    stopifnot(all(is.numeric(logRet)))
                    
                    self$ticker = ticker # we need to specify default values here
                    self$n_obs = n_obs
                    self$date=date
                    self$adjPrice=adjPrice
                    self$logRet=logRet
                    
                    invisible(self) 
                    }
                  )
              )



# Descriptive statistic --------------------------------------------------
#this method is build in order to visualize the basic descriptive statistic of 
#prices or returns. Beyond this statistics, some graphs are shown such as scatterplot
#and histogram. 
#The arguments passed to the functions are:
# 1) values: can be either adjusted price or logaritmic returns
# 2) pos1: refers to the first observation to consider;
#          By default is equal to one (start from the first observation available)
# 3) pos2: refers to the first observation to consider; 
#          By default correspond to the last observations available 

stock$set("public", "descriptive_stat", function(values,pos1=1,pos2=self$n_obs){
  #check about the arguments passed to the function, not less than 50 observations
  stopifnot(pos2>pos1,pos1>=1,pos2<=self$n_obs,abs(pos2-pos1)>50)
  # "values" can be either price or returns 
  stopifnot(any(is.numeric(values)))
  #with this if condition, you can understand if the values passed to the function
  #are return (they have zero mean) instaed of prices 
  if(round(mean(values),0) == 0 ){
    
    label="logaritmic returns "
  }
  else{
    label="prices"
  }
  #print of results
  cat("\nThe mean of",self$ticker,label,"is ",mean(values))
  cat("\nThe standard deviation of",self$ticker,label,"is ",sd(values))
  cat("\nThe skewness of",self$ticker,label,"is ",skewness(values))
  cat("\nThe kurtosis of",self$ticker,label,"is ",kurtosis(values))
  
  test=jarque.bera.test(values) #verify the normality hypothesis with a test

  print(test)
  if(test$p.value>=0.05){ #print an output according to the result of the test
    cat("\nAccording to Jarque Bera test for",self$ticker, label, ", the hypothesis of normality is accepted")
  }
  else{
    paste("\nAccording to Jarque Bera test for",self$ticker,label,", the hypothesis of normality is rejected")
  }
  cat("\n\n")
  
  #scatter plot of the financial time series, here 'pos1' and 'pos2' arguments are used
  p1=ggplot(data = as.data.frame(values), aes(x = self$date[pos1:pos2],y=values ))+geom_line()+
    labs(title = self$ticker, 
         x = "TIME", y = paste(self$ticker,"",label))+
    theme(panel.background = element_rect(fill = "light grey"))
  
  #Histogram of the finacial time series
  p2=ggplot(data = as.data.frame(values), aes(x = values)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "steelblue", alpha = 0.7) +
    geom_density(color = "red", linewidth = 1)  +
    labs(title = paste("Histogram of",self$ticker," ",label), y = "Density")
  
  #put the plots together
  combined_plot = (p1 + p2 + plot_layout(ncol = 2, widths = c(1, 1)))
  return(combined_plot)
})




# stationarity ------------------------------------------------------------


#this function evaluate the stationarity of a time series (either for prices or returns)


stock$set("public", "stationarity", function(values){
  options(warn=-1)
  stopifnot(any(is.numeric(values)))
  test=adf.test(values) #dickey fuller test applied to the argument passed
  print(test)
  count=0 #counter initialized to zero
  if(test$p.value <= 0.05){
    cat("\nThe time series is stationary")
  }
  while(test$p.value > 0.05){ # repeat the test until the condition is not met.
    count=count+1 #update the counter 
    test=adf.test(na.omit(diff(values,differences=count))) #repeat the test differenciating the time series
  }
  #if the time series is not stationary than it is integrated of some order to define.
  #the order is defined by "count"
  if(count != 0){
    cat("\nThe series is integrated of order",count)
  }
  
  #data related to acf and pacf of the time series until lag 20  
  acf_data <- acf(ts(values), lag.max = 20, plot = FALSE)
  pacf_data <- pacf(ts(values), lag.max = 20, plot = FALSE)
  
  #create a dataframe containing the values of the functions ACF and PACF
  df_acf <- data.frame(lag = acf_data$lag, acf = acf_data$acf)
  df_pacf <- data.frame(lag = pacf_data$lag, pacf = pacf_data$acf)

  #graph in ggplot of acf
  p1=ggplot(df_acf, aes(x = lag, y = acf)) +
    geom_bar(stat = "identity", fill = "steelblue",width = 0.4) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    ylim(-0.25, 1) +
    labs(title = paste0("Auto-correlation function (ACF) of ",self$ticker), x = "Lag", y = "ACF")
  
  
  #graph in ggplot of pacf
  p2=ggplot(df_pacf, aes(x = lag, y = pacf)) +
    geom_bar(stat = "identity", fill = "steelblue", width=0.4) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    ylim(-0.25, 1) +
    labs(title = paste0("Partial ACF of ",self$ticker), x = "Lag", y = "PACF")
  
  #combinantion of the two graphs
  combined_plot = (p1 + p2 + plot_layout(ncol = 2, widths = c(1, 1)))
  return(combined_plot)
})








# arima model -------------------------------------------------------------
#this method fit an ARIMA model for prices. 
#the arguments passed to the functions are:
# 1) pos1: refers to the first observation to consider;
#          By default is equal to one (start from the first observation available)
# 2) pos2: refers to the first observation to consider; 
#          By default correspond to the last observations available
# 3) ord: refers to the order of the model. It consists of a three integer cointained in a vector
#         They corrispond respectively to order (P, D, Q) of the ARIMA model 
stock$set("public", "arimaModel", function(pos1=1,pos2=self$n_obs,ord){
  #some defensive programming
  #the last conditions in order to choose a sufficiently big timespan in order to fit the model properly
  stopifnot(pos2>pos1,pos1>=1,pos2<=self$n_obs,abs(pos2-pos1)>50)
  stopifnot(any(is.numeric(ord)))
  options(warn=-1)
  cat("\n------------- ARIMA(",paste0(ord[1],";",ord[2],";",ord[3],")"),"MODEL FOR",self$ticker,"-------------\n")
  #fitting the model
  fit=arima0(ts(self$adjPrice[pos1:pos2]),order=c(ord[1],ord[2],ord[3]),include.mean = FALSE)
  forecast=predict(fit,n.ahead=20) #evaluating the forecasts 20 step ahead
  #creating the output to display later
  Coefficients <- fit$coef
  Std_err <- sqrt(diag(fit$var.coef))
  z_val=Coefficients/Std_err
  p_val=pnorm(abs(z_val),lower.tail = FALSE)
  print(cbind(Coefficients,Std_err,z_val,p_val))
  cat("\nThe value of log-likelihood is: ",fit$loglik)
  cat("\nThe AIC value is: ",fit$aic)
  cat("\n")
  
  
  #create data frame in order to display graphs, it will be displayed the time series
  #and the forecasts done per the future observations with upper and lower bounds
  df=data.frame(adjPrice=tail(self$adjPrice[pos1:pos2],200),
                time=tail(self$date[pos1:pos2],200),
                pred=c(rep(0,180),forecast$pred),
                se=c(rep(0,180),forecast$se))
  df_pred <- df[df$pred != 0, ]
  p1= ggplot(df, aes(x = time)) +
           geom_line(aes(y = adjPrice, color = "Actual")) +
           geom_line(data = df_pred, aes(y = pred, color = "Forecast")) +
           geom_ribbon(aes(ymin = pred - 1.96 * se, ymax = pred + 1.96 * se), alpha = 0.2, fill = "yellow", color = NA) +
           labs(x = "Time", y = paste("Adjusted Price of",self$ticker), color = "") +
           scale_color_manual(values = c("Actual" = "blue", "Forecast" = "red")) +
           theme_minimal()

  
  
  
  #graphs of the residuals (errors) of the model, the should be with zero mean
  p2=ggplot(data.frame(residuals = fit$residuals), aes(x = seq_along(residuals), y = residuals)) +
          geom_line() +
          geom_hline(yintercept = mean(fit$residuals), color = "red") +
          labs(x = "Observation", y = "Residual", title = "Residual Plot") +
          theme_minimal()
  
  
  
  residuals <- residuals(fit)
  acf_resid <- acf(residuals,plot = FALSE)
  
  #acf for residuals of the model in order to give a goodness of fit for the model
  conf_level <- 1.96/sqrt(length(residuals))
  acf_resid_df <- data.frame(lag = acf_resid$lag, acf = acf_resid$acf, conf_level = conf_level)
  acf_resid_df <- acf_resid_df[acf_resid_df$acf > acf_resid_df$conf_level | acf_resid_df$acf < -acf_resid_df$conf_level,]
  
  
  p3=ggplot(data = acf_resid_df, aes(x = lag, y = acf)) +
          geom_bar(stat = "identity", width=0.3, fill = "grey50") +
          geom_hline(yintercept = conf_level, linetype = "dashed") +
          geom_hline(yintercept = -conf_level, linetype = "dashed") +
          ylim(-1, 1) +
          xlab("Lag") +
          ylab("Autocorrelation") +
          ggtitle("ACF of Residuals") +
          theme_bw()
  
  
  
  # invisible(self)
  # combine the plot
  combined_plot = (p1 / (p2 + p3)) + plot_layout(ncol = 1)
  return(combined_plot)
})




# GARCH -------------------------------------------------------------------
#this method fit an GARCH model for logarithmic returns. 
#the arguments passed to the functions are:
# 1) pos1: refers to the first observation to consider;
#          By default is equal to one (start from the first observation available)
# 2) pos2: refers to the first observation to consider; 
#          By default correspond to the last observations available
# 3) ord: refers to the order of the model. It consists of two integers cointained in a vector
#         They corrispond respectively to order (P, Q) of the GARCH model 

stock$set("public", "garchModel", function(pos1=1,pos2=self$n_obs,ord){
  #some defensive programming
  #the last conditions in order to choose a sufficiently big timespan in order to fit the model properly
  stopifnot(pos2>pos1,pos1>=1,pos2<=self$n_obs,abs(pos2-pos1)>50)
  stopifnot(any(is.numeric(ord)))
  options(warn=-1)
  acf_data <- acf(ts((self$logRet[pos1:pos2])^2), lag.max = 20, plot = FALSE)
  pacf_data <- pacf(ts((self$logRet[pos1:pos2])^2), lag.max = 20, plot = FALSE)
  
  df_acf <- data.frame(lag = acf_data$lag, acf = acf_data$acf)
  df_pacf <- data.frame(lag = pacf_data$lag, pacf = pacf_data$acf)
  
  p1=ggplot(df_acf, aes(x = lag, y = acf)) +
          geom_bar(stat = "identity", fill = "steelblue",width = 0.4) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
          ylim(0, 1) +
          labs(title = paste0("ACF of squared log-return of ",self$ticker), x = "Lag", y = "ACF")
 
  
  p2=ggplot(df_pacf, aes(x = lag, y = pacf)) +
          geom_bar(stat = "identity", fill = "steelblue", width=0.4) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
          ylim(0, 1) +
          labs(title = paste0("Partial ACF of squared log-return of ",self$ticker), x = "Lag", y = "PACF")

  #specification of the model
  spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(ord[1], ord[2])),
                     mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                     distribution.model = "norm")
  cat("\n------------- GARCH(",paste0(ord[1],";",ord[2],")"),"MODEL FOR",self$ticker,"-------------\n")
  #fitting of the model
  fit <- ugarchfit(spec = spec, data = self$logRet[pos1:pos2]) 
  
  
  print(fit@fit$matcoef)
  
  forecasts <- ugarchforecast(fit, n.ahead = 20)

  
  p3 =ggplot() + 
    geom_line(aes(x = index(self$date[pos1:pos2]), y = sigma(fit))) +
    labs(title = "Conditional variance over the time", x = "Time", y = "
         Value of conditional variance")
  
  resStdSq=fit@fit$residuals/(sqrt(fit@fit$sigma))
  
  
  
  acf_data <- acf(ts((resStdSq)^2), lag.max = 20, plot = FALSE)
  pacf_data <- pacf(ts((resStdSq)^2), lag.max = 20, plot = FALSE)
  
  df_acf <- data.frame(lag = acf_data$lag, acf = acf_data$acf)
  df_pacf <- data.frame(lag = pacf_data$lag, pacf = pacf_data$acf)
  
  p4=ggplot(df_acf, aes(x = lag, y = acf)) +
    geom_bar(stat = "identity", fill = "steelblue",width = 0.4) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    ylim(0, 1) +
    labs(title = paste0("ACF of squared standardized residuals of the model"), 
         x = "Lag", y = "ACF")

  
  combined_plot = ((p1 + p2) /(p3 +p4) + plot_layout(ncol = 1))
  return(combined_plot)
})




# VAR ---------------------------------------------------------------------
#This method fit a VAR model.
#The arguments passed to the methods are:
#1) data: a matrix containing per each column return of a different company.
#   data must have names of columns, otherwise the IRF is not plotted;
#2) p: is the order of the VAR model;
#3) n.ahead: is the number of steps ahead. This number is necessary for IRF (the 
#         graphs returned by the function).



stock$set("public", "VARModel", function(data,p,n.ahead){
  stopifnot(ncol(data)>1)
  stopifnot(! is.null(colnames(data)),length(colnames(data))==ncol(data))
  stopifnot(p%%1==0,p>=1,n.ahead%%1==0,n.ahead>1)
  options(warn=-1)
  k=ncol(data)
  

  varest=vars::VAR(data,p=p,type = "none")#fitting the model
  var_rid=stepwise_back(varest,p=p,k)#this function make the stepwise backward
  for(i in 1:k){#printing the output equation by equation
    cat("\nEstimated equation (with significant parameters selection) for ",colnames(data)[i],"\n")
    print(coef(var_rid)[[i]])#this is a list of lists
    cat("\n")
  }
  cat("\n")
  cat("\nVariance and covariance matrix: \n")
  print(summary(var_rid)$covres)
  #check for model stationarity 
  cat("\n\nModules of roots of the VAR model fitted:",1/Mod(roots(var_rid)))
  if(sum(1/Mod(roots(var_rid)) <=1)){
    cat("\nThe VAR model is NOT stationary\n")
  }else{
    cat("\nThe VAR model is stationary\n")
  }
  
  for(i in 1:k){
    test=causality(x = var_rid,cause = colnames(data)[i])$Granger
    if(test$p.value<=0.05){
      cat("\n",colnames(data)[i], "cause (",colnames(data)[-i],") according to Granger causality")
    }else{
      cat("\n",colnames(data)[i], "do not cause (",colnames(data)[-i],") according to Granger causality")
    }
  }
  
  
  irf=irf(var_rid,n.ahead = n.ahead,ortho = TRUE)

  irf_data <- irf$irf[[1]]
  colnames(irf_data)=colnames(data)
  #plotting the IRF, this must be done outside the for cicle because, then inside
  #we add to something already created the new plot
  irf_df <- data.frame(x = rep(seq(nrow(irf_data)), ncol(irf_data)), 
                       y = as.vector(irf_data), 
                       variable = rep(colnames(irf_data), each = nrow(irf_data)))
  p <- ggplot(irf_df, aes(x = x, y = y, color = variable)) +
    geom_line(linewidth= 1.0) +
    labs(title = paste("Impulse Response Functions from ",colnames(data)[1]))
  
  for(i in 2:k){
    irf_data <- irf$irf[[i]]
    colnames(irf_data)=colnames(data)
    irf_df <- data.frame(x = rep(seq(nrow(irf_data)), ncol(irf_data)), 
                         y = as.vector(irf_data), 
                         variable = rep(colnames(irf_data), each = nrow(irf_data)))
    
    p <- p + ggplot(irf_df, aes(x = x, y = y, color = variable)) +
      geom_line(linewidth= 1.0) +
      labs(title = paste("Impulse Response Functions from ",colnames(data)[i]))
    
  }
  #combine the plots
  p <- p + plot_layout(ncol = 2)

  plotcomb=wrap_plots(p)
  return(plotcomb)
})


# Portfolio analysis ------------------------------------------------------
#This method perform a portfolio analysis according to the Markovitz model.
#The arguments passed to the methods are:
#1) data: a matrix containing per each column return of a different company
#2) expRet: the expected return to obtain

stock$set("public", "portfolioAnalysis", function(data,expRet){
  stopifnot(all(is.numeric(data)))
  stopifnot(is.numeric(expRet) & length(expRet)==1)
  N <- ncol(data)
  mu <- colMeans(data)
  Sigma <- var(data)
  A <- t(rep(1,N))%*%solve(Sigma)%*%mu
  B <- t(rep(1,N))%*%solve(Sigma)%*%rep(1,N)
  C <- t(mu)%*%solve(Sigma)%*%mu
  D <- A^2-B*C
  A <- as.vector(A)
  B <- as.vector(B)
  C <- as.vector(C)
  D <- as.vector(D)
  lambda1 <- as.vector((A-B*expRet)/D)
  lambda2 <- as.vector((A*expRet-C)/D)
  
  w.ott <- as.vector(lambda1*solve(Sigma)%*%mu+lambda2*solve(Sigma)%*%rep(1,N))
  
  frontier <- function(mu.bar) {
    -B/D * mu.bar^2 + 2*A/D * mu.bar - C/D
  }
  res=rbind(colnames(data),round(w.ott,4))
  cat("\nThe weights for each titles are: \n")
  write.table(res, quote = FALSE, row.names = FALSE,col.names = FALSE)
  cat("\nThe expected return is ",t(w.ott)%*%mu)
  cat("\nThe variance is ",t(w.ott)%*%Sigma%*%w.ott) #quantification of the risk
  
  mu.bar <- seq(0.0001, 0.15, length = 10000)

  #creating the graph
  df <- data.frame(mu.bar, frontier(mu.bar))
  
  p1=ggplot(df, aes(x = sqrt(frontier(mu.bar)), y = mu.bar)) +
    geom_path(color = "black", size = 1) +
    geom_point(data = data.frame(x = c(sqrt(diag(Sigma)),sqrt(1/B)), y = c(colMeans(data), A/B)), 
               aes(x = x, y = y), color = c(rep("light green",ncol(data)),"orange"), size = 3, shape = 16) +
    scale_x_continuous(limits = c(0, 10)) +
    xlab(expression(sigma[p])) +
    ylab("Expected return")
  return(p1)
})


# helper functions --------------------------------------------------------


#function that create log-return from prices
toLogReturn=function(prices){
  stopifnot(all(is.numeric(prices)))
  logRet=100*c(mean(diff(log(prices))),diff(log(prices)))
  return(logRet)
}


#the arguments of this functions are:
#1) input date: a date in format yyyy/mm/dd (the one chosen from the user interface)
#2) data_vec: a vector of dates (corresponds to dates of downloaded data)
#the problem is that the stock market is not open all the days in the year, but the 
#user is allowed the day he wants from the calendar.
find_nearest_date <- function(input_date, date_vec) {
  stopifnot(all(grepl("\\d{4}-\\d{2}-\\d{2}", date_vec)))
  stopifnot(grepl("\\d{4}-\\d{2}-\\d{2}", input_date))
  stopifnot(length(input_date) == 1)
  
  nearest_index <- which.min(abs(as.numeric(date_vec) - as.numeric(input_date)))
  return(date_vec[nearest_index])
}


#this function is a function used by method VARModel.
#take as argument a varest object (crated by vars::VAR()) and it is necessary for 
#the stepwise backward selection. Basically return as output all the equations of the 
#not anymore in a list of lists but one below the other.
result=function(varest){
  stopifnot(class(varest)=="varest")
  output=coef(varest)
  mat_coef=c()
  for(i in 1:length(output)){
    mat_coef=rbind(mat_coef,output[[i]])
  }
  return(mat_coef)
}


#this is the function that perform the stepwise backward of the full fitted VAR model.
#take as arguments:
#1) varest: the full VAR model with non significant parameters
#2) p: the order of the VAR model
#3) k: the number of columns of the data

stepwise_back=function(varest,p,k){
  stopifnot(class(varest)=="varest")
  stopifnot(p >= 0 | k >= 1)
  
  mat_coef_st=result(varest)#recall to the function
  p_val=mat_coef_st[,4] #selection of the p value
  pos_diag=seq(1,k*k*p,by=k*p+1) #position of the variables not to remove
  pos=which(p_val >=0.1 & ! p_val %in% p_val[pos_diag]) #position of the variables to remove
  mat_coef_st[pos,]=0 #put the line as zero

  mat_par=matrix(mat_coef_st[,1],k,k*p,byrow = T) #order in a matrix
  mat_restr=ifelse(mat_par != 0,1,0)#build the matrix restriction of 1 and 0
  var_rid=vars::restrict(varest,method = "man",resmat = mat_restr)

  count=length(pos)#number of parameters removed so far

  while(length(pos) != 0){
    #re do until the condition is met (all the parameters are significant)
    mat_coef=result(var_rid)
    righe_sost=apply(mat_coef_st, 1, function(x) !all(x == 0))
    mat_coef_st[righe_sost,]=mat_coef

    p_val=mat_coef_st[,4]
    pos_diag=seq(1,k*k*p,by=k*p+1)
    pos=which(p_val==max(p_val[p_val >=0.05 & ! p_val %in% p_val[pos_diag]]))
    mat_coef_st[pos,]=0

    mat_par=matrix(mat_coef_st[,1],k,k*p,byrow = T) 
    mat_restr=ifelse(mat_par != 0,1,0)
    var_rid=vars::restrict(varest,method = "man",resmat = mat_restr)
    count=count+1
  }

  cat("\nBy the stepwise backward,",count-1,"parameters have been removed over",k*p*k)
  return(var_rid)
}






# data=cbind(AAPL$logRet,MSFT$logRet,AMZN$logRet,PG$logRet,JNJ$logRet,NKE$logRet)
# # varest=vars::VAR(data,p=1)
# # class(varest)
# # data=100*diff(log(data))
# AAPL$portfolioAnalysis(data,0.05)

# risultati(AAPL$adjPrice)
# colnames(data)=c("a","b","c","d","e")
