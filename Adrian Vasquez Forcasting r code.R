require(dplyr)
require("fpp3")
require(ggplot2)

train_Data = read.csv("C:/Users/avasq/OneDrive/Desktop/599 forcasting independent studies/kaggle data set/data/train.csv/train.csv", head=T, stringsAsFactors=F, na.strings='')

test_dat<- read.csv("C:/Users/avasq/OneDrive/Desktop/599 forcasting independent studies/kaggle data set/data/test.csv", head=T, stringsAsFactors=F, na.strings='')


#####test daily data

test_dat_tsibble<-test_dat%>%
  mutate(date=ymd(date))%>%
  as_tsibble(index = date, key= c(id,store_nbr,family,onpromotion))

daly_stor3_grocery1_test <- test_dat_tsibble%>%
  filter(store_nbr ==3, family== 'GROCERY I')%>%
  summarise(onpromotion= sum(onpromotion))

####test monthly data

test_monthly_tsibble<-test_dat%>%
  mutate(date=yearmonth(date))%>%
  as_tsibble(index = date, key= c(id,store_nbr,family,onpromotion))


tail(test_monthly_tsibble)

monthly_stor3_grocery1_test <- test_monthly_tsibble%>%
  filter(store_nbr ==3, family== 'GROCERY I')%>%
  select(id,store_nbr ,family)
summarise(onpromotion= sum(onpromotion), id=id)



#####train daily tsibble data
train_Data_tsibble= train_Data %>%
  mutate(date = ymd(date))%>%
  as_tsibble(index = date, key= c(id,store_nbr,family,onpromotion, sales))%>%  select(-date);

###store 3 grocery
daly_stor3_grocery1 <- train_Data_tsibble%>%
  filter(store_nbr ==3, family== 'GROCERY I')%>%
  summarise(sales= sum(sales), onpromotion=sum(onpromotion))

## turn index into monthly and delete the old date colom from data
train_Data_monthly_tbs= train_Data %>%
  mutate(Monthly_date = yearmonth(date))%>%
  select(-date)%>%
  as_tsibble(index = Monthly_date, key= c(id,store_nbr,family,onpromotion,sales))

tail(train_Data_monthly_tbs)
str(train_Data_monthly_tbs)
head(train_Data_monthly_tbs)

###store 3 grocery
Test_daly_stor3_grocery1 <- test_dat_tsibble%>%
  filter(store_nbr ==3, family== 'GROCERY I')%>%
  summarise(sales= sum(sales), onpromotion= sum(onpromotion))


########monthly with sales sumed###################
train_Data_monthly_tbs= train_Data %>%
  mutate(Monthly_date = yearmonth(date))%>%
  as_tsibble(index = Monthly_date, key= c(id,store_nbr,family,onpromotion,sales))



mean(monthly_stor3_grocery1$Monthly_sales)


############Monthly store 3 wrangle and data visulization##############

monthly_stor3_grocery1 <- train_Data_monthly_tbs%>%
  filter(store_nbr ==3, family== 'GROCERY I')%>%
  summarise(Monthly_sales= sum(sales), onpromotion= sum(onpromotion))
autoplot(monthly_stor3_grocery1)
gg_season(monthly_stor3_grocery1, labels = "both")+  labs(y = "sales",
                                                          title = "Seasonal plot: store 3 grocery depart sales")
gg_subseries(monthly_stor3_grocery1)


###eploration of seasonality

recent_sales <-monthly_stor3_grocery1 |>
  ACF(Monthly_sales ) |>
  autoplot() + labs(title="sales AFC store 3 grocery")

monthly_stor3_grocery1|>
  ACF(Monthly_sales , lag_max = 48) |>
  autoplot() +
  labs(title="monthly_stor3_grocery1")

stor3_fetures <- monthly_stor3_grocery1 |>
  features(Monthly_sales , feature_set(pkgs = "feasts"))


####Simple forecasting

naive_fit <- monthly_stor3_grocery1 |>
  model(Mean = MEAN(Monthly_sales),Naïve= NAIVE(Monthly_sales),Seasonalnaïve = SNAIVE(Monthly_sales))

niave_fit <- monthly_stor3_grocery1 |>
  model(Mean = MEAN(Monthly_sales),
        `Naïve` = NAIVE(Monthly_sales),
        `Seasonal naïve` = SNAIVE(Monthly_sales))


#Forecaset simple
simple_fc <- niave_fit |> forecast(h = 15)
#evaluate
accuracy(niave_fit)
niave_fit%>% gg_tsresiduals()


#visulaize
simple_fc |>
  autoplot(monthly_stor3_grocery1, level = NULL) +
  autolayer(
    filter_index(monthly_stor3_grocery1),
    colour = "black"
  ) +
  labs(
    y = "sales",
    title = "Forecasts for store 3 grocery department"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

### residuals

#visualiztion of just naive

monthly_stor3_grocery1 |>
  model(NAIVE(Monthly_sales)) |>
  forecast(h = 10) |>
  autoplot(monthly_stor3_grocery1) +
  labs(title="Monthly sales store three grocery department", y="sales" )
tail(monthly_stor3_grocery1)

###residual of simple forecast and risidual plot
residual<-augment(niave_fit)
accuracy(naive_fit)

monthly_stor3_grocery1 |>
  model(NAIVE(Monthly_sales)) |>gg_tsresiduals()

autoplot(monthly_stor3_grocery1)+
  labs(title="Monthly Sales Store 3 Grocery",
       y="sales")


######ACF
monthly_stor3_grocery1 |> features(Monthly_sales, feat_acf)

######decomposition of monthly data#########
### note that for decomposition data must be monthly or quartely.
monthly_stor3_grocery1 <- train_Data_monthly_tbs%>%
  filter(store_nbr ==3, family== 'GROCERY I')%>%
  summarise(Monthly_sales= sum(sales), onpromotion=sum(onpromotion))
###additive decomposition
Additive<-monthly_stor3_grocery1 |>
  model(classical_decomposition(Monthly_sales, type = "additive")) |>
  components() |>
  autoplot() +
  labs(title = "Classical additive decomp")
Additive<-monthly_stor3_grocery1 |>
  model(classical_decomposition(Monthly_sales, type = "additive")) 
Additive_stl<-monthly_stor3_grocery1 |>
  model(STL(Monthly_sales ))%>% components()

autoplot(Additive_stl)

multiplicativ<-monthly_stor3_grocery1 |>
  model(classical_decomposition(Monthly_sales, type = "multiplicative")) |>
  components() |>
  autoplot() +
  labs(title = "Classical multiplcative decomp")

augment(multiplicativ)
accuracy(multiplicativ)

monthly_stor3_grocery1%>% model(classical_decomposition(Monthly_sales, type = "additive"))|>components() |>
  autoplot() +
  labs(title = "Classical multiplcative decomp")

Additive |>
  forecast()|>
  autoplot(monthly_stor3_grocery1)+  labs(y = "Sales", title = "sales")




######STL DEcomp

monthly_stor3_grocery1 |>
  model(classical_decomposition(Monthly_sales, type = "additive")) |>
  components() |>
  autoplot() +
  labs(title = "Classical additive decomposition of total monthly sales")
######stl
dcmp <- monthly_stor3_grocery1 |>
  model(stl = STL(Monthly_sales))
components(dcmp)

fit_dcmp <- monthly_stor3_grocery1 |>
  model(stlf = decomposition_model(STL(Monthly_sales  ~ trend(window = 7), robust = TRUE), NAIVE(season_adjust)))
accuracy(fit_dcmp)
augment(fit_dcmp)

# fit_dcmp visualization

fit_dcmp |>
  forecast() |>
  autoplot(monthly_stor3_grocery1)+
  labs(y = "Number of people",
       title = "monthly store 3 grocery")

accuracy(fit_dcmp)
augment(fit_dcmp)


it_dcmp <- monthly_stor3_grocery1 |>
  model(stlf = decomposition_model( X_13ARIMA_SEATS(Monthly_sales ~ x11()), NAIVE(season_adjust)))
accuracy(fit_dcmp)

##X!! Decomp
#In particular, trend-cycle estimates are available for all observations including the end points, and the 
#seasonal component is allowed to vary slowly over time. X-11 also handles trading day variation, 
#holiday effects and the effects of known predictors
x11_dcmp <- monthly_stor3_grocery1 |>
  model(x11 = X_13ARIMA_SEATS(Monthly_sales ~ x11())) |>
  components()

autoplot(x11_dcmp) +
  labs(title =
         "Decomposition of total US retail employment using X-11.")
x11_dcmp |>
  gg_subseries(seasonal)

accuracy(x11_dcmp)

autoplot(x11_dcmp) + labs(title ="X11 Decomposition of grocery sales for store 3.")

x11_dcmp |>
  gg_subseries(seasonal)

x11_dcmp |>
  ggplot(aes(x = Monthly_date )) +
  geom_line(aes(y = Monthly_sales , colour = "Data")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Sales",
       title = "x11 forcast store 3 grocery department") +
  scale_colour_manual(
    values = c("gray", "#0072B2", "#D55E00"),
    breaks = c("Data", "Seasonally Adjusted", "Trend")
  )


#####Seat Decomp

seats_dcmp <- monthly_stor3_grocery1 |>
  model(seats = X_13ARIMA_SEATS(Monthly_sales ~ seats())) |>
  components()
autoplot(seats_dcmp) +
  labs(title =
         "Decomposition of total US retail employment using SEATS")
augment(seats_dcmp)
accuracy(seats_dcmp)
seats_dcmp |>
  gg_subseries(seasonal)

#########moving average of monthly to get trend#######


MA_monthly_stor3_grocery1 <- monthly_stor3_grocery1 |>
  mutate(
    `12-MA` = slider::slide_dbl(Monthly_sales, mean,
                                .before = 5, .after = 6, .complete = TRUE),
    `2x12-MA` = slider::slide_dbl(`12-MA`, mean,
                                  .before = 1, .after = 0, .complete = TRUE) )
autoplot(MA_monthly_stor3_grocery1)

MA_monthly_stor3_grocery1 |>
  autoplot(Monthly_sales, colour = "gray") +
  geom_line(aes(y = `2x12-MA`), colour = "#D55E00") +
  labs(y = "sales",
       title = "Total monthly sales store 3 grocery")

MA_monthly_stor3_grocery1%>%forecast(h=5)

tail(monthly_stor3_grocery1)


####ETS\\


###random ETS
fit_EST <- monthly_stor3_grocery1 |>
  model(ETS(sales ~ error("A") + trend("N") + season("N")))
fc <- fit |>
  forecast(h = 5)

fc |>
  autoplot(monthly_stor3_grocery1) +
  geom_line(aes(y = .fitted), col="#D55E00",
            data = augment(fit)) +
  labs(y="store sales", title="store 3 grocery sales") +
  guides(colour = "none")
#### auto ETS
ETS_AUTO_fit <- monthly_stor3_grocery1 |>
  model(ETS(Monthly_sales))
report(fit)


accuracy(ETS_AUTO_fit)

ETS_AUTO_fit |>
  autoplot(monthly_stor3_grocery1)+
  labs(title="Store 3 grocery department EST",
       y="Monthly_sales")

###ADditive trend and no seasonality

fit2 <- monthly_stor3_grocery1 |>
  model(AAN = ETS(Monthly_sales ~ error("M") + trend("A") + season("N")))
fc_EST <- fit2 |> forecast(h = 10)
augment(fit2)
accuracy(fit2)


###ADditive trend and no  seasonality

MAM <- monthly_stor3_grocery1 |> 
  model(AN = ETS(Monthly_sales ~ trend("A") + season("N")))

autoplot(MAM)
fc_EST <- MAM |> forecast(h = 10)
augment(MAM)
accuracy(MAM)
report(MAM)
MAM |>
  forecast(h = 12) |>
  autoplot(monthly_stor3_grocery1)+
  labs(title="Holt Linear trend Model",
       y="sales")

###fables EST
FableEST<-monthly_stor3_grocery1 |>
  model(FABleEST = ETS(Monthly_sales))
FableEST%>%forecast(h=12)%>%autoplot(monthly_stor3_grocery1)+
  labs(title="Fables Automatic Exponential Smoothing model ",
       y="sales")
report(FableEST)
accuracy(FableEST)

BBB<-components(FableEST)
autoplot(BBB)


####ARIMA
monthly_stor3_grocery1 |> ACF(Monthly_sales) |>
  autoplot() + labs(subtitle = "Store 3 grocery sales")
monthly_stor3_grocery1 |> ACF(difference(Monthly_sales)) |>
  autoplot() + labs(subtitle = "Differenced Store 3 grocery sales")
###number of diferences
monthly_stor3_grocery1 |>
  features(Monthly_sales, unitroot_ndiffs)
###seasonal differencing
monthly_stor3_grocery1 |>
  features(Monthly_sales, unitroot_nsdiffs)

###ARIMA FIt
Arima_fit<- monthly_stor3_grocery1%>% model(ARIMA(Monthly_sales~pdq()))
report(Arima_fit)
accuracy(Arima_fit)
Arima_fit |> forecast(h=12) |>
  autoplot(monthly_stor3_grocery1) +
  labs(y = "Sales", title = "ARIMA(0,1,1)(1,0,0)[12] ")

fitauto_arima <- monthly_stor3_grocery1 |>
  model(arima012011 = ARIMA(Monthly_sales ~ pdq(0,1,2) ),
        arima210011 = ARIMA(Monthly_sales ~ pdq(2,1,0) ),
        auto = ARIMA(Monthly_sales, stepwise = FALSE, approx = FALSE))
report(fitauto_arima)
accuracy(fitauto_arima)

fitauto_arima%>% forecast(h=12)%>% autoplot(monthly_stor3_grocery1)

augment(fit_arima) |>
  ggplot(aes(x = Monthly_date )) +
  geom_line(aes(y = Monthly_sales, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "forcast over actual"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL)
         
 ###regression of monthly
 #regression model
         regression_fit <- monthly_stor3_grocery1 |>
           model(TSLM(Monthly_sales  ~ onpromotion  + trend() + season()))%>% 
           #regression coeficents and r ^2
           report(regression_fit)  
         
         #accuracy of regression
         accuracy(regression_fit)
         
         ####model evaluation
         report(regression_fit)
         
         glance(regression_fit) |>
           select(adj_r_squared, CV, AIC, AICc, BIC)
         
         #chart of the regression
         
         augment(regression_fit) |>
           ggplot(aes(x = Monthly_date )) +
           geom_line(aes(y = Monthly_sales, colour = "Data")) +
           geom_line(aes(y = .fitted, colour = "Fitted")) +
           labs(y = NULL, title = "regression Changes in monthly sales " ) +
           scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
           guides(colour = guide_legend(title = NULL))
         
         ### Chart of the residuals
         regression_fit |> gg_tsresiduals()
         
         # prediction with the new data
         test_store3 <- test_monthly_tsibble%>%
           filter(store_nbr ==3, family== 'GROCERY I')%>%
           select(id,store_nbr , family,Monthly_date=date)%>%
           summarise( onpromotion=sum(onpromotion))  
         
         
         #forecast new data
         
         fc_reg<-regression_fit%>% forecast(test_store3)
         
         
         #regresssion visulization
         fc_regressuib<-regression_fit%>% forecast(new_data=test_store3)
         
         tsibble::fill_gaps(regression_fit);
         
         regression_fit |> gg_tsresiduals()
         
         
         #dynamic Regression
         fit_dynamic_trend_seasonality <- monthly_stor3_grocery1 |>
           model(ARIMA(Monthly_sales  ~ onpromotion  + trend()+season()))
         
         report(fit_dynamic_trend_seasonality)
         
         
         accuracy(fit_dynamic_trend_seasonality)
         
         fit_dynamic_trend_seasonality |> gg_tsresiduals()
         
         
         ##chart of regressionresiduals
         
         bind_rows(
           `Regression residuals` =
             as_tibble(residuals(fit_dynamic_trend_seasonality, type = "regression")),
           `ARIMA residuals` =
             as_tibble(residuals(fit_dynamic_trend_seasonality, type = "innovation")),
           .id = "type") |>
           mutate(
             type = factor(type, levels=c(
               "Regression residuals", "ARIMA residuals"))
           ) |>
           ggplot(aes(x = Monthly_date , y = .resid)) +
           geom_line() +
           facet_grid(vars(type))
         
         augment(fit_dynamic_trend_seasonality) |>
           ggplot(aes(x = Monthly_date )) +
           geom_line(aes(y = Monthly_sales, colour = "Data")) +
           geom_line(aes(y = .fitted, colour = "Fitted")) +
           labs(y = NULL, title = "Dynamic regression actual vrs fitted valuesin monthly sales " ) +
           scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
           guides(colour = guide_legend(title = NULL))
         
         
         monthly_stor3_grocery1_test <- test_monthly_tsibble%>%
           filter(store_nbr ==3, family== 'GROCERY I')%>%
           select(id,store_nbr,family)%>%
           summarise(onpromotion= sum(onpromotion))
         
         fc_dynamic_month <-fit_dynamic_trend_seasonality%>% forecast(monthly_stor3_grocery1_test)
         
         ##HArmonic regression
         
         fit_harmonic <- model(monthly_stor3_grocery1,
                               `K = 1` = ARIMA(log(Monthly_sales) ~  fourier(K=1) + PDQ(0,0,0)),
                               `K = 2` = ARIMA(log(Monthly_sales) ~ fourier(K=2) + PDQ(0,0,0)),
                               `K = 3` = ARIMA(log(Monthly_sales) ~ fourier(K=3) + PDQ(0,0,0)),
                               `K = 4` = ARIMA(log(Monthly_sales) ~ fourier(K=4) + PDQ(0,0,0)),
                               `K = 5` = ARIMA(log(Monthly_sales) ~ fourier(K=5) + PDQ(0,0,0)),
                               `K = 61` = ARIMA(log(Monthly_sales) ~ fourier(K=6) + PDQ(1,0,0)),
                               `K = 6` = ARIMA(log(Monthly_sales) ~ fourier(K=6) + PDQ(0,0,0)))
         report(fit_harmonic)
         accuracy(fit_harmonic)
         
         #visualize harmonic
         fit_harmonic |>
           forecast(h = "2 years")|>
           autoplot(monthly_stor3_grocery1, level = 95) +
           facet_wrap(vars(.model), ncol = 2) +
           guides(colour = "none", fill = "none", level = "none") +
           geom_label(
             aes(x = yearmonth("2017 Jan"), y = 4250,
                 label = paste0("AICc = ", format(AICc))),
             data = glance(fit_harmonic)
           ) +
           labs(title= "Store 3 grocery sales",
                y="log(sales")
         
         
         augment(fit_harmonic) |>
           ggplot(aes(x = Monthly_date )) +
           geom_line(aes(y = Monthly_sales, colour = "Data")) +
           geom_line(aes(y = .fitted, colour = "Fitted")) +
           labs(y = NULL, title = "Harmonic regression actual vrs fitted valuesin monthly sales " ) +
           scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
           guides(colour = guide_legend(title = NULL))
         
         
         
         
         #Profit model without hieracal and using daily data
         require(fable.prophet)
         
         
         
         fit_profit_month<- monthly_stor3_grocery1 |>
           model(profit= prophet(Monthly_sales ~ onpromotion+
                                   season(period = "month", order = 12)+
                                   season(period = "year", order = 24) )) 
         accuracy(fit_profit_month)
         report(fit_profit_month)
         
         
         
         
         monthly_stor3_grocery1_test <- test_monthly_tsibble%>%
           filter(store_nbr ==3, family== 'GROCERY I')%>%
           select(id,store_nbr ,family)%>%
           summarise(onpromotion= sum(onpromotion))
         
         fc_profit_month <-fit_profit%>% forecast(new_data =monthly_stor3_grocery1_test)
         
         fit_profit_month |> gg_tsresiduals() 
         
         
         fit_profit_month |>
           components() |>
           autoplot()
         
         glance(fit_profit_month) 
         
         fc <- fit_profit_month |> forecast(new_data = monthly_stor3_grocery1_test) |> autoplot(monthly_stor3_grocery1_test)
         
         
         
         ### Forecasting Hieracal Data using profit
         
         
         
         #### Final Profit Model that I wasnt able to run becaus e it took days to run
         ###THis model will work but it takes 2 days to generate a report, and I needed to stop it from runing so I could 
         #continue to use r for my other classes but I got it to run one time However, I forgot to add the ID which I need 
         #in order to submit it to kaggle.
         
         train_Data_tsibble= train_Data %>%
           mutate(date = ymd(date))%>%
           as_tsibble(index = date, key= c(id,store_nbr,family,onpromotion, sales))%>%  select(-date);
         
         
    daily_store_agregated_by_fam <- train_Data_tsibble |>
           aggregate_key(store_nbr/family, sales = sum(sales), onpromotion=sum(onpromotion),id=id)
         tail(daily_store_agregated_by_fam)
         
         #visualizations for agregated data
         
         #agregate by stor then family
         store_agregated_by_fam <- train_Data_monthly_tbs |>
           aggregate_key(store_nbr/family, ag_Monthly_sales = sum(sales))
         
         store_agregated_by_fam |>
           filter(store_nbr== 1|store_nbr==2|store_nbr==3|store_nbr==4, is_aggregated(family)) |>
           select(-family) |>
           mutate(store_nbr = factor(store_nbr, levels=c("1","2","3","4"))) |>
           gg_season(ag_Monthly_sales) +
           facet_wrap(vars(store_nbr), nrow = 2, scales = "free_y")+
           labs(y = "sales ('000)")
         
         require(fable.prophet)
         fit_profit<- daily_store_agregated_by_fam |>
           model(profit= prophet(sales ~ onpromotion+
                                   season(period = "day", order = 10) +
                                   season(period = "week", order = 5) +
                                   season(period = "year", order = 3)) )%>%
           reconcile(bu=bottom_up(profit),ols = min_trace(profit, method = "ols"),mint = min_trace(profit , method = "mint_shrink"))
         
         
         ###Transform and agregate test data
         test_dat_tsibble<-test_dat%>%
           mutate(date=ymd(date))%>%
           as_tsibble(index = date, key= c(id,store_nbr,family,onpromotion))%>%  select(-date);
         
         test_agregated_by_fam <- test_dat_tsibble |>
           aggregate_key(store_nbr/family, onpromotion=sum(onpromotion),id=id)
         
         #forecast with new data
         
         fc_profit<- fit_profit%>% forecast(new_data = test_agregated_by_fam )
         
         
         