#setup
rm(list = ls()) 
directory <- "C:/Users/nicus/OneDrive - Academia de Studii Economice din Bucuresti/Documents/Facultate/Econometrie/Proiect-Econometrie/"

#Install pachete
PackageNames <- c("tidyverse", "stargazer", "magrittr", "lmtest", "sandwich", 
                  "olsrr", "moments","whitestrap", "car", "stats", "tsoutliers", "olsrr", "Metrics")
library("scales")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

# ----------- CURATARE DATE -------------------------

date <- read.csv(paste0(directory, "house_offers.csv"))

drop <- c("id","location", "location_area", "type", "partitioning", "real_estate_type", "height_regime", "garages_count", "seller_type")
date = date[,!(names(date) %in% drop)]

date$balconies_count[is.na(date$balconies_count)] = 0
date$parking_lots_count[is.na(date$parking_lots_count)] = 0
sapply(date, function(x) sum(is.na(x)))
date = date %>% drop_na()

date <- date[date$bathrooms_count < 5, ]
date <- date[date$kitchens_count < 5, ]
date <- date[date$parking_lots_count < 5, ]
date <- date[date$balconies_count < 5, ]
date <- date[date$price < 500000, ]

date$construction_year[date$construction_year == 'Parter'] <- 0
date$comfort[date$comfort == 'lux'] <- 4

date$construction_year <- as.integer(date$construction_year)
date$comfort <- as.integer(date$comfort)

stargazer(date, type = "text")

write.csv(date, "C:/Users/nicus/OneDrive - Academia de Studii Economice din Bucuresti/Documents/Facultate/Econometrie/Proiect-Econometrie/house_offers_f.csv",
          row.names = FALSE)


# ------------------ Regresie Simpla --------------------------

# ------ Model regresie simpla folosind useful_surface 

date <- read.csv(paste0(directory, "house_offers_f.csv"))

model1 <- lm(formula = price ~ useful_surface, data = date)
summary(model1)

# Graficul observatiilor cu dreapta estimata
plot(x = date$useful_surface, y = date$price)
abline(a = model1$coefficients['(Intercept)'], 
       b = model1$coefficients['useful_surface'],
       col = 'red')
# pachetul GGPLOT2 care ne ajuta sa obtinem grafice mult mai aspectuase
ggplot(data = date, mapping = aes(x = useful_surface, y = price)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(labels = comma) + 
  scale_y_continuous(labels = comma)

# ------- Model regresie simpla folosind rooms_count

model2 <- lm(formula = price ~ rooms_count, data = date)
summary(model2)

# Graficul observatiilor cu dreapta estimata
plot(x = date$rooms_count, y = date$price)
abline(a = model2$coefficients['(Intercept)'], 
       b = model2$coefficients['rooms_count'],
       col = 'red')
ggplot(data = date, mapping = aes(x = rooms_count, y = price)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(labels = comma) + 
  scale_y_continuous(labels = comma)

# -> In urma testelor alegem modelul 1 cu care continuam analiza cu testarea ipotezelor

date %<>% mutate(pricehat = fitted(model1))
stargazer(date, type = "text")
ggplot(data = date, mapping = aes(x = useful_surface)) +
  geom_point(mapping = aes(y = price, color = 'Price - actual value')) +
  geom_point(mapping = aes(y = pricehat, color = 'Price - predicted value')) + 
  xlab('Useful Surface')

# Reziduuri
date %<>% mutate(uhat = residuals(model1))
stargazer(date, type = "text")
ggplot(date, aes(x = useful_surface)) +
  geom_point(aes(y = price, col = 'Price - actual value')) +
  geom_point(aes(y = uhat, col = 'Price uhat')) +
  xlab('Useful Surface')

head(date, 10)

# Graficul valorilor si reziduurilor reale si previzionate
ggplot(date, aes(x = useful_surface)) +
  geom_point(aes(y = price, color = 'Price - actual value')) +
  geom_point(aes(y = pricehat, color = 'Pricehat - predicted value')) +
  geom_point(aes(y = uhat, color = 'Residual uhat')) +
  geom_smooth(aes(y = price, color = 'Fitted line'), 
              method = "lm", se = FALSE) +
  xlab('Useful Surface')


# ---> testare Heteroschedasticitate

bptest(model1)
white_test(model1)

# Exista heteroschedasticitate care trebuie corectata
# Folosim metoda WLS

#testam mai multe variatii de weights
wols1 <- lm(price ~ useful_surface , data = date, weights = 1/useful_surface)
wols2 <- lm(price ~ useful_surface , data = date, weights = 1/useful_surface^2)
wols3 <- lm(price ~ useful_surface , data = date, weights = 1/abs(fitted(model1)))
wols4 <- lm(price ~ useful_surface , data = date, weights = 1/fitted(model1)^2)
wols5 <- lm(price ~ useful_surface , data = date, weights = 1/resid(model1)^2)
wols6 <- lm(price ~ useful_surface , data = date, weights = 1/abs(resid(model1)))
wols7 <- lm(price ~ useful_surface , data = date, weights = 1/sqrt(useful_surface))

#wols5 este cel mai performant model
date %<>% mutate(uhat = residuals(wols5))
stargazer(date, type = "text")
ggplot(date, aes(x = useful_surface)) +
  geom_point(aes(y = price, col = 'Price - actual value')) +
  geom_point(aes(y = uhat, col = 'Price uhat')) +
  xlab('Useful Surface')

bptest(wols5) #p-value = 1 > 0.01
white_test(wols5)


# -------- testam autocorelarea ---------------

# Testul Durbin-Watson
dw_test <- durbinWatsonTest(model1)

dw_test

# Deoarece avem o valoare a D-W Statistic de 1.97 ( 0 < 1.97 < 4) si un p-value de 0.3 > 0.05 rezulta ca
# modelul nu are autocorelatii

#----------- testam normalitatea ----------------

# Aplicam cei 5 pasi in testarea normalitatii reziduurilor 
# 1. Graficul Residuals vs Fitted
# 2. Graficul Q-Q plot
# 3. Histograma reziduurilor
# 4. Boxplotul reziduurilor
# 5. Testele de normalitate (Shapiro-Wilk si Jarque Bera)

# 1. Graficul Residuals vs Fitted
plot(model1) # primul grafic

# Pasul 2 - Graficul 'Q-Q plot'
plot(model1) # al doilea grafic

# Pasul 3 - Histograma reziduurilor
ggplot(data = date) +
  theme_bw() +
  geom_histogram(mapping = aes(x = uhat), col = 'grey')+
  xlab('Reziduuri') + 
  ylab('Count') +
  ggtitle('Histograma reziduurilor') + 
  theme(plot.title = element_text(hjust = 0.5))

skewness(date$uhat) #1.16 > 0 -> histograma centrata in dreapta
kurtosis(date$uhat) # 8.67 > 3 -> histograma platicurtica (ascutita)

# Pasul 4 - Graficele de tip Boxplot
ggplot(date, aes(x=uhat, y=price)) + 
  geom_boxplot() +
  theme_bw()+
  xlab('Reziduuri') + 
  ylab('Pret') +
  ggtitle('Boxplot reziduuri') + 
  theme(plot.title = element_text(hjust = 0.5))

# Pasul 5 - Testarea normalitatii cu ajutorul testelor specifice acestei ipoteze
# Testul Jarque-Bera pentru normalitate
# H0: distributie normala, Ha: distributie nenormala
jarque.bera.test(date$uhat)
# deoarece p-value < 0.05 => reziduurile nu sunt normal distribuite
ols_test_normality(model1)

# Distanta Cook este folosita pentru a identifica punctele de date influente. 
# Ulterior, vom elimina aceste puncte
# si vom rerula modelul si retesta ipoteza de normalitate
ols_plot_cooksd_bar(model1) 
ols_plot_cooksd_chart(model1)

date_fil <- date[-c(211, 513, 3131, 3229, 5347, 4665, 3943, 3482, 3485, 1321), ]

model_fil <- lm(formula = price ~ useful_surface, data = date_fil)
summary(model_fil)

date_fil %<>% mutate(uhat = resid(model_fil)) # extragem reziduurile din model

#Retestam prin Jarque-Bera
jarque.bera.test(date_fil$uhat)
#Testul respinge ipoteza de normalitate

ols_plot_cooksd_bar(model_fil) 
ols_plot_cooksd_chart(model_fil)

date_fil <- date_fil[-c(450, 142, 813, 2537, 2335, 2858, 3008, 3418, 3852, 4205, 4622,
                    4473, 4411, 4784, 5516, 934, 3705), ]
model_fil <- lm(formula = price ~ useful_surface, data = date_fil)
summary(model_fil)
date_fil %<>% mutate(uhat = resid(model_fil)) # extragem reziduurile din model


#Retestam prin Jarque-Bera
jarque.bera.test(date_fil$uhat)
#Testul respinge ipoteza de normalitate


#incarcam setul de date pentru predictii
pred_date <- read.csv(paste0(directory, "pred.csv"))
pred_date = pred_date %>% drop_na()
#facem predictii pe interval de incredere
preds <- predict(model1, newdata = pred_date, interval="confidence", level=0.90)

#calculam indicatorii rmse si mae
residuals = residuals(model1)
rmse <- sqrt(mean(residuals^2)) #rmse = 35990
mae <- mean(abs(residuals)) #mae = 53695