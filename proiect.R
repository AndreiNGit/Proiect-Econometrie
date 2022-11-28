#setup
rm(list = ls()) 
directory <- "C:/Users/nicus/OneDrive - Academia de Studii Economice din Bucuresti/desktop/Proiect econometrie/"

#Install pachete
PackageNames <- c("tidyverse", "stargazer", "magrittr")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

# Quality of Life - Stability
date <- read.csv(paste0(directory, "QualityOfLife.csv"))

date %<>% select(TotalQualityOfLife, Stability, Rights, Health, Safety, Climate, Costs, Popularity)
str(date)
stargazer(date, type = "text")
#primele 10 date
head(date, 10)

# Corelatie
cor(date)

# Regresia simpla: salary = beta0 + beta1*roe + u
model_date <- lm(formula = TotalQualityOfLife ~ Stability, data = date)
summary(model_date)
model_date$coefficients['Stability']

# pachetul GGPLOT2 care ne ajuta sa obtinem grafice mult mai aspectuase
ggplot(data = date, mapping = aes(x = Stability, y = TotalQualityOfLife)) +
  theme_bw() + # setarea temei graficului
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#Predictii

date %<>% mutate(TotalQualityOfLifeHat = fitted(model_date))
stargazer(date, type = "text")
ggplot(data = date, mapping = aes(x = Stability)) +
  geom_point(mapping = aes(y = TotalQualityOfLife, color = 'TotalQualityOfLife - actual value')) +
  geom_point(mapping = aes(y = TotalQualityOfLifeHat, color = 'TotalQualityOfLifeHat - predicted value')) + 
  xlab('Stability')

# Reziduuri
date %<>% mutate(uhat = residuals(model_date))
stargazer(date, type = "text")
ggplot(date, aes(x = Stability)) +
  geom_point(aes(y = TotalQualityOfLife, col = 'TotalQualityOfLife - actual value')) +
  geom_point(aes(y = uhat, col = 'Residual uhat')) +
  xlab('Stability')

head(date, 10)

#----------------------

# Quality of Life - Rights
date <- read.csv(paste0(directory, "QualityOfLife.csv"))

date %<>% select(TotalQualityOfLife, Rights)
str(date)
#Ne arata medie si altele
stargazer(date, type = "text")
#primele 10 date
# head(date, 10)

# Corelatie
cor(date)

# Regresia simpla: salary = beta0 + beta1*roe + u
model_date <- lm(formula = TotalQualityOfLife ~ Rights, data = date)
summary(model_date)
model_date$coefficients['Rights']

# pachetul GGPLOT2 care ne ajuta sa obtinem grafice mult mai aspectuase
ggplot(data = date, mapping = aes(x = Rights, y = TotalQualityOfLife)) +
  theme_bw() + # setarea temei graficului
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)




# Quality of Life - Health
date <- read.csv(paste0(directory, "QualityOfLife.csv"))

date %<>% select(TotalQualityOfLife, Health)
str(date)
#Ne arata medie si altele
stargazer(date, type = "text")
#primele 10 date
# head(date, 10)

# Corelatie
cor(date)

# Regresia simpla: salary = beta0 + beta1*roe + u
model_date <- lm(formula = TotalQualityOfLife ~ Health, data = date)
summary(model_date)
model_date$coefficients['Health']

# pachetul GGPLOT2 care ne ajuta sa obtinem grafice mult mai aspectuase
ggplot(data = date, mapping = aes(x = Health, y = TotalQualityOfLife)) +
  theme_bw() + # setarea temei graficului
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)





# Quality of Life - Safety
date <- read.csv(paste0(directory, "QualityOfLife.csv"))

date %<>% select(TotalQualityOfLife, Safety)
str(date)
#Ne arata medie si altele
stargazer(date, type = "text")
#primele 10 date
# head(date, 10)

# Corelatie
cor(date)

# Regresia simpla: salary = beta0 + beta1*roe + u
model_date <- lm(formula = TotalQualityOfLife ~ Safety, data = date)
summary(model_date)
model_date$coefficients['Safety']

# pachetul GGPLOT2 care ne ajuta sa obtinem grafice mult mai aspectuase
ggplot(data = date, mapping = aes(x = Safety, y = TotalQualityOfLife)) +
  theme_bw() + # setarea temei graficului
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# Quality of Life - Climate
date <- read.csv(paste0(directory, "QualityOfLife.csv"))

date %<>% select(TotalQualityOfLife, Climate)
str(date)
#Ne arata medie si altele
stargazer(date, type = "text")
#primele 10 date
# head(date, 10)

# Corelatie
cor(date)

# Regresia simpla: salary = beta0 + beta1*roe + u
model_date <- lm(formula = TotalQualityOfLife ~ Climate, data = date)
summary(model_date)
model_date$coefficients['Climate']

# pachetul GGPLOT2 care ne ajuta sa obtinem grafice mult mai aspectuase
ggplot(data = date, mapping = aes(x = Climate, y = TotalQualityOfLife)) +
  theme_bw() + # setarea temei graficului
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Quality of Life - Costs
date <- read.csv(paste0(directory, "QualityOfLife.csv"))

date %<>% select(TotalQualityOfLife, Costs)
str(date)
#Ne arata medie si altele
stargazer(date, type = "text")
#primele 10 date
# head(date, 10)

# Corelatie
cor(date)

# Regresia simpla: salary = beta0 + beta1*roe + u
model_date <- lm(formula = TotalQualityOfLife ~ Costs, data = date)
summary(model_date)
model_date$coefficients['Costs']

# pachetul GGPLOT2 care ne ajuta sa obtinem grafice mult mai aspectuase
ggplot(data = date, mapping = aes(x = Costs, y = TotalQualityOfLife)) +
  theme_bw() + # setarea temei graficului
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Quality of Life - Popularity
date <- read.csv(paste0(directory, "QualityOfLife.csv"))
View(date)
hist(date$Stability)

date %<>% select(TotalQualityOfLife, Popularity)
str(date)
#Ne arata medie si altele
stargazer(date, type = "text")
#primele 10 date
# head(date, 10)

# Corelatie
cor(date)

# Regresia simpla: salary = beta0 + beta1*roe + u
model_date <- lm(formula = TotalQualityOfLife ~ Popularity, data = date)
summary(model_date)
model_date$coefficients['Popularity']

# pachetul GGPLOT2 care ne ajuta sa obtinem grafice mult mai aspectuase
ggplot(data = date, mapping = aes(x = Popularity, y = TotalQualityOfLife)) +
  theme_bw() + # setarea temei graficului
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)