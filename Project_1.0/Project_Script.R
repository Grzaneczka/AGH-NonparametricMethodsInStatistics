# Skript do sprawozdania 
library(dplyr)
library(tidyr)
library(ggplot2)
library(tseries)

# Dane 
#ilość symulacji 
N <- 1000
#długość próby 
lenghts <- seq(10, 40, by = 3)
#stopnie swobody
dfs <- seq(5, 25, by = 2)
#poziomy istotno?ci
alphas <- c(.02, .05, .1, .2)

params <- expand.grid(lenght = lenghts, df = dfs, alpha =  alphas)

# Test Shapiro-Wilka
set.seed(20)
powers_sw <- sapply(1:nrow(params),
                 function(i){
                   length <- params[i,1]
                   df <- params[i, 2]
                   alpha <- params[i, 3]
                   p_vector <- sapply(rep(df, N),
                                      function(x){
                                        my_sample <- rt(length, df)
                                        shapiro.test(my_sample)$p.value
                                        })
                   mean(p_vector < alpha)
                 })

shapiro <- bind_cols(params, power = powers_sw)

# Test Kołmogorowa-Smirnowa
powers_ks <- sapply(1:nrow(params),
                    function(i){
                      length <- params[i,1]
                      df <- params[i, 2]
                      alpha <- params[i, 3]
                      p_vector <- sapply(rep(df, N),
                                         function(x){
                                           my_sample <- rt(length, df)
                                           ks.test(my_sample, pnorm)$p.value
                                         })
                      mean(p_vector < alpha)
                    })

kolmogorow <- bind_cols(params, power = powers_ks)

# Test Jarque-Bera
powers_jb <- sapply(1:nrow(params),
                 function(i){
                   length <- params[i,1]
                   df <- params[i, 2]
                   alpha <- params[i, 3]
                   p_vector <- sapply(rep(df, N),
                                      function(x){
                                        my_sample <- rt(length, df)
                                        jarque.bera.test(my_sample)$p.value
                                        })
                   mean(p_vector < alpha)
                 })

jarque <- bind_cols(params, power = powers_jb)

# Wykresy Testu Shaprio-Wilka

  ggplot(shapiro, aes(x = df, y = power, color = factor(lenght))) +
  labs(title = "Test Shapiro-Wilka") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line() + 
  facet_wrap(~ alpha, nrow = 2)

# Wykresy Testu Kołmogorowa-Smirnowa
  ggplot(kolmogorow, aes(x = df, y = power, color = factor(lenght))) +
  labs(title = "Test Komogorowa-Smirnowa") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line() + 
  facet_wrap(~ alpha, nrow = 2)

# Wykresy Testu Jarque-Bera
  
  ggplot(jarque, aes(x = df, y = power, color = factor(lenght))) +
  labs(title = "Test Jaque-Bera") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line() + 
  facet_wrap(~ alpha, nrow = 2)


# Wykres porównawczy mocy testów dla alpha = 5% i length = 16. Na osi X znajdują się stopnie swobody, natomiast na osi Y moc testów.
  shapiro_wide <- spread(shapiro, lenght, power)
  kolmogorow_wide <- spread(kolmogorow, lenght, power)
  jarque_wide <- spread(jarque, lenght, power)
  
  shapiro_wide <- shapiro_wide[which(shapiro_wide[,2] == .05), c(1,2,5)]
  shapiro_wide$name <- "Shapiro-Wilka"
  colnames(shapiro_wide)[3] = "power"
  kolmogorow_wide <- kolmogorow_wide[which(kolmogorow_wide[,2] == .05), c(1,2,5)]
  kolmogorow_wide$name <- "Kolmogorowa"
  colnames(kolmogorow_wide)[3] = "power"
  jarque_wide <- jarque_wide[which(jarque_wide[,2] == .05), c(1,2,5)]
  jarque_wide$name <- "Jarque-Bera" 
  colnames(jarque_wide)[3] = "power"
  
  all_wide <- data.frame(shapiro_wide)
  all_wide <- rbind(all_wide, kolmogorow_wide, jarque_wide)
  
  ggplot(all_wide, aes(x = df, y = power, color = factor(name))) +
  labs(title = "Zestawienie dla alpha = 5% i length = 16") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line()
  
# Wykres porównawczy mocy testów dla alpha = 5% i df = 11. Na osi X znajdują się stopnie swobody, natomiast na osi Y moc testów.
  
  shapiro_wide <- spread(shapiro, df, power)
  kolmogorow_wide <- spread(kolmogorow,  df, power)
  jarque_wide <- spread(jarque,  df, power)
  
  shapiro_wide <- shapiro_wide[which(shapiro_wide[,2] == .05), c(1,2,6)]
  shapiro_wide$name <- "Shapiro-Wilka"
  colnames(shapiro_wide)[3] = "power"
  kolmogorow_wide <- kolmogorow_wide[which(kolmogorow_wide[,2] == .05), c(1,2,6)]
  kolmogorow_wide$name <- "Kolmogorowa"
  colnames(kolmogorow_wide)[3] = "power"
  jarque_wide <- jarque_wide[which(jarque_wide[,2] == .05), c(1,2,6)]
  jarque_wide$name <- "Jarque-Bera" 
  colnames(jarque_wide)[3] = "power"
  
  all_wide <- data.frame(shapiro_wide)
  all_wide <- rbind(all_wide, kolmogorow_wide, jarque_wide)
  
  ggplot(all_wide, aes(x = lenght, y = power, color = factor(name))) +
  labs(title = "Zestawienie dla alpha = 5% i df = 11") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line()
  
# Wykres porównawczy mocy testów dla length = 16 i df = 11. Na osi X znajdują się stopnie swobody, natomiast na osi Y moc testów.
  
  shapiro_wide <- spread(shapiro, df, power)
  kolmogorow_wide <- spread(kolmogorow,  df, power)
  jarque_wide <- spread(jarque,  df, power)
  
  shapiro_wide <- shapiro_wide[which(shapiro_wide[,1] == 16), c(1,2,6)]
  shapiro_wide$name <- "Shapiro-Wilka"
  colnames(shapiro_wide)[3] = "power"
  kolmogorow_wide <- kolmogorow_wide[which(kolmogorow_wide[,1] == 16), c(1,2,6)]
  kolmogorow_wide$name <- "Kolmogorowa"
  colnames(kolmogorow_wide)[3] = "power"
  jarque_wide <- jarque_wide[which(jarque_wide[,1] == 16), c(1,2,6)]
  jarque_wide$name <- "Jarque-Bera" 
  colnames(jarque_wide)[3] = "power"
  
  all_wide <- data.frame(shapiro_wide)
  all_wide <- rbind(all_wide, kolmogorow_wide, jarque_wide)
  
  ggplot(all_wide, aes(x = alpha, y = power, color = factor(name))) +
  labs(title = "Zestawienie dla length = 16 i df = 11") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line()
  