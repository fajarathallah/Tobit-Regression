# import library
library(tidyverse)
library(AER)
library(car)

# Memanggil dataset
data1<-read.csv('data2.csv')
data1

#Preprocessing data
data_clean <- data1 %>% 
  select(-kabupaten_kota)
data_clean

#Statistik Deskriptif
descriptive_stats <- data_clean %>%
  summarise_all(list(mean = ~mean(.),
                     sd = ~sd(.),
                     var = ~var(.)))
descriptive_stats

hist(data_clean$jumlah_angka_kematian_akibat_aids,breaks = 12)

#Membuat Pearson Korelasi
## Memuat pustaka yang diperlukan
library(ggplot2)
library(reshape2)
library(tidyr)
## Menghitung matriks korelasi
cor_matrix <- cor(data_clean)

## Mengubah data ke format panjang
melted_cor_matrix <- melt(cor_matrix)

## Membuat heatmap korelasi
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  geom_text(aes(Var1, Var2, label = round(value, 2)), color = "black", size = 4) +
  labs(title = " Korelasi antar variabel", x = "", y = "")

data_clean <- rename(data_clean,y = jumlah_angka_kematian_akibat_aids, x1 = jumlah_kasus_donor_darah_positif_hiv, 
                     x2 = jumlah_kasus_penyakit_sifilis ,x3 =jumlah_penggunaan_kondom)


library(GGally)

ggpairs(data_clean[,c("y", "x1", "x2","x3")])

#Penanganan Outlier
## Boxplot for each variable
boxplot(data_clean$y, main = "Boxplot of y (jumlah_angka_kematian_akibat_aids)")
boxplot(data_clean$x1, main = "Boxplot of x1 (jumlah_kasus_donor_darah_positif_hiv)")
boxplot(data_clean$x2, main = "Boxplot of x2 (jumlah_kasus_penyakit_sifilis)")
boxplot(data_clean$x3, main = "Boxplot of x3 (jumlah_penggunaan_kondom)")

#karena variabel x1 dan x3 mengandung outlier maka akan ditangani dengan melakukan tranformasi variabel x1 dan x3.
## Transform x1 and x3 using log-plus-one transformation
data_cleaned <- data_clean %>% 
  mutate(x1 = log(x1 + 1), 
         x3 = log(x3 + 1))

## View the new dataset
data_cleaned

# Menghitung proposi data tersensor
## Menghitung jumlah dan proporsi data tersensor di sebelah kiri
jumlah_tersensor_kiri <- sum(data_cleaned$y == 0)
proporsi_tersensor_kiri <- jumlah_tersensor_kiri / nrow(data_cleaned)

## Menampilkan hasil
cat("Jumlah data tersensor di sebelah kiri (0):",
    jumlah_tersensor_kiri, "\n")
cat("Proporsi data tersensor di sebelah kiri (0):", proporsi_tersensor_kiri, "\n")


#Menampilkan plot melihat sebarab data tersensor
library(ggplot2)

ggplot(data_cleaned, aes(x = x2, y = y)) + 
  geom_point() + 
  labs(x = "jumlah_kasus_penyakit_sifilis", y = "Jumlah Angka Kematian Akibat AIDS")+
  annotate("rect", xmin = min(data_cleaned$x2), xmax = max(data_clean$x2), ymin = -0.5, ymax = 0.5, alpha = 0.4, color = "red", fill = NA)



ggplot(data_cleaned, aes(x = x1, y = y)) + 
  geom_point() + 
  labs(x = "jumlah_kasus_donor_darah_positif_hiv", y = "Jumlah Angka Kematian Akibat AIDS")+
  annotate("rect", xmin = min(data_cleaned$x1), xmax = max(data_cleaned$x1), ymin = -0.5, ymax = 0.5, alpha = 0.4, color = "red", fill = NA)

#Membuat regresi Tobit 1
tobit_model_1 <-tobit(y ~ x1 + x2 +x3, left = 0, data = data_cleaned)
summary(tobit_model_1)

#Uji Signifikansi Parameter 3 variabel
##Uji Simultan (LRT)
### Buat model terbatas tanpa prediktor
tobit_model_0 <- tobit(y ~ 1, left = 0, data = data_clean)

### Lakukan uji likelihood ratio
lrtest_result <- lrtest(tobit_model_1, tobit_model_0)
print(lrtest_result)

## Uji Partial wald
Anova(tobit_model_1,type='II',test='Wald')

# Asumsi- asumsi regresi
## Asumsi normalitas
### Extract residuals
residuals_1 <- residuals(tobit_model_1)
residuals_1
hist(residuals_1)

###Normality of residuals
library(nortest)
ad.test(residuals_1)

##Asumsi Homoskedastisitas
###Extract residuals and fitted values
residuals <- residuals(tobit_model_1, type = "deviance")
fitted_values <- fitted(tobit_model_1)

### Construct auxiliary regression
aux_model <- lm(I(residuals^2) ~ fitted_values + I(fitted_values^2))

### Perform White test
white_test_statistic <- nrow(data_cleaned) * summary(aux_model)$r.squared
p_value <- 1 - pchisq(white_test_statistic, df = 2)

### Display the results
cat("White Test Statistic:", white_test_statistic, "\n")
cat("p-value:", p_value, "\n")

##Multikolinearitas
####Multicollinearity (VIF)
vif_values <- vif(tobit_model_1)
print(vif_values)

# Dilakukan kesesuain model AIC dan R-squared baik 1 variabel dan keseluruhan variabel
## x2 saja
tobit_model_x2 <-tobit(y ~ x2, left = 0, data = data_cleaned)
summary(tobit_model_x2)
### Menghitung AIC
aic_value_x2 <- AIC(tobit_model_x2)
print(paste("AIC:", aic_value_x2))
### Menghitung pseudo R-squared
ll_full_x2 <- logLik(tobit_model_x2) # log-likelihood model penuh
ll_null_x2 <- logLik(tobit(y ~ 1, left = L, data = data_clean)) # log-likelihood model null
pseudo_r_squared_x2 <- 1 - (as.numeric(ll_full_x2) / as.numeric(ll_null_x2))
print(paste("Pseudo R-squared:", pseudo_r_squared_x2))

##x3 saja
tobit_model_x3 <-tobit(y ~ x3, left = 0, data = data_cleaned)
summary(tobit_model_x3)
### Menghitung AIC
aic_value_x3 <- AIC(tobit_model_x3)
print(paste("AIC:", aic_value_x3))
### Menghitung pseudo R-squared
ll_full_x3 <- logLik(tobit_model_x3) # log-likelihood model penuh
ll_null_x3 <- logLik(tobit(y ~ 1, left = L, data = data_clean)) # log-likelihood model null
pseudo_r_squared_x3 <- 1 - (as.numeric(ll_full_x3) / as.numeric(ll_null_x3))
print(paste("Pseudo R-squared:", pseudo_r_squared_x3))

##Keseluruhan dengan variabel full
###Menghitung AIC
aic_value_1 <- AIC(tobit_model_1)
print(paste("AIC:", aic_value_1))
### Menghitung pseudo R-squared
ll_full_1 <- logLik(tobit_model_1) # log-likelihood model penuh
ll_null_1 <- logLik(tobit(y ~ 1, left = L, data = data_clean)) # log-likelihood model null
pseudo_r_squared_1 <- 1 - (as.numeric(ll_full_1) / as.numeric(ll_null_1))
print(paste("Pseudo R-squared:", pseudo_r_squared_1))

#Regresi Tobit 2
tobit_model2 <-tobit(y ~  x2 + x3, left = L, data = data_clean)
summary(tobit_model2)

# UJi simultan tanpa x1
##Buat model terbatas tanpa prediktor
tobit_model_0 <- tobit(y ~ 1, left = L, data = data_clean)
##Lakukan uji likelihood ratio
lrtest_result <- lrtest(tobit_model2, tobit_model_0)
print(lrtest_result)

#Uji partial tanpa x1
Anova(tobit_model_1,type='II',test='Wald')

#UJi normalitas
shapiro_test <- shapiro.test(residuals)
print(shapiro_test)

#Multikolinearitas
vif_values <- vif(tobit_model2)
print(vif_values)

# Dilakukan kesesuain model AIC dan R-squared baik 1 variabel dan keseluruhan variabel
# #Menghitung AIC
aic_value <- AIC(tobit_model2)
print(paste("AIC:", aic_value))
##Menghitung pseudo R-squared
ll_full <- logLik(tobit_model2) # log-likelihood model penuh
ll_null <- logLik(tobit(y ~ 1, left = L, data = data_clean)) # log-likelihood model null
pseudo_r_squared <- 1 - (as.numeric(ll_full) / as.numeric(ll_null))
print(paste("Pseudo R-squared:", pseudo_r_squared))



