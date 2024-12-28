install.packages("ggplot2")
install.packages("dplyr")
install.packages("car")

#Langkah 1: Membuat Data
#Kita akan membuat data simulasi untuk analisis regresi linear sederhana. Misalkan kita ingin menganalisis hubungan antara jam memancing (X) dan jumlah ikan yang didapat (Y).
# Mengatur seed untuk reproduktifitas
set.seed(456)
# Membuat data simulasi
n <- 100
jam_memancing <- runif(n, 1, 12)  # Jam memancing antara 1 hingga 12
error <- rnorm(n, mean = 0, sd = 3)  # Error normal
jumlah_ikan <- 5 + 2 * jam_memancing + error  # Model regresi
# Menggabungkan data ke dalam data frame
data <- data.frame(jam_memancing, jumlah_ikan)

#Langkah 2: Uji Asumsi
#1.Linearitas: Hubungan antara variabel independen dan dependen harus linear.
#2.Normalitas: Residual harus terdistribusi normal.
#3.Homoscedasticity: Varians residual harus konstan.
# Uji linearitas
plot(data$jam_memancing, data$jumlah_ikan, main = "Scatterplot Jam Memancing vs Jumlah Ikan", xlab = "Jam Memancing", ylab = "Jumlah Ikan")
# Uji normalitas residual
model <- lm(jumlah_ikan ~ jam_memancing, data = data)
residuals <- model$residuals
shapiro.test(residuals)  # Uji Shapiro-Wilk untuk normalitas
# Uji homoscedasticity
plot(model$fitted.values, residuals, main = "Plot Residuals", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")

#Langkah 3: Analisis
# Melakukan regresi linear
model <- lm(jumlah_ikan ~ jam_memancing, data = data)
# Menampilkan ringkasan model
summary(model)

#Langkah 4: Visualisasi
# Visualisasi hasil regresi
library(ggplot2)

ggplot(data, aes(x = jam_memancing, y = jumlah_ikan)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Regresi Linear Sederhana: Jam Memancing vs Jumlah Ikan",
       x = "Jam Memancing",
       y = "Jumlah Ikan")

