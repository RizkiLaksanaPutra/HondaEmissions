library(ggplot2)
library(readr)
library(scales)

df = read.csv("Honda.csv")
bensin = df$FUELCONSUMPTION
emisi = df$EMISSIONS

# Deklarasi
x = bensin
y = emisi
n = length(bensin)

# Pangkat
x_pangkat = x ^ 2
y_pangkat = y ^ 2

# XY
xy = x * y

# Total
Σx = sum(bensin)
Σy = sum(emisi)
Σx_pangkat = sum(x_pangkat)
Σy_pangkat = sum(y_pangkat)
Σxy = sum(xy)

# Rata-rata
mean_x = Σx / length(x)
mean_y = Σy / length(y)

# Regresi linear
b = (n * Σxy - Σx * Σy) / (n * Σx_pangkat - (Σx)^2)
a = mean_y - (b * mean_x)

y_predict = a + b * x

# Korelasi
r = (n * Σxy - Σx * Σy) / sqrt((n * Σx_pangkat - (Σx)^2) * (n * Σy_pangkat - (Σy)^2))

# Determinasi
r_squared = (r ^ 2)
determinasi = paste(round(r_squared, 2), "%")

# Error
e = y - y_predict

data <- data.frame(x = x, y = y, y_predict = y_predict, error = e)

# Visualisasi
ggplot(
    data,
    aes(x = x, y = y)
) + geom_point(
    size = 1.5
) + geom_line(
    aes(y = y_predict),
    color = "red"
) + geom_segment(
    aes(xend = x, yend = y_predict),
    color = "blue",
    data = data
) + labs(
    x = "FUEL CONSUMPTION",
    y = "EMISSION"
) + ggtitle(
    "Regresi linear emisi pada mobil Honda (2000 - 2022)"
) + geom_text(
    x = max(x),
    y = max(y),
    label = paste(
        "r = ",
        floor(r * 100) / 100
    ), hjust = 2,
    vjust = 0
) + geom_text(
    label = paste("Y =", round(a, 3), " + ",round(b, 3), "X"), 
    x = mean(x),
    y = max(y),
    hjust = 0.5,
    vjust = -1
)
