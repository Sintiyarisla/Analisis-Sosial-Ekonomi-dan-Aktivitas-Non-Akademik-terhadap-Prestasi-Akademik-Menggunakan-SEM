rm(list = ls(all = TRUE))
graphics.off()
shell("cls")

## Import Data
data <- read.csv("C:\\Users\\ASUS\\OneDrive\\Documents\\Semester 4\\Analisis Multivariat\\UAS\\ResearchInformation3.csv", header = TRUE)

## Visualisasi Data Per-Kolom 
options(repr.plot.width = 16, repr.plot.height = 4)

n_cols <- ncol(data)
par(mfrow = c(1, min(4, n_cols)), mar = c(4, 4, 2, 1))

for (col in colnames(data)) {
  if (is.numeric(data[[col]])) {
    hist(data[[col]],
         main = paste("Histogram of", col),
         xlab = col,
         col = "lightblue",
         border = "black")
  } else {
    counts <- table(data[[col]])
    barplot(counts,
            main = paste("Barplot of", col),
            xlab = col,
            col = "lightgreen",
            border = "black",
            las = 2) 
  }
}

par(mfrow = c(1, 1))


## Pre-Processing 

### Missing Value

sum(is.na(data))
p <- ncol(data)
colMeans(is.na(data)) * 100


### Data Duplikat

duplikat <- data[duplicated(data), ]
print(duplikat)


### Ubah numerik 

data_numeric <- data

for (col in colnames(data_numeric)) {
  if (!is.numeric(data_numeric[[col]])) {
    data_numeric[[col]] <- as.numeric(as.factor(data_numeric[[col]]))
  }
}


## Visualisasi setelah pre-processing
options(repr.plot.width = 16, repr.plot.height = 4)
par(mfrow = c(1, 4), mar = c(4, 4, 2, 1))
num_plots <- 0
for (col in colnames(data_numeric)) {
  if (is.numeric(data_numeric[[col]])) {
    hist(data_numeric[[col]],
         main = paste("Histogram of", col),
         xlab = col,
         col = "lightblue",
         border = "black")
    num_plots <- num_plots + 1
  }
}
par(mfrow = c(1, 1))


## Normalisasi Z-score

data_z <- as.data.frame(lapply(data_numeric, function(x) {
  if (is.numeric(x)) (x - mean(x)) / sd(x) else x
}))

data_z

## Statistika Deskriptif

### Data sebelum pre-processing
summary_stats <- summary(data)
print(summary_stats)

### Data Setelah pre-processing dan normalisasi
summary_stats <- summary(data_numeric)
print(summary_stats)

### Skew dan Kurtosis
library(psych)
describe(data_numeric)

## Uji Asumsi

### Uji Normal Multivariat
library(psych)

mardia(data_z)


### Uji Multikolinearitas

#### Determinan Matriks Kovarians
data_manifest <- data_z[, c(
  "Income", "Hometown", "English", "Computer", "Extra",
  "Job", "Gaming", "Preparation", "Attendance", "Last",
  "Overall", "Semester", "HSC", "SSC"
)]
data_manifest[] <- lapply(data_manifest, function(x) as.numeric(as.character(x)))
cov_matrix <- cov(data_manifest, use = "complete.obs")
det_cov <- det(cov_matrix)

print(paste("Determinan matriks kovarians:", det_cov))

#### Pake VIF

library(car)

data_manifest_clean <- na.omit(data_manifest)

model_vif <- lm(Overall ~ Income + Hometown + English + Computer + Extra +
                  Job + Gaming + Preparation + Attendance + Last + Semester + HSC + SSC,
                data = data_manifest_clean)

vif_values <- vif(model_vif)

print(vif_values)


### Uji Kecukupan Sampel

#### KMO
library(psych)
r <- cor(data_z)
KMO(r)


## CFA (Confirmatory Factor Analysis)
library(lavaan)

### CFA untuk konstruk Sosial Ekonomi (X1)
model_cfa_x1 <- '
  SosialEkonomi =~ Hometown + English + Computer + Income + Job
'

### CFA untuk konstruk Aktivitas Non-Akademik (X2)
model_cfa_x2 <- '
  AktivitasNonAkademik =~ Extra + Gaming + Preparation + Attendance
'

### CFA untuk konstruk Prestasi Akademik (Y)
model_cfa_y <- '
  PrestasiAkademik =~ Semester + Overall + HSC + Last
'

### Fit masing-masing model
fit_x1 <- cfa(model_cfa_x1, data = data_z, std.lv = TRUE)
fit_x2 <- cfa(model_cfa_x2, data = data_z, std.lv = TRUE)
fit_y  <- cfa(model_cfa_y,  data = data_z, std.lv = TRUE)

summary(fit_x1, fit.measures = TRUE, standardized = TRUE)

## Visualisasi CFA Konstruk Sosial Ekonomi

library(semPlot)
semPaths(
  object = fit_x1,
  what = "path",
  whatLabels = "std",
  style = "ram",
  layout = "tree",
  rotation = 2,
  sizeMan = 6,
  sizeLat = 7,
  edge.label.cex = 1.2,
  label.cex = 1.3,
  color = list(lat = "lightblue", man = "lightgreen")
)

summary(fit_x2, fit.measures = TRUE, standardized = TRUE)

## Visualisasi CFA Konstruk Aktivitas Non-Akademik

library(semPlot)
semPaths(
  object = fit_x2,
  what = "path",
  whatLabels = "std",
  style = "ram",
  layout = "tree",
  rotation = 2,
  sizeMan = 6,
  sizeLat = 7,
  edge.label.cex = 1.2,
  label.cex = 1.3,
  color = list(lat = "lightcoral", man = "lightyellow")
)

summary(fit_y,  fit.measures = TRUE, standardized = TRUE)

## Visualisasi CFA Konstruk Prestasi Akademik

library(semPlot)
semPaths(
  object = fit_y,
  what = "path",
  whatLabels = "std",
  style = "ram",
  layout = "tree",
  rotation = 2,
  sizeMan = 6,
  sizeLat = 7,
  edge.label.cex = 1.2,
  label.cex = 1.3,
  color = list(lat = "lightpink", man = "lightcyan")
)

## Ringkasan Fit Indeks CFA
cfa_fits <- data.frame(
  Konstruk = c("SosialEkonomi", "AktivitasNonAkademik", "PrestasiAkademik"),
  CFI = c(fitMeasures(fit_x1, "cfi"),
          fitMeasures(fit_x2, "cfi"),
          fitMeasures(fit_y,  "cfi")),
  RMSEA = c(fitMeasures(fit_x1, "rmsea"),
            fitMeasures(fit_x2, "rmsea"),
            fitMeasures(fit_y,  "rmsea")),
  SRMR = c(fitMeasures(fit_x1, "srmr"),
           fitMeasures(fit_x2, "srmr"),
           fitMeasures(fit_y,  "srmr")),
  TLI = c(fitMeasures(fit_x1, "tli"),
          fitMeasures(fit_x2, "tli"),
          fitMeasures(fit_y,  "tli"))
)
print(cfa_fits)

## Perhitungan Composite Reliability (CR)
# Fungsi untuk menghitung CR dari fit CFA
hitung_CR <- function(fit) {
  std <- standardizedSolution(fit)
  lambda <- std$est[std$op == "=~"]
  theta <- 1 - lambda^2
  CR <- sum(lambda)^2 / (sum(lambda)^2 + sum(theta))
  return(CR)
}

CR_x1 <- hitung_CR(fit_x1)
CR_x2 <- hitung_CR(fit_x2)
CR_y  <- hitung_CR(fit_y)

cr_df <- data.frame(
  Konstruk = c("SosialEkonomi", "AktivitasNonAkademik", "PrestasiAkademik"),
  CR = c(CR_x1, CR_x2, CR_y)
)

print(cr_df)

## Structural Equation Modeling (SEM)

model_sem <- '
  # Konstruk Sosial Ekonomi (X1)
  SosialEkonomi =~ Income + Hometown + English + Computer + Job
  
  # Konstruk Aktivitas Non-Akademik (X2)
  AktivitasNonAkademik =~ Extra + Gaming + Preparation + Attendance
  
  # Konstruk Prestasi Akademik (Y)
  PrestasiAkademik =~ Last + Semester + Overall + HSC 
  
  # Hubungan antar konstruk
  PrestasiAkademik ~ SosialEkonomi + AktivitasNonAkademik
'

library(lavaan)
fit_sem <- sem(model_sem, data = data_z, std.lv = TRUE)

summary(fit_sem, fit.measures = TRUE, standardized = TRUE)


## Visualisasi SEM
library(semPlot)

semPaths(
  object = fit_sem,
  what = "path",
  whatLabels = "std",
  style = "ram",
  layout = "tree",
  rotation = 2,
  sizeMan = 7,
  sizeLat = 7,
  color = "lightgray",
  edge.label.cex = 1.2,
  label.cex = 1.3
)

