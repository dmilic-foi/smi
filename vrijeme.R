
Sys.setlocale("LC_TIME", "Croatian")

# Pripremanje dataseta
vrijeme1 <- read.delim("./weather1.txt", header=T, sep="\t", na.strings = c(""))
vrijeme1$T <- as.numeric(sub("," , ".", vrijeme1$T))
vrijeme1$WS <- as.numeric(sub("," , ".", vrijeme1$WS))
vrijeme1$NS <- as.numeric(sub("," , ".", vrijeme1$NS))
vrijeme1$EW <- as.numeric(sub("," , ".", vrijeme1$EW))
vrijeme1$Date <- sub("velj" , "vlj", vrijeme1$Date)
vrijeme1$Date <- as.Date(vrijeme1$Date, format="%d.%b.%y")
vrijeme1 <- vrijeme1[1:7]


# Prebrojava neprazne podatke u svakom stupcu zadanog dataseta
countData <- function(dataset) {
  count_dataset <- c()
  for (i in 1:ncol(dataset)) {
    count_dataset[i] <- sum(!is.na(dataset[i]))
  }
  return(setNames(count_dataset, names(dataset)))
}

# Racuna aritmeticku sredinu vrijednosti u stupcima zadanog dataseta
meanValue <- function(dataset) {
  mean_dataset <- c()
  for (i in 2:ncol(dataset)) {
    mean_dataset[i] <- mean(dataset[[i]], na.rm=TRUE)
  }
  return(setNames(mean_dataset, names(dataset)))
}

# Racuna medijan vrijednosti u stupcima zadanog dataseta
medianValue <- function(dataset) {
  median_dataset <- c()
  for (i in 2:ncol(dataset)) {
    median_dataset[i] <- median(dataset[[i]], na.rm=TRUE)
  }
  return(setNames(median_dataset, names(dataset)))
}

# Racuna standardnu devijaciju vrijednosti u stupcima zadanog dataseta
standardDeviation <- function(dataset) {
  deviation_dataset <- c()
  for (i in 2:ncol(dataset)) {
    deviation_dataset[i] <- sd(dataset[[i]], na.rm=TRUE)
  }
  return(setNames(deviation_dataset, names(dataset)))
}

# Racuna koeficijent varijacije vrijednosti u stupcima zadanog dataseta
coefOfVariation <- function(dataset) {
  return(standardDeviation(dataset)/meanValue(dataset))
}

godisnjeDobaEnum <- function() {
  list(ZIMA="0", PROLJECE="1", LJETO="2", JESEN="3")
}

getGodisnjeDoba <- function(datum) {
  zima <- as.Date("2000-12-21", format = "%Y-%m-%d")
  proljece <- as.Date("2000-3-21",  format = "%Y-%m-%d")
  ljeto <- as.Date("2000-6-21",  format = "%Y-%m-%d")
  jesen <- as.Date("2000-9-23",  format = "%Y-%m-%d")
  
  # Convert year to 2000
  datum <- as.Date(strftime(datum, format="2000-%m-%d"))
  
  if (datum >= proljece & datum < ljeto) return(godisnjeDobaEnum()$PROLJECE)
  else if (datum >= ljeto & datum < jesen) return(godisnjeDobaEnum()$LJETO)
  else if (datum >= jesen & datum < zima) return(godisnjeDobaEnum()$JESEN)
  else if (datum >= zima | datum < proljece) return(godisnjeDobaEnum()$ZIMA)
}

subsetByAbsValueRange <- function(df, minval, maxval) {
  rows = c()
  cols = c()
  for (i in 1:nrow(df)) {
    for (j in 1:ncol(mcor)) {
      if (!is.na(df[i,j]) & abs(df[i,j]) >= minval & abs(df[i,j]<maxval)) {
        rows = c(rows, i)
        cols = c(cols, j)
      }
    }
  }
  return(data.frame(df[unique(rows), unique(cols)]))
}

# Izracunava godisnja doba za svaki datum u datasetu
godisnjaDoba <- c()
for (i in 1:nrow(vrijeme1)) {
  datum <- vrijeme1[i,]$Date
  datum <- as.Date(datum, format="%d.%b.%y")
  if (is.na(datum)) {
    godisnjeDoba = NA
  } else {
    godisnjeDoba = getGodisnjeDoba(datum)
  }
  godisnjaDoba[i] <- godisnjeDoba
}

# Dodaje novu varijablu s godisnjim dobima 
vrijeme1$god_doba <- godisnjaDoba

# Razvrstava podskupove po godisnjim dobima
vrijeme1_zima = subset(vrijeme1, god_doba==0)[names(vrijeme1) != "god_doba"]
vrijeme1_proljece = subset(vrijeme1, god_doba==1)[names(vrijeme1) != "god_doba"]
vrijeme1_ljeto = subset(vrijeme1, god_doba==2)[names(vrijeme1) != "god_doba"]
vrijeme1_jesen = subset(vrijeme1, god_doba==3)[names(vrijeme1) != "god_doba"]

# Prebrojavanje podataka za svako godisnje doba
count <- data.frame(
    zima = countData(vrijeme1_zima),
    proljece = countData(vrijeme1_proljece),
    ljeto = countData(vrijeme1_ljeto),
    jesen = countData(vrijeme1_jesen)
)
cat("Broj podataka po godisnjim dobima\n")
cat("--------\n")
print(format(count, digit=2))
cat("======================================\n\n")

# Aritmeticke sredine podataka za svako godisnje doba
aritmsr <- data.frame(
    zima = meanValue(vrijeme1_zima),
    proljece = meanValue(vrijeme1_proljece),
    ljeto = meanValue(vrijeme1_ljeto),
    jesen = meanValue(vrijeme1_jesen)
)
cat("Aritmeticke sredine podataka po godisnjim dobima\n")
cat("--------\n")
print(format(aritmsr, digit=2))
cat("======================================\n\n")

# Medijani podataka za svako godisnje doba
medijan <- data.frame(
    zima = medianValue(vrijeme1_zima),
    proljece = medianValue(vrijeme1_proljece),
    ljeto = medianValue(vrijeme1_ljeto),
    jesen = medianValue(vrijeme1_jesen)
)
cat("Medijani podataka po godisnjim dobima\n")
cat("--------\n")
print(format(medijan, digit=2))
cat("======================================\n\n")

# Standardne devijacije podataka za svako godisnje doba
stdev <- data.frame(
    zima = standardDeviation(vrijeme1_zima),
    proljece = standardDeviation(vrijeme1_proljece),
    ljeto = standardDeviation(vrijeme1_ljeto),
    jesen = standardDeviation(vrijeme1_jesen)
)
cat("Standardne devijacije podataka po godisnjim dobima\n")
cat("--------\n")
print(format(stdev, digit=2))
cat("======================================\n\n")

# Koeficijenti varijacije podataka za svako godisnje doba
koefvar <- data.frame(
    zima=coefOfVariation(vrijeme1_zima),
    proljece=coefOfVariation(vrijeme1_proljece),
    ljeto=coefOfVariation(vrijeme1_ljeto),
    jesen=coefOfVariation(vrijeme1_jesen)
)
cat("Koeficijenti varijacije podataka po godisnjim dobima\n")
cat("--------\n")
print(format(koefvar, digit=2))
cat("======================================\n\n")

plot(vrijeme1$Date, vrijeme1$T, type="l", xlab="Mjesec", ylab="Temperatura", col="red", xaxt="n")
axis.Date(1, at=seq(vrijeme1$Date[2], tail(vrijeme1$Date, n=1), by="1 mon"), format="%m-%Y")

plot(vrijeme1$Date, vrijeme1$H, type="l", xlab="Mjesec", ylab="Vlaga zraka", col="blue", xaxt="n")
axis.Date(1, at=seq(vrijeme1$Date[2], tail(vrijeme1$Date, n=1), by="1 mon"), format="%m-%Y")

plot(vrijeme1$Date, vrijeme1$WS, type="l", xlab="Mjesec", ylab="Brzina vjetra", col="blue", xaxt="n")
axis.Date(1, at=seq(vrijeme1$Date[2], tail(vrijeme1$Date, n=1), by="1 mon"), format="%m-%Y")

plot(vrijeme1$Date, vrijeme1$WD, type="l", xlab="Mjesec", ylab="Smjer vjetra", col="blue", xaxt="n")
axis.Date(1, at=seq(vrijeme1$Date[2], tail(vrijeme1$Date, n=1), by="1 mon"), format="%m-%Y")

plot(vrijeme1$Date, vrijeme1$NS, type="l", xlab="Mjesec", ylab="Projekcija smjera vjetra", col="blue", xaxt="n")
lines(vrijeme1$Date, vrijeme1$EW, col="red")
axis.Date(1, at=seq(vrijeme1$Date[2], tail(vrijeme1$Date, n=1), by="1 mon"), format="%m-%Y")

# Matrica korelacije
mcor <- cor(vrijeme1[,2:(length(vrijeme1)-1)], use='pairwise')

# Izvlaci podskup iz matrice korelacija koje imaju korelaciju vecu od 0.7 (apsolutno)
high_cor <- subsetByAbsValueRange(mcor, 0.7, 1)
high_cor_subset <- vrijeme1[c(names(high_cor))]

# Crta liniju na grafu za svaku varijablu koje imaju visoku korelaciju
plot(c(as.Date("1999-01-01"),as.Date("2000-09-01")), c(0,400), col="white", type="l", xlab="Mjesec", ylab="", xaxt="n")
axis.Date(1, at=seq(vrijeme1$Date[2], tail(vrijeme1$Date, n=1), by="1 mon"), format="%m-%Y")
for (var in high_cor_subset) {
  lines(vrijeme1$Date, var, col="red")
}

# Testira normalnost razdiobe Shapiro-Wilkovim testom
for (var in names(high_cor)) {
  cat(var,"\n")
  var <- vrijeme1[,which(colnames(vrijeme1)==var)]
  norm <- shapiro.test(vrijeme1$WD)
  cat("p-vrijednost:",norm$p.value, "\n")
  if (norm$p.value < 0.05){
    cat("Distribucija nije normalna\n")
  } else {
    cat("Distribucija je normalna\n")
  }
  cat("======================================\n\n")
}

# Testira razliku među godišnjim dobima ANOVA testom
# H0 - aritmetičke sredine za pojedina godišnja doba su iste
# H1 - barem jedno godišnje doba ima drugačiju aritmetičku sredinu

# Temperatura
anova_T <- aov(T~god_doba, data=vrijeme1)
cat("ANOVA - Temperatura\n")
cat("--------\n")
summary(anova_T)

pval <- summary(anova_T)[[1]][["Pr(>F)"]][1]
if (pval < 0.05) {
    cat("Postoji signifikantna razlika međugodišnjim dobima\n")
}

# Vlaga
anova_H <- aov(H~god_doba, data=vrijeme1)
cat("ANOVA - Vlaga zraka\n")
cat("--------\n")
summary(anova_H)

pval <- summary(anova_H)[[1]][["Pr(>F)"]][1]
if (pval < 0.05) {
    cat("Postoji signifikantna razlika medu godisnjim dobima\n")
} else {
    cat("Ne postoji signifikantna razlika medu godisnjim dobima\n")
}
cat("======================================\n\n")

# Brzina vjetra
anova_WS <- aov(WS~god_doba, data=vrijeme1)
cat("ANOVA - Brzina vjetra\n")
cat("--------\n")
summary(anova_WS)

pval <- summary(anova_WS)[[1]][["Pr(>F)"]][1]
if (pval < 0.05) {
    cat("Postoji signifikantna razlika medu godisnjim dobima\n")
} else {
    cat("Ne postoji signifikantna razlika medu godisnjim dobima\n")
}
cat("======================================\n\n")

# Smjer vjetra
anova_WD <- aov(WD~god_doba, data=vrijeme1)
cat("ANOVA - Smjer vjetra\n")
cat("--------\n")
summary(anova_WD)

pval <- summary(anova_WD)[[1]][["Pr(>F)"]][1]
if (pval < 0.05) {
    cat("Postoji signifikantna razlika medu godisnjim dobima\n")
} else {
    cat("Ne postoji signifikantna razlika medu godisnjim dobima\n")
}
cat("======================================\n\n")

# Projekcija vjetra S-J
anova_NS <- aov(NS~god_doba, data=vrijeme1)
cat("ANOVA - Projekcija S-J\n")
cat("--------\n")
summary(anova_NS)

pval <- summary(anova_NS)[[1]][["Pr(>F)"]][1]
if (pval < 0.05) {
    cat("Postoji signifikantna razlika medu godisnjim dobima\n")
} else {
    cat("Ne postoji signifikantna razlika medu godisnjim dobima\n")
}
cat("======================================\n\n")

# Projekcija vjetra I-Z 
anova_EW <- aov(EW~god_doba, data=vrijeme1)
cat("ANOVA - Projekcija I-Z\n")
cat("--------\n")
summary(anova_EW)

pval <- summary(anova_EW)[[1]][["Pr(>F)"]][1]
if (pval < 0.05) {
    cat("Postoji signifikantna razlika medu godisnjim dobima\n")
} else {
    cat("Ne postoji signifikantna razlika medu godisnjim dobima\n")
}
cat("======================================\n\n")



### VRIJEME 4 ###
# Pripremanje dataseta
vrijeme4 <- read.delim("./weather4.txt", header=T, sep="\t", na.strings = c(""))
jan <- vrijeme4[names(vrijeme4)[c(1,3,4,5)]]
names(jan) <- c("Tmax", "Date", "Time", "TimeNr")
jan$Mon <- c(rep("jan", nrow(jan)))

feb <- vrijeme4[names(vrijeme4)[7:10]]
names(feb) <- c("Tmax", "Date", "Time", "TimeNr")
feb$Mon <- c(rep("feb", nrow(feb)))

jul <- vrijeme4[names(vrijeme4)[12:15]]
names(jul) <- c("Tmax", "Date", "Time", "TimeNr")
jul$Mon <- c(rep("jul", nrow(jul)))

vrijeme4 <- merge(jan, feb, all=TRUE)
vrijeme4 <- merge(vrijeme4, jul, all=TRUE)

vrijeme4 <- na.omit(vrijeme4)
vrijeme4$Date <- as.Date(vrijeme4$Date, format="%d.%m.%Y")
vrijeme4 <- vrijeme4[order(vrijeme4$Date),]
vrijeme4$Tmax <- as.numeric(sub("," , ".", vrijeme4$Tmax))
vrijeme4$TimeNr <- as.numeric(sub("," , ".", vrijeme4$TimeNr))

anova_vr4 <- aov(TimeNr~Mon, data=vrijeme4)
cat("ANOVA - TimeNr\n")
cat("--------\n")
summary(anova_vr4)

pval <- summary(anova_vr4)[[1]][["Pr(>F)"]][1]
if (pval < 0.05) {
    cat("Postoji signifikantna razlika izmedu testiranih mjeseci\n")
} else {
    cat("Ne postoji signifikantna razlika izmedu testiranih mjeseci\n")
}
cat("======================================\n\n")



