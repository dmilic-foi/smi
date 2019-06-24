
Sys.setlocale("LC_TIME", "Croatian")

# Pripremanje dataseta
vrijeme1 <- read.delim("./weather1.txt", header=T, sep="\t", na.strings = c(""))
vrijeme1$T <- as.numeric(sub("," , ".", vrijeme1$T))
vrijeme1$WS <- as.numeric(sub("," , ".", vrijeme1$WS))
vrijeme1$NS <- as.numeric(sub("," , ".", vrijeme1$NS))
vrijeme1$EW <- as.numeric(sub("," , ".", vrijeme1$EW))
vrijeme1$X.1 <- as.numeric(sub("," , ".", vrijeme1$X.1))
vrijeme1$Date <- sub("velj" , "veljaca", vrijeme1$Date)
vrijeme1$Date <- as.Date(vrijeme1$Date, format="%d.%b.%y")


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
  list(ZIMA=0, PROLJECE=1, LJETO=2, JESEN=3)
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

# Dodaje novu varijablu s godisnjim dobima 
vrijeme1$god_doba <- godisnjaDoba

# Razvrstava podskupove po godisnjim dobima
vrijeme1_zima = subset(vrijeme1, god_doba==0)[names(vrijeme1) != "god_doba"]
vrijeme1_proljece = subset(vrijeme1, god_doba==1)[names(vrijeme1) != "god_doba"]
vrijeme1_ljeto = subset(vrijeme1, god_doba==2)[names(vrijeme1) != "god_doba"]
vrijeme1_jesen = subset(vrijeme1, god_doba==3)[names(vrijeme1) != "god_doba"]

# Prebrojavanje podataka za svako godisnje doba
count_zima <- countData(vrijeme1_zima)
count_proljece <- countData(vrijeme1_proljece)
count_ljeto <- countData(vrijeme1_ljeto)
count_jesen <- countData(vrijeme1_jesen)

# Aritmeticke sredine podataka za svako godisnje doba
mean_zima <- meanValue(vrijeme1_zima)
mean_proljece <- meanValue(vrijeme1_proljece)
mean_ljeto <- meanValue(vrijeme1_ljeto)
mean_jesen <- meanValue(vrijeme1_jesen)

# Medijani podataka za svako godisnje doba
median_zima <- medianValue(vrijeme1_zima)
median_proljece <- medianValue(vrijeme1_proljece)
median_ljeto <- medianValue(vrijeme1_ljeto)
median_jesen <- medianValue(vrijeme1_jesen)

# Standardne devijacije podataka za svako godisnje doba
sd_zima <- standardDeviation(vrijeme1_zima)
sd_proljece <- standardDeviation(vrijeme1_proljece)
sd_ljeto <- standardDeviation(vrijeme1_ljeto)
sd_jesen <- standardDeviation(vrijeme1_jesen)

# Koeficijenti varijacije podataka za svako godisnje doba
variation_zima <- coefOfVariation(vrijeme1_zima)
variation_proljece <- coefOfVariation(vrijeme1_proljece)
variation_ljeto <- coefOfVariation(vrijeme1_ljeto)
variation_jesen <- coefOfVariation(vrijeme1_jesen)


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
  print(var)
  var <- vrijeme1[,which(colnames(vrijeme1)==var)]
  norm <- shapiro.test(vrijeme1$WD)
  print(norm$p.value)
  if (norm$p.value < 0.05){
    print("Distribucija nije normalna")
  }
}

# Usporeduje postoji li signifikantrna razlika u prosjecnoj temperaturi sa pojedino godisnje doba u odnosu na prosjecnu temperaturu cijele godine
# H0 - Prosjecna temperatura je konstantrna tijekom cijele godine
# H1 - Prosjecna temperatura nije konstantna tijekom cjele godine
for (i in 2:7) {
  print(colnames(vrijeme1)[i])
  print("==================")
  mean_var_all <- c(mean_zima[i], mean_proljece[i], mean_ljeto[i], mean_jesen[i])
  chisq_var <- chisq.test(c(mean_var_all, rep(mean(mean_var_all),4)))
  print(chisq_var)
  if (chisq_var$p.value <0.05) {
    print("Ima razlike po godisnjim dobima")
  } else {
    print("Nema razlike po godisnjim dobima")
  }
  print("================")
  print("")
  print("")
}

library('dummies')
vrijeme_dummy <- cbind(vrijeme1,dummy(vrijeme1$god_doba, sep="_"))
colnames(vrijeme_dummy)[colnames(vrijeme_dummy)=="vrijeme1_0"] <- "zima"
colnames(vrijeme_dummy)[colnames(vrijeme_dummy)=="vrijeme1_1"] <- "proljece"
colnames(vrijeme_dummy)[colnames(vrijeme_dummy)=="vrijeme1_2"] <- "ljeto"
colnames(vrijeme_dummy)[colnames(vrijeme_dummy)=="vrijeme1_3"] <- "jesen"

# Testiranje postoji li signifikantna razlika u koeficijentima dummy varijabli
# H0 - Postoji razlika imedu koeficijenata dummyvarijabli
# H1 - Ne postijo razlikau koeficijentima
regr<-(lm(T~zima+proljece+ljeto, data=vrijeme_dummy))
tt <- t.test(regr$coefficients)
if (tt$p.value<0.05){
  print("Nema razlike po godisnjim dobima")
} else {
  print("Postoji razlika po godisnjim dobima")
}

# Vlaga
regr<-(lm(H~zima+proljece+ljeto, data=vrijeme_dummy))
tt <- t.test(regr$coefficients)
if (tt$p.value<0.05){
  print("Nema razlike po godisnjim dobima")
} else {
  print("Postoji razlika po godisnjim dobima")
}

# Brzina vjetra
regr<-(lm(WS~zima+proljece+ljeto, data=vrijeme_dummy))
tt <- t.test(regr$coefficients)
if (tt$p.value<0.05){
  print("Nema razlike po godisnjim dobima")
} else {
  print("Postoji razlika po godisnjim dobima")
}

# Smjer vjetra
regr<-(lm(WD~zima+proljece+ljeto, data=vrijeme_dummy))
tt <- t.test(regr$coefficients)
if (tt$p.value<0.05){
  print("Nema razlike po godisnjim dobima")
} else {
  print("Postoji razlika po godisnjim dobima")
}

# Projekcija S-J
regr<-(lm(NS~zima+proljece+ljeto, data=vrijeme_dummy))
tt <- t.test(regr$coefficients)
if (tt$p.value<0.05){
  print("Nema razlike po godisnjim dobima")
} else {
  print("Postoji razlika po godisnjim dobima")
}

# Projekcija I-Z
regr<-(lm(EW~zima+proljece+ljeto, data=vrijeme_dummy))
tt <- t.test(regr$coefficients)
if (tt$p.value<0.05){
  print("Nema razlike po godisnjim dobima")
} else {
  print("Postoji razlika po godisnjim dobima")
}


mean_zima1999 <- meanValue(vrijeme1[vrijeme1$god_doba==0 & format(vrijeme1$Date, "%Y")=="1999",])
mean_proljece1999 <- meanValue(vrijeme1[vrijeme1$god_doba==1 & format(vrijeme1$Date, "%Y")=="1999",])
mean_ljeto1999 <- meanValue(vrijeme1[vrijeme1$god_doba==2 & format(vrijeme1$Date, "%Y")=="1999",])
mean_jesen1999 <- meanValue(vrijeme1[vrijeme1$god_doba==3 & format(vrijeme1$Date, "%Y")=="1999",])

mean_zima2000 <- meanValue(vrijeme1[vrijeme1$god_doba==0 & format(vrijeme1$Date, "%Y")=="2000",])
mean_proljece2000 <- meanValue(vrijeme1[vrijeme1$god_doba==1 & format(vrijeme1$Date, "%Y")=="2000",])
mean_ljeto2000 <- meanValue(vrijeme1[vrijeme1$god_doba==2 & format(vrijeme1$Date, "%Y")=="2000",])
mean_jesen2000 <- meanValue(vrijeme1[vrijeme1$god_doba==3 & format(vrijeme1$Date, "%Y")=="2000",])


# Zima
for (i in 2:7) {
  print(colnames(vrijeme1)[i])
  print("==================")
  mean_var_all <- c(mean_zima1999[i], mean_zima2000[i])
  chisq_var <- chisq.test(c(mean_var_all, rep(mean(mean_var_all),2)))
  print(chisq_var)
  if (chisq_var$p.value <0.05) {
    print("Ima razlike po godinama")
  } else {
    print("Nema razlike po godinama")
  }
  print("================")
  print("")
  print("")
}

# Proljece
for (i in 2:7) {
  print(colnames(vrijeme1)[i])
  print("==================")
  mean_var_all <- c(mean_proljece1999[i], mean_proljece2000[i])
  chisq_var <- chisq.test(c(mean_var_all, rep(mean(mean_var_all),2)))
  print(chisq_var)
  if (chisq_var$p.value <0.05) {
    print("Ima razlike po godinama")
  } else {
    print("Nema razlike po godinama")
  }
  print("================")
  print("")
  print("")
}

# Ljeto
for (i in 2:7) {
  print(colnames(vrijeme1)[i])
  print("==================")
  mean_var_all <- c(mean_ljeto1999[i], mean_ljeto2000[i])
  chisq_var <- chisq.test(c(mean_var_all, rep(mean(mean_var_all),2)))
  print(chisq_var)
  if (chisq_var$p.value <0.05) {
    print("Ima razlike po godinama")
  } else {
    print("Nema razlike po godinama")
  }
  print("================")
  print("")
  print("")
}

# Jesen
# Nema podataka za 2000 godinu







