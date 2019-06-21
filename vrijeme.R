
vrijeme1 <- read.delim("./weather1.txt", header=T, sep="\t")

Sys.setlocale("LC_TIME", "Croatian")

# Prebrojava neprazne podatke u svakom stupcu zadanog dataseta
countData <- function(dataset) {
  count_dataset <- c()
  for (i in 1:ncol(dataset)) {
    count_dataset[i] <- sum(dataset[i]!='' & !is.na(dataset[i]))
  }
  return(setNames(count_dataset, names(dataset)))
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
  datum <- gsub("velj", "veljaca", vrijeme1[i,]$Date)
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
count_zima <- countData(vrijeme1_zima)
count_proljece <- countData(vrijeme1_proljece)
count_ljeto <- countData(vrijeme1_ljeto)
count_jesen <- countData(vrijeme1_jesen)

