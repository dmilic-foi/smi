
vrijeme1 <- read.delim("./weather1.txt", header=T, sep="\t")

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
  
  if (datum >= proljece && datum < ljeto) return(godisnjeDobaEnum()$PROLJECE)
  else if (datum >= ljeto && datum < jesen) return(godisnjeDobaEnum()$LJETO)
  else if (datum >= jesen && datum < zima) return(godisnjeDobaEnum()$JESEN)
  else if (datum >= zima || datum < proljece) return(godisnjeDobaEnum()$ZIMA)
}