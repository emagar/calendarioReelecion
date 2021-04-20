# clean memory
rm(list = ls())

# libraries and functions needed
library(lubridate)   # easy dates

dd <- "/home/eric/Desktop/MXelsCalendGovt/calendariosReelec"
setwd(dd) 

c <- read.csv("fechasEleccionesMexicoDesde1994.csv", stringsAsFactors = FALSE)
c[c=="--"] <- NA
c <- c[-which(is.na(c$edon)),]
c$notas <- c$artConst <- NULL
c$estado <- NULL
c$yr1st <- c$nTerms <- NULL
c <- c[order(c$ord),]; c$ord <- NULL

estados <- c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua", "DF/CDMX", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México", "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas")
edos <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")

# convert dates
sel.c <- grep("^y[0-9]{4}", colnames(c))
d <- c[,sel.c] # subset columns for manipulation
f <- function(x) sub(pattern = "([0-9]+)[a-z]+", replacement = "\\1", x)
d <- apply(d, 2, f)
f <- function(x) sub(pattern = "^([0-9])$", replacement = "0\\1", x)
d <- apply(d, 2, f)
m <- c[,sel.c] # subset columns for manipulation
f <- function(x) sub(pattern = "([0-9]+)", replacement = "", x)
m <- apply(m, 2, f)
m[m=="ene"] <- "01"
m[m=="feb"] <- "02"
m[m=="mar"] <- "03"
m[m=="abr"] <- "04"
m[m=="may"] <- "05"
m[m=="jun"] <- "06"
m[m=="jul"] <- "07"
m[m=="ago"] <- "08"
m[m=="sep"] <- "09"
m[m=="oct"] <- "10"
m[m=="nov"] <- "11"
m[m=="dic"] <- "12"
y <- seq(from=1994, by=1, length=length(sel.c)) # row with years
y <- matrix(rep(y, nrow(m)), nrow = nrow(m), ncol = ncol(m), byrow = TRUE)
for (i in 1:nrow(d)){
    for (j in 1:ncol(d)){
        d[i,j] <- paste(y[i,j], m[i,j], d[i,j], sep = "")
    }
}
d[is.na(m)] <- NA
c[,sel.c] <- d # return manipulated data to data frame
rm(y,m,d,f)

# subset relevant rows, turn into lists with dates by state
f <- function(x) x[!is.na(x)]
go <- c[which(c$elec=="gob"),]
go$elec <- NULL
go$edon <- NULL
go <- apply(go, 1, f)
names(go) <- edos
go <- lapply(go, ymd)
#
dl <- c[which(c$elec=="dloc"),]
dl$elec <- NULL
dl$edon <- NULL
dl <- apply(dl, 1, f)
names(dl) <- edos
dl <- lapply(dl, ymd)
#
mu <- c[which(c$elec=="ayun"),]
mu$elec <- NULL
mu$edon <- NULL
mu <- apply(mu, 1, f)
names(mu) <- edos
mu <- lapply(mu, ymd)
#
pr <- c[which(c$elec=="pres"),]
pr$elec <- NULL
pr$edon <- NULL
pr <- f(pr)
pr <- ymd(pr)
#
df <- c[which(c$elec=="dip"),]
df$elec <- NULL
df$edon <- NULL
df <- f(df)
df <- ymd(df)
#
se <- c[which(c$elec=="sen"),]
se$elec <- NULL
se$edon <- NULL
se <- f(se)
se <- ymd(se)


##########
## plot ##
##########

#png("plot/cal.png", width=700, height=500)
#pdf("plot/cal.pdf", width=10, height=7)
par(mar=c(2.5,.2,3.5,1)) ## SETS B L U R MARGIN SIZES
#plot(y = c(0,-32), x = ymd(c(19940101, 20250101)), xlim = ymd(c(19920101, 20240501)), type = "n", axes = FALSE, xlab = "", ylab = "", main = "Calendarios electorales estatales")
plot(y = c(1,-32), x = ymd(c(19940101, 20250101)), xlim = ymd(c(19920101, 20240501)), type = "n", axes = FALSE, xlab = "", ylab = "", main = "Calendarios electorales cada vez más ordenados/aburridos")
axis(1, at = ymd(paste(1994:2025, "0101", sep = "")), labels = FALSE)
axis(1, at = ymd(paste(1994:2024, "0701", sep = "")),
     labels = c(1994, paste("'", 95:99, sep=""), 2000, paste("'", c( paste(0, 1:9, sep=""), 10:23 ), sep=""), 2024),
     tick = FALSE)
abline(h=-1:-32, col="gray90")
abline(v=ymd(paste(1994:2025, "0101", sep = "")), col="gray90")
abline(v=pr,          lwd = 1.25) # fed els
text(y=-32.5, x=pr[1], pos=3, "Federal", srt = 90, col = "gray35", cex = .75)
text(y=-32.5, x=df[2], pos=3, "midterm", srt = 90, col = "gray35", cex = .75)
abline(v=df, lty = 2, lwd = 1) # midterms
polygon(x = ymd(c(19900101,19900101,19940101,19940101)), y = c(2,-35,-35,2),  col = "white", border = "white")
polygon(x = ymd(c(19900101,19900101,20500101,20500101)),  y = c(-.5,7,7,-.5), col = "white", border = "white")
for (i in 1:32){
    #i <- 1 # debug
    e <- go
    points(y = rep(-i, length(e[[i]])), x = e[[i]], pch = 19, col = "darkgreen", cex = 1.75)
    e <- mu
    points(y = rep(-i, length(e[[i]])), x = e[[i]], pch = 19, col = "red", cex = 1.15)
    e <- dl
    points(y = rep(-i, length(e[[i]])), x = e[[i]], pch = 19, col = "cyan", cex = .5)
}
tmp <- ymd(20090101)
tmp <- c(tmp - years(6), tmp, tmp + years(6)) # xloc of legent points
points(x = tmp, y = c(1,1,1), pch = 19, col = c("darkgreen", "red", "cyan"), cex = c(1.75,1.15,.5))
#text(x = tmp, y = c(.5,.5,.5), labels = c("gubernatorial","municipal","state assembly"), pos = 4)
text(x = tmp, y = c(1,1,1), labels = c("gobernador","ayuntamientos","Congreso del estado"), pos = 4)
text(x = tmp[1] - years(7), y = 1, labels = c("Leyenda: elección de"), pos = 4)
#
text(x = rep(ymd(19940101), 32), y = -1:-32, names(mu), pos =2) # state abbrev.
mtext(text = "https://github.com/emagar/calendarioReelecion", side = 1, line = 1.5, cex = .67, col = "gray", adj = 1)
#dev.off()

