
# install.packages("mirt")
library("mirt")




birpl_model <- "F = 1-15
                CONSTRAIN = (1-15, a1)"


birpl_model_v1 <- "F = 1-15
                CONSTRAIN = (1-10, a1)"


library(readr)
ikikategorili <- read_csv("dichotomous.csv")[,-1]
head(ikikategorili[,1:5])
summary(ikikategorili)


veri <- read_csv("veri.csv")
dat1 <- key2binary(veri[,-1],
    key = c(2,3,4,5,2,3,4,5,2,3,4,5))
head(veri[,1:5])
head(dat1)



itemstats(ikikategorili)


 

birpl_model <- "F = 1-15
                CONSTRAIN = (1-15, a1)"
birpl_uyum <- mirt(data = ikikategorili, model = birpl_model,SE=TRUE)



library(psych)
summary(omega(ikikategorili, plot = F))



birpl_uyum <- mirt(data = ikikategorili, model = birpl_model,SE=TRUE)





Q3 <- residuals(birpl_uyum, type = 'Q3', method = 'ML')






Q3[lower.tri(Q3,diag = TRUE)] <- NA
sum(abs(Q3) >0.2,na.rm=TRUE)


 M2(birpl_uyum)




itemfit(birpl_uyum)




birpl_model <- "F = 1-15
                CONSTRAIN = (1-15, a1)"
birpl_uyum <- mirt(data = ikikategorili, model = birpl_model)


birpl_par <- coef(birpl_uyum, 
                  IRTpars = TRUE, 
                  simplify = TRUE)
birpl_par$items



birpl_par <- coef(birpl_uyum, 
IRTpars = TRUE, simplify = TRUE)
birpl_par$items
  





birpl_par <- coef(birpl_uyum, 
                  IRTpars = TRUE, 
                  simplify = TRUE)
birpl_par$items



plot(birpl_uyum,type = "trace", which.items = 1:15)




plot(birpl_uyum, type = "trace", which.items = 1:15,
     layout=c(5, 3),theta_lim = c(-4, 4))



plot(birpl_uyum,
     type = "trace", 
which.items = 1:15, 
layout=c(5, 3),
panel=function(x, y){
panel.grid(h=-1, v=-1)
panel.xyplot(x, y)
panel.abline(h=0.5, lwd=1,
        lty=1)})



plot(birpl_uyum, type = "trace", which.items = 1:15,
facet_items = FALSE)






ikipl_model <- "F = 1 - 15"

ikipl_uyum <- mirt(data = ikikategorili, model = ikipl_model,
itemtype = "2PL", SE=TRUE)
M2(ikipl_uyum)
itemfit(ikipl_uyum)
ikipl_par <- coef(ikipl_uyum, IRTpars = TRUE, simplify = TRUE)
ikipl_par$items
plot(ikipl_uyum, type = "trace", which.items = 1:15)
plot(ikipl_uyum, type = "trace", which.items = 1:15,facet_items = FALSE,
     abline=c(h=0.5))



ucpl_model <- "F = 1 - 15"
ucpl_uyum <- mirt(data = ikikategorili, model = ucpl_model,
itemtype = "3PL")
ucpl_par <- coef(ucpl_uyum, IRTpars = TRUE, simplify = TRUE)
M2(ucpl_uyum)
itemfit(ucpl_uyum)



ucpl_par$items
plot(ucpl_uyum, type = "trace", which.items = 1:15)
plot(ucpl_uyum, type = "trace", which.items = 1:15,facet_items = FALSE,
     abline=c(h=0.5))




ML   <- fscores(ikipl_uyum, method="ML",full.scores.SE=TRUE)
MAP  <- fscores(ikipl_uyum, method="MAP", full.scores.SE=TRUE)
EAP  <- fscores(ikipl_uyum, method="EAP",full.scores.SE=TRUE)


head(ML)
head(MAP)
head(EAP)

yetenek <- data.frame(ML= ML[,1],MAP=MAP[,1],EAP=EAP[,1])
apply(yetenek,2,summary)
yetenek_v1 <- yetenek[!is.infinite(yetenek$ML),]
apply(yetenek_v1,2,summary)



cor(yetenek_v1)
pairs(yetenek_v1)


anova(birpl_uyum,ikipl_uyum)
anova(ikipl_uyum,ucpl_uyum)


p <- 1/(1+exp(-(1-1.2)))
p * (1-p)



b <- c(1.2)
theta <- seq(-4,4,0.01)

prob <- c()
  for(j in 1:length(theta)){
    dir <- 1/(1 + exp(-(theta[j] - b)))
    prob[j] <- dir
    j=j+1
  }
bilgi =  prob * (1- prob)

library(ggplot2)

p <- data.frame(prob,bilgi)
MBF <- ggplot(p, aes(theta, bilgi)) +
  geom_line()
MBF



b <- 1.2
a <- 0.8
theta <- seq(-4,4,0.01)
p <- 1/(1+exp(-(0.8*(1-1.2))))
a^2 * p * (1-p)



prob <- c()
  for(j in 1:length(theta)){
    dir <- 1/(1 + exp(-(a*(theta[j] - b))))
    prob[j] <- dir
    j=j+1
  }
bilgi =  a*a * prob * (1- prob)

p <- data.frame(prob,bilgi)
MBF2 <- ggplot(p, aes(theta, bilgi)) +  geom_line()
MBF2



plot(birpl_uyum, 
type = "infotrace", 
which.items = 5)

plot(ikipl_uyum, 
type = "infotrace", 
which.items = 5)


plot(ucpl_uyum, 
type = "infotrace", 
which.items = 5)



plot(ikipl_uyum, 
type = "infotrace", 
which.items = 1:15, layout=c(5, 3))

plot(ikipl_uyum, 
type = "infotrace", 
which.items = 2:15, layout=c(5, 3))


madde1 <- extract.item(ikipl_uyum, 1)
Theta <- matrix(seq(-6,6, by = .1))
info.1 <- iteminfo(madde1, Theta)
plot(Theta, info.1, type = 'l', main = 'Item information')

tinfo <- testinfo(ikipl_uyum, 
Theta,which.items = 1:5)
plot(Theta, tinfo, type = 'l')

tinfo <- testinfo(ikipl_uyum, 
Theta,which.items = 1:10)
plot(Theta, tinfo, type = 'l')



tinfo <- testinfo(ikipl_uyum, 
Theta,which.items = 1:15)
plot(Theta, tinfo, type = 'l')


tinfo <- testinfo(ikipl_uyum, Theta,which.items = c(1,3:5,7:10,11))
plot(Theta, tinfo, type = 'l')

plot(ikipl_uyum, type='infoSE')


