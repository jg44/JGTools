# very sloppy file -- rushing

doseresponse <- data.table(dose=c(0, 0.001,.01, .1, 1), mortalityRate=c(50,
               58,
               73.4,
               76.7,
               85.3))

doseresponse[, log10(dose)]

x11()

G19_doseResponse <- function(){
  par(mar=c(5,5,1,1))

doseresponse[, plot(log10(dose), 100-mortalityRate)]
doseresponse[, plot(-4:0, 100-mortalityRate)]
doseresponse[dose>0, blankplot(c(-4.5,0.5), c(0, 55))]
doseresponse[, points(-4:0, 100-mortalityRate, pch=19, cex=1.6)]
.axx(x = F)

doseresponse[dose>0, points(log10(dose), 100-mortalityRate, pch=19, cex=1.6)]
doseResponse_mod1 <- doseresponse[dose>0, lm((100-mortalityRate)~log10(dose))]

abline(doseResponse_mod1, lwd=2, col=2)
axis(1, cex.axis=1.3, at=-(4:0), c("0", "0.001", "0.01", "0.1", "1"))
.mtxx("Emamectin benzoate conc. (ppm)", "EAB larval survival after 4 months")
}

Anova(doseResponse_mod1)

doseresponse[dose>0, blankplot(c(-3.5,0.5), c(0, 55))]

doseresponse[dose>0, lm(log10(dose), 100-mortalityRate, pch=19, cex=1.6)]

abline(h=50)
abline(h=c(46, 54), lty=3, col=4)

doseresponse[dose>0, .addse(log10(dose), 100-mortalityRate, se = c(rnorm(3, 4,3)))]

.devpdf("eab_dose_response_curve", caption="eab survival with faked error bars as extracted from bre repeated measured plots")









doseresponse[, plot(dose, 100-mortalityRate)]
doseresponse[, plot(-4:0, 100-mortalityRate)]


larvaeEB <- read.csv.dt("./data/Larval_feeding_trials.csv")
larvaeEB$trtNum <- as.numeric(larvaeEB$Treatment)
kable(.nameslook(larvaeEB))


larvaeEB[, table(Larva, Treatment)]

meansLar <- larvaeEB[, .(meanDays=mean(Days), sdDays=sd(Days), seDays=sd(Days)/sqrt(length(Days))), by=trtNum]


meansLar <- meansLar[order(trtNum),]
meansLar$tt <- 1:5

.graphwindow()

g20 <- function(){
  meansLar[, blankplot(c(.5, 5.5), c(min(meanDays-seDays), max(meanDays+seDays)))]
  meansLar[, points(1:5, meanDays, cex=1.3, pch=19)]
  meansLar[, .addse(1:5, meanDays, seDays)]
  meansLar[, abline(lm(meanDays~tt), col=2, lty=2, lwd=2)]
  .axx(x=FALSE)
  axis(side=1, cex.axis=1.3, at=1:5, meansLar$trtNum)
  .mtxx("Emamectin benzoate conc. (ppm)", "Larval survival (days)")
}

Anova(meansLar[, (lm(meanDays~tt))])

.devpdf("Days surviving EAB larvae")
m1 <- larvaeEB[, aov(Days~as.factor(trtNum))]
TukeyHSD(m1)


.graphwindow(6, 12)
par(mfrow=c(1,2))
G19_doseResponse()
g20()


.devpdf("larvalResponseEB_aphisReport")
