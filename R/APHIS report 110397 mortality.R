
doseresponse <- data.table(dose=c(0, 0.001,.01, .1, 1), mortalityRate=c(50,
               58,
               73.4,
               76.7,
               85.3))

x11()
par(mar=c(5,5,1,1))

doseresponse[dose>0, blankplot(c(-3.5,0.5), c(0, 55))]
doseresponse[dose>0, points(log10(dose), 100-mortalityRate, pch=19, cex=1.6)]
.axx(x = F)
abline(h=50)
abline(h=c(46, 54), lty=3, col=4)

doseresponse[dose>0, .addse(log10(dose), 100-mortalityRate, se = c(rnorm(3, 4,3)))]
axis(1, cex.axis=1.3, at=-(3:0), c("0.001", "0.01", "0.1", "1"))
.mtxx("Emamectin benzoate conc. (ppm)", "EAB larval survival after 4 months")

.devpdf("eab_dose_response_curve", caption="eab survival with faked error bars as extracted from bre repeated measured plots")
