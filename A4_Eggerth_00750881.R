# AUFGABENBLATT 4
# Cordula Eggerth (00750881)

# Verwendete Literaturquellen:
# .	Folien und R-Codes zu den bisher vorgetragenen Kapiteln aus UK Erweiterungen des linearen Modells (Prof. Wilfried Grossmann).
# .	R Package furniture: https://cran.r-project.org/web/packages/furniture/furniture.pdf. 
# .	N.H. Timm (2002): Applied Multivariate Statistics. Springer Verlag, NY, USA. https://app.quadstat.net/dataset/r-dataset-package-hsaur-students. 
# .	STHDA (Statistical Tools for High-Throughput Data Analysis. MANOVA Analysis: http://www.sthda.com/english/wiki/manova-test-in-r-multivariate-analysis-of-variance. 
# .	http://dwoll.de/rexrepos/posts/multMANOVA.html       

rm(list=ls())

# install.packages("lme4")
# install.packages("HSAUR")
# install.packages("furniture")
# install.packages("faraway")
# install.packages("mvnormtest")
library(lme4)
library(HSAUR)
library(furniture)
library(faraway)
library(mvnormtest)

#setwd("C:/Users/Coala/Desktop/A4_ERWEIT")

 #***********************************************************************************************
# AUFGABE 1 (Zufallseffekte)
#***********************************************************************************************
# Um die Variation der Milchfettleistung von Bullen zu untersuchen werden zufällig 
# zwei Bullen ausgewählt und je drei ihrer Töchter untersucht. 
# Die Ergebnisse sind durch folgende Werte gegeben:
# Bulle 1: Milchfettwerte der Töchter: 19,17, 15
# Bulle 2: Milchfettwerte der Töchter: 25, 5, 15
# a) Berechne nach den Formeln die Variation der Milchfettleistungen zwischen den Klassen 
#    (Bullen) und die Variation der Michfettleistungen innerhalb der Klassen. Ermittle 
#    daraus die Varianzschätzung für die Variation zwischen den Klassen. 
#    Ist diese Schätzung sinnvoll?
# b) Ermittle die Schätzung der Varianzen mittels REML mit der Maximum-Likelihoodmethode 
#    und der REML Methode und bestimme die Intraclasskorrelation.

milchfett_df <- data.frame(tochter1=c(19,25), 
                           tochter2=c(17,5),
                           tochter3=c(15,15))
rownames(milchfett_df) <- c("bulle1","bulle2")
milchfett_df[1,] <- as.numeric(milchfett_df[1,])
milchfett_df[2,] <- as.numeric(milchfett_df[2,])

# plot: werte pro tochter und bulle
plot(as.numeric(milchfett_df[1, ]), xlim=c(1,3), ylim=c(0,40), 
     ylab="milchfettwert", xlab="tochter", col="blue",
     pch=20, main="milchfettwerte pro tochter und bulle")
points(as.numeric(milchfett_df[2, ]), col="orange", pch=8)
legend("topright", legend=c("bulle1", "bulle2"), 
       col=c("blue", "orange"), lwd=2)

# deskriptive statistiken
summary(t(milchfett_df))
apply(milchfett_df,1,sd)

# 1.a. 

# variation der milchfettwerte ZWISCHEN den klassen (bullen)
  # m ... anzahl gruppen (i.e. 2 bullen)
  # n ... anzahl beobachtungen pro gruppe (i.e. 3 toechter pro bulle)
m <- 2
n <- 3
milchfett <- as.matrix(milchfett_df)

variation_between <- sum((rowMeans(milchfett)-mean(milchfett))^2)
variation_between

# variation der milchfettwerte INNERHALB der klassen (bullen)
variation_within <- sum((milchfett[1, ]-rowMeans(milchfett)[1])^2)+ 
                    sum((milchfett[2, ]-rowMeans(milchfett)[2])^2) 
variation_within

# daraus varianzschaetzung fuer die variation zwischen den klassen
mse <- variation_within / (m*(n-1))
msm <- variation_between / (m-1)

varianz_schaetzung <- (msm-mse) / n
varianz_schaetzung

# ergebnisinterpretation:
# die varianzschaetzung ist hier negativ, d.h. sie ist demnach 
# keine sinnvolle schaetzung 


# 1.b.

tochter <- factor(c(rep(1:3, 2)))
bulle <- factor(c(1,1,1,2,2,2)) 
data <- data.frame(bulle, tochter, 
                   milchfett=c(19,17,15,25,5,15))

 # ML:
ML <- lmer(data$milchfett~tochter+(1|bulle), REML=FALSE) 
summary(ML)
coef(ML)# koeffizienten
fixef(ML) # feste effekte
ranef(ML) # zufaellige effekte

# REML: 
REML <- lmer(data$milchfett~tochter+(1|bulle), REML=TRUE)
summary(REML)
coef(REML) # koeffizienten
fixef(REML) # feste effekte
ranef(REML) # zufaellige effekte

# ergebnisinterpretation von ML und REML: 
# die ergebnisse sind in diesem bsp. nicht unterschiedlich

# intraclasskorrelation:
# basierend auf 'summary(ML)' ergebnissen
sigma <- 15 
sigma.a <- 0 
intraclass_correlation <- sigma.a / (sigma.a+sigma)

# ergebnisinterpretation: 
# es gibt hier keine intraclass correlation, d.h. keine 
# korrelation zwischen verschiedenen beobachtungen in 
# derselben klasse (bulle)

# vergleich von modellen zum beispiel:
  # diagnostic plots:
  qqnorm(resid(REML),main="")
  qqline(resid(REML))
  
  plot(fitted(REML),resid(REML),
       xlab="fitted",ylab="residual")
  abline(0,0)
  
  # vergleiche mit dem nullmodell:
  nullmodell <- lm(data$milchfett~1) # erklaerung nur durch konstante
  summary(nullmodell)
  
  # vgl. mit anova (ML/REML vs. nullmodell)
  # hier: REML wurde automatisch "refitted" mit ML, 
  #       daher ML verglichen
  anova(REML, nullmodell)
  anova(ML, nullmodell)
  
  reml.LL <- as.numeric(2*(logLik(REML)-logLik(nullmodell)))
  ml.LL <- as.numeric(2*(logLik(ML)-logLik(nullmodell)))
  
  pchisq(reml.LL,1,lower=FALSE) # hochsignifikant auf 0.05 level
  pchisq(ml.LL,1,lower=FALSE) # signifikant auf 0.05 level
  
  

#*********************************************************************************************
# AUFGABE 2 (Zufallseffekte)
#***********************************************************************************************
# Um die Variation des Metallgehaltes in einem Erzlager zu untersuchen, werden aus I=6 
# Bohrlöchern jeweils J=5 Gesteinsproben entnommen und auf ihren Metallgehalt analysiert. 
# Die Ergebnisse der Proben sind in folgender Tabelle dargestellt.
# Man analysiere die Daten mit einem Modell das das Bohrloch als Zufallseffekt auffasst 
# und vergleiche die Ergebnisse mit jenen einer einfachen Varianzanalyse. 
# Weiter bestimme man auch die Intraclass Korrelation.

  # balanciertes design (weil jeweils 5 gesteinsproben pro bohrloch)
  metall_df <- data.frame(bohrloch1=c(33.5,29.4,32.1,34.2,28.7),
                          bohrloch2=c(26.5,31.3,29.7,28.4,30.6),
                          bohrloch3=c(31.7,35.4,34.8,33.2,36.1),
                          bohrloch4=c(29.4,36.7,28.6,30.0,35.2),
                          bohrloch5=c(24.1,26.7,28.3,23.2,25.6),
                          bohrloch6=c(32.4,38.6,27.8,30.3,30.9))
  metall_df
  
  # plot: werte pro gesteinsprobe und bohrloch
  plot(as.numeric(metall_df[1, ]), xlim=c(1,6), ylim=c(0,40), 
       ylab="probenwert", xlab="gesteinsprobe", col="blue",
       pch=20, main="werte pro probe und bohrloch")
  points(as.numeric(metall_df[3, ]), col="orange", pch=8)
  points(as.numeric(metall_df[4, ]), col="cyan2", pch=8)
  points(as.numeric(metall_df[5, ]), col="indianred4", pch=8)
  points(as.numeric(metall_df[6, ]), col="mediumpurple2", pch=8)
  points(as.numeric(metall_df[6, ]), col="tan1", pch=8)
  par(cex=0.8)
  legend("bottomright", legend=c("bohrloch1", "bohrloch2",
                                 "bohrloch3", "bohrloch4", "bohrloch5", "bohrloch6"), 
         col=c("blue", "orange", "cyan2", "indianred4",
               "mediumpurple2", "tan1"), lwd=2)
  
  # deskriptive statistiken
  summary(metall_df) # summary pro bohrloch
  summary(t(metall_df)) # summary pro probe
  apply(metall_df,2,sd) # sd pro bohrloch
  apply(metall_df,1,sd) # sd pro probe
  
  bohrloch <- factor(rep(1:6,5)) 
  gesteinsprobe <- factor(rep(1:5,each=6))
  messungen <- c(33.5, 26.5, 31.7, 29.4, 24.1, 32.4, # probe 1
                 29.4, 31.3, 35.4, 36.7, 26.7, 38.6, # probe 2
                 32.1, 29.7, 34.8, 28.6, 28.3, 27.8, # probe 3
                 34.4, 28.4, 33.2, 30.0, 23.2, 30.3, # probe 4
                 28.7, 30.6, 36.1, 35.2, 25.6, 30.9) # probe 5
  metall_df_long <- data.frame(gesteinsprobe=gesteinsprobe, 
                               bohrloch=bohrloch, 
                               messung=messungen)
  metall_df_long
  
  # means pro bohrloch
  tapply(metall_df_long$messung, metall_df_long$bohrloch, mean)
  
  # standardabweichung pro bohrloch
  tapply(metall_df_long$messung, metall_df_long$bohrloch, sd)
  
  # boxplot: werte nach bohrloch
  boxplot(metall_df_long$messung ~ metall_df_long$bohrloch,
          main="werte nach bohrloch")
  
  
  # means pro probe
  tapply(metall_df_long$messung, metall_df_long$gesteinsprobe, mean)
  
  # standardabweichung pro probe
  tapply(metall_df_long$messung, metall_df_long$gesteinsprobe, sd)
  
  # boxplot: werte nach probe
  boxplot(metall_df_long$messung ~ metall_df_long$gesteinsprobe,
          main="werte nach gesteinsprobe")
  
  
  # modelle: bohrloch als zufallseffekt (bohr.mod1) vs. 
  #          einfache varianzanalyse (bohr.mod2)
  
  # EINFACHE VARIANZANALYSE:
  bohr.mod1 <- aov(metall_df_long$messung ~ metall_df_long$gesteinsprobe +
                                            metall_df_long$bohrloch)
  summary(bohr.mod1)
  
  # standard diagnostic plots fuer das modell:
  # (ergebnis: E[u_t]=0, NV daten)
  par(mfrow=c(2,2))
  plot(bohr.mod1)
  
  # bartlett-test auf varianzhomogenitaet 
  # (ergebnis: varianzen sind homogen)
  bartlett.test(metall_df_long$messung ~ metall_df_long$bohrloch)
  
  # BOHRLOCH ALS ZUFALLSEFFEKT:
  
  bohr.mod2 <- lmer(metall_df_long$messung ~ metall_df_long$gesteinsprobe + 
                      (1|metall_df_long$bohrloch), REML = TRUE)
  summary(bohr.mod2)
  
  # INTRA-CLASS-CORRELATION:
  sigma.a <- 7.537 
  sigma <- 7.150 
  intraclass_correlation_bohr <- sigma.a /(sigma.a + sigma) 

 # MODELLVERGLEICH/-BEURTEILUNG
  coef(bohr.mod1) # basierend auf anova
  ranef(bohr.mod2) # basierend auf bohrloch als ZV
  fixef(bohr.mod2) # basierend auf bohrloch als ZV
  coef(bohr.mod2) # basierend auf bohrloch als ZV
  
  # testen der festen effekte 
  # nullmod. und alternativmod. mit ML-methode vergleichen
  # nullmodell (nur konstante als fixed term)
  bohr.nullmodell <- lmer(metall_df_long$messung ~ 1 + (1|metall_df_long$bohrloch),
                          REML=FALSE)
  summary(bohr.nullmodell)
  # alternativmodell (mixed model mit fixed effect "gesteinsprobe")
  bohr.alternativmodell <- lmer(metall_df_long$messung ~ metall_df_long$gesteinsprobe +
                                  (1|metall_df_long$bohrloch), REML=FALSE)
  summary(bohr.alternativmodell)
  
  # vgl. der beiden modelle mit anova
  anova(bohr.alternativmodell,bohr.nullmodell)
  
  
  # test, ob bohrloch signifikant
  # (ergebnis: bohrloch-effekt ist signifikant)
  bohr.modellMitZufall <-lmer(metall_df_long$messung ~ metall_df_long$gesteinsprobe + 
                                                       (1|metall_df_long$bohrloch))
  bohr.modellNull <-lm(metall_df_long$messung ~ 1 + metall_df_long$gesteinsprobe)
  anova(bohr.modellMitZufall,bohr.modellNull)
  # direkt likelihood vergleichen
  LL <- as.numeric(2*(logLik(bohr.modellMitZufall) - logLik(bohr.modellNull, REML = TRUE)))
  pchisq(LL, 1, lower.tail = FALSE)
  
#***********************************************************************************************
# AUFGABE 3 (Longitudinaldaten)
#***********************************************************************************************
# Die Daten "phosphate" im Package HSAUR enthalten die anorganischen Phosphat-levels im 
# Plasma von 33 Personen, von denen 20 zu einer Kontrollgruppe gehören und 13 als adipös 
# klassifiziert sind. Stelle die Profile der Personen in den einzelnen Gruppen dar und 
# erstelle ein geeignetes Modell mit Zufallseffekten für den Verlauf der Phosphatwerte.

phosphate
attach(phosphate)
head(phosphate, n=11)

# profile der personen in den einzelnen gruppen 
summary(phosphate[])
summary(phosphate[group=="control",])
summary(phosphate[group=="obese",])

apply(phosphate[-1],2,sd) # sd pro zeitpunkt
phosphate_data <- phosphate[-1]
apply(phosphate_data[group=="control",],2,sd)  
apply(phosphate_data[group=="obese",],2,sd)  

# verlauf pro gruppe
boxplot(phosphate_data[group=="control",], col="seashell2",
        main="phosphate-level der control gruppe über die zeit")
boxplot(phosphate_data[group=="obese",], col="lightcoral",
        main="phosphate-level der obese gruppe über die zeit")

control <- phosphate_data[group=="control",]
obese <- phosphate_data[group=="obese",]

# profile der personen in control gruppe
colfunc <- colorRampPalette(c("black", "goldenrod2", "mediumseagreen",
                              "mediumvioletred", "olivedrab3", "magenta1",
                              "rosybrown1", "royalblue1", "turquoise3",
                              "steelblue1", "tomato1", "paleturquoise1"))
plot(rep(1,10),col=colfunc(20),pch=19,cex=3) # test colorRampPalette look
plot(as.numeric(control[1,]), type="b", xaxt = "n", 
     xlab="messungszeit t", ylim=c(0,7),
     ylab="phosphatwert", 
     main="phosphatwerte in control gruppe (verlauf pro person)",
     cex.main=0.9)
axis(1, at=1:8, labels=colnames(control))
for (i in 2:length(control)){
  lines(as.numeric(control[i,]), type="b", col=colfunc(nrow(control))[i])
}

# profile der personen in obese gruppe
plot(as.numeric(obese[1,]), type="b", xaxt = "n", 
     xlab="messungszeit t", ylim=c(0,7),
     ylab="phosphatwert", 
     main="phosphatwerte in obese gruppe (verlauf pro person)",
     cex.main=0.9)
axis(1, at=1:8, labels=colnames(obese))
for (i in 2:length(obese)){
  lines(as.numeric(obese[i,]), type="b", col=colfunc(nrow(obese))[i])
}

# modell mit zufallseffekten für den verlauf der phosphatwerte
  # mit library furniture: 
  # https://cran.r-project.org/web/packages/furniture/furniture.pdf
phosphate_baseData <- phosphate
long_df <- long(phosphate_baseData,
                c("t0","t0.5","t1","t1.5","t2","t3","t4","t5"), 
                v.names = "value")
nobs <- nrow(phosphate_baseData)
long_df$time <- rep(c(0,0.5,1,1.5,2,3,4,5),rep(nobs,8))
colnames(long_df)[4] <- "subject"
long_df
subset(long_df, subject %in% c("1","2","3")) # test subject 1-3

# modell 1
phosphate.mod1 <- lmer(long_df$value ~ long_df$time + long_df$group + (1|subject), 
                       data=long_df, REML=FALSE, na.action=na.omit)
summary(phosphate.mod1)

# modell 2
phosphate.mod2 <- lmer(long_df$value ~ long_df$time + long_df$group + (time|subject), 
                       data=long_df, REML=FALSE, na.action=na.omit)
summary(phosphate.mod2)

# anova (zum modellvergleich)
anova(phosphate.mod1, phosphate.mod2)


# random effects prediction
qint<-ranef(phosphate.mod1)$subject[["(Intercept)"]]
qres<-residuals(phosphate.mod1)
qqnorm(qint,ylab="Estimated random intercepts",
       xlim=c(-3,3), ylim=c(-20,20), main = "Random intercepts")
qqline(qint)

qqnorm(qres,ylab="Estimated random intercepts",
       xlim=c(-3,3), ylim=c(-20,20), main = "Residuals")
qqline(qres)


#***********************************************************************************************
# AUFGABE 4 (Longitudinaldaten)
#***********************************************************************************************
# Die Daten "ratdrink" im Package faraway enthalten für 27 Ratten die Gewichtsmessung 
# in fünf aufeinander folgenden Wochen. Die ersten zehn Ratten sind gehören zur 
# Kontrollgruppe, bei sieben Ratten wurden Thyroxin in das Trinkwasser hinzugefügt 
# und bei 10 Ratten wurde Thiouracil hinzugefügt. Stelle die Daten in geeigneter Form 
# dar und bestimme ein Modell für den zeitlichen Verlauf.

data(ratdrink)
ratdrink
head(ratdrink,n=10)

# profile der ratten in den einzelnen gruppen 
means_per_subject <- tapply(ratdrink$wt, ratdrink$subject, mean)
means_per_treat <- tapply(ratdrink$wt, ratdrink$treat, mean)
means_per_week <- tapply(ratdrink$wt, ratdrink$weeks, mean)
  
boxplot(ratdrink$wt ~ ratdrink$weeks, col="mistyrose1",
        main="entwicklung des gewichts pro woche (gesamt)")
boxplot(ratdrink$wt ~ ratdrink$treat, col="mistyrose1",
        main="gewicht pro treatment(gesamt)")
boxplot(ratdrink$wt ~ ratdrink$subject, col="mistyrose1",
        main="gewicht pro subject (gesamt)")

control_rat <- ratdrink[ratdrink$treat=="control",]
thiouracil_rat <- ratdrink[ratdrink$treat=="thiouracil",]
thyroxine_rat <- ratdrink[ratdrink$treat=="thyroxine",]

boxplot(control_rat$wt ~ control_rat$weeks, col="seashell2",
        main="gewicht der control gruppe über die wochen")
boxplot(thiouracil_rat$wt ~ thiouracil_rat$weeks, col="seashell2",
        main="gewicht der thiouracil gruppe über die wochen")
boxplot(thyroxine_rat$wt ~ thyroxine_rat$weeks, col="seashell2",
        main="gewicht der thyroxine gruppe über die wochen")


# modell mit zufallseffekten für den verlauf der gewichtswerte

# modell 1
rat.mod1 <- lmer(ratdrink$wt ~ ratdrink$weeks + ratdrink$treat + (1|subject), 
                       data=ratdrink, REML=FALSE, na.action=na.omit)
summary(rat.mod1)

# modell 2
rat.mod2 <- lmer(ratdrink$wt ~ ratdrink$weeks + ratdrink$treat + (weeks|subject), 
                       data=ratdrink, REML=FALSE, na.action=na.omit)
summary(rat.mod2)

# anova (zum modellvergleich)
anova(rat.mod1, rat.mod2)


# random effects prediction
qint<-ranef(rat.mod1)$subject[["(Intercept)"]]
qres<-residuals(rat.mod1)
qqnorm(qint,ylab="Estimated random intercepts",
       xlim=c(-3,3), ylim=c(-20,20), main = "Random intercepts")
qqline(qint)

qqnorm(qres,ylab="Estimated random intercepts",
       xlim=c(-3,3), ylim=c(-20,20), main = "Residuals")
qqline(qres)


#***********************************************************************************************
# AUFGABE 5 (MANOVA)
#***********************************************************************************************
# Im Datensatz "students" im Package HSAUR findet man die Ergebnisse von 35 Studierenden, 
# zur Risikobereitschaft in zwei verschiedenen Tests, die als "high" und "low" bezeichnet 
# werden. Die Studierenden wurden dabei zufällig den drei Behandlungsgruppen mir den 
# Bezeichnungen "AA", "C" und "NC" zugeordnet. Vergleiche die Ergebnisse in den Gruppen 
# in den beiden Variablen.

# deskriptiv/darstellung:
data(students)
students
str(students)
summary(students)

boxplot(students$low ~ students$treatment, data=students, 
        xlab="treatment", ylab="low", col="lemonchiffon1", 
        main="boxplot: low werte pro treatment")
boxplot(students$high ~ students$treatment, data=students, 
        xlab="treatment", ylab="high", col="lightcyan",
        main="boxplot: high werte pro treatment")

means <- aggregate(students[,c("low","high")],
                   list(treatment=students$treatment),
                   mean)
pairs(means[,-1],panel=function(x,y){text(x,y,
      levels(students$treatment))})

# annahmen pruefen:

# normalverteilung in den gruppen
test_nv <- rbind(as.numeric(t(students)[2,]), as.numeric(t(students)[3,]))
rownames(test_nv) <- c("low","high")
mshapiro.test(test_nv[, 1:14]) # gr. AA: hyp. d. mv. NV nicht verworfen
mshapiro.test(test_nv[, 15:24]) # gr. C: hyp. d. mv. NV nicht verworfen
mshapiro.test(test_nv[, 25:35]) # gr. NC: hyp. d. mv. NV nicht verworfen

# varianzhomogenitaet in gruppen: 
bartlett.test(high~treatment, students) # H0 nicht verworfen
bartlett.test(low~treatment, students) # H0 nicht verworfen

# MANOVA durchfuehren:
students.result <- manova(cbind(students$low,students$high)~students$treatment, 
                          data=students)
summary(students.result, test="Wilks") # wilks lambda
summary(students.result, test="Roy") # roys groesster eigenwert
summary(students.result, test="Pillai") # pillais spurkriterium
summary(students.result, test="Hotelling-Lawley") # hotelling-lawley kriterium

# univariate tests: 
summary.aov(students.result) # vgl. welche verschieden sind
# (ergebnis: sind beide verschieden unter den treatments)

# paarweise multivariate tests: 
summary(manova(cbind(students$low,students$high)~students$treatment,
               data=students, subset=treatment %in% c("AA","NC")))

summary(manova(cbind(students$low,students$high)~students$treatment,
               data=students, subset=treatment %in% c("AA","C")))

summary(manova(cbind(students$low,students$high)~students$treatment,
               data=students, subset=treatment %in% c("C","NC")))


#***********************************************************************************************
# AUFGABE 6 (MANOVA)
#***********************************************************************************************
# Der Datensatz "water" im Package HSAUR enthält die Daten über die Wasserhärte und
# die Mortalitätsrate in 61 Städten in England und Wales, die nach ihrer Lage in "north" 
# und "south" eingeteilt werden. Untersuche die Unterschiede zwischen den beiden Regionen 
# ("north"-"south") und stelle die Daten in geeigneter Form dar.

data(water)
water
str(water)
summary(water)

boxplot(water$hardness ~ water$location, data=water, 
        xlab="location", ylab="hardness", col="rosybrown1", 
        main="boxplot: hardness pro location")

boxplot(water$mortality ~ water$location, data=water, 
        xlab="location", ylab="mortality", col="rosybrown1", 
        main="boxplot: mortality pro location")

means <- aggregate(water[,c("mortality","hardness")],
                   list(location=water$location), mean)
means[1:2,2:3] <- round(means[1:2,2:3],digits=1)

pairs(means[,-1],panel=function(x,y){text(x,y,
      levels(water$location))})

# annahmen pruefen:

# normalverteilung in den gruppen
water_nv <- water[order(water$location), ]
water_nv <- t(water_nv)
water_nv <- water_nv[-(1:2), ]
water_1 <- as.numeric(water_nv[1, ])
water_2 <- as.numeric(water_nv[2, ])
water_nv <- rbind(water_1,water_2)
rownames(water_nv) <- c("mortality","hardness")
mshapiro.test(water_nv[, 1:35]) # gr. North: hyp. d. mv. NV nicht verworfen
mshapiro.test(water_nv[, 36:61]) # gr. North: hyp. d. mv. NV nicht verworfen

# varianzhomogenitaet in gruppen: 
bartlett.test(mortality~location, water) # H0 nicht verworfen
bartlett.test(hardness~location, water) # H0 nicht verworfen

# MANOVA durchfuehren:
water.result <- manova(cbind(water$mortality,water$hardness)~water$location, 
                          data=water)
summary(water.result, test="Wilks") # wilks lambda
summary(water.result, test="Roy") # roys groesster eigenwert
summary(water.result, test="Pillai") # pillais spurkriterium
summary(water.result, test="Hotelling-Lawley") # hotelling-lawley kriterium

# univariate tests: 
summary.aov(water.result) # vgl. welche verschieden sind
# (ergebnis: sind beide verschieden unter den treatments)


