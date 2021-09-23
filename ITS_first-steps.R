# Anwendung des Tutorials von Lopez Bernal et al. auf die KV-Daten
# Erstes Herantasten an das Vorgehen zur Analyse
# Author: NE
# Date: 2021/06/24

# load required packages
library(readxl)
library(foreign)
library(tsModel)
library(lmtest)
library(Epi)
library(splines)
library(vcd)
library(AER)
library(MASS)
library(dplyr)

# load data
kv_data <- read_excel("Data/KV-Daten.xlsx")

############################################################
#### Beispiel U-Untersuchungen gesamt ######################
############################################################

# compute rates
kv_data$rate <- with(kv_data, U_Gesamt/Pop2019*10000)
# aktuell Bevölkerungsdaten < 18 Jahre für 2019 genutzt

### Deskriptive analysis ###
# tabulate U-Untersuchungen before and after the intervention
summary(kv_data$U_Gesamt[kv_data$Intervention==0])
summary(kv_data$U_Gesamt[kv_data$Intervention==1])
summary(kv_data$rate[kv_data$Intervention==0])
summary(kv_data$rate[kv_data$Intervention==1])

### Poisson regression model ###
#Poisson with the standardised population as an offset
model_u_ges <- glm(U_Gesamt ~ offset(log(Pop2019)) + Intervention +
                     Zeit + Intervention*Zeit, family=poisson, kv_data)
summary(model_u_ges)
summary(model_u_ges)$dispersion
round(ci.lin(model_u_ges,Exp=T),3)

# Test für overdispersion
# Overdispersion: Varianz größer als erwartet --> Poisson Regression nicht
# anwendbar
dispersiontest(model_u_ges)
# --> Overdispersion liegt vor

#Overdispersion: Quasi-Poisson model
# In the model above we have not allowed for overdispersion - in order
# to do this we can use a quasipoisson model, which allows the variance
# to be proportional rather than equal to the mean
model2_u_ges <- glm(U_Gesamt ~ offset(log(Pop2019)) + Intervention +
                      Zeit + Intervention*Zeit,
                    family=quasipoisson, kv_data)
summary(model2_u_ges)
summary(model2_u_ges)$dispersion
round(ci.lin(model2_u_ges,Exp=T),3)
# auch nicht gut

# deshalb das negative binomial model
model3_u_ges <- glm.nb(U_Gesamt ~ offset(log(Pop2019)) + Intervention +
                         Zeit + Intervention*Zeit,
                       data = kv_data)
summary(model3_u_ges)
summary(model3_u_ges)$dispersion
round(ci.lin(model3_u_ges,Exp=T),3)

# Model checking and autocorrelation
# Check the residuals by plotting against time
res <- residuals(model3_u_ges,type="deviance")
plot(kv_data$Zeit,res,ylim=c(-5,10),pch=19,cex=0.7,col=grey(0.6),
     main="Residuals over time",ylab="Deviance residuals",xlab="Date")
abline(h=0,lty=2,lwd=2)
# Further check for autocorrelation by examining the autocorrelation and
#   partial autocorrelation functions
acf(res)
pacf(res)
# --> keine Autokorrelation

# Adjustieren für Ferien und Feiertage
model4_u_ges <- glm.nb(U_Gesamt ~ offset(log(Pop2019)) + Intervention +
                         Zeit + Intervention*Zeit +
                         Ferien + Feiertag,
                       data = kv_data)
summary(model4_u_ges)
summary(model4_u_ges)$dispersion
round(ci.lin(model4_u_ges,Exp=T),3)

# Monate als Dummy-Variablen
kv_data <- kv_data %>%
  mutate(Jan = ifelse(Monat == 1, 1, 0),
         Feb = ifelse(Monat == 2, 1, 0),
         Mar = ifelse(Monat == 3, 1, 0),
         Apr = ifelse(Monat == 4, 1, 0),
         Mai = ifelse(Monat == 5, 1, 0),
         Jun = ifelse(Monat == 6, 1, 0),
         Jul = ifelse(Monat == 7, 1, 0),
         Aug = ifelse(Monat == 8, 1, 0),
         Sep = ifelse(Monat == 9, 1, 0),
         Okt = ifelse(Monat == 10, 1, 0),
         Nov = ifelse(Monat == 11, 1, 0),
         Dez = ifelse(Monat == 12, 1, 0))

# Adjustieren für Saisonalität (Monate als Dummy-Variablen)
model6_u_ges <- glm.nb(U_Gesamt ~ offset(log(Pop2019)) + Intervention +
                         Zeit + Intervention*Zeit +
                         Ferien + Feiertag + Jan + Feb + Mar + Apr +
                         Mai + Jun + Jul + Aug + Okt + Nov + Dez,
                       data = kv_data)
summary(model6_u_ges)
summary(model6_u_ges)$dispersion
round(ci.lin(model6_u_ges,Exp=T),3)
# Frage: Reichen uns die Daten aus, um die typische Saisonalität zu
# modellieren oder wird es nicht zum Teil durch den Pandemie-Effekt
# modelliert?

# Nochmaliges checken auf Autokorrelation
# Check the residuals by plotting against time
res <- residuals(model6_u_ges,type="deviance")
plot(kv_data$Zeit,res,ylim=c(-5,10),pch=19,cex=0.7,col=grey(0.6),
     main="Residuals over time",ylab="Deviance residuals",xlab="Date")
abline(h=0,lty=2,lwd=2)

# Further check for autocorrelation by examining the autocorrelation and
#   partial autocorrelation functions
acf(res)
pacf(res)
# --> keine Autokorrelation

## Adjustieren für Saisonalität, wenn man Poisson nehmen würde
model_u_ges_test <- glm(U_Gesamt ~ offset(log(Pop2019)) + Intervention +
                      Zeit + Intervention*Zeit +
                      harmonic(Monat,2,12),
                    family=quasipoisson, kv_data)
summary(model_u_ges_test)
summary(model_u_ges_test)$dispersion
round(ci.lin(model_u_ges_test,Exp=T),3)

# Löschen der Wochen im März 2020 vor dem, da unsicher, inwiefern die
# Intervention da schon gewirkt hat
kv_data1 <- kv_data %>%
  filter(! Kalenderwoche %in% c("2020-10", "2020-11"))
# anwenden des vorherigen finalen Models
model6_u_ges <- glm.nb(U_Gesamt ~ offset(log(Pop2019)) + Intervention +
                         Zeit + Intervention*Zeit +
                         Ferien + Feiertag + Jan + Feb + Mar + Apr +
                         Mai + Jun + Jul + Aug + Okt + Nov + Dez,
                       data = kv_data1)
summary(model6_u_ges)
summary(model6_u_ges)$dispersion
round(ci.lin(model6_u_ges,Exp=T),3)
# keine Veränderung

# Modellierung 3-phasig: Phase 2 Lockdown ab Kalenderwoche 12; Phase 3
# schrittweise Öffnungen (Kita) ab Kalenderwoche 27
kv_data3 <- kv_data %>%
  mutate(Intervention2 = case_when(Zeit < 65 ~ "phase1",
                                   Zeit >= 65 & Zeit < 79 ~ "phase2",
                                   Zeit >= 79 ~ "phase3"))
model8_u_ges <- glm.nb(U_Gesamt ~ offset(log(Pop2019)) + Intervention2 +
                         Zeit + Intervention2*Zeit +
                         Ferien + Feiertag + Jan + Feb + Mar + Apr +
                         Mai + Jun + Jul + Aug + Okt + Nov + Dez,
                       data = kv_data3)
summary(model8_u_ges)
summary(model8_u_ges)$dispersion
round(ci.lin(model8_u_ges,Exp=T),3)
