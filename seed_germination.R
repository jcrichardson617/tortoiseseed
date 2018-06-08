#%germination####
perc <- read.csv(file = "percforanova.csv", header = TRUE)
perc$species

library(MASS)
library(aod)
library(car)
library(multcomp)
library(ggplot2)
library(grid)
library(gridExtra)
library(rotl)
library(plyr) #Maybe? Was messing pipes up
library(dplyr)

allthecrap <- function(spec) {
  spec %>%
    group_by(passage, scat) %>%
    summarize(prop = sum(successes) / sum(total),
              SD = prop/sqrt(7))
}

cyperus <- subset(perc, species == 'cyperus')
y <- cbind(cyperus$successes, cyperus$total - cyperus$successes)
model <- glm(y ~ cyperus$passage * cyperus$scat, binomial)
summary(model)
Anova(model)
allthecrap(cyperus)

eustachys <- subset(perc, species == 'eustachys')
y <- cbind(eustachys$successes, eustachys$total - eustachys$successes)
model <- glm(y ~ eustachys$passage * eustachys$scat, binomial)
summary(model)
Anova(model)
allthecrap(eustachys)

Fcymosa <- subset(perc, species == 'Fcymosa')
y <- cbind(Fcymosa$successes, Fcymosa$total - Fcymosa$successes)
model <- glm(y ~ Fcymosa$passage * Fcymosa$scat, binomial)
summary(model)
Anova(model)
allthecrap(Fcymosa)

physalis <- subset(perc, species == 'physalis')
y <- cbind(physalis$successes, physalis$total - physalis$successes)
model <- glm(y ~ physalis$passage * physalis$scat, binomial)
summary(model)
Anova(model)
allthecrap((physalis))

sjam <- subset(perc, species == 'sjam')
y <- cbind(sjam$successes, sjam$total - sjam$successes)
model <- glm(y ~ sjam$scat, quasibinomial)
summary(model)
Anova(model)
allthecrap(sjam)

waltheria <- subset(perc, species == 'waltheria')
y <- cbind(waltheria$successes, waltheria$total - waltheria$successes)
model <- glm(y ~ waltheria$scat, quasibinomial)
summary(model)
Anova(model)
allthecrap(waltheria)

catros <- subset(perc, species == 'catros')
y <- cbind(catros$successes, catros$total - catros$successes)
model <- glm(y ~ catros$passage * catros$scat, binomial)
summary(model)
Anova(model)
allthecrap(catros)

opuntia <- subset(perc, species == 'opuntia')
y <- cbind(opuntia$successes, opuntia$total - opuntia$successes)
model <- glm(y ~ opuntia$passage * opuntia$scat, binomial)
summary(model)
Anova(model)
allthecrap(opuntia)

allplants_proportion_means <- ddply(perc, .(species,passage,scat), summarize,
      mean = sum(successes)/sum(total),
      se = sqrt(sum(successes)/sum(total)*(1-sum(successes)/sum(total))/length(total)))
allplants_proportion_means <- subset(allplants_proportion_means, allplants_proportion_means$mean != 0)


levels(allplants_proportion_means$species) <- c("Catharanthus roseus",
                                                "Cyperus ligularis",
                                                "Eustachys neglecta",
                                                "Fimbristylis cymosa",
                                                "Opuntia humifosa",
                                                "Physalis angustifolia",
                                                "Stachytarpheta jamaicensis",
                                                "Waltheria indica")

####germination rate####
rates <- read.csv(file = "rateforanova.csv", header = TRUE)
rates <- na.omit(rates)

alltherates <- function(spec) {
  spec %>%
    group_by(passage, scat) %>%
    summarize(avg = mean(days),
              SD = sd(days))
}

cyperus <- subset(rates, species == 'cyperus')
tapply(cyperus$days,list(cyperus$passage,cyperus$scat),mean)
model <- glm(days ~ passage * scat, Gamma, data = cyperus)
summary(model)
Anova(model)
alltherates(cyperus)

eustachys <- subset(rates, species == 'eustachys')
tapply(eustachys$days,list(eustachys$passage,eustachys$scat),mean)
model <- glm(days ~ passage * scat, Gamma, data = eustachys)
summary(model)
Anova(model)
alltherates(eustachys)

Fcymosa <- subset(rates, species == 'Fcymosa')
tapply(Fcymosa$days,list(Fcymosa$passage,Fcymosa$scat),mean)
model <- glm(days ~ passage * scat, Gamma, data = Fcymosa)
summary(model)
Anova(model)
alltherates(Fcymosa)

physalis <- subset(rates, species == 'physalis')
tapply(physalis$days,list(physalis$passage,physalis$scat),mean)
model <- glm(days ~ passage * scat, Gamma, data = physalis)
summary(model)
Anova(model)
alltherates(physalis)

sjam <- subset(rates, species == 'sjam')
tapply(sjam$days,list(sjam$passage,sjam$scat),mean)
model <- glm(days ~ scat, Gamma, data = sjam)
summary(model)
Anova(model)
alltherates(sjam)

waltheria <- subset(rates, species == 'waltheria')
tapply(waltheria$days,list(waltheria$passage,waltheria$scat),mean)
model <- glm(days ~ scat, Gamma, data = waltheria)
summary(model)
Anova(model)
alltherates(waltheria)

catros <- subset(rates, species == 'catros')
tapply(catros$days,list(catros$passage,catros$scat),mean)
model <- glm(days ~ passage * scat, Gamma, data = catros)
summary(model)
Anova(model)
alltherates(catros)

opuntia <- subset(rates, species == 'opuntia')
tapply(opuntia$days,list(opuntia$passage,opuntia$scat),mean)
model <- glm(days ~ passage * scat, Gamma, data = opuntia)
summary(model)
Anova(model)
alltherates(opuntia)

allplants_rate_means <- ddply(rates, .(species,passage,scat), summarize,
                                mean = mean(days),
                                se = sd(days)/sqrt(length(days)))

levels(allplants_rate_means$species) <- c("Catharanthus roseus",
                                          "Cyperus ligularis",
                                          "Eustachys neglecta",
                                          "Fimbristylis cymosa",
                                          "Opuntia humifosa",
                                          "Physalis angustifolia",
                                          "Stachytarpheta jamaicensis",
                                          "Waltheria indica")

#GROUPED %####
groupedperc <- perc
levels(groupedperc$species) <- c("Dry",
                                 "Dry",
                                 "Dry",
                                 "Dry",
                                 "Fleshy",
                                 "Dry",
                                 "Fleshy",
                                 "Dry",
                                 "Dry")

y <- cbind(groupedperc$successes, groupedperc$total - groupedperc$successes)
model <- glm(y ~ groupedperc$passage * groupedperc$scat * groupedperc$species, binomial)
summary(model)
Anova(model)

dry <- subset(groupedperc, species == 'Dry')
y <- cbind(dry$successes, dry$total - dry$successes)
model <- glm(y ~ dry$passage * dry$scat, binomial)
summary(model)
Anova(model)

fleshy <- subset(groupedperc, species == 'Fleshy')
y <- cbind(fleshy$successes, fleshy$total - fleshy$successes)
model <- glm(y ~ fleshy$passage * fleshy$scat, binomial)
summary(model)
Anova(model)


groupedplants_proportion_means <- ddply(groupedperc, .(species,passage,scat), summarize,
                                    mean = (sum(successes)/sum(total)),
                                    se = (sqrt(sum(successes)/sum(total)*(1-sum(successes)/sum(total))/length(total))))
groupedplants_proportion_means <- subset(groupedplants_proportion_means, groupedplants_proportion_means$mean != 0)

#GROUPED rate####
groupedrates <- rates
levels(groupedrates$species) <- c("Dry",
                                 "Dry",
                                 "Dry",
                                 "Dry",
                                 "Fleshy",
                                 "Dry",
                                 "Fleshy",
                                 "Dry",
                                 "Dry")

alltherates <- function(spec) {
  spec %>%
    group_by(passage, scat) %>%
    summarize(avg = mean(days),
              SD = sd(days))
}

model <- glm(days ~ passage * scat * species, Gamma, data = groupedrates)
summary(model)
Anova(model)

dry <- subset(groupedrates, species == 'Dry')
model <- glm(days ~ passage * scat, Gamma, data = dry)
summary(model)
Anova(model)
alltherates(dry)

fleshy <- subset(groupedrates, species == 'Fleshy')
model <- glm(days ~ passage * scat, Gamma, data = fleshy)
summary(model)
Anova(model)
alltherates(dry)

groupedplants_rate_means <- ddply(groupedrates, .(species,passage,scat), summarize,
                              mean = mean(days),
                              se = sd(days)/sqrt(length(days)))