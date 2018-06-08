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
library(lme4)

passagetime <- read.csv(file = "passagetime.csv", header = TRUE)


#passage time####
head(passagetime)

passagetime$date_eaten <- as.Date(strptime(passagetime$date_eaten, "%m/%d/%Y"))
passagetime$date_excreted <- as.Date(strptime(passagetime$date_excreted, "%m/%d/%Y"))

passagetime$days <- as.numeric(passagetime$date_excreted - passagetime$date_eaten)
class(passagetime$days)
passagetime$days <- passagetime$days + 15

passagetimenoNA <- na.omit(passagetime)

mean(passagetime$days, na.rm = TRUE)
sd(passagetime$days, na.rm = TRUE)/sqrt(length(passagetimenoNA$days))

(gut_passage_times_by_species <- passagetimenoNA %>%
                                  group_by(species) %>%
                                  summarize(avg = mean(days, na.rm = TRUE),
                                            SD = sd(days, na.rm =TRUE),
                                            SE = sd(days)/sqrt(length(days))))

(gut_passage_time_overall <- passagetimenoNA %>%
                              summarize(avg = mean(days, na.rm = TRUE),
                              SE = sd(days)/sqrt(length(days))))

model <- glm(days ~ species, Gamma, data = passagetimenoNA)
summary(model)
Anova(model)
summary(glht(model, mcp(species="Tukey")))

levels(passagetimenoNA$species) <- c("dry", "dry", "dry", "dry", "fleshy", "fleshy", "dry", "dry")

model <- glmer(days ~ species + (1|individual), Gamma, data = passagetimenoNA)
summary(model)
Anova(model)
summary(glht(model, mcp(species="Tukey")))

#recovery percentage####
head(passagetime)
(recovery_percentage_by_species <- passagetime %>%
                                    group_by(species) %>%
                                    summarize(
                                      count = n(),
                                      percent = sum(recovered == 1) / count * 100))

