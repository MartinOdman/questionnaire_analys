
#load library =================================================================
library(readr)
library(ggplot2)
library(tidyverse)
library(arsenal)
library(cowplot)
library(lubridate)
library(viridis)
library(writexl)
library(extrafont)

loadfonts(device = "win")

#load data ====================================================================
#dat <- read_csv("data/Data4.csv")

dat <- read_delim("data/Data4.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

#Clean up data
dat[dat$F2 == 1963, names(dat) == "F2"] <- 2023-1963 #Change birthyear to age

#specify factor variable level and label
dat$ID <- factor(dat$ID)
dat$F1 <- factor(dat$F1, levels = c(1,2,3,4), labels = c("Male", "Female", "Other", "NoResponse"), exclude = 999)

dat$F3 <- factor(dat$F3, levels = c(1,2,3), labels = c("Yes", "No", "dk"), exclude = 999)
dat$F4 <- factor(dat$F4, levels = c(1,2,3,4,5), labels = c("Happ", "CI", "SoundGen", "CombApp", "None"), exclude = 999)
dat$F5 <- factor(dat$F5, levels = c(1,2,3), labels = c("bilat", "R", "L"), exclude = 999)

dat$F6[dat$F6 == 999] <- NA
dat$F7[dat$F7 == 999] <- NA
dat$F8[dat$F8 == 999] <- NA
dat$F9[dat$F9 == 999] <- NA

dat$F18[dat$F18 == 999] <- NA
dat$F19[dat$F19 == 999] <- NA
dat$F20[dat$F20 == 999] <- NA

dat$F22_1[dat$F22_1 == 999] <- NA
dat$F22_2[dat$F22_2 == 999] <- NA
dat$F22_3[dat$F22_3 == 999] <- NA
dat$F22_4[dat$F22_4 == 999] <- NA
dat$F22_5[dat$F22_5 == 999] <- NA
dat$F22_6[dat$F22_6 == 999] <- NA
dat$F22_7[dat$F22_7 == 999] <- NA

dat$F24[dat$F24 == 999] <- NA
dat$F25[dat$F25 == 999] <- NA
dat$F26[dat$F26 == 999] <- NA

dat$F28_1[dat$F28_1 == 999] <- NA
dat$F28_2[dat$F28_2 == 999] <- NA
dat$F28_3[dat$F28_3 == 999] <- NA
dat$F28_4[dat$F28_4 == 999] <- NA
dat$F28_5[dat$F28_5 == 999] <- NA
dat$F28_6[dat$F28_6 == 999] <- NA
dat$F28_7[dat$F28_7 == 999] <- NA


dat$F10 <- factor(dat$F10, levels = c(1,2), labels = c("constant", "occasional"))

dat$F11 <- factor(dat$F11, levels = c(1,2,3,4,5,6,7,8,9), labels = c("R", "L", "BothR", "BothL", "Both", "Head", "Other", "dk", "multiple" ), exclude = 999)
dat$F12 <- factor(dat$F12, levels = c(1,2,3), labels = c("Yes", "No", "dk"), exclude = 999)
dat$F13 <- factor(dat$F13, levels = c(1,2,3,4,5,6), labels = c("tone", "noise", "music", "crickets", "other", "multiple"), exclude = 999)
dat$F14 <- factor(dat$F14, levels = c(1,2,3,4, 5), labels = c("High", "Mid", "Low", "dk", "multiple"))
dat$F15 <- factor(dat$F15, levels = c(1,2,3), labels = c("Yes", "No", "dk"), exclude = 999)
dat$F16 <- factor(dat$F16, levels = c(1,2,3,4,5), labels = c("Never", "Seldom", "Sometimes", "Usually", "Always"), exclude = 999)
dat$F17 <- factor(dat$F17, levels = c(1,2,3), labels = c("Yes", "No", "dk"), exclude = 999)

dat$F21 <- factor(dat$F21, levels = c(1,2), labels = c("Yes", "No"), exclude = 999)
dat$F22 <- factor(dat$F22, levels = c(1,2,3,4,5,6,7), labels = c("Brus", "Natur", "Omvärld", "Musik", "Tal", "Övrigt", "Flera"), exclude = 999)
dat$F23 <- factor(dat$F23, levels = c(1,2,3,4,5,6), labels = c("Högtalare", "Hörlur", "HA", "Ljudgenerator", "Övrigt", "Flera"), exclude = 999)

dat$F27 <- factor(dat$F27, levels = c(1,2), labels = c("Yes", "No"), exclude = 999)
dat$F28 <- factor(dat$F28, levels = c(1,2,3,4,5,6,7), labels = c("Brus", "Natur", "Omvärld", "Musik", "Tal", "Övrigt", "Flera"), exclude = 999)
dat$F29 <- factor(dat$F29, levels = c(1,2,3,4,5,6), labels = c("Högtalare", "Hörlur", "HA", "Ljudgenerator", "Övrigt", "Flera"), exclude = 999)

#Rename variables
names(dat)[names(dat) == "F1"] <- "sex"
names(dat)[names(dat) == "F2"] <- "age"
names(dat)[names(dat) == "F3"] <- "hearing"
names(dat)[names(dat) == "F4"] <- "device"
names(dat)[names(dat) == "F5"] <- "whatdevice"
names(dat)[names(dat) == "F6"] <- "ha.date"
names(dat)[names(dat) == "F7"] <- "ha.use.days"
names(dat)[names(dat) == "F8"] <- "ha.hours.day"
names(dat)[names(dat) == "F9"] <- "first.tin.date"
names(dat)[names(dat) == "F10"] <- "tin.con.occ"
names(dat)[names(dat) == "F11"] <- "tin.lat"
names(dat)[names(dat) == "F12"] <- "tin.loud.varies"
names(dat)[names(dat) == "F13"] <- "tin.sounds.like"
names(dat)[names(dat) == "F14"] <- "tin.pitch"
names(dat)[names(dat) == "F15"] <- "tin.reduce.env"
names(dat)[names(dat) == "F16"] <- "sound.tolerance"
names(dat)[names(dat) == "F17"] <- "sound.worse.tin"

names(dat)[names(dat) == "F18"] <- "sleep.lo"
names(dat)[names(dat) == "F19"] <- "sleep.aw"
names(dat)[names(dat) == "F20"] <- "sleep.an"
names(dat)[names(dat) == "F21"] <- "sleep.lb"
names(dat)[names(dat) == "F22"] <- "sleep.lb.type"
names(dat)[names(dat) == "F23"] <- "sleep.lb.source"

names(dat)[names(dat) == "F24"] <- "home.lo"
names(dat)[names(dat) == "F25"] <- "home.aw"
names(dat)[names(dat) == "F26"] <- "home.an"
names(dat)[names(dat) == "F27"] <- "home.lb"
names(dat)[names(dat) == "F28"] <- "home.lb.type"
names(dat)[names(dat) == "F29"] <- "home.lb.source"

#Create new variables =========================================================

#sum and recode of laterality
dat$tin.lat.sum <- dat$F11_1 + dat$F11_2 + dat$F11_3 + dat$F11_4 + dat$F11_5 + dat$F11_6 + dat$F11_7 + dat$F11_8
dat$tin.lat.recode <- (paste(dat$F11_1, dat$F11_2, dat$F11_3, dat$F11_4, dat$F11_5, dat$F11_6, dat$F11_7, dat$F11_8, sep=""))

#Sum of tinnitus type sleep
dat$sleep.lb.type.sum <- dat$F22_1 + dat$F22_2 + dat$F22_3 + dat$F22_4 + dat$F22_5 + dat$F22_6

#Sum of tinnitus type home
dat$home.lb.type.sum <- dat$F28_1 + dat$F28_2 + dat$F28_3 + dat$F28_4 + dat$F28_5 + dat$F28_6

#Tinnitus duration in months
dat$tin.dur.months <- interval(dat$first.tin.date, "2023-04-26") %/% months(1)

#Ha use months
dat$device.dur.months <- interval(dat$ha.date, "2023-04-26") %/% months(1)

#Specify HA-user and none HA-user
dat <- dat %>%
  mutate(HA.user = case_when(device == "Happ" & device.dur.months >= 6 & ha.use.days >= 5 & ha.hours.day >= 6 ~ 'Happ.user',
                          device == 'None' ~ 'No.device'))

#Specify useful function for "multiple" combinations
BinToDec <- function(x) 
  sum(2^(which(rev(unlist(strsplit(as.character(x), "")) == 1))-1))

#Descriptives =================================================================

tab1 <- tableby(sex ~ age + hearing + device + tin.con.occ + tin.dur.months + tin.lat + tin.pitch + tin.loud.varies + tin.reduce.env + sound.tolerance + sound.worse.tin, data = dat)
summary(tab1, text = TRUE, test = FALSE)

arsenal::write2word(tab1, "descriptives_arsenal.docx", title = "My table",
                    quiet = TRUE)

# Analysis
# Fråga 1 =====================================================================
# Vilken kategori av ljud, presenterad från vilken ljudgivare används för ljudberikning av personer med tinnitus

#Check crosstables
table(dat$sleep.lb.type, dat$sleep.lb.source)
table(dat$home.lb.type, dat$home.lb.source)

#Count combinations into new data frames
sleep.cc <- dat %>%
              group_by(sleep.lb.type, sleep.lb.source) %>%
              summarise(count = n())

home.cc <- dat %>%
              group_by(home.lb.type, home.lb.source) %>%
              summarise(count = n())

#Omit NA from new data frames
sleep.cc <- na.omit(sleep.cc)
home.cc <- na.omit(home.cc)

#Table for proportion?
#sleep.cc.tab <- tableby(data = sleep.cc, sleep.lb.type ~ sleep.lb.source)
#summary(sleep.cc.tab, text = TRUE)

# Stacked bars. NB!! - hardcoded colors to match
sleep.bar <- ggplot(sleep.cc, aes(fill=sleep.lb.type, y=count, x=sleep.lb.source)) + 
  geom_bar(position="stack", stat="identity", color = "black")+
  #scale_fill_brewer(palette="viridis")+
  scale_fill_manual(values=c(viridis(7)))+
  xlab("Ljudkälla")+
  ylab("Antal")+
  ggtitle("Insomning")+
  theme_minimal()+
  theme(text = element_text(size = 24), plot.title = element_text(hjust = 0.5, size = 30))+
  guides(fill=guide_legend(title="Ljudtyp"))

home.bar <- ggplot(home.cc, aes(fill=home.lb.type, y=count, x=home.lb.source)) + 
  geom_bar(position="stack", stat="identity", color = "black")+
  #scale_fill_brewer(palette="YlGnBu")+
  scale_fill_manual(values=c(viridis(7)))+
  xlab("Ljudkälla")+
  ylab("Antal")+
  ggtitle("Hemmiljö")+
  theme_minimal()+
  theme(text = element_text(size = 24), plot.title = element_text(hjust = 0.5, size = 30))+
  guides(fill=guide_legend(title="Ljudtyp"))

plot_grid(sleep.bar, home.bar,
          ncol = 2, nrow = 1, labels = c())

# Fråga 2 =====================================================================
# Finns det samband mellan högre/lägre besvärsgrad och användning av ljudberikning? 

#Create table of t.test

#Empty matrix
t.res <- matrix(NA,6,8)

#tests
home.an.t <- t.test(subset(dat$home.an, dat$home.lb == "Yes"), subset(dat$home.an, dat$home.lb == "No"))
home.aw.t <- t.test(subset(dat$home.aw, dat$home.lb == "Yes"), subset(dat$home.aw, dat$home.lb == "No"))
home.lo.t <- t.test(subset(dat$home.lo, dat$home.lb == "Yes"), subset(dat$home.lo, dat$home.lb == "No"))

sleep.an.t <- t.test(subset(dat$sleep.an, dat$sleep.lb == "Yes"), subset(dat$sleep.an, dat$sleep.lb == "No"))
sleep.aw.t <- t.test(subset(dat$sleep.aw, dat$sleep.lb == "Yes"), subset(dat$sleep.aw, dat$sleep.lb == "No"))
sleep.lo.t <- t.test(subset(dat$sleep.lo, dat$sleep.lb == "Yes"), subset(dat$sleep.lo, dat$sleep.lb == "No"))

#write to matrix
t.res[1,1] <- mean(subset(dat$sleep.an, dat$home.lb == "Yes"))
t.res[1,2] <- sd(subset(dat$sleep.an, dat$home.lb == "Yes"))
t.res[1,3] <- mean(subset(dat$sleep.an, dat$home.lb == "No"))
t.res[1,4] <- sd(subset(dat$sleep.an, dat$home.lb == "No"))
t.res[1,5] <- sleep.an.t$statistic
t.res[1,6] <- sleep.an.t$parameter
t.res[1,7] <- sleep.an.t$p.value

t.res[2,1] <- mean(subset(dat$sleep.aw, dat$home.lb == "Yes"))
t.res[2,2] <- sd(subset(dat$sleep.aw, dat$home.lb == "Yes"))
t.res[2,3] <- mean(subset(dat$sleep.aw, dat$home.lb == "No"))
t.res[2,4] <- sd(subset(dat$sleep.aw, dat$home.lb == "No"))
t.res[2,5] <- sleep.aw.t$statistic
t.res[2,6] <- sleep.aw.t$parameter
t.res[2,7] <- sleep.aw.t$p.value

t.res[3,1] <- mean(subset(dat$sleep.lo, dat$home.lb == "Yes"))
t.res[3,2] <- sd(subset(dat$sleep.lo, dat$home.lb == "Yes"))
t.res[3,3] <- mean(subset(dat$sleep.lo, dat$home.lb == "No"))
t.res[3,4] <- sd(subset(dat$sleep.lo, dat$home.lb == "No"))
t.res[3,5] <- sleep.lo.t$statistic
t.res[3,6] <- sleep.lo.t$parameter
t.res[3,7] <- sleep.lo.t$p.value

t.res[4,1] <- mean(subset(dat$home.an, dat$home.lb == "Yes"))
t.res[4,2] <- sd(subset(dat$home.an, dat$home.lb == "Yes"))
t.res[4,3] <- mean(subset(dat$home.an, dat$home.lb == "No"))
t.res[4,4] <- sd(subset(dat$home.an, dat$home.lb == "No"))
t.res[4,5] <- home.an.t$statistic
t.res[4,6] <- home.an.t$parameter
t.res[4,7] <- home.an.t$p.value

t.res[5,1] <- mean(subset(dat$home.aw, dat$home.lb == "Yes"))
t.res[5,2] <- sd(subset(dat$home.aw, dat$home.lb == "Yes"))
t.res[5,3] <- mean(subset(dat$home.aw, dat$home.lb == "No"))
t.res[5,4] <- sd(subset(dat$home.aw, dat$home.lb == "No"))
t.res[5,5] <- home.aw.t$statistic
t.res[5,6] <- home.aw.t$parameter
t.res[5,7] <- home.aw.t$p.value

t.res[6,1] <- mean(subset(dat$home.lo, dat$home.lb == "Yes"))
t.res[6,2] <- sd(subset(dat$home.lo, dat$home.lb == "Yes"))
t.res[6,3] <- mean(subset(dat$home.lo, dat$home.lb == "No"))
t.res[6,4] <- sd(subset(dat$home.lo, dat$home.lb == "No"))
t.res[6,5] <- home.lo.t$statistic
t.res[6,6] <- home.lo.t$parameter
t.res[6,7] <- home.lo.t$p.value

#Correct p-values
t.res[,8] <- p.adjust(t.res[,7], method = "BH")

#round values
t.res[,1:6] <- round(t.res[,1:6], 2)
t.res[,7] <- round(t.res[,7], 4)

#Make df
t.res <- as.data.frame(t.res)

#name rows and cols
rownames(t.res) <- c("sleep an", "sleep aw", "sleep lo", "home an", "home aw", "home lo")
t.res <- cbind(rownames(t.res), data.frame(t.res, row.names=NULL))

colnames(t.res) <- c("Group/NRS" , "Yes - Mean", "Yes - SD", "No - Mean", "No - SD", "t", "df", "p", "p corrected (BH)")


#write df to xlsx
write_xlsx(t.res, "t_test_results.xlsx")

#Create plots
an.sleep.plot <- ggplot(dat, aes(x=sleep.lb, y=sleep.an, fill = sleep.lb)) + 
  geom_boxplot(lwd = 0.75) +
  #scale_fill_brewer(palette="viridis")+
  scale_fill_manual(values=c(viridis(7)[4], viridis(7)[7]))+
  xlab("")+
  ylab("Tinnitus Annoyance (NRS)")+
  ylim(0,100)+
  theme_minimal()+
  theme(legend.position="none", text = element_text(size = 20),
        panel.grid.major = element_line(colour = "#cecece"),
        panel.grid.minor = element_line(colour = "#e2e2e2"))

aw.sleep.plot <- ggplot(dat, aes(x=sleep.lb, y=sleep.aw, fill = sleep.lb)) + 
  geom_boxplot(lwd = 0.75) +
  #scale_fill_brewer(palette="Accent")+
  scale_fill_manual(values=c(viridis(7)[4], viridis(7)[7]))+
  xlab("")+
  ylab("Tinnitus Awareness (NRS)")+
  ylim(0,100)+
  theme_minimal()+
  theme(legend.position="none", text = element_text(size = 20),
        panel.grid.major = element_line(colour = "#cecece"),
        panel.grid.minor = element_line(colour = "#e2e2e2"))

lo.sleep.plot <- ggplot(dat, aes(x=sleep.lb, y=sleep.lo, fill = sleep.lb)) + 
  geom_boxplot(lwd = 0.75) +
  #scale_fill_brewer(palette="Accent")+
  scale_fill_manual(values=c(viridis(7)[4], viridis(7)[7]))+
  xlab("")+
  ylab("Tinnitus Loudness (NRS)")+
  ylim(0,100)+
  theme_minimal()+
  theme(legend.position="none", text = element_text(size = 20),
        panel.grid.major = element_line(colour = "#cecece"),
        panel.grid.minor = element_line(colour = "#e2e2e2"))
  #guides(fill=guide_legend(title=NULL))+
  #scale_fill_manual(values=c("#7FC97F", "#BEAED4"), 
   #                 name="Använder ljudberikning",
    #                breaks=c("Yes", "No"),
    #                labels=c("Ja", "Nej"))

an.home.plot <- ggplot(subset(dat, !is.na(home.lb)), aes(x=home.lb, y=home.an, fill = home.lb)) + 
  geom_boxplot(lwd = 0.75) +
  #scale_fill_brewer(palette="Accent")+
  scale_fill_manual(values=c(viridis(7)[4], viridis(7)[7]))+
  xlab("")+
  ylab("Tinnitus Annoyance (NRS)")+
  ylim(0,100)+
  theme_minimal()+
  theme(legend.position="none", text = element_text(size = 20),
        panel.grid.major = element_line(colour = "#cecece"),
        panel.grid.minor = element_line(colour = "#e2e2e2"))

aw.home.plot <- ggplot(subset(dat, !is.na(home.lb)), aes(x=home.lb, y=home.aw, fill = home.lb)) + 
  geom_boxplot(lwd = 0.75) +
  #scale_fill_brewer(palette="Accent")+
  scale_fill_manual(values=c(viridis(7)[4], viridis(7)[7]))+
  xlab("")+
  ylab("Tinnitus Awareness (NRS)")+
  ylim(0,100)+
  theme_minimal()+
  theme(legend.position="none", text = element_text(size = 20),
        panel.grid.major = element_line(colour = "#cecece"),
        panel.grid.minor = element_line(colour = "#e2e2e2"))

lo.home.plot <- ggplot(subset(dat, !is.na(home.lb)), aes(x=home.lb, y=home.lo, fill = home.lb)) + 
  geom_boxplot(lwd = 0.75) +
  #scale_fill_brewer(palette="Accent")+
  scale_fill_manual(values=c(viridis(7)[4], viridis(7)[7]))+
  xlab("")+
  ylab("Tinnitus Loudness (NRS)")+
  ylim(0,100)+
  theme_minimal()+
  theme(legend.position="none", text = element_text(size = 20),
        panel.grid.major = element_line(colour = "#cecece"),
        panel.grid.minor = element_line(colour = "#e2e2e2"))
#guides(fill=guide_legend(title=NULL))+
#scale_fill_manual(values=c("#7FC97F", "#BEAED4"), 
#                 name="Använder ljudberikning",
#                breaks=c("Yes", "No"),
#                labels=c("Ja", "Nej"))

plot_grid(an.sleep.plot, aw.sleep.plot, lo.sleep.plot, 
          an.home.plot, aw.home.plot, lo.home.plot, 
          ncol = 3, nrow = 2, labels = c("A", "B", "C", "D", "E", "F"))

# Fråga 3 =====================================================================
# Skillnader mellan hur personer med tin och tin+ha använder sig av ljudberikning

#table and Chi Square test
table(dat$HA.user, dat$sleep.lb)
chisq.test(dat$HA.user, dat$sleep.lb)

table(dat$HA.user, dat$home.lb)
chisq.test(dat$HA.user, dat$home.lb)

