#install.packages("ggExtra")
#install.packages("scales")
#install.packages("tidyverse")

library(ggExtra)
library(scales)
library(tidyverse)

setwd("[CSV data folder]")

#######################################################
## META-REVIEW OF CRIMINAL RECORD HIRING EXPERIMENTS ##
#######################################################

aud <- read_csv("prior-audits.csv")
wth <- read_csv("prior-surveys.csv")

aud$audit <- 1
wth$audit <- 0
df <- bind_rows(aud, wth)

# labels #

df$record <- factor(df$record, levels = c("yes", "no", "comb"))
df$race <- factor(df$race, levels = c("black", "latino", "white", "pooled"))
df$design <- factor(df$design, levels = c("in person", "online", "survey"))

# reshape wide, by criminal record #

df.no.rec <- df %>% filter(record == "no") %>% group_by(num, end.year, design, race, no.rec.num) %>% summarize(call.no.rec = callback, n.no.rec = n.apps)
df.rec <- df %>% filter(record == "yes") %>% group_by(num, end.year, design, race, rec.num) %>% summarize(call.rec = callback, n.rec = n.apps)
df.rec.merge <- full_join(df.no.rec, df.rec, by = c("num", "end.year", "design", "race"), relationship = "many-to-many")
df.rec.merge <- df.rec.merge %>% drop_na()
df.rec.merge <- df.rec.merge %>% filter(num != 25 | (num == 25 & no.rec.num == rec.num)) #keep only same-credential comparisons in study 25 (Lindsay, 2021)
df.rec.merge <- df.rec.merge %>% filter(num != 33 | (num == 33 & no.rec.num == rec.num)) #keep only same-background comparisons in study 32 (Santos et al., 2023)
df.rec.merge <- df.rec.merge %>% mutate(diff = (call.rec - call.no.rec), pdiff = 100 * (diff / call.no.rec), ndiff = (n.rec + n.no.rec), n = if_else(!is.na(diff), 1, 0))
df.rec.merge <- data.frame(df.rec.merge)

# reshape wide, by black/white #

df.white <- df %>% filter(race == "white") %>% group_by(num, end.year, design, record, rec.num, no.rec.num) %>% summarize(call.white = callback, n.white = n.apps)
df.black <- df %>% filter(race == "black") %>% group_by(num, end.year, design, record, rec.num, no.rec.num) %>% summarize(call.black = callback, n.black = n.apps)
df.race.merge <- full_join(df.white, df.black, by = c("num", "end.year", "design", "record"), relationship = "many-to-many")
df.race.merge <- df.race.merge %>% filter(num != 15 | (num == 15 & record != "yes") | (num == 15 & record == "yes" & (rec.num.x == rec.num.y))) #Agan and Starr (2018) NJ
df.race.merge <- df.race.merge %>% filter(num != 16 | (num == 16 & record != "yes") | (num == 16 & record == "yes" & (rec.num.x == rec.num.y))) #Agan and Starr (2018) NY
df.race.merge <- df.race.merge %>% filter(num != 17 | (num == 17 & record != "yes") | (num == 17 & record == "yes" & (rec.num.x == rec.num.y))) #Leasure (2019)
df.race.merge <- df.race.merge %>% filter(num != 20 | (num == 20 & record != "yes") | (num == 20 & record == "yes" & (rec.num.x == rec.num.y))) #Leasure and Andersen (2020)
df.race.merge <- df.race.merge %>% filter(num != 22 | (num == 22 & record != "yes") | (num == 22 & record == "yes" & (rec.num.x == rec.num.y))) #Leasure and Kaminski (2021a) BW
df.race.merge <- df.race.merge %>% filter(num != 24 | (num == 24 & record != "yes") | (num == 24 & record == "yes" & (rec.num.x == rec.num.y))) #Leasure and Zhang (2021)
df.race.merge <- df.race.merge %>% filter(num != 25 | (num == 25 & record == "comb") | (num == 25 & record == "no" & (no.rec.num.x == no.rec.num.y)) | (num == 25 & record == "yes" & (rec.num.x == rec.num.y))) #Lindsay (2021)
df.race.merge <- df.race.merge %>% filter(num != 26 | (num == 26 & record != "yes") | (num == 26 & record == "yes" & (rec.num.x == rec.num.y))) #Ripper (2022)
df.race.merge <- df.race.merge %>% filter(num != 30 | (num == 30 & record != "yes") | (num == 30 & record == "yes" & (rec.num.x == rec.num.y))) #DeWitt and Denver (2020)
df.race.merge <- df.race.merge %>% select(-c("rec.num.x", "no.rec.num.x", "rec.num.y", "no.rec.num.y"))
df.race.merge <- df.race.merge %>% drop_na()
df.race.merge <- df.race.merge %>% mutate(diff = (call.black - call.white), pdiff = 100 * (diff / call.white), ndiff = (n.white + n.black), n = if_else(!is.na(diff), 1, 0))
df.race.merge <- data.frame(df.race.merge)

# callback differences, by criminal record #

diff.rec <- df.rec.merge %>% summarize(num = sum(!is.na(unique(num))), n = sum(n), mn.pdiff = mean(pdiff), mn.pdiff.wt = weighted.mean(pdiff, w = ndiff))
diff.rec.race <- df.rec.merge %>% filter(race != "pooled") %>% group_by(race) %>% summarize(num = sum(!is.na(unique(num))), n = sum(n), mn.pdiff = mean(pdiff), mn.pdiff.wt = weighted.mean(pdiff, w = ndiff))
diff.rec.des <- df.rec.merge %>% group_by(design) %>% summarize(num = sum(!is.na(unique(num))), n = sum(n), mn.pdiff = mean(pdiff), mn.pdiff.wt = weighted.mean(pdiff, w = ndiff))

# callback differences, by black/white #

diff.race <- df.race.merge %>% summarize(num = sum(!is.na(unique(num))), n = sum(n), mn.pdiff = mean(pdiff), mn.pdiff.wt = weighted.mean(pdiff, w = ndiff))
diff.race.des <- df.race.merge %>% group_by(design) %>% summarize(num = sum(!is.na(unique(num))), n = sum(n), mn.pdiff = mean(pdiff), mn.pdiff.wt = weighted.mean(pdiff, w = ndiff))

# TABLE 1 #

diff.rec
diff.rec.race
diff.rec.des
diff.race
diff.race.des

# FIGURE 1 #

x1 <- ggplot(df.rec.merge, aes(x = end.year, y = pdiff)) + geom_hline(yintercept = 0, linetype = "dashed") + geom_point(aes(size = ndiff, color = as.factor(design)), alpha = 0.8, position = position_jitter(width = 1, height = 0, seed = 20230824)) + scale_y_continuous(label = percent_format(scale = 1)) + scale_color_viridis_d(name = "By Study Design", labels = c("In-Person Audit", "Online Audit", "Opt-In Survey")) + scale_size_continuous(range = c(2, 10)) + guides(size = "none", color = guide_legend(override.aes = list(size = 5))) + theme_bw(base_size = 18) + theme(legend.position = "top") + labs(x = "End Year of Data Collection", y = "Relative Difference in Callback Rate", title = element_blank()) + geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "darkgray") + geom_text(x = 2021, y = -41.7, label = "X[R]", fontface = "bold", parse = T, size.unit = "pt", size = 12) + geom_text(x = 2021, y = -7.7, label = "X[G]", fontface = "bold", parse = T, size.unit = "pt", size = 12) + annotate(geom = "text", x = 1957, y = 31, label = "Disadvantage to\nNo Criminal Record", angle = 90, size = 5, color = "gray40") + annotate(geom = "text", x = 1957, y = -27, label = "Disadvantage to\nCriminal Record", angle = 90, size = 5, color = "gray40")
jpeg(file = "Fig1.jpg", width = 13, height = 8, units = "in", res = 750)
ggMarginal(x, type = "density", margins = "y", groupFill = T)
dev.off()

quantile(df.rec.merge$pdiff, c(.05, .95))
lm1 <- lm(pdiff ~ end.year, data = df.rec.merge)
lm2 <- lm(pdiff ~ end.year + design, data = df.rec.merge)
coef(lm1)
coef(lm2)
coef(lm1)[2] * 60

# FIGURE 2 #

datapoints <- data.frame(race = c("black", "white"), record = c(-34.3, -50.3), google = c(-7.1, -8.4))
df.rec.merge %>% filter(race == "black" | race == "white") %>% ggplot(aes(x = end.year, y = pdiff)) + geom_hline(yintercept = 0, linetype = "dashed") + geom_point(aes(size = ndiff, color = as.factor(design)), alpha = 0.8, position = position_jitter(width = 2, height = 1, seed = 20230824)) + scale_y_continuous(label = percent_format(scale = 1), limits = c(-101, 70)) + scale_color_viridis_d(name = "By Study Design", labels = c("In-Person Audit", "Online Audit", "Opt-In Survey")) + scale_size_continuous(range = c(2, 10)) + guides(size = "none", color = guide_legend(override.aes = list(size = 5))) + theme_bw(base_size = 18) + theme(legend.position = "top") + labs(x = "End Year of Data Collection", y = "Relative Difference in Callback Rate", title = element_blank()) + geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "darkgray") + geom_text(data = datapoints, aes(x = 2021, y = record), position = "identity", label = "X[R]", parse = T, fontface = "bold", size.unit = "pt", size = 12) + geom_text(data = datapoints, aes(x = 2021, y = google), position = "identity", label = "X[G]", parse = T, fontface = "bold", size.unit = "pt", size = 12) + annotate(geom = "text", x = 1998, y = 38, label = "Disadvantages\nNo Criminal Record", angle = 90, size = 4.8, color = "gray40") + annotate(geom = "text", x = 1998, y = -33, label = "Disadvantages\nCriminal Record", angle = 90, size = 4.8, color = "gray40") + facet_wrap(~ race, nrow = 2, labeller = as_labeller(c("black" = "Black Applicants", "white" = "White Applicants")))
ggsave(file = "Fig2.jpg", width = 13, height = 13, dpi = 750)

lm1b <- lm(pdiff ~ end.year, data = df.rec.merge, subset = race == "black")
lm1w <- lm(pdiff ~ end.year, data = df.rec.merge, subset = race == "white")
coef(lm1b)
coef(lm1w)

# FIGURE 3 #

y <- ggplot(data = df.race.merge, aes(x = end.year, y = pdiff)) + geom_hline(yintercept = 0, linetype = "dashed") + geom_point(aes(size = ndiff, color = as.factor(design)), alpha = 0.8, position = position_jitter(width = 2, height = 1, seed = 20230824)) + scale_y_continuous(label = percent_format(scale = 1), limits = c(-100, 50)) + scale_color_viridis_d(name = "By Study Design", labels = c("In-Person Audit", "Online Audit", "Opt-In Survey")) + scale_size_continuous(range = c(2, 10)) + guides(size = "none", color = guide_legend(override.aes = list(size = 4))) + theme_bw(base_size = 18) + theme(legend.position = "top") + labs(x = "End Year of Data Collection", y = "Relative Difference in Callback Rate") + geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "darkgray") + geom_text(x = 2021, y = 30.0, label = "X", fontface = "bold", size.unit = "pt", size = 12) + annotate(geom = "text", x = 1999, y = 26, label = "Disadvantage to\nWhite Applicants", angle = 90, size = 5, color = "gray40") + annotate(geom = "text", x = 1999, y = -26, label = "Disadvantage to\nBlack Applicants", angle = 90, size = 4, color = "gray40")
jpeg(file = "Fig3.jpg", width = 13, height = 8, units = "in", res = 750)
ggMarginal(y, type = "density", margins = "y", groupFill = T)
dev.off()

lm3 <- lm(pdiff ~ end.year, data = df.race.merge)
lm4 <- lm(pdiff ~ end.year + design, data = df.race.merge)
coef(lm3)
coef(lm4)

###################################################
## FIGURE 4: DISTRIBUTION OF WILLINGNESS TO HIRE ##
###################################################

yougov <- read_csv("yougov-callback.csv")

ggplot(yougov, aes(x = as.factor(tx), y = callback)) + geom_boxplot(outlier.shape = NA) + geom_jitter(aes(color = as.factor(tx4), fill = as.factor(tx4)), alpha = 0.8, position = position_jitter(width = 0.2, height = 0.5, seed = 20230824)) + theme_bw(base_size = 18) + scale_y_continuous(breaks = seq(1,7,1)) + scale_x_discrete(labels = c("Google Clean", "Google Hit", "Google Clean", "Google Hit", "Google Clean", "Google Hit", "Google Clean", "Google Hit")) + theme(legend.position = "none") + scale_color_viridis_d() + scale_fill_viridis_d() + labs(x = "", y = "Willingness to Hire (1 = Very Unlikely; 7 = Very Likely)", color = "", fill = "", caption = "") + stat_summary(fun.y = mean, geom = "point", color="black", size = 4) + stat_summary(fun.y = mean, geom = "point", shape = 3, color="black", size = 5, stroke = 1.25) + annotate("text", x = c(1.5, 3.5, 5.5, 7.5), y = -0.4, size = 5, color = "gray30", label = c("Clean Record", "Criminal Record", "Clean Record", "Criminal Record")) + annotate("text", x = c(2.5, 6.5), y = -0.7, size = 5, color = "gray30", label = c("BLACK APPLICANT", "WHITE APPLICANT")) + coord_cartesian(ylim = c(0.5, 7.5), clip = "off")
ggsave(file = "Fig4.jpg", width = 13, height = 8, dpi = 750)

