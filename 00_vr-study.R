library("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))


library(devtools)
source_url("https://raw.githubusercontent.com/M-Colley/rCode/main/r_functionality.R")


main_df <- read_xlsx(path = "data-main.xlsx", sheet = "Results")
main_df <- as.data.frame(main_df)
names(main_df)



# participant 4 got sick
main_df <- subset(main_df, userID!=4)


# Make sure the subject column is a factor
main_df$probandenID <- as.factor(main_df$probandenID)
main_df$scenarioID <- as.factor(main_df$scenarioID)
main_df$repExposure <- as.factor(main_df$repExposure)



main_df$ps_score <- rowSums(main_df[, c("perceivedSafety1", "perceivedSafety2", "perceivedSafety3", "perceivedSafety4")]) / 4.0

# UEQ-S
main_df$pragmatic_quality <- rowSums(main_df[,c("ueqs1", "ueqs2", "ueqs3", "ueqs4")])/4.0
main_df$hedonic_quality <- rowSums(main_df[,c("ueqs5", "ueqs6", "ueqs7", "ueqs8")])/4.0

# Trust
main_df$TiA_Understanding2 <- 6 - main_df$TiA_Understanding2
main_df$TiA_Understanding4 <- 6 - main_df$TiA_Understanding4

main_df$overallTiAUnderstanding <- rowSums(main_df[, c("TiA_Understanding1", "TiA_Understanding2", "TiA_Understanding3", "TiA_Understanding4")]) / 4.0
main_df$overallTiATrust <- rowSums(main_df[, c("TiA_Trust1", "TiA_Trust2")]) / 2.0





# get relevant factors in new column
# Conditions should be correct already!
main_df$traffic <- with(main_df, ifelse(main_df$scenarioID %in% c(2, 4), "automated", "mixed"))
main_df$ehmi <- with(main_df, ifelse(main_df$scenarioID %in% c(1, 2), "with eHMI", "without eHMI"))

main_df$traffic <- as.factor(main_df$traffic)
main_df$ehmi <- as.factor(main_df$ehmi)

levels(main_df$traffic)
levels(main_df$repExposure)
levels(main_df$ehmi)



#### Mental Workload ####
checkAssumptionsForAnovaThreeFactors(data = main_df, y = "TLX1", factor_1 = "traffic", factor_2 = "ehmi", factor_3 = "repExposure")


model <- art(TLX1 ~ traffic * ehmi * repExposure + (1|probandenID), data = main_df)
summary(model)
anova(model)


#### overallTiAUnderstanding ####
checkAssumptionsForAnovaThreeFactors(data = main_df, y = "overallTiAUnderstanding", factor_1 = "traffic", factor_2 = "ehmi", factor_3 = "repExposure")


model <- art(overallTiAUnderstanding ~ traffic * ehmi * repExposure + (1|probandenID), data = main_df)
summary(model)
anova(model)



#### overallTiATrust ####
checkAssumptionsForAnovaThreeFactors(data = main_df, y = "overallTiATrust", factor_1 = "traffic", factor_2 = "ehmi", factor_3 = "repExposure")


model <- art(overallTiATrust ~ traffic * ehmi * repExposure + (1|probandenID), data = main_df)
summary(model)
anova(model)




#### pragmatic_quality ####
checkAssumptionsForAnovaThreeFactors(data = main_df, y = "pragmatic_quality", factor_1 = "traffic", factor_2 = "ehmi", factor_3 = "repExposure")


model <- art(pragmatic_quality ~ traffic * ehmi * repExposure + (1|probandenID), data = main_df)
summary(model)
anova(model)



#### hedonic_quality ####
checkAssumptionsForAnovaThreeFactors(data = main_df, y = "hedonic_quality", factor_1 = "traffic", factor_2 = "ehmi", factor_3 = "repExposure")


model <- art(hedonic_quality ~ traffic * ehmi * repExposure + (1|probandenID), data = main_df)
summary(model)
anova(model)




###########PerceivedSafety####
checkAssumptionsForAnovaThreeFactors(data = main_df, y = "ps_score", factor_1 = "traffic", factor_2 = "ehmi", factor_3 = "repExposure")


model <- np.anova(formel = ps_score ~ traffic * ehmi * repExposure + Error(probandenID/( traffic * ehmi * repExposure)), data = main_df, method = 0, compact = T)
model

reportNPAVChi(model, dv = "perceived safety")


model <- art(ps_score ~ traffic * ehmi * repExposure + (1|probandenID), data = main_df)
summary(model)
anova(model)







reportMeanAndSD(data = main_df, iv = "traffic", dv = "ps_score")
reportMeanAndSD(data = main_df, iv = "ehmi", dv = "ps_score")
reportMeanAndSD(data = main_df, iv = "repExposure", dv = "ps_score")



main_df %>% ggplot() +
  theme_bw(base_size = myfontsize + 1) +
  aes(x = repExposure, y = ps_score, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_colour_manual(values=wes_palette("Cavalcanti1", n=5)) + 
  ylab("Perceived Safety") +
  theme(legend.title = element_blank(), legend.background = element_blank(), legend.position = c(0.75, 0.84), legend.text = element_text(size = myfontsize - 10)) +
  xlab("Repetition") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "point", size = 4.0,  aes(group = 1)) +
  stat_summary(fun = mean, geom = "line", size = 2, aes(group = 1)) +
  stat_summary(fun = mean, geom = "line", linetype = "dashed", size = 1) +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .5, position = position_dodge(width = 0.1))# 95 % mean_cl_normal is 95% confidence intervals
ggsave("plots/perceived_safety_repetition.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)



main_df %>% ggplot() +
  theme_bw(base_size = myfontsize + 1) +
  aes(x = ehmi, y = ps_score, fill = traffic, colour = traffic, group = traffic) +
  scale_colour_manual(values=wes_palette("Cavalcanti1", n=5)) + 
  ylab("Perceived Safety") +
  theme(legend.title = element_blank(), legend.background = element_blank(), legend.position = c(0.75, 0.84), legend.text = element_text(size = myfontsize - 10)) +
  xlab("eHMI") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "point", size = 4.0,  aes(group = 1)) +
  stat_summary(fun = mean, geom = "line", size = 2, aes(group = 1)) +
  stat_summary(fun = mean, geom = "line", linetype = "dashed", size = 1) +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .5, position = position_dodge(width = 0.1))# 95 % mean_cl_normal is 95% confidence intervals
ggsave("plots/perceived_safety_eHMI.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)





