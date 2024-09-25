library("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))


library(devtools)
source_url("https://raw.githubusercontent.com/M-Colley/rCode/main/r_functionality.R")



main_df <- read_xlsx(path = "data-main.xlsx", sheet = "Results")
main_df <- as.data.frame(main_df)
names(main_df)



# participant 4 got sick
main_df <- subset(main_df, probandenID != 4)


# Make sure the subject column is a factor
main_df$probandenID <- as.factor(main_df$probandenID)
main_df$scenarioID <- as.factor(main_df$scenarioID)
main_df$repExposure <- as.factor(main_df$repExposure)

print(main_df$probandenID)


main_df$ps_score <- rowSums(main_df[, c("perceivedSafety1", "perceivedSafety2", "perceivedSafety3", "perceivedSafety4")]) / 4.0


# UEQ-S
main_df$pragmatic_quality <- rowSums(main_df[, c("ueqs1", "ueqs2", "ueqs3", "ueqs4")]) / 4.0
main_df$hedonic_quality <- rowSums(main_df[, c("ueqs5", "ueqs6", "ueqs7", "ueqs8")]) / 4.0

# Trust
main_df$TiA_Understanding2 <- 6 - main_df$TiA_Understanding2
main_df$TiA_Understanding4 <- 6 - main_df$TiA_Understanding4

main_df$overallTiAUnderstanding <- rowSums(main_df[, c("TiA_Understanding1", "TiA_Understanding2", "TiA_Understanding3", "TiA_Understanding4")]) / 4.0
main_df$overallTiATrust <- rowSums(main_df[, c("TiA_Trust1", "TiA_Trust2")]) / 2.0

### Crossing Decision ###
main_df$crossing_decision <- rowSums(main_df[, c("crossingDecisionSpeed", "crossingDecisionDist", "crossingDecisionComm", "crossingDecisionOwt")]) / 4.0





# get relevant factors in new column
# Conditions should be correct already!
main_df$traffic <- with(main_df, ifelse(main_df$scenarioID %in% c(2, 4), "automated", "mixed"))
main_df$ehmi <- with(main_df, ifelse(main_df$scenarioID %in% c(1, 2), "with eHMI", "without eHMI"))

main_df$traffic <- as.factor(main_df$traffic)
main_df$ehmi <- as.factor(main_df$ehmi)

levels(main_df$traffic)
levels(main_df$repExposure)
levels(main_df$ehmi)

#### ComEval ####
##### comEval2 Polite | Impolite #####
checkAssumptionsForAnovaThreeFactors(data = main_df, y = "comEval2", factor_1 = "traffic", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(comEval2 ~ traffic * ehmi * repExposure + Error(probandenID / (traffic * ehmi * repExposure)), data = main_df) |> anova()
reportART(model = modelART, dv = "comEval2")
reportMeanAndSD(data = main_df, iv = "ehmi", dv = "comEval2")
d <- dunnTest(comEval2 ~ repExposure, data = main_df, method = "holm", two.sided = FALSE)
reportDunnTest(data = main_df, d = d, iv = "repExposure", dv = "comEval2")

main_df %>% ggplot() +
  aes(x = repExposure, y = comEval2, fill = traffic, colour = traffic, group = traffic) +
  scale_color_see() +
  ylab("Politeness") +
  theme(legend.position.inside = c(0.75, 0.84)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05), alpha = 0.5)
ggsave("plots/politness_comEval_2_repeated_exposure.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)

main_df %>% ggplot() +
  aes(x = repExposure, y = comEval2, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Politeness") +
  theme(legend.position.inside = c(0.75, 0.84)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05), alpha = 0.5)
ggsave("plots/politness_comEval_2_ehmi.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)

p <- main_df %>% ggplot() +
  aes(x = repExposure, y = comEval2, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Politeness") +
  theme(legend.position.inside = c(0.3, 0.8)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05), alpha = 0.5)
p + facet_grid(~traffic)
ggsave("plots/politness_comEval_2_ehmi_rep_traffic.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


##### comEval3 Machine-like #####
checkAssumptionsForAnovaThreeFactors(data = main_df, y = "comEval3", factor_1 = "traffic", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(comEval3 ~ traffic * ehmi * repExposure + Error(probandenID / (traffic * ehmi * repExposure)), data = main_df) |> anova()
reportART(model = modelART, dv = "comEval3")

##### comEval4 Not clear #####
checkAssumptionsForAnovaThreeFactors(data = main_df, y = "comEval4", factor_1 = "traffic", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(comEval4 ~ traffic * ehmi * repExposure + Error(probandenID / (traffic * ehmi * repExposure)), data = main_df) |> anova()
reportART(model = modelART, dv = "comEval4")
reportMeanAndSD(data = main_df, iv = "ehmi", dv = "comEval4")
d <- dunnTest(comEval4 ~ repExposure, data = main_df, method = "holm", two.sided = FALSE)
reportDunnTest(data = main_df, d = d, iv = "repExposure", dv = "comEval4")

##### comEval5 Naturalness #####
checkAssumptionsForAnovaThreeFactors(data = main_df, y = "comEval5", factor_1 = "traffic", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(comEval5 ~ traffic * ehmi * repExposure + Error(probandenID / (traffic * ehmi * repExposure)), data = main_df) |> anova()
reportART(model = modelART, dv = "comEval5")
reportMeanAndSD(data = main_df, iv = "ehmi", dv = "comEval5")


##### comEval6 Appropriateness #####
checkAssumptionsForAnovaThreeFactors(data = main_df, y = "comEval6", factor_1 = "traffic", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(comEval6 ~ traffic * ehmi * repExposure + Error(probandenID / (traffic * ehmi * repExposure)), data = main_df) |> anova()
reportART(model = modelART, dv = "comEval6")
reportMeanAndSD(data = main_df, iv = "ehmi", dv = "comEval6")


p <- main_df %>% ggplot() +
  aes(x = repExposure, y = comEval6, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Appropriateness") +
  theme(legend.position.inside = c(0.8, 0.1)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05), alpha = 0.5)
p + facet_grid(~traffic)
ggsave("plots/appropriateness_comEval_6.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


#### Presence ####
checkAssumptionsForAnovaThreeFactors(data = main_df, y = "presence", factor_1 = "traffic", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(presence ~ traffic * ehmi * repExposure + Error(probandenID / (traffic * ehmi * repExposure)), data = main_df) |> anova()
reportART(model = modelART, dv = "presence")

reportMeanAndSD(data = main_df, iv = "ehmi", dv = "presence")

main_df %>% ggplot() +
  aes(x = ehmi, y = presence, fill = traffic, colour = traffic, group = traffic) +
  scale_color_see() +
  ylab("Presence") +
  theme(legend.position.inside = c(0.75, 0.84)) +
  xlab("eHMI") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05), alpha = 0.5)
ggsave("plots/Result_VR_presence_1.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


main_df %>% ggplot() +
  aes(x = repExposure, y = presence, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Presence") +
  theme(legend.position.inside = c(0.25, 0.1)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05), alpha = 0.5)
ggsave("plots/Result_VR_presence_2.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)



#### Mental Workload ####
checkAssumptionsForAnovaThreeFactors(data = main_df, y = "TLX1", factor_1 = "traffic", factor_2 = "ehmi", factor_3 = "repExposure")


modelART <- art(TLX1 ~ traffic * ehmi * repExposure + Error(probandenID / (traffic * ehmi * repExposure)), data = main_df) |> anova()
reportART(model = modelART, dv = "mental workload")


reportMeanAndSD(data = main_df, iv = "ehmi", dv = "TLX1")

# model <- np.anova(formel = TLX1 ~ traffic * ehmi * repExposure + Error(probandenID/( traffic * ehmi * repExposure)), data = main_df, method = 0, compact = T)
# model

d <- dunnTest(TLX1 ~ repExposure, data = main_df, method = "holm", two.sided = FALSE)

reportDunnTest(data = main_df, d = d, iv = "repExposure", dv = "TLX1")
# reportDunnTestTable(data = main_df, iv = "repExposure", dv = "TLX1")

reportMeanAndSD(data = main_df, iv = "repExposure", dv = "TLX1")





#### overallTiAUnderstanding ####
checkAssumptionsForAnovaThreeFactors(data = main_df, y = "overallTiAUnderstanding", factor_1 = "traffic", factor_2 = "ehmi", factor_3 = "repExposure")


modelART <- art(overallTiAUnderstanding ~ traffic * ehmi * repExposure + Error(probandenID / (traffic * ehmi * repExposure)), data = main_df) |> anova()
reportART(model = modelART, dv = "overallTiAUnderstanding")


d <- dunnTest(overallTiAUnderstanding ~ repExposure, data = main_df, method = "holm", two.sided = FALSE)
reportDunnTest(data = main_df, d = d, iv = "repExposure", dv = "overallTiAUnderstanding")

reportMeanAndSD(data = main_df, iv = "ehmi", dv = "overallTiAUnderstanding")
reportMeanAndSD(data = main_df, iv = "repExposure", dv = "overallTiAUnderstanding")



#### overallTiATrust ####
checkAssumptionsForAnovaThreeFactors(data = main_df, y = "overallTiATrust", factor_1 = "traffic", factor_2 = "ehmi", factor_3 = "repExposure")


modelART <- art(overallTiATrust ~ traffic * ehmi * repExposure + Error(probandenID / (traffic * ehmi * repExposure)), data = main_df) |> anova()
reportART(model = modelART, dv = "overallTiATrust")
reportMeanAndSD(data = main_df, iv = "ehmi", dv = "overallTiATrust")

# reportMeanAndSD(data = main_df, iv = "repExposure", dv = "overallTiATrust")


#### pragmatic_quality ####
checkAssumptionsForAnovaThreeFactors(data = main_df, y = "pragmatic_quality", factor_1 = "traffic", factor_2 = "ehmi", factor_3 = "repExposure")


modelART <- art(pragmatic_quality ~ traffic * ehmi * repExposure + Error(probandenID / (traffic * ehmi * repExposure)), data = main_df) |> anova()
reportART(model = modelART, dv = "pragmatic quality")
reportMeanAndSD(data = main_df, iv = "ehmi", dv = "pragmatic_quality")
d <- dunnTest(pragmatic_quality ~ repExposure, data = main_df, method = "holm", two.sided = FALSE)
reportDunnTest(data = main_df, d = d, iv = "repExposure", dv = "pragmatic_quality")


main_df %>% ggplot() +
  aes(x = repExposure, y = pragmatic_quality, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Pragmatic Quality") +
  theme(legend.position.inside = c(0.35, 0.84)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05), alpha = 0.5)
ggsave("plots/Result_VR_pragmatic_quality.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)




#### hedonic_quality ####
checkAssumptionsForAnovaThreeFactors(data = main_df, y = "hedonic_quality", factor_1 = "traffic", factor_2 = "ehmi", factor_3 = "repExposure")


modelART <- art(hedonic_quality ~ traffic * ehmi * repExposure + Error(probandenID / (traffic * ehmi * repExposure)), data = main_df) |> anova()
reportART(model = modelART, dv = "hedonic quality")
reportMeanAndSD(data = main_df, iv = "ehmi", dv = "hedonic_quality")

main_df %>% ggplot() +
  aes(x = repExposure, y = hedonic_quality, fill = traffic, colour = traffic, group = traffic) +
  scale_color_see() +
  ylab("Hedonic Quality") +
  theme(legend.position.inside = c(0.25, 0.9)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05), alpha = 0.5)
ggsave("plots/Result_VR_hedonic_quality.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)




#### Crossing Decision ####
checkAssumptionsForAnovaThreeFactors(data = main_df, y = "crossing_decision", factor_1 = "traffic", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(crossing_decision ~ traffic * ehmi * repExposure + Error(probandenID / (traffic * ehmi * repExposure)), data = main_df) |> anova()
reportART(model = modelART, dv = "crossing decision")

#### Crossing Decision Speed ####
checkAssumptionsForAnovaThreeFactors(data = main_df, y = "crossingDecisionSpeed", factor_1 = "traffic", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(crossingDecisionSpeed ~ traffic * ehmi * repExposure + Error(probandenID / (traffic * ehmi * repExposure)), data = main_df) |> anova()
reportART(model = modelART, dv = "crossingDecisionSpeed")

p <- main_df %>% ggplot() +
  aes(x = repExposure, y = crossingDecisionSpeed, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Crossing Decision Speed") +
  theme(legend.position.inside = c(0.8, 0.15)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05), alpha = 0.5)
p + facet_grid(~traffic)
ggsave("plots/crossing_decision_speed_vr.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


#### Crossing Decision Distance ####
checkAssumptionsForAnovaThreeFactors(data = main_df, y = "crossingDecisionDist", factor_1 = "traffic", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(crossingDecisionDist ~ traffic * ehmi * repExposure + Error(probandenID / (traffic * ehmi * repExposure)), data = main_df) |> anova()
reportART(model = modelART, dv = "crossingDecisionDist")


#### Crossing Decision Communication ####
checkAssumptionsForAnovaThreeFactors(data = main_df, y = "crossingDecisionComm", factor_1 = "traffic", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(crossingDecisionComm ~ traffic * ehmi * repExposure + Error(probandenID / (traffic * ehmi * repExposure)), data = main_df) |> anova()
reportART(model = modelART, dv = "crossingDecisionComm")
reportMeanAndSD(data = main_df, iv = "traffic", dv = "crossingDecisionComm")
reportMeanAndSD(data = main_df, iv = "ehmi", dv = "crossingDecisionComm")

main_df %>% ggplot() +
  aes(x = repExposure, y = crossingDecisionComm, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Crossing Decision \n Communication") +
  theme(legend.position.inside = c(0.75, 0.14)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05), alpha = 0.5)
ggsave("plots/Result_VR_crossing_decision_communication_exposure_eHMI.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


main_df %>% ggplot() +
  aes(x = ehmi, y = crossingDecisionComm, fill = traffic, colour = traffic, group = traffic) +
  scale_color_see() +
  ylab("Crossing Decision \n Communication") +
  theme(legend.position.inside = c(0.75, 0.84)) +
  xlab("eHMI") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05), alpha = 0.5)
ggsave("plots/Result_VR_crossing_decision_communication_traffic_eHMI.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)

#### Crossing Decision Own Waiting Time ####
checkAssumptionsForAnovaThreeFactors(data = main_df, y = "crossingDecisionOwt", factor_1 = "traffic", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(crossingDecisionOwt ~ traffic * ehmi * repExposure + Error(probandenID / (traffic * ehmi * repExposure)), data = main_df) |> anova()
reportART(model = modelART, dv = "crossingDecisionOwt")
reportMeanAndSD(data = main_df, iv = "traffic", dv = "crossingDecisionOwt")



main_df %>% ggplot() +
  aes(x = repExposure, y = crossingDecisionComm, fill = traffic, colour = traffic, group = traffic) +
  scale_color_see() +
  ylab("Crossing Decision \n Own Waiting Time") +
  theme(legend.position.inside = c(0.25, 0.84)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05), alpha = 0.5)
ggsave("plots/Result_VR_crossing_decision_owt.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)




#### Crossing Decision Overall ####
sum.cd.speed <- sum(main_df$crossingDecisionSpeed)
sum.cd.communication <- sum(main_df$crossingDecisionComm)
sum.cd.owt <- sum(main_df$crossingDecisionOwt)
sum.cd.distance <- sum(main_df$crossingDecisionDist)
sum.count <- sum.cd.speed + sum.cd.communication + sum.cd.distance + sum.cd.owt

cd.percentage <- c((sum.cd.speed / sum.count) * 100, (sum.cd.communication / sum.count) * 100, (sum.cd.distance / sum.count) * 100, (sum.cd.owt / sum.count) * 100)
# print(answer)

data.cd <- data.frame(
  decision = c("Crossing Decision Speed", "Crossing Decision Communication", "Crossing Decision Distance", "Crossing Decision Own waiting time"),
  count = c(sum.cd.speed, sum.cd.communication, sum.cd.distance, sum.cd.owt)
)

data.cd$decision.short <- c("Speed", "Communication", "Distance", "Own waiting time")

data.cd$fraction <- data.cd$count / sum(data.cd$count)
data.cd$ymax <- cumsum(data.cd$fraction)
data.cd$ymin <- c(0, head(data.cd$ymax, n = -1))
data.cd$labelPosition <- (data.cd$ymax + data.cd$ymin) / 2
data.cd$label <- paste0(data.cd$decision.short, "\n value: ", round(x = data.cd$fraction * 100, digits = 2), " %")

ggplot(data.cd, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = decision)) +
  theme_bw(base_size = 5) +
  geom_rect() +
  geom_label(x = 3.5, aes(y = labelPosition, label = label), size = 4) +
  scale_fill_see() +
  coord_polar(theta = "y") +
  xlim(c(-0.3, 4)) +
  theme_void() +
  theme(legend.position.inside = "none", text = element_text(family = "sans")) +
  ggsave("plots/crossing_decision_percentage.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)



########### PerceivedSafety####
checkAssumptionsForAnovaThreeFactors(data = main_df, y = "ps_score", factor_1 = "traffic", factor_2 = "ehmi", factor_3 = "repExposure")



## model <- np.anova(formel = ps_score ~ traffic * ehmi * repExposure + Error(probandenID / (traffic * ehmi * repExposure)), data = main_df, method = 0, compact = T)
## model

## reportNPAVChi(model, dv = "perceived safety")


modelART <- art(ps_score ~ traffic * ehmi * repExposure + Error(probandenID / (traffic * ehmi * repExposure)), data = main_df) |> anova()
reportART(model = modelART, dv = "ps_score")



reportMeanAndSD(data = main_df, iv = "traffic", dv = "ps_score")
reportMeanAndSD(data = main_df, iv = "ehmi", dv = "ps_score")
reportMeanAndSD(data = main_df, iv = "repExposure", dv = "ps_score")

d <- dunnTest(ps_score ~ repExposure, data = main_df, method = "holm", two.sided = FALSE)
reportDunnTest(data = main_df, d = d, iv = "repExposure", dv = "ps_score")


main_df %>% ggplot() +
  aes(x = repExposure, y = ps_score, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Perceived Safety") +
  theme(legend.position.inside = c(0.75, 0.84)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05), alpha = 0.5)
ggsave("plots/perceived_safety_repetition.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)

main_df %>% ggplot() +
  aes(x = repExposure, y = ps_score, fill = traffic, colour = traffic, group = traffic) +
  scale_color_see() +
  ylab("Perceived Safety") +
  theme(legend.position.inside = c(0.25, 0.84)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05), alpha = 0.5)


main_df %>% ggplot() +
  aes(x = ehmi, y = ps_score, fill = traffic, colour = traffic, group = traffic) +
  scale_color_see() +
  ylab("Perceived Safety") +
  theme(legend.position.inside = c(0.75, 0.84)) +
  xlab("eHMI") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05), alpha = 0.5)
ggsave("plots/perceived_safety_eHMI.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)

main_df %>% ggplot() +
  aes(x = traffic, y = ps_score, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Perceived Safety") +
  theme(legend.position.inside = c(0.75, 0.84)) +
  xlab("traffic") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05), alpha = 0.5)
