# setwd("/Users/daniel/Documents/Daten_DK/masterthesis/results/")

library("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))


library(devtools)
source_url("https://raw.githubusercontent.com/M-Colley/rCode/main/r_functionality.R")
# install.packages('BayesFactor')
library(BayesFactor)

#### VR-IMPORT GERMAN SUBJECTS #####
main_df_ger_vr <- read_xlsx(path = "data-main.xlsx", sheet = "Results")
main_df_ger_vr <- as.data.frame(main_df_ger_vr)
names(main_df_ger_vr)



# participant 4 got sick
main_df_ger_vr <- subset(main_df_ger_vr, probandenID != 4)


# Make sure the subject column is a factor
main_df_ger_vr$probandenID <- as.factor(main_df_ger_vr$probandenID)
main_df_ger_vr$scenarioID <- as.factor(main_df_ger_vr$scenarioID)
main_df_ger_vr$repExposure <- as.factor(main_df_ger_vr$repExposure)

print(main_df_ger_vr$probandenID)


main_df_ger_vr$ps_score <- rowSums(main_df_ger_vr[, c("perceivedSafety1", "perceivedSafety2", "perceivedSafety3", "perceivedSafety4")]) / 4.0


# UEQ-S
main_df_ger_vr$pragmatic_quality <- rowSums(main_df_ger_vr[, c("ueqs1", "ueqs2", "ueqs3", "ueqs4")]) / 4.0
main_df_ger_vr$hedonic_quality <- rowSums(main_df_ger_vr[, c("ueqs5", "ueqs6", "ueqs7", "ueqs8")]) / 4.0

# Trust
main_df_ger_vr$TiA_Understanding2 <- 6 - main_df_ger_vr$TiA_Understanding2
main_df_ger_vr$TiA_Understanding4 <- 6 - main_df_ger_vr$TiA_Understanding4

main_df_ger_vr$overallTiAUnderstanding <- rowSums(main_df_ger_vr[, c("TiA_Understanding1", "TiA_Understanding2", "TiA_Understanding3", "TiA_Understanding4")]) / 4.0
main_df_ger_vr$overallTiATrust <- rowSums(main_df_ger_vr[, c("TiA_Trust1", "TiA_Trust2")]) / 2.0

### Crossing Decision ###
## main_df_ger_vr$crossing_decision <- rowSums(main_df_ger_vr[, c("crossingDecisionSpeed", "crossingDecisionDist", "crossingDecisionComm", "crossingDecisionOwt")]) / 4.0





# get relevant factors in new column
# Conditions should be correct already!
main_df_ger_vr$traffic <- with(main_df_ger_vr, ifelse(main_df_ger_vr$scenarioID %in% c(2, 4), "automated", "mixed"))
main_df_ger_vr$ehmi <- with(main_df_ger_vr, ifelse(main_df_ger_vr$scenarioID %in% c(1, 2), "with eHMI", "without eHMI"))

main_df_ger_vr$traffic <- as.factor(main_df_ger_vr$traffic)
main_df_ger_vr$ehmi <- as.factor(main_df_ger_vr$ehmi)

levels(main_df_ger_vr$culturalBackground)
levels(main_df_ger_vr$repExposure)
levels(main_df_ger_vr$ehmi)
## Remove ###
filtered_main_df_ger_vr <- main_df_ger_vr[, !names(main_df_ger_vr) %in% c(
  "id", "submitdate", "seed", "startdate", "datestamp", "lastpage", "startlanguage",
  "userID", "interviewtime", "groupTime7525", "probandenIDTime", "repExposureTime",
  "repExposureTime", "scenarioIDTime", "groupTime7519", "userIDTime", "groupTime7520", "perceivedSafetyTime", "TLX1Time",
  "crossingDecisionTime", "TrustiAUnderPTime", "presenceTime", "groupTime7521", "ueqsGermanTime",
  "comEvalTime"
)]
filtered_main_df_ger_vr$study <- as.factor("VR")
filtered_main_df_ger_vr$culturalBackground <- as.factor("Germany")

filtered_main_df_ger_vr <- subset(filtered_main_df_ger_vr, scenarioID == 2 | scenarioID == 4)
#### WEB-IMPORT GERMAN SUBJECTS ####
main_df_ger_web <- read_xlsx(path = "QuestionnaireResult_Germany.xlsx", sheet = "QuestionnaireResult_Germany")
main_df_ger_web <- as.data.frame(main_df_ger_web)
names(main_df_ger_web)

# participant id = 13: USA - Germany - USA
main_df_ger_web <- subset(x = main_df_ger_web, subject_id != "0b8ac4da-03de-4574-a4bc-c7227f4886ab")

colnames(main_df_ger_web) <- c(
  "id", "TLX1", "TiA_Understanding1", "TiA_Understanding2", "TiA_Trust1", "TiA_Understanding3", "TiA_Trust2", "TiA_Understanding4",
  "Attention_Check1", "Attention_Check2", "ueqs8", "comEval2", "comEval3", "comEval4", "comEval5", "comEval6",
  "crossingDecisionComm", "crossingDecisionDist", "crossingDecisionOwt", "crossingDecisionSpeed",
  "perceivedSafety1", "perceivedSafety2", "perceivedSafety3", "perceivedSafety4", "reallyExperiencing",
  "repExposure", "scenarioID", "probandenID", "timeStamp", "typicalBehavior", "ueqs1", "ueqs2", "ueqs3", "ueqs4", "ueqs5",
  "ueqs6", "ueqs7", "presence"
)


# Make sure the subject column is a factor
main_df_ger_web$probandenID <- as.factor(main_df_ger_web$probandenID)
main_df_ger_web$scenarioID <- as.factor(main_df_ger_web$scenarioID)
main_df_ger_web$repExposure <- as.factor(main_df_ger_web$repExposure)

main_df_ger_web$ps_score <- rowSums(main_df_ger_web[, c("perceivedSafety1", "perceivedSafety2", "perceivedSafety3", "perceivedSafety4")]) / 4.0


# UEQ-S
main_df_ger_web$pragmatic_quality <- rowSums(main_df_ger_web[, c("ueqs1", "ueqs2", "ueqs3", "ueqs4")]) / 4.0
main_df_ger_web$hedonic_quality <- rowSums(main_df_ger_web[, c("ueqs5", "ueqs6", "ueqs7", "ueqs8")]) / 4.0

# Trust
main_df_ger_web$TiA_Understanding2 <- 6 - main_df_ger_web$TiA_Understanding2
main_df_ger_web$TiA_Understanding4 <- 6 - main_df_ger_web$TiA_Understanding4

main_df_ger_web$overallTiAUnderstanding <- rowSums(main_df_ger_web[, c("TiA_Understanding1", "TiA_Understanding2", "TiA_Understanding3", "TiA_Understanding4")]) / 4.0
main_df_ger_web$overallTiATrust <- rowSums(main_df_ger_web[, c("TiA_Trust1", "TiA_Trust2")]) / 2.0

# get relevant factors in new column
# Conditions should be correct already!
main_df_ger_web$traffic <- with(main_df_ger_web, ifelse(main_df_ger_web$scenarioID %in% c(2, 4), "automated", "mixed"))
main_df_ger_web$ehmi <- with(main_df_ger_web, ifelse(main_df_ger_web$scenarioID %in% c(1, 2), "with eHMI", "without eHMI"))

main_df_ger_web$traffic <- as.factor(main_df_ger_web$traffic)
main_df_ger_web$ehmi <- as.factor(main_df_ger_web$ehmi)

main_df_ger_web$study <- as.factor("WEB")
main_df_ger_web$culturalBackground <- as.factor("Germany")

levels(main_df_ger_web$culturalBackground)
levels(main_df_ger_web$repExposure)
levels(main_df_ger_web$ehmi)

## Remove ###
filtered_main_df_ger_web <- main_df_ger_web[, !names(main_df_ger_web) %in% c(
  "id", "Attention_Check1",
  "Attention_Check2", "typicalBehavior",
  "timeStamp", "reallyExperiencing"
)]


#### WEB-IMPORT USA SUBJECTS ####
main_df_usa_web <- read_xlsx(path = "QuestionnaireResult_USA.xlsx", sheet = "QuestionnaireResult_USA")
main_df_usa_web <- as.data.frame(main_df_usa_web)
names(main_df_usa_web)


colnames(main_df_usa_web) <- c(
  "id", "TLX1", "TiA_Understanding1", "TiA_Understanding2", "TiA_Trust1", "TiA_Understanding3", "TiA_Trust2", "TiA_Understanding4",
  "Attention_Check1", "Attention_Check2", "ueqs8", "comEval2", "comEval3", "comEval4", "comEval5", "comEval6",
  "crossingDecisionComm", "crossingDecisionDist", "crossingDecisionOwt", "crossingDecisionSpeed",
  "perceivedSafety1", "perceivedSafety2", "perceivedSafety3", "perceivedSafety4", "reallyExperiencing",
  "repExposure", "scenarioID", "probandenID", "timeStamp", "typicalBehavior", "ueqs1", "ueqs2", "ueqs3", "ueqs4", "ueqs5",
  "ueqs6", "ueqs7", "presence"
)


# Make sure the subject column is a factor
main_df_usa_web$probandenID <- as.factor(main_df_usa_web$probandenID)
main_df_usa_web$scenarioID <- as.factor(main_df_usa_web$scenarioID)
main_df_usa_web$repExposure <- as.factor(main_df_usa_web$repExposure)

main_df_usa_web$ps_score <- rowSums(main_df_usa_web[, c("perceivedSafety1", "perceivedSafety2", "perceivedSafety3", "perceivedSafety4")]) / 4.0


# UEQ-S
main_df_usa_web$pragmatic_quality <- rowSums(main_df_usa_web[, c("ueqs1", "ueqs2", "ueqs3", "ueqs4")]) / 4.0
main_df_usa_web$hedonic_quality <- rowSums(main_df_usa_web[, c("ueqs5", "ueqs6", "ueqs7", "ueqs8")]) / 4.0

# Trust
main_df_usa_web$TiA_Understanding2 <- 6 - main_df_usa_web$TiA_Understanding2
main_df_usa_web$TiA_Understanding4 <- 6 - main_df_usa_web$TiA_Understanding4

main_df_usa_web$overallTiAUnderstanding <- rowSums(main_df_usa_web[, c("TiA_Understanding1", "TiA_Understanding2", "TiA_Understanding3", "TiA_Understanding4")]) / 4.0
main_df_usa_web$overallTiATrust <- rowSums(main_df_usa_web[, c("TiA_Trust1", "TiA_Trust2")]) / 2.0

# get relevant factors in new column
# Conditions should be correct already!
main_df_usa_web$traffic <- with(main_df_usa_web, ifelse(main_df_usa_web$scenarioID %in% c(2, 4), "automated", "mixed"))
main_df_usa_web$ehmi <- with(main_df_usa_web, ifelse(main_df_usa_web$scenarioID %in% c(1, 2), "with eHMI", "without eHMI"))

main_df_usa_web$traffic <- as.factor(main_df_usa_web$traffic)
main_df_usa_web$ehmi <- as.factor(main_df_usa_web$ehmi)

main_df_usa_web$study <- as.factor("WEB")
main_df_usa_web$culturalBackground <- as.factor("USA")

levels(main_df_usa_web$culturalBackground)
levels(main_df_usa_web$repExposure)
levels(main_df_usa_web$ehmi)

#### Merging ####
main_df_german <- rbind(filtered_main_df_ger_web, filtered_main_df_ger_vr)
main_df_web <- rbind(main_df_ger_web, main_df_usa_web)
main_df_german$repExposure <- gsub("ONE", 1, main_df_german$repExposure)
main_df_german$repExposure <- gsub("TWO", 2, main_df_german$repExposure)
main_df_german$repExposure <- gsub("THREE", 3, main_df_german$repExposure)
main_df_german$repExposure <- as.factor(main_df_german$repExposure)

#### Germany: VR-Study & Online-Based ####
##### Mental Workload #####
anovaBF(TLX1 ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "TLX1")

anovaBF(TLX1 ~ ehmi * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()

bf <- anovaBF(TLX1 ~ ehmi * study, whichRandom = "probandenID", data = main_df_german) |> bayesfactor_models()
print(bf)
anovaBF(TLX1 ~ repExposure * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()

##### overallTiAUnderstanding #####
anovaBF(overallTiAUnderstanding ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "overallTiAUnderstanding")

anovaBF(overallTiAUnderstanding ~ ehmi * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "ehmi", dv = "overallTiAUnderstanding")

anovaBF(overallTiAUnderstanding ~ repExposure * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
d <- dunnTest(overallTiAUnderstanding ~ repExposure, data = main_df_german, method = "holm", two.sided = FALSE)
reportDunnTest(data = main_df_german, d = d, iv = "repExposure", dv = "overallTiAUnderstanding")

##### overallTiATrust#####
anovaBF(overallTiATrust ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "overallTiATrust")

anovaBF(overallTiATrust ~ ehmi * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "ehmi", dv = "overallTiATrust")

anovaBF(overallTiATrust ~ repExposure * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
d <- dunnTest(overallTiATrust ~ repExposure, data = main_df_german, method = "holm", two.sided = FALSE)
reportDunnTest(data = main_df_german, d = d, iv = "repExposure", dv = "overallTiATrust")



##### ps_score #####
anovaBF(ps_score ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "ps_score")

anovaBF(ps_score ~ ehmi * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "ehmi", dv = "ps_score")

anovaBF(ps_score ~ repExposure * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
d <- dunnTest(ps_score ~ repExposure, data = main_df_german, method = "holm", two.sided = FALSE)
reportDunnTest(data = main_df_german, d = d, iv = "repExposure", dv = "ps_score")

##### Crossing Decision: Speed#####
anovaBF(crossingDecisionSpeed ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "crossingDecisionSpeed")

bf <- anovaBF(crossingDecisionSpeed ~ study, whichRandom = "probandenID", data = main_df_german) |> bayesfactor_models()
print(bf)
##### Crossing Decision: Distance#####
anovaBF(crossingDecisionDist ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "crossingDecisionDist")
bf <- anovaBF(crossingDecisionDist ~ study, whichRandom = "probandenID", data = main_df_german) |> bayesfactor_models()
print(bf)
##### Crossing Decision: Communication#####
anovaBF(crossingDecisionComm ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "crossingDecisionComm")

##### Crossing Decision: Own waiting Time#####
anovaBF(crossingDecisionOwt ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "crossingDecisionOwt")

#### Presence#####
anovaBF(presence ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "presence")

##### pragmatic_quality #####
anovaBF(pragmatic_quality ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "pragmatic_quality")

##### hedonic_quality#####
anovaBF(hedonic_quality ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "hedonic_quality")

##### comEval2#####
anovaBF(comEval2 ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "comEval2")

##### comEval3#####
anovaBF(comEval2 ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "comEval3")

##### comEval4#####
anovaBF(comEval2 ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "comEval4")

##### comEval5 Naturalness#####
anovaBF(comEval2 ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "comEval5")

##### comEval6#####
anovaBF(comEval2 ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "comEval6")


#### Germany Online-Based & USA Online-Based ####
main_df_web <- main_df_web[, !names(main_df_web) %in% c("Attention_Check1", "Attention_Check2", "timeStamp")]
main_df_web$repExposure <- gsub("ONE", "1", main_df_web$repExposure)
main_df_web$repExposure <- gsub("TWO", "2", main_df_web$repExposure)
main_df_web$repExposure <- gsub("THREE", "3", main_df_web$repExposure)
main_df_web$repExposure <- as.factor(main_df_web$repExposure)


##### Mental Workload #####
checkAssumptionsForAnovaThreeFactors(data = main_df_web, y = "TLX1", factor_1 = "culturalBackground", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(TLX1 ~ culturalBackground * ehmi * repExposure + Error(probandenID / (ehmi * repExposure)), data = main_df_web) |> anova()
reportART(model = modelART, dv = "mental workload")
d <- dunnTest(TLX1 ~ repExposure, data = main_df_web, method = "holm", two.sided = FALSE)
reportDunnTest(data = main_df_web, d = d, iv = "repExposure", dv = "TLX1")


main_df_web %>% ggplot() +
  aes(x = culturalBackground, y = TLX1, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_colour_see() +
  ylab("Mental Workload") +
  theme(legend.title = element_blank(), legend.background = element_blank(), legend.position = c(0.75, 0.875), legend.text = element_text(size = myfontsize - 10)) +
  xlab("Cultural Background") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))

p <- main_df_web %>% ggplot() +
  aes(x = repExposure, y = TLX1, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Mental Workload") +
  theme(legend.title = element_blank(), legend.background = element_blank(), legend.position = c(0.8, 0.85), legend.text = element_text(size = myfontsize - 10)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))
p + facet_grid(~culturalBackground)
ggsave("plots/result_web_ger_us_tlx1.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)



##### overallTiAUnderstanding#####
checkAssumptionsForAnovaThreeFactors(data = main_df_web, y = "overallTiAUnderstanding", factor_1 = "culturalBackground", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(overallTiAUnderstanding ~ culturalBackground * ehmi * repExposure + Error(probandenID / (ehmi * repExposure)), data = main_df_web) |> anova()
reportART(model = modelART, dv = "overallTiAUnderstanding")
reportMeanAndSD(data = main_df_web, iv = "ehmi", dv = "overallTiAUnderstanding")

##### overallTiATrust#####
checkAssumptionsForAnovaThreeFactors(data = main_df_web, y = "overallTiATrust", factor_1 = "culturalBackground", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(overallTiATrust ~ culturalBackground * ehmi * repExposure + Error(probandenID / (ehmi * repExposure)), data = main_df_web) |> anova()
reportART(model = modelART, dv = "overallTiATrust")
reportMeanAndSD(data = main_df_web, iv = "ehmi", dv = "overallTiATrust")

##### PerceivedSafety #####
checkAssumptionsForAnovaThreeFactors(data = main_df_web, y = "ps_score", factor_1 = "culturalBackground", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(ps_score ~ culturalBackground * ehmi * repExposure + Error(probandenID / (ehmi * repExposure)), data = main_df_web) |> anova()
reportART(model = modelART, dv = "ps_score")


##### Crossing Decision: General #####
## FOR Germany: main_df_ger_web || FOR USA: main_df_usa_web
sum.cd.speed <- sum(main_df_usa_web$crossingDecisionSpeed)
sum.cd.communication <- sum(main_df_usa_web$crossingDecisionComm)
sum.cd.owt <- sum(main_df_usa_web$crossingDecisionOwt)
sum.cd.distance <- sum(main_df_usa_web$crossingDecisionDist)
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
  theme_bw(base_size = myfontsize - 10) +
  theme(
    legend.position = "", axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  ggtitle("Crossing Decision USA") +
  geom_rect() +
  geom_label(x = 3.5, aes(y = labelPosition, label = label), size = 4) +
  scale_fill_see() +
  # scale_fill_brewer(palette=4) +
  coord_polar(theta = "y") +
  xlim(c(-0.3, 4)) +
  theme(legend.position = "none", text = element_text(family = "sans"))

##### Crossing Decision: Speed#####
checkAssumptionsForAnovaThreeFactors(data = main_df_web, y = "crossingDecisionSpeed", factor_1 = "culturalBackground", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(crossingDecisionSpeed ~ culturalBackground * ehmi * repExposure + Error(probandenID / (ehmi * repExposure)), data = main_df_web) |> anova()
reportART(model = modelART, dv = "crossingDecisionSpeed")

##### Crossing Decision: Distance#####
checkAssumptionsForAnovaThreeFactors(data = main_df_web, y = "crossingDecisionDist", factor_1 = "culturalBackground", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(crossingDecisionDist ~ culturalBackground * ehmi * repExposure + Error(probandenID / (ehmi * repExposure)), data = main_df_web) |> anova()
reportART(model = modelART, dv = "crossingDecisionDist")

##### Crossing Decision: Communication#####
checkAssumptionsForAnovaThreeFactors(data = main_df_web, y = "crossingDecisionComm", factor_1 = "culturalBackground", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(crossingDecisionComm ~ culturalBackground * ehmi * repExposure + Error(probandenID / (ehmi * repExposure)), data = main_df_web) |> anova()
reportART(model = modelART, dv = "crossingDecisionComm")
reportMeanAndSD(data = main_df_web, iv = "ehmi", dv = "crossingDecisionComm")

##### Crossing Decision: Own waiting Time#####
checkAssumptionsForAnovaThreeFactors(data = main_df_web, y = "crossingDecisionOwt", factor_1 = "culturalBackground", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(crossingDecisionOwt ~ culturalBackground * ehmi * repExposure + Error(probandenID / (ehmi * repExposure)), data = main_df_web) |> anova()
reportART(model = modelART, dv = "crossingDecisionOwt")

main_df_web %>% ggplot() +
  aes(x = repExposure, y = crossingDecisionOwt, fill = culturalBackground, colour = culturalBackground, group = culturalBackground) +
  scale_colour_see() +
  ylab("Crossing Decision\nOwn Waiting Time") +
  theme(legend.title = element_blank(), legend.background = element_blank(), legend.position = c(0.45, 0.925), legend.text = element_text(size = myfontsize - 10)) +
  xlab("Repeated Expsoure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))
ggsave("plots/result_web_cd_owt_ger_usa.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)




p <- main_df_web %>% ggplot() +
  aes(x = repExposure, y = crossingDecisionOwt, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Crossing Decision\nOwn Waiting Time") +
  theme(legend.title = element_blank(), legend.background = element_blank(), legend.position = c(0.8, 0.1), legend.text = element_text(size = myfontsize - 10)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))
p + facet_grid(~culturalBackground)
ggsave("plots/result_web_cd_owt_ger_usa2.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)



##### Presence#####
checkAssumptionsForAnovaThreeFactors(data = main_df_web, y = "presence", factor_1 = "culturalBackground", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(presence ~ culturalBackground * ehmi * repExposure + Error(probandenID / (ehmi * repExposure)), data = main_df_web) |> anova()
reportART(model = modelART, dv = "presence")

##### pragmatic_quality #####
checkAssumptionsForAnovaThreeFactors(data = main_df_web, y = "pragmatic_quality", factor_1 = "culturalBackground", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(pragmatic_quality ~ culturalBackground * ehmi * repExposure + Error(probandenID / (ehmi * repExposure)), data = main_df_web) |> anova()
reportART(model = modelART, dv = "pragmatic quality")
reportMeanAndSD(data = main_df_web, iv = "ehmi", dv = "pragmatic_quality")

##### hedonic_quality#####
checkAssumptionsForAnovaThreeFactors(data = main_df_web, y = "hedonic_quality", factor_1 = "culturalBackground", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(hedonic_quality ~ culturalBackground * ehmi * repExposure + Error(probandenID / (ehmi * repExposure)), data = main_df_web) |> anova()
reportART(model = modelART, dv = "hedonic quality")

##### ComEval #####
###### comEval2 Polite | Impolite ######
checkAssumptionsForAnovaThreeFactors(data = main_df_web, y = "comEval2", factor_1 = "culturalBackground", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(comEval2 ~ culturalBackground * ehmi * repExposure + Error(probandenID / (ehmi * repExposure)), data = main_df_web) |> anova()
reportART(model = modelART, dv = "comEval2")

###### comEval3 Machine-like ######
checkAssumptionsForAnovaThreeFactors(data = main_df_web, y = "comEval3", factor_1 = "culturalBackground", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(comEval3 ~ culturalBackground * ehmi * repExposure + Error(probandenID / (ehmi * repExposure)), data = main_df_web) |> anova()
reportART(model = modelART, dv = "comEval3")

###### comEval4 Not clear######
checkAssumptionsForAnovaThreeFactors(data = main_df_web, y = "comEval4", factor_1 = "culturalBackground", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(comEval4 ~ culturalBackground * ehmi * repExposure + Error(probandenID / (ehmi * repExposure)), data = main_df_web) |> anova()
reportART(model = modelART, dv = "comEval4")
reportMeanAndSD(data = main_df_web, iv = "ehmi", dv = "comEval4")

###### comEval5 Naturalness######
checkAssumptionsForAnovaThreeFactors(data = main_df_web, y = "comEval5", factor_1 = "culturalBackground", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(comEval5 ~ culturalBackground * ehmi * repExposure + Error(probandenID / (ehmi * repExposure)), data = main_df_web) |> anova()
reportART(model = modelART, dv = "comEval5")

###### comEval6######
checkAssumptionsForAnovaThreeFactors(data = main_df_web, y = "comEval6", factor_1 = "culturalBackground", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(comEval5 ~ culturalBackground * ehmi * repExposure + Error(probandenID / (ehmi * repExposure)), data = main_df_web) |> anova()
reportART(model = modelART, dv = "comEval6")

##### comEval6 Appropriateness #####
checkAssumptionsForAnovaThreeFactors(data = main_df_web, y = "comEval6", factor_1 = "culturalBackground", factor_2 = "ehmi", factor_3 = "repExposure")
modelART <- art(comEval6 ~ culturalBackground * ehmi * repExposure + Error(probandenID / (ehmi * repExposure)), data = main_df_web) |> anova()
reportART(model = modelART, dv = "comEval6")
reportMeanAndSD(data = main_df_web, iv = "ehmi", dv = "comEval6")
