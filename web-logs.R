library("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))


library(devtools)
source_url("https://raw.githubusercontent.com/M-Colley/rCode/main/r_functionality.R")


library(ggforce)

getwd()

setwd("./Logs/")
filesTotal <- list.files(path = ".", pattern = ".*TOTAL.csv", recursive = FALSE, )
main_Total_vr <- do.call(rbind, lapply(filesTotal, function(x) read.csv(x, stringsAsFactors = FALSE, sep = ";")))
main_Total_vr <- subset(main_Total_vr, subject_id != "4")

main_Total_vr$eHMI <- gsub("OFF", "without eHMI", main_Total_vr$eHMI)
main_Total_vr$eHMI <- gsub("ON", "with eHMI", main_Total_vr$eHMI)

main_Total_vr$traffic <- gsub("MIXED", "mixed", main_Total_vr$traffic)
main_Total_vr$traffic <- gsub("ONLY_AV", "automated", main_Total_vr$traffic)

main_Total_vr$repeated_exposure <- gsub("ONE", "1", main_Total_vr$repeated_exposure)
main_Total_vr$repeated_exposure <- gsub("TWO", "2", main_Total_vr$repeated_exposure)
main_Total_vr$repeated_exposure <- gsub("THREE", "3", main_Total_vr$repeated_exposure)

main_Total_vr$scenario_id <- gsub("SCENARIO_1", "1", main_Total_vr$scenario_id)
main_Total_vr$scenario_id <- gsub("SCENARIO_2", "2", main_Total_vr$scenario_id)
main_Total_vr$scenario_id <- gsub("SCENARIO_3", "3", main_Total_vr$scenario_id)
main_Total_vr$scenario_id <- gsub("SCENARIO_4", "4", main_Total_vr$scenario_id)

main_Total_vr$scene_variation <- as.factor("WEST")

main_Total_vr$subject_id <- as.factor(main_Total_vr$subject_id)
main_Total_vr$eHMI <- as.factor(main_Total_vr$eHMI)
main_Total_vr$traffic <- as.factor(main_Total_vr$traffic)
main_Total_vr$repeated_exposure <- as.factor(main_Total_vr$repeated_exposure)

main_Total_vr$study <- as.factor("VR")
main_Total_vr$culturalBackground <- as.factor("Germany")
colnames(main_Total_vr)[3] <- "ehmi"
colnames(main_Total_vr)[5] <- "rep_exposure"
colnames(main_Total_vr)[7] <- "t"
main_Total_vr$t <- strptime(as.character(main_Total_vr$t), "%d.%m.%Y %H:%M:%S")
main_Total_vr$t <- format(main_Total_vr$t, "%Y-%m-%d %H:%M:%S")
main_Total_vr$checkpoint_reached <- strptime(as.character(main_Total_vr$checkpoint_reached), "%d.%m.%Y %H:%M:%S")
main_Total_vr$checkpoint_reached <- format(main_Total_vr$checkpoint_reached, "%Y-%m-%d %H:%M:%S")

main_Total_vr$total_duration_calc <- main_Total_vr$duration_sidewalk_start + main_Total_vr$duration_first_lane + main_Total_vr$duration_second_lane + main_Total_vr$duration_sidewalk_park

## Remove ###
main_Total_vr <- main_Total_vr[, !names(main_Total_vr) %in% c("X.", "traffice")]
names(main_Total_vr)


setwd("../")

#### EYE-TRACKING ####
##### GERMANY #####
df_eye_gaze_ger <- read_xlsx(path = "LoggingEyeTracking_Germany.xlsx", sheet = "LoggingEyeTracking_Germany")
df_eye_gaze_ger <- as.data.frame(df_eye_gaze_ger)
# participant id = 13: USA - Germany - USA
df_eye_gaze_ger <- subset(x = df_eye_gaze_ger, subject_id != "0b8ac4da-03de-4574-a4bc-c7227f4886ab")
df_eye_gaze_ger$ehmi <- gsub("OFF", "without eHMI", df_eye_gaze_ger$ehmi)
df_eye_gaze_ger$ehmi <- gsub("ON", "with eHMI", df_eye_gaze_ger$ehmi)

df_eye_gaze_ger$traffic <- gsub("MIXED", "mixed", df_eye_gaze_ger$traffic)
df_eye_gaze_ger$traffic <- gsub("ONLY_AV", "automated", df_eye_gaze_ger$traffic)

df_eye_gaze_ger$rep_exposure <- gsub("ONE", "1", df_eye_gaze_ger$rep_exposure)
df_eye_gaze_ger$rep_exposure <- gsub("TWO", "2", df_eye_gaze_ger$rep_exposure)
df_eye_gaze_ger$rep_exposure <- gsub("THREE", "3", df_eye_gaze_ger$rep_exposure)

colnames(df_eye_gaze_ger)[4] <- "scenario_id"

df_eye_gaze_ger$subject_id <- as.factor(df_eye_gaze_ger$subject_id)
df_eye_gaze_ger$ehmi <- as.factor(df_eye_gaze_ger$ehmi)
df_eye_gaze_ger$traffic <- as.factor(df_eye_gaze_ger$traffic)
df_eye_gaze_ger$rep_exposure <- as.factor(df_eye_gaze_ger$rep_exposure)
df_eye_gaze_ger$area_of_interest <- as.factor(df_eye_gaze_ger$area_of_interest)

##### USA #####
df_eye_gaze_usa <- read_xlsx(path = "LoggingEyeTracking_USA.xlsx", sheet = "LoggingEyeTracking_USA")
df_eye_gaze_usa <- as.data.frame(df_eye_gaze_usa)
df_eye_gaze_usa$ehmi <- gsub("OFF", "without eHMI", df_eye_gaze_usa$ehmi)
df_eye_gaze_usa$ehmi <- gsub("ON", "with eHMI", df_eye_gaze_usa$ehmi)

df_eye_gaze_usa$traffic <- gsub("MIXED", "mixed", df_eye_gaze_usa$traffic)
df_eye_gaze_usa$traffic <- gsub("ONLY_AV", "automated", df_eye_gaze_usa$traffic)

df_eye_gaze_usa$rep_exposure <- gsub("ONE", "1", df_eye_gaze_usa$rep_exposure)
df_eye_gaze_usa$rep_exposure <- gsub("TWO", "2", df_eye_gaze_usa$rep_exposure)
df_eye_gaze_usa$rep_exposure <- gsub("THREE", "3", df_eye_gaze_usa$rep_exposure)
colnames(df_eye_gaze_usa)[4] <- "scenario_id"

df_eye_gaze_usa$subject_id <- as.factor(df_eye_gaze_usa$subject_id)
df_eye_gaze_usa$ehmi <- as.factor(df_eye_gaze_usa$ehmi)
df_eye_gaze_usa$traffic <- as.factor(df_eye_gaze_usa$traffic)
df_eye_gaze_usa$rep_exposure <- as.factor(df_eye_gaze_usa$rep_exposure)
df_eye_gaze_usa$area_of_interest <- as.factor(df_eye_gaze_usa$area_of_interest)

#### POSITION ####
##### GERMANY #####
df_position_germany <- read_xlsx(path = "LoggingPosition_German.xlsx", sheet = "LoggingPosition_German")
df_position_germany <- as.data.frame(df_position_germany)
# participant id = 13: USA - Germany - USA
df_position_germany <- subset(x = df_position_germany, subject_id != "0b8ac4da-03de-4574-a4bc-c7227f4886ab")
df_position_germany$subject_id <- as.factor(df_position_germany$subject_id)
df_position_germany$ehmi <- as.factor(df_position_germany$ehmi)
df_position_germany$traffic <- as.factor(df_position_germany$traffic)
df_position_germany$rep_exposure <- as.factor(df_position_germany$rep_exposure)

##### USA #####
df_position_usa <- read_xlsx(path = "LoggingPosition_USA.xlsx", sheet = "LoggingPosition_USA")
df_position_usa <- as.data.frame(df_position_usa)
df_position_usa$subject_id <- as.factor(df_position_usa$subject_id)
df_position_usa$ehmi <- as.factor(df_position_usa$ehmi)
df_position_usa$traffic <- as.factor(df_position_usa$traffic)
df_position_usa$rep_exposure <- as.factor(df_position_usa$rep_exposure)

#### TOTAL ####
##### GERMANY #####
df_total_germany <- read_xlsx(path = "LoggingTotal_Germany.xlsx", sheet = "LoggingTotal_Germany")
df_total_germany <- as.data.frame(df_total_germany)
# participant id = 13: USA - Germany - USA
df_total_germany <- subset(x = df_total_germany, subject_id != "0b8ac4da-03de-4574-a4bc-c7227f4886ab")
colnames(df_total_germany)[4] <- "scenario_id"

df_total_germany$rep_exposure <- gsub("ONE", "1", df_total_germany$rep_exposure)
df_total_germany$rep_exposure <- gsub("TWO", "2", df_total_germany$rep_exposure)
df_total_germany$rep_exposure <- gsub("THREE", "3", df_total_germany$rep_exposure)

df_total_germany$scenario_id <- gsub("SCENARIO_2", "2", df_total_germany$scenario_id)
df_total_germany$scenario_id <- gsub("SCENARIO_4", "4", df_total_germany$scenario_id)

# Make sure the subject column is a factor
df_total_germany$subject_id <- as.factor(df_total_germany$subject_id)
df_total_germany$scenario_id <- as.factor(df_total_germany$scenario_id)
df_total_germany$rep_exposure <- as.factor(df_total_germany$rep_exposure)

df_total_germany$traffic <- with(df_total_germany, ifelse(df_total_germany$scenario_id %in% c(2, 4), "automated", "mixed"))
df_total_germany$ehmi <- with(df_total_germany, ifelse(df_total_germany$scenario_id %in% c(1, 2), "with eHMI", "without eHMI"))
df_total_germany$traffic <- as.factor(df_total_germany$traffic)
df_total_germany$ehmi <- as.factor(df_total_germany$ehmi)
df_total_germany$study <- as.factor("WEB")
df_total_germany$culturalBackground <- as.factor("Germany")
df_total_germany$total_duration_calc <- df_total_germany$duration_sidewalk_start + df_total_germany$duration_first_lane + df_total_germany$duration_second_lane + df_total_germany$duration_sidewalk_park

names(df_total_germany)

##### USA #####
df_total_usa <- read_xlsx(path = "LoggingTotal_USA.xlsx", sheet = "LoggingTotal_USA")
df_total_usa <- as.data.frame(df_total_usa)
colnames(df_total_usa)[4] <- "scenario_id"
# Make sure the subject column is a factor

df_total_usa$rep_exposure <- gsub("ONE", "1", df_total_usa$rep_exposure)
df_total_usa$rep_exposure <- gsub("TWO", "2", df_total_usa$rep_exposure)
df_total_usa$rep_exposure <- gsub("THREE", "3", df_total_usa$rep_exposure)

df_total_usa$scenario_id <- gsub("SCENARIO_2", "2", df_total_usa$scenario_id)
df_total_usa$scenario_id <- gsub("SCENARIO_4", "4", df_total_usa$scenario_id)

df_total_usa$subject_id <- as.factor(df_total_usa$subject_id)
df_total_usa$scenario_id <- as.factor(df_total_usa$scenario_id)
df_total_usa$rep_exposure <- as.factor(df_total_usa$rep_exposure)

df_total_usa$traffic <- with(df_total_usa, ifelse(df_total_usa$scenario_id %in% c(2, 4), "automated", "mixed"))
df_total_usa$ehmi <- with(df_total_usa, ifelse(df_total_usa$scenario_id %in% c(1, 2), "with eHMI", "without eHMI"))

df_total_usa$traffic <- as.factor(df_total_usa$traffic)
df_total_usa$ehmi <- as.factor(df_total_usa$ehmi)
df_total_usa$study <- as.factor("WEB")
df_total_usa$culturalBackground <- as.factor("USA")
df_total_usa$total_duration_calc <- df_total_usa$duration_sidewalk_start + df_total_usa$duration_first_lane + df_total_usa$duration_second_lane + df_total_usa$duration_sidewalk_park

names(df_total_usa)

### MERGING ####
main_Total_web <- rbind(df_total_usa, df_total_germany)
df_total_germany <- df_total_germany[, !names(df_total_germany) %in% c("id")]
main_Total_germany <- rbind(main_Total_vr, df_total_germany)

#### Crashes VR & WEB GERMANY ####
anovaBF(number_of_crashes ~ study, whichRandom = "probandenID", data = main_Total_germany) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(main_df = main_Total_germany, iv = "study", dv = "number_of_crashes")
anovaBF(number_of_crashes ~ ehmi * study, whichRandom = "probandenID", data = main_Total_germany) |>
  bayesfactor_models() |>
  report()
anovaBF(number_of_crashes ~ rep_exposure * study, whichRandom = "probandenID", data = main_Total_germany) |>
  bayesfactor_models() |>
  report()
main_Total_germany %>% ggplot() +
  aes(x = study, y = number_of_crashes, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_colour_see() +
  ylab("Number of Collisions") +
  theme(legend.position.inside = c(0.75, 0.9)) +
  xlab("Study Designs") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5, ) +
  scale_x_discrete(labels = c("WEB" = "Online-Based", "VR" = "VR")) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05)) # 95 % mean_cl_boot is 95% confidence intervals





temp <- main_Total_germany
temp$study <- gsub("WEB", "Online-Based", temp$study)
temp %>% ggplot() +
  aes(x = rep_exposure, y = number_of_crashes, fill = study, colour = study, group = study) +
  scale_colour_see() +
  ylab("Number of Collisions") +
  # scale_fill_discrete(labels=c("WEB" = "Online-Based", "VR" = "VR")) +
  theme(legend.position.inside = c(0.25, 0.9)) +
  xlab("Repeated Expsoure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5, ) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05)) # 95 % mean_cl_boot is 95% confidence intervals



#### duration_sidewalk_start VR & WEB GERMANY ####
anovaBF(duration_sidewalk_start ~ study, whichRandom = "probandenID", data = main_Total_germany) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(main_df = main_Total_germany, iv = "study", dv = "duration_sidewalk_start")
anovaBF(duration_sidewalk_start ~ ehmi * study, whichRandom = "probandenID", data = main_Total_germany) |>
  bayesfactor_models() |>
  report()
anovaBF(duration_sidewalk_start ~ rep_exposure * study, whichRandom = "probandenID", data = main_Total_germany) |>
  bayesfactor_models() |>
  report()
main_Total_germany %>% ggplot() +
  aes(x = study, y = duration_sidewalk_start, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_colour_see() +
  ylab("Duration Sidewalk Start") +
  theme(legend.position.inside = c(0.65, 0.9)) +
  xlab("Study Designs") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5, ) +
  scale_x_discrete(labels = c("WEB" = "Online-Based", "VR" = "VR")) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05)) # 95 % mean_cl_boot is 95% confidence intervals

temp <- main_Total_germany
temp$study <- gsub("WEB", "Online-Based", temp$study)
temp %>% ggplot() +
  aes(x = rep_exposure, y = duration_sidewalk_start, fill = study, colour = study, group = study) +
  scale_colour_see() +
  ylab("Duration Sidewalk Start") +
  # scale_fill_discrete(labels=c("WEB" = "Online-Based", "VR" = "VR")) +
  theme(legend.position.inside = c(0.45, 0.25)) +
  xlab("Repeated Expsoure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5, ) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05)) # 95 % mean_cl_boot is 95% confidence intervals




#### duration_first_lane VR & WEB GERMANY ####
anovaBF(duration_first_lane ~ study, whichRandom = "probandenID", data = main_Total_germany) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(main_df = main_Total_germany, iv = "study", dv = "duration_first_lane")
anovaBF(duration_first_lane ~ ehmi * study, whichRandom = "probandenID", data = main_Total_germany) |>
  bayesfactor_models() |>
  report()
anovaBF(duration_first_lane ~ rep_exposure * study, whichRandom = "probandenID", data = main_Total_germany) |>
  bayesfactor_models() |>
  report()
main_Total_germany %>% ggplot() +
  aes(x = study, y = duration_first_lane, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_colour_see() +
  ylab("Duration First Lane") +
  theme(legend.position.inside = c(0.25, 0.9)) +
  xlab("Study Designs") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5, ) +
  scale_x_discrete(labels = c("WEB" = "Online-Based", "VR" = "VR")) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05)) 

temp <- main_Total_germany
temp$study <- gsub("WEB", "Online-Based", temp$study)
temp %>% ggplot() +
  aes(x = rep_exposure, y = duration_first_lane, fill = study, colour = study, group = study) +
  scale_colour_see() +
  ylab("Duration First Lane") +
  # scale_fill_discrete(labels=c("WEB" = "Online-Based", "VR" = "VR")) +
  theme(legend.position.inside = c(0.45, 0.25)) +
  xlab("Repeated Expsoure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5, ) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05)) # 95 % mean_cl_boot is 95% confidence intervals


#### duration_second_lane VR & WEB GERMANY ####
anovaBF(duration_second_lane ~ study, whichRandom = "probandenID", data = main_Total_germany) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(main_df = main_Total_germany, iv = "study", dv = "duration_second_lane")
anovaBF(duration_second_lane ~ ehmi * study, whichRandom = "probandenID", data = main_Total_germany) |>
  bayesfactor_models() |>
  report()
anovaBF(duration_second_lane ~ rep_exposure * study, whichRandom = "probandenID", data = main_Total_germany) |>
  bayesfactor_models() |>
  report()
main_Total_germany %>% ggplot() +
  aes(x = study, y = duration_second_lane, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_colour_see() +
  ylab("Duration Second Lane") +
  theme(legend.position.inside = c(0.75, 0.9)) +
  xlab("Study Designs") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5, ) +
  scale_x_discrete(labels = c("WEB" = "Online-Based", "VR" = "VR")) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05)) # 95 % mean_cl_boot is 95% confidence intervals

temp <- main_Total_germany
temp$study <- gsub("WEB", "Online-Based", temp$study)
temp %>% ggplot() +
  aes(x = rep_exposure, y = duration_second_lane, fill = study, colour = study, group = study) +
  scale_colour_see() +
  ylab("Duration Second Lane") +
  # scale_fill_discrete(labels=c("WEB" = "Online-Based", "VR" = "VR")) +
  theme(legend.position.inside = c(0.25, 0.85)) +
  xlab("Repeated Expsoure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5, ) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05)) # 95 % mean_cl_boot is 95% confidence intervals

#### total_duration ####
anovaBF(total_duration_calc ~ study, whichRandom = "probandenID", data = main_Total_germany) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(main_df = main_Total_germany, iv = "study", dv = "total_duration_calc")
anovaBF(total_duration_calc ~ ehmi * study, whichRandom = "probandenID", data = main_Total_germany) |>
  bayesfactor_models() |>
  report()
anovaBF(total_duration_calc ~ rep_exposure * study, whichRandom = "probandenID", data = main_Total_germany) |>
  bayesfactor_models() |>
  report()
main_Total_germany %>% ggplot() +
  aes(x = study, y = total_duration_calc, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_colour_see() +
  ylab("Total Duration") +
  theme(legend.position.inside = c(0.25, 0.9)) +
  xlab("Study Designs") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5, ) +
  scale_x_discrete(labels = c("WEB" = "Online-Based", "VR" = "VR")) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05)) # 95 % mean_cl_boot is 95% confidence intervals

temp <- main_Total_germany
temp$study <- gsub("WEB", "Online-Based", temp$study)
temp %>% ggplot() +
  aes(x = rep_exposure, y = total_duration_calc, fill = study, colour = study, group = study) +
  scale_colour_see() +
  ylab("Total Duration") +
  # scale_fill_discrete(labels=c("WEB" = "Online-Based", "VR" = "VR")) +
  theme(legend.position.inside = c(0.5, 0.25)) +
  xlab("Repeated Expsoure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5, ) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05)) # 95 % mean_cl_boot is 95% confidence intervals



#### Crashes ####
checkAssumptionsForAnovaThreeFactors(data = main_Total_web, y = "number_of_crashes", factor_1 = "culturalBackground", factor_2 = "ehmi", factor_3 = "rep_exposure")
modelART <- art(number_of_crashes ~ culturalBackground * ehmi * rep_exposure + Error(subject_id / (ehmi * rep_exposure)), data = main_Total_web) |> anova()
reportART(model = modelART, dv = "number of collisions")
reportMeanAndSD(main_df = main_Total_web, iv = "culturalBackground", dv = "number_of_crashes")
reportMeanAndSD(main_df = main_Total_web, iv = "ehmi", dv = "number_of_crashes")
d <- dunnTest(number_of_crashes ~ rep_exposure, data = main_Total_web, method = "holm", two.sided = FALSE)
reportDunnTest(main_df = main_Total_web, d = d, iv = "rep_exposure", dv = "number_of_crashes")

main_Total_web %>% ggplot() +
  aes(x = rep_exposure, y = number_of_crashes, fill = culturalBackground, colour = culturalBackground, group = culturalBackground) +
  scale_colour_see() +
  ylab("Number of Collisions") +
  theme(legend.position.inside = c(0.2, 0.84)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05)) # 95 % mean_cl_boot is 95% confidence intervals
ggsave("plots/result_web_usa_ger_collision_1.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


main_Total_web %>% ggplot() +
  aes(x = rep_exposure, y = number_of_crashes, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_colour_see() +
  ylab("Number of Collisions") +
  theme(legend.position.inside = c(0.2, 0.84)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05)) # 95 % mean_cl_boot is 95% confidence intervals
ggsave("plots/result_web_usa_ger_collision_2.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


p <- main_Total_web %>% ggplot() +
  aes(x = rep_exposure, y = number_of_crashes, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_colour_see() +
  ylab("Number of Collisions") +
  theme(legend.position.inside = c(0.2, 0.85)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = .05)) # 95 % mean_cl_boot is 95% confidence intervals
p + facet_grid(~culturalBackground)
ggsave("plots/result_web_usa_ger_collision_3.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


#### duration_sidewalk_start ####
checkAssumptionsForAnovaThreeFactors(data = main_Total_web, y = "duration_sidewalk_start", factor_1 = "culturalBackground", factor_2 = "ehmi", factor_3 = "rep_exposure")
modelART <- art(duration_sidewalk_start ~ culturalBackground * ehmi * rep_exposure + Error(subject_id / (ehmi * rep_exposure)), data = main_Total_web) |> anova()
reportART(model = modelART, dv = "duration_sidewalk_start")
reportMeanAndSD(main_df = main_Total_web, iv = "ehmi", dv = "duration_sidewalk_start")

#### duration_first_lane ####
checkAssumptionsForAnovaThreeFactors(data = main_Total_web, y = "duration_first_lane", factor_1 = "culturalBackground", factor_2 = "ehmi", factor_3 = "rep_exposure")
modelART <- art(duration_first_lane ~ culturalBackground * ehmi * rep_exposure + Error(subject_id / (ehmi * rep_exposure)), data = main_Total_web) |> anova()
reportART(model = modelART, dv = "duration_first_lane")
reportMeanAndSD(main_df = main_Total_web, iv = "ehmi", dv = "duration_first_lane")
d <- dunnTest(duration_first_lane ~ rep_exposure, data = main_Total_web, method = "holm", two.sided = FALSE)
reportDunnTest(main_df = main_Total_web, d = d, iv = "rep_exposure", dv = "duration_first_lane")

main_Total_web %>% ggplot() +
  aes(x = rep_exposure, y = duration_first_lane, fill = culturalBackground, colour = culturalBackground, group = culturalBackground) +
  scale_colour_see() +
  ylab("Duration - First Lane") +
  theme(legend.position.inside = c(0.2, 0.84)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05)) # 95 % mean_cl_boot is 95% confidence intervals
ggsave("plots/result_web_usa_ger_firstLane_1.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)

main_Total_web %>% ggplot() +
  aes(x = rep_exposure, y = duration_first_lane, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_colour_see() +
  ylab("Duration - First Lane") +
  theme(legend.position.inside = c(0.2, 0.84)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05)) # 95 % mean_cl_boot is 95% confidence intervals
ggsave("plots/result_web_usa_ger_firstLane_2.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


p <- main_Total_web %>% ggplot() +
  aes(x = rep_exposure, y = duration_first_lane, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_colour_see() +
  ylab("Duration - First Lane") +
  theme(legend.position.inside = c(0.2, 0.85)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .1, position = position_dodge(width = .1)) # 95 % mean_cl_boot is 95% confidence intervals
p + facet_grid(~culturalBackground)
ggsave("plots/result_web_usa_ger_firstLane_3.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)



#### duration_second_lane ####
checkAssumptionsForAnovaThreeFactors(data = main_Total_web, y = "duration_second_lane", factor_1 = "culturalBackground", factor_2 = "ehmi", factor_3 = "rep_exposure")
modelART <- art(duration_second_lane ~ culturalBackground * ehmi * rep_exposure + Error(subject_id / (ehmi * rep_exposure)), data = main_Total_web) |> anova()
reportART(model = modelART, dv = "duration_second_lane")
main_Total_web %>% ggplot() +
  aes(x = rep_exposure, y = duration_second_lane, fill = culturalBackground, colour = culturalBackground, group = culturalBackground) +
  scale_colour_see() +
  ylab("Duration - Second Lane") +
  theme(legend.position.inside = c(0.2, 0.84)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5, ) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05)) # 95 % mean_cl_boot is 95% confidence intervals
ggsave("plots/result_web_usa_ger_second_lane_1.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)



main_Total_web %>% ggplot() +
  aes(x = rep_exposure, y = duration_second_lane, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_colour_see() +
  ylab("Duration Second Lane") +
  theme(legend.position.inside = c(0.2, 0.84)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5, ) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05)) # 95 % mean_cl_boot is 95% confidence intervals
ggsave("plots/result_web_usa_ger_second_lane_2.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)

#### duration_sidewalk_park ####
checkAssumptionsForAnovaThreeFactors(data = main_Total_web, y = "duration_sidewalk_park", factor_1 = "culturalBackground", factor_2 = "ehmi", factor_3 = "rep_exposure")
modelART <- art(duration_sidewalk_park ~ culturalBackground * ehmi * rep_exposure + Error(subject_id / (ehmi * rep_exposure)), data = main_Total_web) |> anova()
reportART(model = modelART, dv = "duration_sidewalk_park")


#### total_duration ####
checkAssumptionsForAnovaThreeFactors(data = main_Total_web, y = "total_duration_calc", factor_1 = "culturalBackground", factor_2 = "ehmi", factor_3 = "rep_exposure")
modelART <- art(total_duration_calc ~ culturalBackground * ehmi * rep_exposure + Error(subject_id / (ehmi * rep_exposure)), data = main_Total_web) |> anova()
reportART(model = modelART, dv = "total_duration_calc")
reportMeanAndSD(main_df = main_Total_web, iv = "culturalBackground", dv = "total_duration_calc")

main_Total_web %>% ggplot() +
  aes(x = rep_exposure, y = total_duration_calc, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_colour_see() +
  ylab("Total Crossing Time") +
  theme(legend.position.inside = c(0.2, 0.84)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5, ) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05)) # 95 % mean_cl_boot is 95% confidence intervals
ggsave("plots/result_web_usa_ger_total_time_1.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


p <- main_Total_web %>% ggplot() +
  aes(x = rep_exposure, y = total_duration_calc, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_colour_see() +
  ylab("Total Crossing Time") +
  theme(legend.position.inside = c(0.2, 0.85)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5, ) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .1, position = position_dodge(width = .1)) # 95 % mean_cl_boot is 95% confidence intervals
p + facet_grid(~culturalBackground)
ggsave("plots/result_web_usa_ger_total_time_2.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


#### Position ####
options(max.print = 999999)
##### Position Germany #####
xPos_round_ger <- round(df_position_germany$x_position, digits = 2)
zPos_round_ger <- round(df_position_germany$z_position, digits = 2)
repeatedExp_ger <- df_position_germany$rep_exposure
# Position Checkpoint xPos = 92.95 || zPos = 158.29
# Position Pedestrian xPos = 93.7  || zPos = 170.20
main_Position_reduced_repExp_ger <- data.frame(xPos_round_ger, zPos_round_ger, repeatedExp_ger)
main_Position_filterd_repExp_ger <- count(main_Position_reduced_repExp_ger, xPos_round_ger, zPos_round_ger, repeatedExp_ger)

### Position: Repeated Exposure ONE ###
main_Position_filterd_ger_repExp_One <- filter(main_Position_filterd_repExp_ger, xPos_round_ger < 95 & xPos_round_ger > 92.5 & n > 1 & n != 523 & zPos_round_ger > 157.8 & zPos_round_ger <= 170.2 & repeatedExp_ger == "ONE")
main_Position_filterd_ger_repExp_One %>% ggplot() +
  aes(x = xPos_round_ger, y = zPos_round_ger, size = n) +
  geom_point(alpha = 1, color = "#4575B4") +
  scale_size(range = c(.1, 15)) +
  theme(
    legend.position = "", axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_reverse() +
  ggtitle("Online-Based Study: Germany") +
  geom_hline(yintercept = 168.78, color = "#A9A9A9") + # curb-start on starting point
  geom_hline(yintercept = 167, color = "#A9A9A9") + # curb-END on starting point
  geom_hline(yintercept = 163.62, linetype = "dashed", color = "#A9A9A9") + # median
  geom_hline(yintercept = 160.24, color = "#A9A9A9") + # curb-start at park
  geom_hline(yintercept = 158.54, color = "#A9A9A9") +
  xlab("Repeated Exposure: One") +
  ylab("Walking Direction →")
ggsave("plots/result_web_waiting_behavior_germany_1.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)



## Position: Repeated Exposure TWO ##
main_Position_filterd_ger_repExp_Two <- filter(main_Position_filterd_repExp_ger, xPos_round_ger < 95 & xPos_round_ger > 92.5 & n > 1 & zPos_round_ger > 157.8 & zPos_round_ger <= 170.2 & repeatedExp_ger == "TWO")
main_Position_filterd_ger_repExp_Two %>% ggplot() +
  aes(x = xPos_round_ger, y = zPos_round_ger, size = n) +
  geom_point(alpha = 1, color = "#4575B4") +
  scale_size(range = c(.1, 15)) +
  ggtitle("Online-Based Study: Germany") +
  theme(
    legend.position = "", axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_reverse() +
  geom_hline(yintercept = 168.78, color = "#A9A9A9") + # curb-start on starting point
  geom_hline(yintercept = 167, color = "#A9A9A9") + # curb-END on starting point
  geom_hline(yintercept = 163.62, linetype = "dashed", color = "#A9A9A9") + # median
  geom_hline(yintercept = 160.24, color = "#A9A9A9") + # curb-start at park
  geom_hline(yintercept = 158.54, color = "#A9A9A9") +
  xlab("Repeated Exposure: Two") +
  ylab("Walking Direction →")
ggsave("plots/result_web_waiting_behavior_germany_2.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


main_Position_filterd_repExp_Three <- filter(main_Position_filterd_repExp_ger, xPos_round_ger < 95 & xPos_round_ger > 92.5 & n > 1 & zPos_round_ger > 157.8 & zPos_round_ger <= 170.2 & repeatedExp_ger == "THREE")
main_Position_filterd_repExp_Three %>% ggplot() +
  aes(x = xPos_round_ger, y = zPos_round_ger, size = n) +
  geom_point(alpha = 1, color = "#4575B4") +
  scale_size(range = c(.1, 15)) +
  theme(
    legend.position = "", axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_reverse() +
  ggtitle("Online-Based Study: Germany") +
  geom_hline(yintercept = 168.78, color = "#A9A9A9") + # curb-start on starting point
  geom_hline(yintercept = 167, color = "#A9A9A9") + # curb-END on starting point
  geom_hline(yintercept = 163.62, linetype = "dashed", color = "#A9A9A9") + # median
  geom_hline(yintercept = 160.24, color = "#A9A9A9") + # curb-start at park
  geom_hline(yintercept = 158.54, color = "#A9A9A9") +
  xlab("Repeated Exposure: Three") +
  ylab("Walking Direction →")
ggsave("plots/result_web_waiting_behavior_germany_3.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


##### Position USA #####
xPos_round_usa <- round(df_position_usa$x_position, digits = 2)
zPos_round_usa <- round(df_position_usa$z_position, digits = 2)
repeatedExp_usa <- df_position_usa$rep_exposure
# Position Checkpoint xPos = 92.95 || zPos = 158.29
# Position Pedestrian xPos = 93.7  || zPos = 170.20
main_Position_reduced_repExp_usa <- data.frame(xPos_round_usa, zPos_round_usa, repeatedExp_usa)
main_Position_filterd_repExp_usa <- count(main_Position_reduced_repExp_usa, xPos_round_usa, zPos_round_usa, repeatedExp_usa)

## Position: Repeated Exposure ONE ##
main_Position_filterd_usa_repExp_One <- filter(main_Position_filterd_repExp_usa, xPos_round_usa <= 95 & xPos_round_usa > 92.5 & n > 1 & n != 489 & zPos_round_usa > 157.8 & zPos_round_usa <= 170.2 & repeatedExp_usa == "ONE")
main_Position_filterd_usa_repExp_One %>% ggplot() +
  aes(x = xPos_round_usa, y = zPos_round_usa, size = n) +
  geom_point(alpha = 1, color = "#4575B4") +
  scale_size(range = c(.1, 15)) +
  theme(
    legend.position = "", axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_reverse() +
  ggtitle("Online-Based Study: USA") +
  geom_hline(yintercept = 168.78, color = "#A9A9A9") + # curb-start on starting point
  geom_hline(yintercept = 167, color = "#A9A9A9") + # curb-END on starting point
  geom_hline(yintercept = 163.62, linetype = "dashed", color = "#A9A9A9") + # median
  geom_hline(yintercept = 160.24, color = "#A9A9A9") + # curb-start at park
  geom_hline(yintercept = 158.54, color = "#A9A9A9") +
  xlab("Repeated Exposure: One") +
  ylab("Walking Direction →")
ggsave("plots/result_web_waiting_behavior_usa_1.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


## Position: Repeated Exposure TWO ##
main_Position_filterd_usa_repExp_Two <- filter(main_Position_filterd_repExp_usa, xPos_round_usa < 95 & xPos_round_usa > 92.5 & n > 1 & n != 165 & n != 242 & zPos_round_usa > 157.8 & zPos_round_usa <= 170.2 & repeatedExp_usa == "TWO")
main_Position_filterd_usa_repExp_Two %>% ggplot() +
  aes(x = xPos_round_usa, y = zPos_round_usa, size = n) +
  geom_point(alpha = 1, color = "#4575B4") +
  scale_size(range = c(.1, 15)) +
  theme(
    legend.position = "", axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_reverse() +
  ggtitle("Online-Based Study: USA") +
  geom_hline(yintercept = 168.78, color = "#A9A9A9") + # curb-start on starting point
  geom_hline(yintercept = 167, color = "#A9A9A9") + # curb-END on starting point
  geom_hline(yintercept = 163.62, linetype = "dashed", color = "#A9A9A9") + # median
  geom_hline(yintercept = 160.24, color = "#A9A9A9") + # curb-start at park
  geom_hline(yintercept = 158.54, color = "#A9A9A9") +
  xlab("Repeated Exposure: Two") +
  ylab("Walking Direction →")
ggsave("plots/result_web_waiting_behavior_usa_2.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)




main_Position_filterd_repExp_Three <- filter(main_Position_filterd_repExp_usa, xPos_round_usa < 95 & xPos_round_usa > 92.5 & n > 1 & zPos_round_usa > 157.8 & zPos_round_usa <= 170.2 & repeatedExp_usa == "THREE")
main_Position_filterd_repExp_Three %>% ggplot() +
  aes(x = xPos_round_usa, y = zPos_round_usa, size = n) +
  geom_point(alpha = 1, color = "#4575B4") +
  scale_size(range = c(.1, 15)) +
  theme(
    legend.position = "", axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_reverse() +
  ggtitle("Online-Based Study: USA") +
  geom_hline(yintercept = 168.78, color = "#A9A9A9") + # curb-start on starting point
  geom_hline(yintercept = 167, color = "#A9A9A9") + # curb-END on starting point
  geom_hline(yintercept = 163.62, linetype = "dashed", color = "#A9A9A9") + # median
  geom_hline(yintercept = 160.24, color = "#A9A9A9") + # curb-start at park
  geom_hline(yintercept = 158.54, color = "#A9A9A9") +
  xlab("Repeated Exposure: Three") +
  ylab("Walking Direction →")
ggsave("plots/result_web_waiting_behavior_usa_3.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


# Position Checkpoint xPos = 92.95 || zPos = 158.29 || Scale: 4 x 20 x 4
# Position Pedestrian xPos = 93.7  || zPos = 170.20


## Position: Repeated Exposure One - Line Diagram ##
options(digits.secs = 4)
repeatedExp_ld <- df_position_germany$rep_exposure
subjectID_ld <- df_position_germany$subject_id
condition_id_ld <- df_position_germany$condition_id
xPos_round_ld <- round(df_position_germany$x_position, digits = 2)
zPos_round_ld <- round(df_position_germany$z_position, digits = 2)
total_duration_calc_ld <- df_position_germany$total_duration_calc
mp_reduced_ld <- data.frame(xPos_round_ld, zPos_round_ld, repeatedExp_ld, subjectID_ld, condition_id_ld)

mp_filtered_ld_SC1_One <- filter(mp_reduced_ld, repeatedExp_ld == "ONE" & condition_id_ld == "SCENARIO_2")
mp_filtered_ld_SC1_One %>% ggplot() +
  geom_rect(aes(xmin = 90.5, xmax = 94.5, ymin = 158.29, ymax = 160.4), color = NA, fill = "#F0F8FF", alpha = 1) + # Checkpoint
  aes(x = xPos_round_ld, y = zPos_round_ld, color = factor(subjectID_ld), fill = subjectID_ld, group = subjectID_ld) +
  geom_path() +
  ggtitle("Scenario 1: with eHMI & Germany") +
  theme(
    legend.position = "", axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_reverse() +
  geom_hline(yintercept = 168.78, color = "#A9A9A9") + # curb-start on starting point
  geom_hline(yintercept = 167, color = "#A9A9A9") + # curb-END on starting point
  geom_hline(yintercept = 163.62, linetype = "dashed", color = "#A9A9A9") + # median
  geom_hline(yintercept = 160.24, color = "#A9A9A9") + # curb-start at park
  geom_hline(yintercept = 158.54, color = "#A9A9A9") +
  xlab("Repeated Exposure: One") +
  ylab("Walking Direction →")
ggsave("plots/result_walking_behavior_sc2_rep1_germany.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


mp_filtered_ld_SC2_One <- filter(mp_reduced_ld, xPos_round_ld > 90.9 & repeatedExp_ld == "ONE" & condition_id_ld == "SCENARIO_4")
mp_filtered_ld_SC2_One %>% ggplot() +
  geom_rect(aes(xmin = 90.5, xmax = 94.5, ymin = 158.29, ymax = 160.4), color = NA, fill = "#F0F8FF", alpha = 1) + # Checkpoint
  aes(x = xPos_round_ld, y = zPos_round_ld, color = factor(subjectID_ld), fill = subjectID_ld, group = subjectID_ld) +
  geom_path() +
  ggtitle("Scenario 2: without eHMI & Germany") +
  theme(
    legend.position = "", axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_reverse() +
  geom_hline(yintercept = 168.78, color = "#A9A9A9") + # curb-start on starting point
  geom_hline(yintercept = 167, color = "#A9A9A9") + # curb-END on starting point
  geom_hline(yintercept = 163.62, linetype = "dashed", color = "#A9A9A9") + # median
  geom_hline(yintercept = 160.24, color = "#A9A9A9") + # curb-start at park
  geom_hline(yintercept = 158.54, color = "#A9A9A9") +
  xlab("Repeated Exposure: One") +
  ylab("Walking Direction →")
ggsave("plots/result_walking_behavior_sc4_rep1_germany.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


mp_filtered_ld_SC1_Two <- filter(mp_reduced_ld, subjectID_ld != "0c415075-c031-4a7f-9b32-f16fd60ab61f" & repeatedExp_ld == "TWO" & condition_id_ld == "SCENARIO_2")
mp_filtered_ld_SC1_Two %>% ggplot() +
  geom_rect(aes(xmin = 90.5, xmax = 94.5, ymin = 158.29, ymax = 160.4), color = NA, fill = "#F0F8FF", alpha = 1) + # Checkpoint
  aes(x = xPos_round_ld, y = zPos_round_ld, color = factor(subjectID_ld), fill = subjectID_ld, group = subjectID_ld) +
  geom_path() +
  ggtitle("Scenario 1: with eHMI & Germany") +
  theme(
    legend.position = "", axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_reverse() +
  geom_hline(yintercept = 168.78, color = "#A9A9A9") + # curb-start on starting point
  geom_hline(yintercept = 167, color = "#A9A9A9") + # curb-END on starting point
  geom_hline(yintercept = 163.62, linetype = "dashed", color = "#A9A9A9") + # median
  geom_hline(yintercept = 160.24, color = "#A9A9A9") + # curb-start at park
  geom_hline(yintercept = 158.54, color = "#A9A9A9") +
  xlab("Repeated Exposure: Two") +
  ylab("Walking Direction →")
ggsave("plots/result_walking_behavior_sc2_rep2_germany.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


mp_filtered_ld_SC2_Two <- filter(mp_reduced_ld, repeatedExp_ld == "TWO" & condition_id_ld == "SCENARIO_4")
mp_filtered_ld_SC2_Two %>% ggplot() +
  geom_rect(aes(xmin = 90.5, xmax = 94.5, ymin = 158.29, ymax = 160.4), color = NA, fill = "#F0F8FF", alpha = 1) + # Checkpoint
  aes(x = xPos_round_ld, y = zPos_round_ld, color = factor(subjectID_ld), fill = subjectID_ld, group = subjectID_ld) +
  geom_path() +
  ggtitle("Scenario 2: without eHMI & Germany") +
  theme(
    legend.position = "", axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_reverse() +
  geom_hline(yintercept = 168.78, color = "#A9A9A9") + # curb-start on starting point
  geom_hline(yintercept = 167, color = "#A9A9A9") + # curb-END on starting point
  geom_hline(yintercept = 163.62, linetype = "dashed", color = "#A9A9A9") + # median
  geom_hline(yintercept = 160.24, color = "#A9A9A9") + # curb-start at park
  geom_hline(yintercept = 158.54, color = "#A9A9A9") +
  xlab("Repeated Exposure: Two") +
  ylab("Walking Direction →")
ggsave("plots/result_walking_behavior_sc4_rep2_germany.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)



mp_filtered_ld_SC1_Three <- filter(mp_reduced_ld, xPos_round_ld < 98.75 & zPos_round_ld > 157.8 & repeatedExp_ld == "THREE" & condition_id_ld == "SCENARIO_2")
mp_filtered_ld_SC1_Three %>% ggplot() +
  geom_rect(aes(xmin = 90.5, xmax = 94.5, ymin = 158.29, ymax = 160.4), color = NA, fill = "#F0F8FF", alpha = 1) + # Checkpoint
  aes(x = xPos_round_ld, y = zPos_round_ld, color = factor(subjectID_ld), fill = subjectID_ld, group = subjectID_ld) +
  geom_path() +
  ggtitle("Scenario 1: with eHMI & Germany") +
  theme(
    legend.position = "", axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_reverse() +
  geom_hline(yintercept = 168.78, color = "#A9A9A9") + # curb-start on starting point
  geom_hline(yintercept = 167, color = "#A9A9A9") + # curb-END on starting point
  geom_hline(yintercept = 163.62, linetype = "dashed", color = "#A9A9A9") + # median
  geom_hline(yintercept = 160.24, color = "#A9A9A9") + # curb-start at park
  geom_hline(yintercept = 158.54, color = "#A9A9A9") +
  xlab("Repeated Exposure: Three") +
  ylab("Walking Direction →")
ggsave("plots/result_walking_behavior_sc2_rep3_germany.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


mp_filtered_ld_SC2_Three <- filter(mp_reduced_ld, xPos_round_ld > 91.25 & zPos_round_ld > 157.8 & repeatedExp_ld == "THREE" & condition_id_ld == "SCENARIO_4")
mp_filtered_ld_SC2_Three %>% ggplot() +
  geom_rect(aes(xmin = 90.5, xmax = 94.5, ymin = 158.29, ymax = 160.4), color = NA, fill = "#F0F8FF", alpha = 1) + # Checkpoint
  aes(x = xPos_round_ld, y = zPos_round_ld, color = factor(subjectID_ld), fill = subjectID_ld, group = subjectID_ld) +
  geom_path() +
  ggtitle("Scenario 2: without eHMI & Germany") +
  theme(
    legend.position = "", axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_reverse() +
  geom_hline(yintercept = 168.78, color = "#A9A9A9") + # curb-start on starting point
  geom_hline(yintercept = 167, color = "#A9A9A9") + # curb-END on starting point
  geom_hline(yintercept = 163.62, linetype = "dashed", color = "#A9A9A9") + # median
  geom_hline(yintercept = 160.24, color = "#A9A9A9") + # curb-start at park
  geom_hline(yintercept = 158.54, color = "#A9A9A9") +
  xlab("Repeated Exposure: Three") +
  ylab("Walking Direction →")
ggsave("plots/result_walking_behavior_sc4_rep3_germany.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)



#### Eye tracking Germany####

levels(df_eye_gaze_ger$area_of_interest)

df_eye_gaze_ger$Time_Pos <- as.POSIXct(df_eye_gaze_ger$t, format = "%d.%m.%Y %H:%M:%OS")

# df_eye_gaze_ger$Time_Pos <- as.POSIXct(df_eye_gaze_ger$T, format = "%d.%m.%Y %H:%M:%OS")
# df_eye_gaze_ger$Time_Pos
# format(df_eye_gaze_ger$Time_Pos, "%Y-%m-%d %H:%M:%OS4")

# df_null <- main_logs %>% group_by(participant_id, condition_id) %>% summarise(sumFixationsNull = sum(area_of_interest == "Nihil"))


df_general_car <- df_eye_gaze_ger %>%
  group_by(subject_id, scenario_id, rep_exposure) %>%
  summarise(sumFixationsCar = sum(area_of_interest == "general_car"))
df_av_body <- df_eye_gaze_ger %>%
  group_by(subject_id, scenario_id, rep_exposure) %>%
  summarise(sumFixationsAVBody = sum(area_of_interest == "av_body"))
df_av_ehmi <- df_eye_gaze_ger %>%
  group_by(subject_id, scenario_id, rep_exposure) %>%
  summarise(sumFixationsAVeHMI = sum(area_of_interest == "av_ehmi"))

test1 <- df_eye_gaze_ger %>%
  group_by(subject_id, scenario_id, rep_exposure) %>%
  summarise(duration = last(Time_Pos) - first(Time_Pos)) # difftime(tail(.$Time_Pos, n=1),.$Time_Pos[1])
test1$totalFixations <- test1$duration * 50 # as 50 Hz was used
test1$totalFixations <- as.numeric(test1$totalFixations)
test1$totalFixations <- round(test1$totalFixations, digits = 0)

test <- merge(df_general_car, df_av_body, by = c("subject_id", "scenario_id", "rep_exposure")) %>%
  merge(., df_av_ehmi, by = c("subject_id", "scenario_id", "rep_exposure")) %>%
  merge(., test1, by = c("subject_id", "scenario_id", "rep_exposure"))


test$sumFixationsNull <- test$totalFixations - test$sumFixationsCar - test$sumFixationsAVBody - test$sumFixationsAVeHMI

# define percentages

test$sumFixationsNull_Percentage <- test$sumFixationsNull / test$totalFixations

test$sumFixationsCar_Percentage <- test$sumFixationsCar / test$totalFixations

test$sumFixationsAVBody_Percentage <- test$sumFixationsAVBody / test$totalFixations
test$sumFixationsAVeHMI_Percentage <- test$sumFixationsAVeHMI / test$totalFixations



# only select relevant columns
library(dplyr)
test2 <- dplyr::select(test, !(sumFixationsNull:totalFixations))

data_long <- gather(test2, aoi, measurement, sumFixationsNull_Percentage:sumFixationsAVeHMI_Percentage, factor_key = TRUE)
data_long

# remove unnecessary strings
data_long$aoi <- gsub("sumFixations", "", data_long$aoi)
data_long$aoi <- gsub("_Percentage", "", data_long$aoi)

data_long$aoi <- as.factor(data_long$aoi)

data_long %>% ggplot() +
  aes(x = scenario_id, y = measurement, fill = aoi, colour = aoi, group = aoi) +
  scale_colour_see() +
  ylab("Percentage Fixation") +
  theme(legend.position.inside = c(0.78, 0.58)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", size = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = .05)) # 95 % mean_cl_boot is 95% confidence intervals

data_long_1 <- subset(data_long, aoi != "Null")

data_long_1 %>% ggplot() +
  aes(x = scenario_id, y = measurement * 100, fill = aoi, colour = aoi, group = aoi) +
  scale_colour_see() +
  ylab("Percentage Fixation - Excluded Null") +
  theme(legend.position.inside = c(0.9, 0.68), legend.text = element_text(size = 10)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, mapping = aes(shape = aoi)) +
  scale_shape_manual(values = 1:nlevels(data_long_1$aoi)) +
  stat_summary(fun = mean, geom = "line", size = 1, aes(linetype = aoi)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = .05))
# ggsave("plots/Fixation_without_null.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


# remove when was not visible
data_long_1 <- subset(data_long_1, subset = measurement != 0)


data_long_1 %>% ggplot() +
  aes(x = scenario_id, y = measurement * 100, colour = aoi, group = aoi) +
  scale_colour_see() +
  ylab("Percentage Fixation  Excluded Null") +
  ggtitle("EyeTracking Data: Germany") +
  theme(legend.position.inside = c(0.9, 0.68), plot.title = element_text(hjust = 0.5)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, mapping = aes(shape = aoi)) +
  scale_shape_manual(values = 1:nlevels(data_long_1$aoi)) +
  stat_summary(fun = mean, geom = "line", size = 1, aes(linetype = aoi)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .1) +
  scale_x_discrete(labels = c("SCENARIO_2" = "With eHMI", "SCENARIO_4" = "Without eHMI"))
# ggsave("plots/Fixation_without_null_no_zero.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)




#### Eye tracking USA ####

levels(df_eye_gaze_usa$area_of_interest)

df_eye_gaze_usa$Time_Pos <- as.POSIXct(df_eye_gaze_usa$t, format = "%d.%m.%Y %H:%M:%OS")

# df_eye_gaze_usa$Time_Pos <- as.POSIXct(df_eye_gaze_usa$T, format = "%d.%m.%Y %H:%M:%OS")
# df_eye_gaze_usa$Time_Pos
# format(df_eye_gaze_usa$Time_Pos, "%Y-%m-%d %H:%M:%OS4")

# df_null <- main_logs %>% group_by(participant_id, condition_id) %>% summarise(sumFixationsNull = sum(area_of_interest == "Nihil"))


df_general_car <- df_eye_gaze_usa %>%
  group_by(subject_id, scenario_id, rep_exposure) %>%
  summarise(sumFixationsCar = sum(area_of_interest == "general_car"))
df_av_body <- df_eye_gaze_usa %>%
  group_by(subject_id, scenario_id, rep_exposure) %>%
  summarise(sumFixationsAVBody = sum(area_of_interest == "av_body"))
df_av_ehmi <- df_eye_gaze_usa %>%
  group_by(subject_id, scenario_id, rep_exposure) %>%
  summarise(sumFixationsAVeHMI = sum(area_of_interest == "av_ehmi"))

test1 <- df_eye_gaze_usa %>%
  group_by(subject_id, scenario_id, rep_exposure) %>%
  summarise(duration = last(Time_Pos) - first(Time_Pos)) # difftime(tail(.$Time_Pos, n=1),.$Time_Pos[1])
test1$totalFixations <- test1$duration * 50 # as 50 Hz was used
test1$totalFixations <- as.numeric(test1$totalFixations)
test1$totalFixations <- round(test1$totalFixations, digits = 0)

test <- merge(df_general_car, df_av_body, by = c("subject_id", "scenario_id", "rep_exposure")) %>%
  merge(., df_av_ehmi, by = c("subject_id", "scenario_id", "rep_exposure")) %>%
  merge(., test1, by = c("subject_id", "scenario_id", "rep_exposure"))


test$sumFixationsNull <- test$totalFixations - test$sumFixationsCar - test$sumFixationsAVBody - test$sumFixationsAVeHMI

# define percentages

test$sumFixationsNull_Percentage <- test$sumFixationsNull / test$totalFixations

test$sumFixationsCar_Percentage <- test$sumFixationsCar / test$totalFixations

test$sumFixationsAVBody_Percentage <- test$sumFixationsAVBody / test$totalFixations
test$sumFixationsAVeHMI_Percentage <- test$sumFixationsAVeHMI / test$totalFixations



# only select relevant columns
library(dplyr)
test2 <- dplyr::select(test, !(sumFixationsNull:totalFixations))

data_long <- gather(test2, aoi, measurement, sumFixationsNull_Percentage:sumFixationsAVeHMI_Percentage, factor_key = TRUE)
data_long

# remove unnecessary strings
data_long$aoi <- gsub("sumFixations", "", data_long$aoi)
data_long$aoi <- gsub("_Percentage", "", data_long$aoi)

data_long$aoi <- as.factor(data_long$aoi)

data_long %>% ggplot() +
  aes(x = scenario_id, y = measurement, fill = aoi, colour = aoi, group = aoi) +
  scale_colour_see() +
  ylab("Percentage Fixation") +
  theme(legend.position.inside = c(0.78, 0.58)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", size = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = .05))

data_long_1 <- subset(data_long, aoi != "Null")

data_long_1 %>% ggplot() +
  aes(x = scenario_id, y = measurement * 100, fill = aoi, colour = aoi, group = aoi) +
  scale_colour_see() +
  ylab("Percentage Fixation - Excluded Null") +
  theme(legend.position.inside = c(0.9, 0.68), legend.text = element_text(size = 10)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, mapping = aes(shape = aoi)) +
  scale_shape_manual(values = 1:nlevels(data_long_1$aoi)) +
  stat_summary(fun = mean, geom = "line", size = 1, aes(linetype = aoi), alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = .05)) +
  scale_x_discrete(labels = c("SCENARIO_2" = "With eHMI", "SCENARIO_4" = "Without eHMI"))
# ggsave("plots/Fixation_without_null.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


# remove when was not visible
data_long_1 <- subset(data_long_1, subset = measurement != 0)


data_long_1 %>% ggplot() +
  aes(x = scenario_id, y = measurement * 100, colour = aoi, group = aoi) +
  scale_colour_see() +
  ylab("Percentage Fixation  Excluded Null") +
  ggtitle("EyeTracking Data: USA") +
  theme(legend.position.inside = c(0.9, 0.68), plot.title = element_text(hjust = 0.5)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, mapping = aes(shape = aoi)) +
  scale_shape_manual(values = 1:nlevels(data_long_1$aoi)) +
  stat_summary(fun = mean, geom = "line", size = 1, aes(linetype = aoi), alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = .05)) +
  scale_x_discrete(labels = c("SCENARIO_2" = "With eHMI", "SCENARIO_4" = "Without eHMI"))
# ggsave("plots/Fixation_without_null_no_zero.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)
