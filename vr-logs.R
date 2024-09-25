library("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path))

library(devtools)
source_url("https://raw.githubusercontent.com/M-Colley/rCode/main/r_functionality.R")


# Get custom file
main_df_Position <- read_xlsx(path = "position_all_updated.xlsx", sheet = "position_all_updated")
main_df_Position <- as.data.frame(main_df_Position)
names(main_df_Position)



setwd("./Logs/")
# Get the files names
filesEyeGaze <- list.files(path = ".", pattern = ".*EYE_GAZE.csv", recursive = FALSE, )
filesPosition <- list.files(path = ".", pattern = ".*POSITION.csv", recursive = FALSE, )
filesTotal <- list.files(path = ".", pattern = ".*TOTAL.csv", recursive = FALSE, )



main_EyeGaze <- do.call(rbind, lapply(filesEyeGaze, function(x) read.csv(x, stringsAsFactors = FALSE, sep = ";", row.names = NULL)))
colnames(main_EyeGaze) <- c("subject_id", "scenario_id", "eHMI", "traffic", "repeated_exposure", "T", "area_of_interest", "delete")

main_Position <- do.call(rbind, lapply(filesPosition, function(x) read.csv(x, stringsAsFactors = FALSE, sep = ",")))
main_Total <- do.call(rbind, lapply(filesTotal, function(x) read.csv(x, stringsAsFactors = FALSE, sep = ";")))

# Participant 4 did not finalize study
main_EyeGaze <- subset(main_EyeGaze, subject_id != "4")
main_Position <- subset(main_Position, subject_id != "4")
main_Total <- subset(main_Total, subject_id != "4")
main_df_Position <- subset(main_Total, subject_id != "4")


### Parse EyeGaze CSV ###
main_EyeGaze$eHMI <- gsub("OFF", "without eHMI", main_EyeGaze$eHMI)
main_EyeGaze$eHMI <- gsub("ON", "with eHMI", main_EyeGaze$eHMI)

main_EyeGaze$traffic <- gsub("MIXED", "mixed", main_EyeGaze$traffic)
main_EyeGaze$traffic <- gsub("ONLY_AV", "automated", main_EyeGaze$traffic)

main_EyeGaze$repeated_exposure <- gsub("ONE", "1", main_EyeGaze$repeated_exposure)
main_EyeGaze$repeated_exposure <- gsub("TWO", "2", main_EyeGaze$repeated_exposure)
main_EyeGaze$repeated_exposure <- gsub("THREE", "3", main_EyeGaze$repeated_exposure)

main_EyeGaze$subject_id <- as.factor(main_EyeGaze$subject_id)
main_EyeGaze$eHMI <- as.factor(main_EyeGaze$eHMI)
main_EyeGaze$traffic <- as.factor(main_EyeGaze$traffic)
main_EyeGaze$repeated_exposure <- as.factor(main_EyeGaze$repeated_exposure)
main_EyeGaze$area_of_interest <- as.factor(main_EyeGaze$area_of_interest)
print(main_EyeGaze$area_of_interest)

main_Position$subject_id <- as.factor(main_Position$subject_id)
main_Position$eHMI <- as.factor(main_Position$eHMI)
main_Position$traffic <- as.factor(main_Position$traffic)
main_Position$rep_exposure <- as.factor(main_Position$rep_exposure)

main_df_Position$subject_id <- as.factor(main_df_Position$subject_id)
main_df_Position$scenario_id <- as.factor(main_df_Position$scenario_id)
# main_df_Position$condition_id <- as.factor(main_df_Position$condition_id)
main_df_Position$eHMI <- as.factor(main_df_Position$eHMI)
main_df_Position$traffic <- as.factor(main_df_Position$traffic)
main_df_Position$repeated_exposure <- as.factor(main_df_Position$repeated_exposure)

### Parse Total CSV ###
main_Total$eHMI <- gsub("OFF", "without eHMI", main_Total$eHMI)
main_Total$eHMI <- gsub("ON", "with eHMI", main_Total$eHMI)

main_Total$traffic <- gsub("MIXED", "mixed", main_Total$traffic)
main_Total$traffic <- gsub("ONLY_AV", "automated", main_Total$traffic)

main_Total$repeated_exposure <- gsub("ONE", "1", main_Total$repeated_exposure)
main_Total$repeated_exposure <- gsub("TWO", "2", main_Total$repeated_exposure)
main_Total$repeated_exposure <- gsub("THREE", "3", main_Total$repeated_exposure)


main_Total$subject_id <- as.factor(main_Total$subject_id)
main_Total$eHMI <- as.factor(main_Total$eHMI)
main_Total$traffic <- as.factor(main_Total$traffic)
main_Total$repeated_exposure <- as.factor(main_Total$repeated_exposure)

setwd("../")


#### Crashes ####
checkAssumptionsForAnovaThreeFactors(data = main_Total, y = "number_of_crashes", factor_1 = "traffic", factor_2 = "eHMI", factor_3 = "repeated_exposure")
modelART <- art(number_of_crashes ~ traffic * eHMI * repeated_exposure + Error(subject_id / (traffic * eHMI * repeated_exposure)), data = main_Total) |> anova()
reportART(model = modelART, dv = "number_of_crashes")
reportMeanAndSD(main_df = main_Total, iv = "traffic", dv = "number_of_crashes")
reportMeanAndSD(main_df = main_Total, iv = "eHMI", dv = "number_of_crashes")
d <- dunnTest(number_of_crashes ~ repeated_exposure, data = main_Total, method = "holm", two.sided = FALSE)
reportDunnTest(main_df = main_Total, d = d, iv = "repeated_exposure", dv = "number_of_crashes")

main_Total %>% ggplot() +
  aes(x = repeated_exposure, y = number_of_crashes, fill = traffic, colour = traffic, group = traffic) +
  scale_color_see() +
  ylab("Number of Collisions") +
  theme(legend.position = c(0.75, 0.84)) +
  xlab("traffic") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05), alpha = 0.5)
ggsave("plots/Result_VR_collisions_repExp_traffic.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


main_Total %>% ggplot() +
  aes(x = repeated_exposure, y = number_of_crashes, fill = eHMI, colour = eHMI, group = eHMI) +
  scale_color_see() +
  ylab("Number of Collisions") +
  theme(legend.position = c(0.75, 0.84)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05), alpha = 0.5)
ggsave("plots/Result_VR_collisions_repExp_ehmi.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)



p <- main_Total %>% ggplot() +
  aes(x = repeated_exposure, y = number_of_crashes, fill = eHMI, colour = eHMI, group = eHMI) +
  scale_colour_see() +
  ylab("Numbeer of Collisions") +
  theme(legend.position = c(0.3, 0.85)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05), alpha = 0.5)
p + facet_grid(~traffic)
ggsave("plots/Result_VR_collisions_repExp_traffic_ehmi.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


#### duration_sidewalk_start ####
checkAssumptionsForAnovaThreeFactors(data = main_Total, y = "duration_sidewalk_start", factor_1 = "traffic", factor_2 = "eHMI", factor_3 = "repeated_exposure")
# model <- np.anova(formel = duration_sidewalk_start ~ traffic * eHMI * repeated_exposure + Error(subject_id/(traffic * eHMI * repeated_exposure)), data = main_Total, method = 0, compact = T)
modelART <- art(duration_sidewalk_start ~ traffic * eHMI * repeated_exposure + Error(subject_id / (traffic * eHMI * repeated_exposure)), data = main_Total) |> anova()
reportART(model = modelART, dv = "duration_sidewalk_start")
reportMeanAndSD(main_df = main_Total, iv = "traffic", dv = "duration_sidewalk_start")
reportMeanAndSD(main_df = main_Total, iv = "eHMI", dv = "duration_sidewalk_start")
d <- dunnTest(duration_sidewalk_start ~ repeated_exposure, data = main_Total, method = "holm", two.sided = FALSE)
reportDunnTest(main_df = main_Total, d = d, iv = "repeated_exposure", dv = "duration_sidewalk_start")

main_Total %>% ggplot() +
  aes(x = eHMI, y = duration_sidewalk_start, fill = traffic, colour = traffic, group = traffic) +
  scale_color_see() +
  ylab("Duration-Sidewalk Start") +
  theme(legend.position = c(0.75, 0.84)) +
  xlab("eHMI") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05), alpha = 0.5)
ggsave("plots/Result_VR_logs_sidewalk_start_1.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)

main_Total %>% ggplot() +
  aes(x = repeated_exposure, y = duration_sidewalk_start, fill = traffic, colour = traffic, group = traffic) +
  scale_color_see() +
  ylab("Duration-Sidewalk Start") +
  theme(legend.position = c(0.25, 0.2)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05), alpha = 0.5)
ggsave("plots/Result_VR_logs_sidewalk_start_2.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


main_Total %>% ggplot() +
  aes(x = repeated_exposure, y = duration_sidewalk_start, fill = eHMI, colour = eHMI, group = eHMI) +
  scale_color_see() +
  ylab("Duration-Sidewalk Start") +
  theme(legend.position = c(0.25, 0.2)) +
  xlab("Repeated Exposure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05), alpha = 0.5)
ggsave("plots/Result_VR_logs_sidewalk_start_3.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


#### duration_first_lane ####
checkAssumptionsForAnovaThreeFactors(data = main_Total, y = "duration_first_lane", factor_1 = "traffic", factor_2 = "eHMI", factor_3 = "repeated_exposure")
modelART <- art(duration_first_lane ~ traffic * eHMI * repeated_exposure + Error(subject_id / (traffic * eHMI * repeated_exposure)), data = main_Total) |> anova()
reportART(model = modelART, dv = "duration_first_lane")
d <- dunnTest(duration_first_lane ~ repeated_exposure, data = main_Total, method = "holm", two.sided = FALSE)
reportDunnTest(main_df = main_Total, d = d, iv = "repeated_exposure", dv = "duration_first_lane")

#### duration_second_lane ####
checkAssumptionsForAnovaThreeFactors(data = main_Total, y = "duration_second_lane", factor_1 = "traffic", factor_2 = "eHMI", factor_3 = "repeated_exposure")
model <- np.anova(formel = duration_second_lane ~ traffic * eHMI * repeated_exposure + Error(subject_id / (traffic * eHMI * repeated_exposure)), data = main_Total, method = 0, compact = T)
modelART <- art(duration_second_lane ~ traffic * eHMI * repeated_exposure + Error(subject_id / (traffic * eHMI * repeated_exposure)), data = main_Total) |> anova()
reportART(model = modelART, dv = "duration_second_lane")

#### duration_sidewalk_park ####
checkAssumptionsForAnovaThreeFactors(data = main_Total, y = "duration_sidewalk_park", factor_1 = "traffic", factor_2 = "eHMI", factor_3 = "repeated_exposure")
modelART <- art(duration_sidewalk_park ~ traffic * eHMI * repeated_exposure + Error(subject_id / (traffic * eHMI * repeated_exposure)), data = main_Total) |> anova()
reportART(model = modelART, dv = "duration_sidewalk_park")
d <- dunnTest(duration_sidewalk_park ~ repeated_exposure, data = main_Total, method = "holm", two.sided = FALSE)
reportDunnTest(main_df = main_Total, d = d, iv = "repeated_exposure", dv = "duration_sidewalk_park")

#### total_duration ####
main_Total$total_duration_calc <- main_Total$duration_sidewalk_start + main_Total$duration_first_lane + main_Total$duration_second_lane + main_Total$duration_sidewalk_park
checkAssumptionsForAnovaThreeFactors(data = main_Total, y = "total_duration_calc", factor_1 = "traffic", factor_2 = "eHMI", factor_3 = "repeated_exposure")
modelART <- art(total_duration_calc ~ traffic * eHMI * repeated_exposure + Error(subject_id / (traffic * eHMI * repeated_exposure)), data = main_Total) |> anova()
reportART(model = modelART, dv = "total_duration_calc")
reportMeanAndSD(main_df = main_Total, iv = "eHMI", dv = "total_duration_calc")
reportMeanAndSD(main_df = main_Total, iv = "traffic", dv = "total_duration_calc")
d <- dunnTest(total_duration_calc ~ repeated_exposure, data = main_Total, method = "holm", two.sided = FALSE)
reportDunnTest(main_df = main_Total, d = d, iv = "repeated_exposure", dv = "total_duration_calc")

main_Total %>% ggplot() +
  aes(x = eHMI, y = total_duration_calc, fill = traffic, colour = traffic, group = traffic) +
  scale_color_see() +
  ylab("Total Duration") +
  theme(legend.position = c(0.75, 0.84)) +
  xlab("eHMI") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05), alpha = 0.5)
ggsave("plots/Result_VR_total_duration_logs.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)



#### Eye tracking ####

levels(main_EyeGaze$area_of_interest)

library(stringi)

main_EyeGaze$T <- stri_replace_last_fixed(main_EyeGaze$T, ":", ".")

main_EyeGaze$Time_Pos <- as.POSIXct(main_EyeGaze$T, format = "%d.%m.%Y %H:%M:%OS")

df_general_car <- main_EyeGaze %>%
  group_by(subject_id, scenario_id, repeated_exposure) %>%
  summarise(sumFixationsCar = sum(area_of_interest == "general_car"))
df_av_body <- main_EyeGaze %>%
  group_by(subject_id, scenario_id, repeated_exposure) %>%
  summarise(sumFixationsAVBody = sum(area_of_interest == "av_body"))
df_av_ehmi <- main_EyeGaze %>%
  group_by(subject_id, scenario_id, repeated_exposure) %>%
  summarise(sumFixationsAVeHMI = sum(area_of_interest == "av_ehmi"))

test1 <- main_EyeGaze %>%
  group_by(subject_id, scenario_id, repeated_exposure) %>%
  summarise(duration = last(Time_Pos) - first(Time_Pos)) # difftime(tail(.$Time_Pos, n=1),.$Time_Pos[1])
test1$totalFixations <- test1$duration * 50 # as 50 Hz was used
test1$totalFixations <- as.numeric(test1$totalFixations)
test1$totalFixations <- round(test1$totalFixations, digits = 0)

test <- merge(df_general_car, df_av_body, by = c("subject_id", "scenario_id", "repeated_exposure")) %>%
  merge(., df_av_ehmi, by = c("subject_id", "scenario_id", "repeated_exposure")) %>%
  merge(., test1, by = c("subject_id", "scenario_id", "repeated_exposure"))


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
  scale_colour_manual(values = wes_palette("Cavalcanti1", n = 11, type = "continuous")) +
  ylab("Percentage Fixation") +
  theme(legend.position = c(0.78, 0.58)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05), alpha = 0.5) +
  scale_x_discrete(labels = c("SCENARIO_1" = "Mixed traffic\nWith eHMI", "SCENARIO_2" = "Aut. traffic\nWith eHMI", "SCENARIO_3" = "Mixed traffic\nWithout eHMI", "SCENARIO_4" = "Aut. traffic\nWithout eHMI"))

data_long_1 <- subset(data_long, aoi != "Null")

data_long_1 %>% ggplot() +
  aes(x = scenario_id, y = measurement * 100, fill = aoi, colour = aoi, group = aoi) +
  scale_colour_see() +
  ylab("Percentage Fixation - Excluded Null") +
  theme(legend.position = c(0.9, 0.68)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, mapping = aes(shape = aoi)) +
  scale_shape_manual(values = 1:nlevels(data_long_1$aoi)) +
  stat_summary(fun = mean, geom = "line", size = 1, aes(linetype = aoi), alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05), alpha = 0.5) +
  scale_x_discrete(labels = c("SCENARIO_1" = "Mixed traffic\nWith eHMI", "SCENARIO_2" = "Aut. traffic\nWith eHMI", "SCENARIO_3" = "Mixed traffic\nWithout eHMI", "SCENARIO_4" = "Aut. traffic\nWithout eHMI"))
# ggsave("plots/Fixation_without_null.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


# remove when was not visible
data_long_1 <- subset(data_long_1, subset = measurement != 0)


data_long_1 %>% ggplot() +
  aes(x = scenario_id, y = measurement * 100, colour = aoi, group = aoi) +
  scale_colour_see() +
  ylab("Percentage Fixation\n Excluded Null") +
  theme(legend.position = c(0.9, 0.68)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, mapping = aes(shape = aoi)) +
  scale_shape_manual(values = 1:nlevels(data_long_1$aoi)) +
  stat_summary(fun = mean, geom = "line", size = 1, aes(linetype = aoi), alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05), alpha = 0.5) +
  scale_x_discrete(labels = c("SCENARIO_1" = "Mixed traffic\nWith eHMI", "SCENARIO_2" = "Aut. traffic\nWith eHMI", "SCENARIO_3" = "Mixed traffic\nWithout eHMI", "SCENARIO_4" = "Aut. traffic\nWithout eHMI"))
# ggsave("plots/Fixation_without_null_no_zero.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)




#### Position ####
library(gapminder)
options(max.print = 999999)

xPos_round <- round(main_Position$x_position, digits = 2)
zPos_round <- round(main_Position$z_position, digits = 2)
repeatedExp <- main_Position$rep_exposure

main_Position_reduced_overall <- data.frame(xPos_round, zPos_round)
main_Position_filterd_overall <- count(main_Position_reduced_overall, xPos_round, zPos_round)

# Position Checkpoint xPos = 92.95 || zPos = 158.29
# Position Pedestrian xPos = 93.7  || zPos = 170.20

main_Position_filterd_overall2 <- filter(main_Position_filterd_overall, xPos_round < 95 & n > 2 & zPos_round > 157.8)

main_Position_reduced_repExp <- data.frame(xPos_round, zPos_round, repeatedExp)
main_Position_filterd_repExp <- count(main_Position_reduced_repExp, xPos_round, zPos_round, repeatedExp)

main_Position_filterd_repExp_All <- filter(main_Position_filterd_repExp, xPos_round < 95 & n > 1 & zPos_round > 157.8)
p <- main_Position_filterd_repExp_All %>% ggplot() +
  aes(x = xPos_round, y = zPos_round, size = n, color = repeatedExp, fill = repeatedExp, group = repeatedExp) +
  geom_point(alpha = 0.3) +
  scale_size(range = c(.1, 15)) +
  theme(legend.position = "") +
  scale_y_reverse()

## Position: Repeated Exposure ONE ##
main_Position_filterd_repExp_One <- filter(main_Position_filterd_repExp, xPos_round < 95 & xPos_round > 92.5 & n > 1 & zPos_round > 157.8 & repeatedExp == "ONE")
main_Position_filterd_repExp_One %>% ggplot() +
  theme_bw(base_size = myfontsize - 10) +
  aes(x = xPos_round, y = zPos_round, size = n) +
  geom_point(alpha = 1, color = "#4575B4") +
  scale_size(range = c(.1, 15)) +
  theme(
    legend.position = "", axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks = element_blank()
  ) +
  scale_y_reverse() +
  geom_hline(yintercept = 168.78, color = "#A9A9A9") + # curb-start on starting point
  geom_hline(yintercept = 167, color = "#A9A9A9") + # curb-END on starting point
  geom_hline(yintercept = 163.62, linetype = "dashed", color = "#A9A9A9") + # median
  geom_hline(yintercept = 160.24, color = "#A9A9A9") + # curb-start at park
  geom_hline(yintercept = 158.54, color = "#A9A9A9") +
  xlab("Repeated Exposure: One") +
  ylab("Walking Direction →")


## Position: Repeated Exposure TWO ##
main_Position_filterd_repExp_Two <- filter(main_Position_filterd_repExp, xPos_round < 95 & n > 1 & zPos_round > 157.8 & repeatedExp == "TWO")
main_Position_filterd_repExp_Two %>% ggplot() +
  theme_bw(base_size = myfontsize - 10) +
  aes(x = xPos_round, y = zPos_round, size = n) +
  geom_point(alpha = 1, color = "#4575B4") +
  scale_size(range = c(.1, 15)) +
  theme(
    legend.position = "", axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks = element_blank()
  ) +
  scale_y_reverse() +
  geom_hline(yintercept = 168.78, color = "#A9A9A9") + # curb-start on starting point
  geom_hline(yintercept = 167, color = "#A9A9A9") + # curb-END on starting point
  geom_hline(yintercept = 163.62, linetype = "dashed", color = "#A9A9A9") + # median
  geom_hline(yintercept = 160.24, color = "#A9A9A9") + # curb-start at park
  geom_hline(yintercept = 158.54, color = "#A9A9A9") +
  xlab("Repeated Exposure: Two") +
  ylab("Walking Direction →")



## Position: Repeated Exposure THREE ##
main_Position_filterd_repExp_Three <- filter(main_Position_filterd_repExp, xPos_round < 94.7 & n > 1 & zPos_round > 157.8 & repeatedExp == "THREE")
main_Position_filterd_repExp_Three %>% ggplot() +
  theme_bw(base_size = myfontsize - 10) +
  aes(x = xPos_round, y = zPos_round, size = n) +
  geom_point(alpha = 1, color = "#4575B4") +
  scale_size(range = c(.1, 15)) +
  theme(
    legend.position = "", axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks = element_blank()
  ) +
  scale_y_reverse() +
  geom_hline(yintercept = 168.78, color = "#A9A9A9") + # curb-start on starting point
  geom_hline(yintercept = 167, color = "#A9A9A9") + # curb-END on starting point
  geom_hline(yintercept = 163.62, linetype = "dashed", color = "#A9A9A9") + # median
  geom_hline(yintercept = 160.24, color = "#A9A9A9") + # curb-start at park
  geom_hline(yintercept = 158.54, color = "#A9A9A9") +
  xlab("Repeated Exposure: Three") +
  ylab("Walking Direction →")

# @MARC

# install.packages("gifski")
# install.packages("gganimate")
library(gifski)
library(gganimate)
main_df_Pos_GGanimate <- rbind(main_Position_filterd_repExp_One, main_Position_filterd_repExp_Two, main_Position_filterd_repExp_Three)
plot_vr <- main_df_Pos_GGanimate %>% ggplot() +
  theme_bw(base_size = myfontsize - 10) +
  aes(x = xPos_round, y = zPos_round, size = n, color = repeatedExp, fill = repeatedExp, group = repeatedExp) +
  geom_point(alpha = 1) +
  scale_size(range = c(.1, 15)) +
  theme(
    legend.position = "", axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks = element_blank()
  ) +
  scale_y_reverse() +
  geom_hline(yintercept = 168.78, color = "#A9A9A9") + # curb-start on starting point
  geom_hline(yintercept = 167, color = "#A9A9A9") + # curb-END on starting point
  geom_hline(yintercept = 163.62, linetype = "dashed", color = "#A9A9A9") + # median
  geom_hline(yintercept = 160.24, color = "#A9A9A9") + # curb-start at park
  geom_hline(yintercept = 158.54, color = "#A9A9A9") +
  labs(title = "Repeated Exposure: {repeatedExp}", x = "Waiting Behavior", y = "Walking Direction →") +
  transition_reveal(as.numeric(repeatedExp)) +
  ease_aes("linear")
plot_vr



## Position: Walking Profile - Line Diagram ##


# Get custom file
mp_changed_new <- read_xlsx(path = "position_checkpoint_reached.xlsx", sheet = "position_checkpoint_reached")
mp_changed_new <- as.data.frame(mp_changed_new)
names(mp_changed_new)

options(digits.secs = 4)

options(digits = 6)
print(as.POSIXct(strptime(paste("00:00", mp_changed$total_duration_calc, sep = ":"), format = "%H:%M:%OS")))
repeatedExp_ld <- mp_changed$rep_exposure
subjectID_ld <- mp_changed$subject_id
condition_id_ld <- mp_changed$condition_id
xPos_round_ld <- round(mp_changed$x_position, digits = 2)
zPos_round_ld <- round(mp_changed$z_position, digits = 2)
total_duration_calc_ld <- mp_changed$total_duration_calc
date_ld <- as.POSIXct(strptime(mp_changed$T, format = "%Y-%m-%d %H:%M:%OS"))
print(date_ld)
mp_reduced_ld <- data.frame(xPos_round_ld, zPos_round_ld, repeatedExp_ld, subjectID_ld, condition_id_ld, date_ld)

## Position: Walking Profile - Scenario 1 & Repeated Exposure One ##
mp_filtered_ld_SC1_One <- filter(mp_reduced_ld, xPos_round_ld < 96 & xPos_round_ld > 92.5 & zPos_round_ld > 157.8 & repeatedExp_ld == "ONE" & condition_id_ld == "SCENARIO_1")
mp_filtered_ld_SC1_One <- mp_filtered_ld_SC1_One[!(mp_filtered_ld_SC1_One$subjectID_ld == 3 & strptime(mp_filtered_ld_SC1_One$date_ld, format = "%Y-%m-%d %H:%M:%OS") > strptime("2022-12-06 10:30:45.062", format = "%Y-%m-%d %H:%M:%OS")), ]
mp_filtered_ld_SC1_One %>% ggplot() +
  theme_bw(base_size = myfontsize - 10) +
  geom_rect(aes(xmin = 92.5, xmax = 94.5, ymin = 158.29, ymax = 160.29), color = NA, fill = "#F0F8FF", alpha = 1) + # Checkpoint
  aes(x = xPos_round_ld, y = zPos_round_ld, color = factor(subjectID_ld), fill = subjectID_ld, group = subjectID_ld) +
  geom_path() +
  ggtitle("Scenario 1: with eHMI & mixed traffic") +
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

## Position: Walking Profile - Scenario 2 & Repeated Exposure One ##
mp_filtered_ld_SC2_One <- filter(mp_reduced_ld, xPos_round_ld < 96 & xPos_round_ld > 92.5 & zPos_round_ld > 157.8 & repeatedExp_ld == "ONE" & condition_id_ld == "SCENARIO_2")
mp_filtered_ld_SC2_One <- mp_filtered_ld_SC2_One[!(mp_filtered_ld_SC2_One$subjectID_ld == 3 & strptime(mp_filtered_ld_SC2_One$date_ld, format = "%Y-%m-%d %H:%M:%OS") > strptime("2022-12-06 10:27:20.273", format = "%Y-%m-%d %H:%M:%OS")), ]
mp_filtered_ld_SC2_One %>% ggplot() +
  theme_bw(base_size = myfontsize - 10) +
  geom_rect(aes(xmin = 92.5, xmax = 94.5, ymin = 158.29, ymax = 160.29), color = NA, fill = "#F0F8FF", alpha = 1) + # Checkpoint
  aes(x = xPos_round_ld, y = zPos_round_ld, color = factor(subjectID_ld), fill = subjectID_ld, group = subjectID_ld) +
  geom_path() +
  ggtitle("Scenario 2: with eHMI & automated traffic") +
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

## Position: Walking Profile - Scenario 3 & Repeated Exposure One ##
mp_filtered_ld_SC3_One <- filter(mp_reduced_ld, xPos_round_ld < 96 & xPos_round_ld > 92.5 & zPos_round_ld > 157.8 & repeatedExp_ld == "ONE" & condition_id_ld == "SCENARIO_3")
mp_filtered_ld_SC3_One <- mp_filtered_ld_SC3_One[!(mp_filtered_ld_SC3_One$subjectID_ld == 9 & strptime(mp_filtered_ld_SC3_One$date_ld, format = "%Y-%m-%d %H:%M:%OS") > strptime("2022-12-09 11:34:53.986", format = "%Y-%m-%d %H:%M:%OS")), ]
mp_filtered_ld_SC3_One %>% ggplot() +
  theme_bw(base_size = myfontsize - 10) +
  geom_rect(aes(xmin = 92.5, xmax = 94.5, ymin = 158.29, ymax = 160.29), color = NA, fill = "#F0F8FF", alpha = 1) + # Checkpoint
  aes(x = xPos_round_ld, y = zPos_round_ld, color = factor(subjectID_ld), fill = subjectID_ld, group = subjectID_ld) +
  geom_path() +
  ggtitle("Scenario 3: without eHMI & mixed traffic") +
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

## Position: Walking Profile - Scenario 4 & Repeated Exposure One ##
mp_filtered_ld_SC4_One <- filter(mp_reduced_ld, xPos_round_ld < 96 & xPos_round_ld > 92.5 & zPos_round_ld > 158.54 & repeatedExp_ld == "ONE" & condition_id_ld == "SCENARIO_4")
mp_filtered_ld_SC4_One %>% ggplot() +
  theme_bw(base_size = myfontsize - 10) +
  geom_rect(aes(xmin = 92.5, xmax = 94.5, ymin = 158.29, ymax = 160.29), color = NA, fill = "#F0F8FF", alpha = 1) + # Checkpoint
  aes(x = xPos_round_ld, y = zPos_round_ld, color = factor(subjectID_ld), fill = subjectID_ld, group = subjectID_ld) +
  geom_path() +
  ggtitle("Scenario 4: without eHMI & automated traffic") +
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


















if (FALSE) {
  main_Position <- read.csv(filesPosition[1], sep = ";", row.names = NULL)
  main_Position <- main_Position[0, ]

  for (p in filesPosition) {
    print(p)
    test <- read.csv(p, sep = ";", row.names = NULL)
    main_Position <- rbind(main_Position, test)
  }



  main_Total <- read.csv(filesTotal[1], sep = ";", row.names = NULL)
  main_Total <- main_Total[0, ]



  for (p in filesTotal) {
    print(p)
    test <- read.csv(p, sep = ";", row.names = NULL)
    main_Total <- rbind(main_Total, test)
  }
}
