# ATTENTION 
# ReadMe: use vr-log.R first!!

## Position: Walking Profile - Scenario 1 & Repeated Exposure Three ##
mp_filtered_ld_SC1_Three <- filter(mp_reduced_ld, xPos_round_ld < 96 & xPos_round_ld > 92.5 & zPos_round_ld > 157.8 & repeatedExp_ld == "THREE" & condition_id_ld == "SCENARIO_1")
mp_filtered_ld_SC1_Three <- mp_filtered_ld_SC1_Three[!(mp_filtered_ld_SC1_Three$subjectID_ld == 3 & strptime(mp_filtered_ld_SC1_Three$date_ld, format = "%Y-%m-%d %H:%M:%OS") > strptime("2022-12-06 10:30:45.062", format = "%Y-%m-%d %H:%M:%OS")), ]
mp_filtered_ld_SC1_Three %>% ggplot() +
  theme_bw(base_size = myfontsize - 10) +
  geom_rect(aes(xmin = 92.5, xmax = 94.5, ymin = 158.29, ymax = 160.29), color = NA, fill = "#F0F8FF", alpha = 1) + # Checkpoint
  aes(x = xPos_round_ld, y = zPos_round_ld, color = factor(subjectID_ld), fill = subjectID_ld, group = subjectID_ld) +
  geom_path() +
  # theme(legend.position="")
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
  xlab("Repeated Exposure: Three") +
  ylab("Walking Direction →")

## Position: Walking Profile - Scenario 2 & Repeated Exposure Three ##
mp_filtered_ld_SC2_Three <- filter(mp_reduced_ld, xPos_round_ld < 96 & xPos_round_ld > 92.5 & zPos_round_ld > 157.8 & repeatedExp_ld == "THREE" & condition_id_ld == "SCENARIO_2")
mp_filtered_ld_SC2_Three <- mp_filtered_ld_SC2_Three[!(mp_filtered_ld_SC2_Three$subjectID_ld == 3 & strptime(mp_filtered_ld_SC2_Three$date_ld, format = "%Y-%m-%d %H:%M:%OS") > strptime("2022-12-06 10:27:20.273", format = "%Y-%m-%d %H:%M:%OS")), ]
mp_filtered_ld_SC2_Three %>% ggplot() +
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
  xlab("Repeated Exposure: Three") +
  ylab("Walking Direction →")

## Position: Walking Profile - Scenario 3 & Repeated Exposure Three ##
mp_filtered_ld_SC3_Three <- filter(mp_reduced_ld, xPos_round_ld < 96 & xPos_round_ld > 92.5 & zPos_round_ld > 157.8 & repeatedExp_ld == "THREE" & condition_id_ld == "SCENARIO_3")
mp_filtered_ld_SC3_Three <- mp_filtered_ld_SC3_Three[!(mp_filtered_ld_SC3_Three$subjectID_ld == 9 & strptime(mp_filtered_ld_SC3_Three$date_ld, format = "%Y-%m-%d %H:%M:%OS") > strptime("2022-12-09 11:34:53.986", format = "%Y-%m-%d %H:%M:%OS")), ]
mp_filtered_ld_SC3_Three %>% ggplot() +
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
  xlab("Repeated Exposure: Three") +
  ylab("Walking Direction →")

## Position: Walking Profile - Scenario 4 & Repeated Exposure Three ##
mp_filtered_ld_SC4_Three <- filter(mp_reduced_ld, xPos_round_ld < 96 & xPos_round_ld > 92.5 & zPos_round_ld > 157.8 & repeatedExp_ld == "THREE" & condition_id_ld == "SCENARIO_4")
mp_filtered_ld_SC4_Three <- mp_filtered_ld_SC4_Three[!(mp_filtered_ld_SC4_Three$subjectID_ld == 10 & strptime(mp_filtered_ld_SC4_Three$date_ld, format = "%Y-%m-%d %H:%M:%OS") > strptime("2022-12-13 01:28:40.411", format = "%Y-%m-%d %H:%M:%OS")), ]
mp_filtered_ld_SC4_Three %>% ggplot() +
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
  xlab("Repeated Exposure: Three") +
  ylab("Walking Direction →")
