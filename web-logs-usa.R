# ReadMe: use kornmueller-web-logs.R first!!

## Position: Repeated Exposure One - Line Diagram ##
options(digits.secs = 4)
repeatedExp_ld_usa <- df_position_usa$rep_exposure
subjectID_ld_usa <- df_position_usa$subject_id
condition_id_ld <- df_position_usa$condition_id
xPos_round_ld_usa <- round(df_position_usa$x_position, digits = 2)
zPos_round_ld_usa <- round(df_position_usa$z_position, digits = 2)
total_duration_calc_ld_usa <- df_position_usa$total_duration_calc
mp_reduced_ld_usa <- data.frame(xPos_round_ld_usa, zPos_round_ld_usa, repeatedExp_ld_usa, subjectID_ld_usa, condition_id_ld)

## USA Scenario_2
mp_filtered_ld_usa_SC1_One <- filter(mp_reduced_ld_usa, xPos_round_ld_usa > 92.5 & repeatedExp_ld_usa == "ONE" & condition_id_ld == "SCENARIO_2")
mp_filtered_ld_usa_SC1_One %>% ggplot() +
  geom_rect(aes(xmin = 90.5, xmax = 94.5, ymin = 158.29, ymax = 160.4), color = NA, fill = "#F0F8FF", alpha = 1) + # Checkpoint
  aes(x = xPos_round_ld_usa, y = zPos_round_ld_usa, color = factor(subjectID_ld_usa), fill = subjectID_ld_usa, group = subjectID_ld_usa) +
  scale_color_see() +
  geom_path() +
  ggtitle("Scenario 1: with eHMI & USA") +
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
ggsave("plots/result_walking_behavior_sc2_rep1_usa.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


mp_filtered_ld_usa_SC2_One <- filter(mp_reduced_ld_usa, xPos_round_ld_usa > 92.5 & zPos_round_ld_usa > 157.8 & repeatedExp_ld_usa == "ONE" & condition_id_ld == "SCENARIO_4")
mp_filtered_ld_usa_SC2_One %>% ggplot() +
  # geom_circle(aes(x0 = 92.95, y0 = 158.24, r = 2)) +
  geom_rect(aes(xmin = 90.5, xmax = 94.5, ymin = 158.29, ymax = 160.4), color = NA, fill = "#F0F8FF", alpha = 1) + # Checkpoint
  aes(x = xPos_round_ld_usa, y = zPos_round_ld_usa, color = factor(subjectID_ld_usa), fill = subjectID_ld_usa, group = subjectID_ld_usa) +
  geom_path() +
  scale_color_see() +
  ggtitle("Scenario 2: without eHMI & USA") +
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
ggsave("plots/result_walking_behavior_sc4_rep1_usa.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


mp_filtered_ld_usa_SC1_Two <- filter(mp_reduced_ld_usa, zPos_round_ld_usa > 157.8 & repeatedExp_ld_usa == "TWO" & condition_id_ld == "SCENARIO_2")
mp_filtered_ld_usa_SC1_Two %>% ggplot() +

  # geom_circle(aes(x0 = 92.95, y0 = 158.24, r = 2)) +
  geom_rect(aes(xmin = 90.5, xmax = 94.5, ymin = 158.29, ymax = 160.4), color = NA, fill = "#F0F8FF", alpha = 1) + # Checkpoint
  aes(x = xPos_round_ld_usa, y = zPos_round_ld_usa, color = factor(subjectID_ld_usa), fill = subjectID_ld_usa, group = subjectID_ld_usa) +
  geom_path() +
  ggtitle("Scenario 1: with eHMI & USA") +
  theme(
    legend.position = "", axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_reverse() +
  scale_color_see() +
  geom_hline(yintercept = 168.78, color = "#A9A9A9") + # curb-start on starting point
  geom_hline(yintercept = 167, color = "#A9A9A9") + # curb-END on starting point
  geom_hline(yintercept = 163.62, linetype = "dashed", color = "#A9A9A9") + # median
  geom_hline(yintercept = 160.24, color = "#A9A9A9") + # curb-start at park
  geom_hline(yintercept = 158.54, color = "#A9A9A9") +
  xlab("Repeated Exposure: Two") +
  ylab("Walking Direction →")
ggsave("plots/result_walking_behavior_sc2_rep2_usa.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


mp_filtered_ld_usa_SC2_Two <- filter(mp_reduced_ld_usa, zPos_round_ld_usa > 157.8 & repeatedExp_ld_usa == "TWO" & condition_id_ld == "SCENARIO_4")
mp_filtered_ld_usa_SC2_Two %>% ggplot() +

  # geom_circle(aes(x0 = 92.95, y0 = 158.24, r = 2)) +
  geom_rect(aes(xmin = 90.5, xmax = 94.5, ymin = 158.29, ymax = 160.4), color = NA, fill = "#F0F8FF", alpha = 1) + # Checkpoint
  aes(x = xPos_round_ld_usa, y = zPos_round_ld_usa, color = factor(subjectID_ld_usa), fill = subjectID_ld_usa, group = subjectID_ld_usa) +
  geom_path() +
  ggtitle("Scenario 2: without eHMI & USA") +
  theme(
    legend.position = "", axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_reverse() +
  scale_color_see() +
  geom_hline(yintercept = 168.78, color = "#A9A9A9") + # curb-start on starting point
  geom_hline(yintercept = 167, color = "#A9A9A9") + # curb-END on starting point
  geom_hline(yintercept = 163.62, linetype = "dashed", color = "#A9A9A9") + # median
  geom_hline(yintercept = 160.24, color = "#A9A9A9") + # curb-start at park
  geom_hline(yintercept = 158.54, color = "#A9A9A9") +
  xlab("Repeated Exposure: Two") +
  ylab("Walking Direction →")
ggsave("plots/result_walking_behavior_sc4_rep2_usa.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)



mp_filtered_ld_usa_SC1_Three <- filter(mp_reduced_ld_usa, repeatedExp_ld_usa == "THREE" &
  condition_id_ld == "SCENARIO_2" &
  subjectID_ld_usa == "8f0b5705-a485-4743-9e1c-8790cad22f64")

mp_filtered_ld_usa_SC1_Three <- filter(mp_reduced_ld_usa, repeatedExp_ld_usa == "THREE" & zPos_round_ld_usa > 157.8 &
  condition_id_ld == "SCENARIO_2" & subjectID_ld_usa != "8f0b5705-a485-4743-9e1c-8790cad22f64" & subjectID_ld_usa != "32de6da0-401d-4cdb-9fbc-e831a21db71a")
mp_filtered_ld_usa_SC1_Three %>% ggplot() +

  #  geom_circle(aes(x0 = 92.95, y0 = 158.24, r = 2)) +
  geom_rect(aes(xmin = 92.5, xmax = 94.5, ymin = 158.29, ymax = 160.29), color = NA, fill = "#F0F8FF", alpha = 1) + # Checkpoint
  aes(x = xPos_round_ld_usa, y = zPos_round_ld_usa, color = factor(subjectID_ld_usa), fill = subjectID_ld_usa, group = subjectID_ld_usa) +
  geom_path() +
  ggtitle("Scenario 1: with eHMI & USA") +
  theme(
    legend.position = "", axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_reverse() +
  scale_color_see() +
  geom_hline(yintercept = 168.78, color = "#A9A9A9") + # curb-start on starting point
  geom_hline(yintercept = 167, color = "#A9A9A9") + # curb-END on starting point
  geom_hline(yintercept = 163.62, linetype = "dashed", color = "#A9A9A9") + # median
  geom_hline(yintercept = 160.24, color = "#A9A9A9") + # curb-start at park
  geom_hline(yintercept = 158.54, color = "#A9A9A9") +
  xlab("Repeated Exposure: Three") +
  ylab("Walking Direction →")
ggsave("plots/result_walking_behavior_sc2_rep3_usa.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)


# mp_filtered_ld_usa_SC2_Three <- filter(mp_reduced_ld_usa, xPos_round_ld_usa < 96 & xPos_round_ld_usa > 92.5  & zPos_round_ld_usa > 157.8
#                                 & repeatedExp_ld_usa == 'TWO' & condition_id_ld == "SCENARIO_4")
mp_filtered_ld_usa_SC2_Three <- filter(mp_reduced_ld_usa, repeatedExp_ld_usa == "THREE" & condition_id_ld == "SCENARIO_4" &
  subjectID_ld_usa != "a2467bc7-59c2-4f1f-9499-588aff62b76c")
mp_filtered_ld_usa_SC2_Three %>% ggplot() +

  # geom_circle(aes(x0 = 92.95, y0 = 158.24, r = 2)) +
  geom_rect(aes(xmin = 90.5, xmax = 94.5, ymin = 158.29, ymax = 160.4), color = NA, fill = "#F0F8FF", alpha = 1) + # Checkpoint
  aes(x = xPos_round_ld_usa, y = zPos_round_ld_usa, color = factor(subjectID_ld_usa), fill = subjectID_ld_usa, group = subjectID_ld_usa) +
  geom_path() +
  ggtitle("Scenario 2: without eHMI & USA") +
  theme(
    legend.position = "", axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_reverse() +
  scale_color_see() +
  geom_hline(yintercept = 168.78, color = "#A9A9A9") + # curb-start on starting point
  geom_hline(yintercept = 167, color = "#A9A9A9") + # curb-END on starting point
  geom_hline(yintercept = 163.62, linetype = "dashed", color = "#A9A9A9") + # median
  geom_hline(yintercept = 160.24, color = "#A9A9A9") + # curb-start at park
  geom_hline(yintercept = 158.54, color = "#A9A9A9") +
  xlab("Repeated Exposure: Three") +
  ylab("Walking Direction →")
ggsave("plots/result_walking_behavior_sc4_rep3_usa.pdf", width = pdfwidth, height = pdfheight + 2, device = cairo_pdf)

# Position Checkpoint xPos = 92.95 || zPos = 158.29
