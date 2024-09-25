# first: kornmueller_web.R

#### Germany: VR-Study & Online-Based ####
##### Mental Workload #####
anovaBF(TLX1 ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "TLX1")

anovaBF(TLX1 ~ ehmi * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
main_df_german %>% ggplot() +
  aes(x = study, y = TLX1, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Mental Workload") +
  theme(legend.position.inside = c(0.75, 0.84)) +
  xlab("Study Designs") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  scale_x_discrete(labels = c("WEB" = "Online-Based", "VR" = "VR")) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))

anovaBF(TLX1 ~ repExposure * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
temp <- main_df_german
temp$study <- gsub("WEB", "Online-Based", temp$study)
temp %>% ggplot() +
  aes(x = repExposure, y = TLX1, fill = study, colour = study, group = study) +
  scale_color_see() +
  ylab("Mental Workload") +
  # scale_fill_discrete(labels=c("WEB" = "Online-Based", "VR" = "VR")) +
  theme(legend.position.inside = c(0.75, 0.9)) +
  xlab("Repeated Expsoure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))



##### overallTiAUnderstanding #####
anovaBF(overallTiAUnderstanding ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
anovaBF(overallTiAUnderstanding ~ ehmi * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
anovaBF(overallTiAUnderstanding ~ repExposure * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
main_df_german %>% ggplot() +
  aes(x = study, y = overallTiAUnderstanding, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Understanding") +
  theme(legend.position.inside = c(0.3, 0.84)) +
  xlab("Study Designs") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  scale_x_discrete(labels = c("WEB" = "Online-Based", "VR" = "VR")) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))

temp <- main_df_german
temp$study <- gsub("WEB", "Online-Based", temp$study)
temp %>% ggplot() +
  aes(x = repExposure, y = overallTiAUnderstanding, fill = study, colour = study, group = study) +
  scale_color_see() +
  ylab("Understanding") +
  # scale_fill_discrete(labels=c("WEB" = "Online-Based", "VR" = "VR")) +
  theme(legend.position.inside = c(0.27, 0.9)) +
  xlab("Repeated Expsoure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))


##### overallTiATrust#####
anovaBF(overallTiATrust ~ ehmi * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
anovaBF(overallTiATrust ~ repExposure * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
main_df_german %>% ggplot() +
  aes(x = study, y = overallTiATrust, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Trust") +
  theme(legend.position.inside = c(0.3, 0.84)) +
  xlab("Study Designs") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  scale_x_discrete(labels = c("WEB" = "Online-Based", "VR" = "VR")) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))

temp <- main_df_german
temp$study <- gsub("WEB", "Online-Based", temp$study)
temp %>% ggplot() +
  aes(x = repExposure, y = overallTiATrust, fill = study, colour = study, group = study) +
  scale_color_see() +
  ylab("Trust") +
  # scale_fill_discrete(labels=c("WEB" = "Online-Based", "VR" = "VR")) +
  theme(legend.position.inside = c(0.27, 0.9)) +
  xlab("Repeated Expsoure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))


##### ps_score #####
anovaBF(ps_score ~ ehmi * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
anovaBF(ps_score ~ repExposure * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
main_df_german %>% ggplot() +
  aes(x = study, y = ps_score, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Perceived Safety") +
  theme(legend.position.inside = c(0.3, 0.84)) +
  xlab("Study Designs") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  scale_x_discrete(labels = c("WEB" = "Online-Based", "VR" = "VR")) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))

temp <- main_df_german
temp$study <- gsub("WEB", "Online-Based", temp$study)
temp %>% ggplot() +
  aes(x = repExposure, y = ps_score, fill = study, colour = study, group = study) +
  scale_color_see() +
  ylab("Perceived Safety") +
  # scale_fill_discrete(labels=c("WEB" = "Online-Based", "VR" = "VR")) +
  theme(legend.position.inside = c(0.27, 0.9)) +
  xlab("Repeated Expsoure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))


##### Crossing Decision: Speed#####
anovaBF(crossingDecisionSpeed ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "crossingDecisionSpeed")
anovaBF(crossingDecisionSpeed ~ ehmi * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
anovaBF(crossingDecisionSpeed ~ repExposure * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
main_df_german %>% ggplot() +
  aes(x = study, y = crossingDecisionSpeed, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Crossing Decision\nSpeed") +
  theme(legend.position.inside = c(0.3, 0.84)) +
  xlab("Study Designs") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  scale_x_discrete(labels = c("WEB" = "Online-Based", "VR" = "VR")) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))

temp <- main_df_german
temp$study <- gsub("WEB", "Online-Based", temp$study)
temp %>% ggplot() +
  aes(x = repExposure, y = crossingDecisionSpeed, fill = study, colour = study, group = study) +
  scale_color_see() +
  ylab("Crossing Decision\nSpeed") +
  # scale_fill_discrete(labels=c("WEB" = "Online-Based", "VR" = "VR")) +
  theme(legend.position.inside = c(0.2, 0.95)) +
  xlab("Repeated Expsoure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))


##### Crossing Decision: Distance#####
anovaBF(crossingDecisionDist ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "crossingDecisionDist")
anovaBF(crossingDecisionDist ~ ehmi * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
anovaBF(crossingDecisionDist ~ repExposure * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
main_df_german %>% ggplot() +
  aes(x = study, y = crossingDecisionDist, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Crossing Decision\nDistance") +
  theme(legend.position.inside = c(0.7, 0.25)) +
  xlab("Study Designs") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  scale_x_discrete(labels = c("WEB" = "Online-Based", "VR" = "VR")) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))

temp <- main_df_german
temp$study <- gsub("WEB", "Online-Based", temp$study)
temp %>% ggplot() +
  aes(x = repExposure, y = crossingDecisionDist, fill = study, colour = study, group = study) +
  scale_color_see() +
  ylab("Crossing Decision\nDistance") +
  # scale_fill_discrete(labels=c("WEB" = "Online-Based", "VR" = "VR")) +
  theme(legend.position.inside = c(0.25, 0.25)) +
  xlab("Repeated Expsoure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))


##### Crossing Decision: Communication#####
anovaBF(crossingDecisionComm ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "crossingDecisionComm")
anovaBF(crossingDecisionComm ~ ehmi * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
anovaBF(crossingDecisionComm ~ repExposure * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
main_df_german %>% ggplot() +
  aes(x = study, y = crossingDecisionComm, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Crossing Decision\nCommunication") +
  theme(legend.position.inside = c(0.7, 0.25)) +
  xlab("Study Designs") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  scale_x_discrete(labels = c("WEB" = "Online-Based", "VR" = "VR")) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))

temp <- main_df_german
temp$study <- gsub("WEB", "Online-Based", temp$study)
temp %>% ggplot() +
  aes(x = repExposure, y = crossingDecisionComm, fill = study, colour = study, group = study) +
  scale_color_see() +
  ylab("Crossing Decision\nCommunication") +
  # scale_fill_discrete(labels=c("WEB" = "Online-Based", "VR" = "VR")) +
  theme(legend.position.inside = c(0.25, 0.85)) +
  xlab("Repeated Expsoure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))


##### Crossing Decision: Own waiting Time#####
anovaBF(crossingDecisionOwt ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "crossingDecisionOwt")
anovaBF(crossingDecisionOwt ~ ehmi * study, whichRandom = "crossingDecisionOwt", data = main_df_german) |>
  bayesfactor_models() |>
  report()
anovaBF(crossingDecisionOwt ~ repExposure * study, whichRandom = "crossingDecisionOwt", data = main_df_german) |>
  bayesfactor_models() |>
  report()
main_df_german %>% ggplot() +
  aes(x = study, y = crossingDecisionOwt, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Crossing Decision\nOwn Waiting Time") +
  theme(legend.position.inside = c(0.7, 0.1)) +
  xlab("Study Designs") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  scale_x_discrete(labels = c("WEB" = "Online-Based", "VR" = "VR")) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))

temp <- main_df_german
temp$study <- gsub("WEB", "Online-Based", temp$study)
temp %>% ggplot() +
  aes(x = repExposure, y = crossingDecisionOwt, fill = study, colour = study, group = study) +
  scale_color_see() +
  ylab("Crossing Decision\nOwn Waiting Time") +
  # scale_fill_discrete(labels=c("WEB" = "Online-Based", "VR" = "VR")) +
  theme(legend.position.inside = c(0.75, 0.85)) +
  xlab("Repeated Expsoure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))

#### Presence#####
anovaBF(presence ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "presence")
anovaBF(presence ~ ehmi * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
anovaBF(presence ~ repExposure * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
main_df_german %>% ggplot() +
  aes(x = study, y = presence, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Presence") +
  theme(legend.position.inside = c(0.25, 0.1)) +
  xlab("Study Designs") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  scale_x_discrete(labels = c("WEB" = "Online-Based", "VR" = "VR")) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))

temp <- main_df_german
temp$study <- gsub("WEB", "Online-Based", temp$study)
temp %>% ggplot() +
  aes(x = repExposure, y = presence, fill = study, colour = study, group = study) +
  scale_color_see() +
  ylab("Presence") +
  # scale_fill_discrete(labels=c("WEB" = "Online-Based", "VR" = "VR")) +
  theme(legend.position.inside = c(0.5, 0.85)) +
  xlab("Repeated Expsoure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))


##### pragmatic_quality #####
anovaBF(pragmatic_quality ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "pragmatic_quality")
anovaBF(pragmatic_quality ~ ehmi * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
anovaBF(pragmatic_quality ~ repExposure * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
main_df_german %>% ggplot() +
  aes(x = study, y = pragmatic_quality, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Pragmatic Quality") +
  theme(legend.position.inside = c(0.25, 0.75)) +
  xlab("Study Designs") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  scale_x_discrete(labels = c("WEB" = "Online-Based", "VR" = "VR")) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))

temp <- main_df_german
temp$study <- gsub("WEB", "Online-Based", temp$study)
temp %>% ggplot() +
  aes(x = repExposure, y = pragmatic_quality, fill = study, colour = study, group = study) +
  scale_color_see() +
  ylab("Pragmatic Quality") +
  # scale_fill_discrete(labels=c("WEB" = "Online-Based", "VR" = "VR")) +
  theme(legend.position.inside = c(0.25, 0.93)) +
  xlab("Repeated Expsoure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))

##### hedonic_quality#####
anovaBF(hedonic_quality ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "hedonic_quality")
anovaBF(hedonic_quality ~ ehmi * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
anovaBF(hedonic_quality ~ repExposure * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
main_df_german %>% ggplot() +
  aes(x = study, y = hedonic_quality, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Hedonic Quality") +
  theme(legend.position.inside = c(0.25, 0.85)) +
  xlab("Study Designs") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  scale_x_discrete(labels = c("WEB" = "Online-Based", "VR" = "VR")) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))

temp <- main_df_german
temp$study <- gsub("WEB", "Online-Based", temp$study)
temp %>% ggplot() +
  aes(x = repExposure, y = hedonic_quality, fill = study, colour = study, group = study) +
  scale_color_see() +
  ylab("Hedonic Quality") +
  # scale_fill_discrete(labels=c("WEB" = "Online-Based", "VR" = "VR")) +
  theme(legend.position.inside = c(0.25, 0.93)) +
  xlab("Repeated Expsoure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))


##### comEval2 Politness#####
anovaBF(comEval2 ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "comEval2")
anovaBF(comEval2 ~ ehmi * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
anovaBF(comEval2 ~ repExposure * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
main_df_german %>% ggplot() +
  aes(x = study, y = comEval2, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Politeness") +
  theme(legend.position.inside = c(0.75, 0.9)) +
  xlab("Study Designs") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  scale_x_discrete(labels = c("WEB" = "Online-Based", "VR" = "VR")) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))

temp <- main_df_german
temp$study <- gsub("WEB", "Online-Based", temp$study)
temp %>% ggplot() +
  aes(x = repExposure, y = comEval2, fill = study, colour = study, group = study) +
  scale_color_see() +
  ylab("Politeness") +
  # scale_fill_discrete(labels=c("WEB" = "Online-Based", "VR" = "VR")) +
  theme(legend.position.inside = c(0.75, 0.85)) +
  xlab("Repeated Expsoure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))

##### comEval3 Machine-Like#####
anovaBF(comEval3 ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "comEval3")
anovaBF(comEval3 ~ ehmi * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
anovaBF(comEval3 ~ repExposure * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
main_df_german %>% ggplot() +
  aes(x = study, y = comEval3, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Machine-Likness") +
  theme(legend.position.inside = c(0.75, 0.9)) +
  xlab("Study Designs") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  scale_x_discrete(labels = c("WEB" = "Online-Based", "VR" = "VR")) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))

temp <- main_df_german
temp$study <- gsub("WEB", "Online-Based", temp$study)
temp %>% ggplot() +
  aes(x = repExposure, y = comEval3, fill = study, colour = study, group = study) +
  scale_color_see() +
  ylab("Machine-Likness") +
  # scale_fill_discrete(labels=c("WEB" = "Online-Based", "VR" = "VR")) +
  theme(legend.position.inside = c(0.25, 0.85)) +
  xlab("Repeated Expsoure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))

##### comEval4 clear#####
anovaBF(comEval4 ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "comEval4")
anovaBF(comEval4 ~ ehmi * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
anovaBF(comEval4 ~ repExposure * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
main_df_german %>% ggplot() +
  aes(x = study, y = comEval4, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Clear") +
  theme(legend.position.inside = c(0.25, 0.9)) +
  xlab("Study Designs") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  scale_x_discrete(labels = c("WEB" = "Online-Based", "VR" = "VR")) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))

temp <- main_df_german
temp$study <- gsub("WEB", "Online-Based", temp$study)
temp %>% ggplot() +
  aes(x = repExposure, y = comEval4, fill = study, colour = study, group = study) +
  scale_color_see() +
  ylab("Clear") +
  # scale_fill_discrete(labels=c("WEB" = "Online-Based", "VR" = "VR")) +
  theme(legend.position.inside = c(0.25, 0.85)) +
  xlab("Repeated Expsoure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))

##### comEval5 Naturalness#####
anovaBF(comEval5 ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "comEval5")
anovaBF(comEval5 ~ ehmi * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
anovaBF(comEval5 ~ repExposure * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
main_df_german %>% ggplot() +
  aes(x = study, y = comEval5, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Naturalness") +
  theme(legend.position.inside = c(0.25, 0.9)) +
  xlab("Study Designs") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  scale_x_discrete(labels = c("WEB" = "Online-Based", "VR" = "VR")) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))

temp <- main_df_german
temp$study <- gsub("WEB", "Online-Based", temp$study)
temp %>% ggplot() +
  aes(x = repExposure, y = comEval5, fill = study, colour = study, group = study) +
  scale_color_see() +
  ylab("Naturalness ") +
  # scale_fill_discrete(labels=c("WEB" = "Online-Based", "VR" = "VR")) +
  theme(legend.position.inside = c(0.25, 0.85)) +
  xlab("Repeated Expsoure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))

##### comEval6 appropriateness#####
anovaBF(comEval6 ~ study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
reportMeanAndSD(data = main_df_german, iv = "study", dv = "comEval6")
anovaBF(comEval6 ~ ehmi * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
anovaBF(comEval6 ~ repExposure * study, whichRandom = "probandenID", data = main_df_german) |>
  bayesfactor_models() |>
  report()
main_df_german %>% ggplot() +
  aes(x = study, y = comEval6, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Appropriateness") +
  theme(legend.position.inside = c(0.25, 0.9)) +
  xlab("Study Designs") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  scale_x_discrete(labels = c("WEB" = "Online-Based", "VR" = "VR")) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))

temp <- main_df_german
temp$study <- gsub("WEB", "Online-Based", temp$study)
temp %>% ggplot() +
  aes(x = repExposure, y = comEval6, fill = study, colour = study, group = study) +
  scale_color_see() +
  ylab("Appropriateness ") +
  # scale_fill_discrete(labels=c("WEB" = "Online-Based", "VR" = "VR")) +
  theme(legend.position.inside = c(0.25, 0.25)) +
  xlab("Repeated Expsoure") +
  stat_summary(fun = mean, geom = "point", size = 4.0) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .25, position = position_dodge(width = 0.05))
