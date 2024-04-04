library(dplyr)
library(forestplot)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(cowplot)
pdf("plots/forest/Passing_forest.pdf", 
    height = 9, 
    title = "Forest Plot of Passing Specifications")
Passing_Results %>% 
  mutate(Lower = Coefficient - 1.96 * SE, 
         Upper = Coefficient + 1.96 * SE) %>%
  forestplot(
    labeltext = Dependent_Variable,
    mean = Coefficient,
    lower = Lower,
    upper = Upper,
    is.summary = F,
    hrzl_lines = list(
      "2" = gpar(lty = 1, col = "black")
    ),
    xlab = "Coefficient Values",
    title = "Forest Plots of Passing Specifications"
  ) %>%
  fp_add_header("Dependent Variable") |>
  fp_set_style(default = gpar(lineend = "round", linejoin = "mitre", lwd = 2, col = "black"),
               box = gpar(fill = "#6D84A1", col = "#BAD1E4"), 
               lines = list(gpar(lwd = 2, col = "orange")), 
               summary = list(
                 gpar(fill = "violet", col = "gray", lwd = 10)
               ))
dev.off()

pdf("plots/forest/Possession_forest.pdf", 
    height = 9, 
    title = "Forest Plot of Possession Specifications")
Possession_Results %>% 
  mutate(Lower = Coefficient - 1.96 * SE, 
         Upper = Coefficient + 1.96 * SE) %>%
  forestplot(
    labeltext = Dependent_Variable,
    mean = Coefficient,
    lower = Lower,
    upper = Upper,
    is.summary = F,
    hrzl_lines = list(
      "2" = gpar(lty = 1, col = "black")
    ),
    xlab = "Coefficient Values",
    title = "Forest Plots of Possession Specifications"
  ) %>%
  fp_add_header("Dependent Variable") |>
  fp_set_style(default = gpar(lineend = "round", linejoin = "mitre", lwd = 2, col = "black"),
               box = gpar(fill = "#6D84A1", col = "#BAD1E4"), 
               lines = list(gpar(lwd = 2, col = "orange")), 
               summary = list(
                 gpar(fill = "violet", col = "gray", lwd = 10)
               ))
dev.off()

pdf("plots/forest/Defend_forest.pdf", 
    height = 9, 
    title = "Forest Plot of Defend Specifications")
Defend_Results %>% 
  mutate(Lower = Coefficient - 1.96 * SE, 
         Upper = Coefficient + 1.96 * SE) %>%
  forestplot(
    labeltext = Dependent_Variable,
    mean = Coefficient,
    lower = Lower,
    upper = Upper,
    is.summary = F,
    hrzl_lines = list(
      "2" = gpar(lty = 1, col = "black")
    ),
    xlab = "Coefficient Values",
    title = "Forest Plots of Defensive Action Specifications"
  ) %>%
  fp_add_header("Dependent Variable") |>
  fp_set_style(default = gpar(lineend = "round", linejoin = "mitre", lwd = 2, col = "black"),
               box = gpar(fill = "#6D84A1", col = "#BAD1E4"), 
               lines = list(gpar(lwd = 2, col = "orange")), 
               summary = list(
                 gpar(fill = "violet", col = "gray", lwd = 10)
               ))
dev.off()

pdf("plots/forest/Shot_forest.pdf", 
    height = 9, 
    title = "Forest Plot of Shot Specifications")
Shot_Results %>% 
  mutate(Lower = Coefficient - 1.96 * SE, 
         Upper = Coefficient + 1.96 * SE) %>%
  forestplot(
    labeltext = Dependent_Variable,
    mean = Coefficient,
    lower = Lower,
    upper = Upper,
    is.summary = F,
    hrzl_lines = list(
      "2" = gpar(lty = 1, col = "black")
    ),
    xlab = "Coefficient Values",
    title = "Forest Plots of Shot Specifications"
  ) %>%
  fp_add_header("Dependent Variable") |>
  fp_set_style(default = gpar(lineend = "round", linejoin = "mitre", lwd = 2, col = "black"),
               box = gpar(fill = "#6D84A1", col = "#BAD1E4"), 
               lines = list(gpar(lwd = 2, col = "orange")), 
               summary = list(
                 gpar(fill = "violet", col = "gray", lwd = 10)
               ))
dev.off()

#Player hetero forestplot
#is-Eng
Eng_Results_C <- bind_rows(Eng_Results_0 %>% 
                             mutate(Dependent_Variable = Eng_Results_1$Dependent_Variable,
                                    group = "non-Eng"), 
                           Eng_Results_1 %>%
                             mutate(group = "Eng")) %>%
  mutate(Lower = Coefficient - 1.96 * SE, 
         Upper = Coefficient + 1.96 * SE) %>%
  group_by(group)
pdf("plots/forest/Eng_plot1.pdf", height = 9, title = "ENG Hetero")
Eng_Results_C %>% 
  filter(Dependent_Variable %in% names(Pass_subset)) %>%
  forestplot(mean = Coefficient,
             lower = Lower,
             upper = Upper,
             labeltext = Dependent_Variable,
             title = "ENG Hetero on Passing",
             xlab = "The estimates",
             legend = c("non-Eng", "Eng"),
             legend_args = fpLegend(
               pos = list("topright"),
               title = "Group",
               r = unit(.1, "snpc"),
               gp = gpar(col = "#CCCCCC", lwd = 1.5)
             )) |>
  fp_set_style(box = c("#C0BFBF", "#C02026"),
               line = c("#C0BFBF", "#C02026"),
               summary = c("darkblue", "red"))
dev.off()
pdf("plots/forest/Eng_plot2.pdf", height = 9, title = "ENG Hetero")
Eng_Results_C %>% 
  filter(Dependent_Variable %in% names(Possession_subset)) %>%
  forestplot(mean = Coefficient,
             lower = Lower,
             upper = Upper,
             labeltext = Dependent_Variable,
             title = "ENG Hetero on Possession",
             xlab = "The estimates",
             legend = c("non-Eng", "Eng"),
             legend_args = fpLegend(
               pos = list("topright"),
               title = "Group",
               r = unit(.1, "snpc"),
               gp = gpar(col = "#CCCCCC", lwd = 1.5)
             )) |>
  fp_set_style(box = c("#C0BFBF", "#C02026"),
               line = c("#C0BFBF", "#C02026"),
               summary = c("darkblue", "red"))
dev.off()
pdf("plots/forest/Eng_plot3.pdf", height = 9, title = "ENG Hetero")
Eng_Results_C %>% 
  filter(Dependent_Variable %in% names(Shot_subset)) %>%
  forestplot(mean = Coefficient,
             lower = Lower,
             upper = Upper,
             labeltext = Dependent_Variable,
             title = "ENG Hetero on Shot",
             xlab = "The estimates",
             legend = c("non-Eng", "Eng"),
             legend_args = fpLegend(
               pos = list("topright"),
               title = "Group",
               r = unit(.1, "snpc"),
               gp = gpar(col = "#CCCCCC", lwd = 1.5)
             )) |>
  fp_set_style(box = c("#C0BFBF", "#C02026"),
               line = c("#C0BFBF", "#C02026"),
               summary = c("darkblue", "red"))
dev.off()
pdf("plots/forest/Eng_plot4.pdf", height = 9, title = "ENG Hetero")
Eng_Results_C %>%
  ggplot(aes(x = Coefficient)) +
  geom_histogram(aes(y = after_stat(density), fill = group), 
                 alpha = 0.5, stat = "bin", color = "black") +
  geom_density(aes(color = group), alpha = 1, size = 1.2) +
  labs(title = "Distribution of Coefficients by Group", x = "Coefficient", y = "Density") +
  scale_fill_manual(values = c("non-Eng" = "#C0BFBF", "Eng" = "#C02026")) + 
  scale_color_manual(values = c("non-Eng" = "#C0BFBF", "Eng" = "#C02026")) + 
  theme_minimal()
dev.off()

#is.eu
EU_Results_C <- bind_rows(EU_Results_0 %>% 
                             mutate(Dependent_Variable = EU_Results_1$Dependent_Variable,
                                    group = "non-EU"), 
                           EU_Results_1 %>%
                             mutate(group = "EU")) %>%
  mutate(Lower = Coefficient - 1.96 * SE, 
         Upper = Coefficient + 1.96 * SE) %>%
  group_by(group)


pdf("plots/forest/EU_plot1.pdf", height = 9, title = "EU Hetero")
EU_Results_C %>%
  filter(Dependent_Variable %in% names(Pass_subset)) %>%
  forestplot(mean = Coefficient,
             lower = Lower,
             upper = Upper,
             labeltext = Dependent_Variable,
             title = "EU Hetero on Passing",
             xlab = "The estimates",
             legend = c("non-EU", "EU"),
             legend_args = fpLegend(
               pos = list("topright"),
               title = "Group",
               r = unit(.1, "snpc"),
               gp = gpar(col = "#CCCCCC", lwd = 1.5)
             )) |>
  fp_set_style(box = c("#C02026", "#C0BFBF"),
               line = c("#C02026", "#C0BFBF"),
               summary = c("darkblue", "red"))
dev.off()

pdf("plots/forest/EU_plot2.pdf", height = 9, title = "EU Hetero")
EU_Results_C %>%
  filter(Dependent_Variable %in% names(Possession_subset)) %>%
  forestplot(mean = Coefficient,
             lower = Lower,
             upper = Upper,
             labeltext = Dependent_Variable,
             title = "EU Hetero on Possession",
             xlab = "The estimates",
             legend = c("non-EU", "EU"),
             legend_args = fpLegend(
               pos = list("topright"),
               title = "Group",
               r = unit(.1, "snpc"),
               gp = gpar(col = "#CCCCCC", lwd = 1.5)
             )) |>
  fp_set_style(box = c("#C02026", "#C0BFBF"),
               line = c("#C02026", "#C0BFBF"),
               summary = c("darkblue", "red"))
dev.off()

pdf("plots/forest/EU_plot3.pdf", height = 9, title = "EU Hetero")
EU_Results_C %>%
  filter(Dependent_Variable %in% names(Shot_subset)) %>%
  forestplot(mean = Coefficient,
             lower = Lower,
             upper = Upper,
             labeltext = Dependent_Variable,
             title = "EU Hetero on Shot",
             xlab = "The estimates",
             legend = c("non-EU", "EU"),
             legend_args = fpLegend(
               pos = list("topright"),
               title = "Group",
               r = unit(.1, "snpc"),
               gp = gpar(col = "#CCCCCC", lwd = 1.5)
             )) |>
  fp_set_style(box = c("#C02026", "#C0BFBF"),
               line = c("#C02026", "#C0BFBF"),
               summary = c("darkblue", "red"))
dev.off()

pdf("plots/forest/EU_plot4.pdf", height = 9, title = "EU Hetero")
EU_Results_C %>%
  ggplot(aes(x = Coefficient)) +
  geom_histogram(aes(y = after_stat(density), fill = group), 
                 alpha = 0.5, stat = "bin", color = "black") +
  geom_density(aes(color = group), alpha = 1, size = 1.2) +
  labs(title = "Distribution of Coefficients by Group", x = "Coefficient", y = "Density") +
  scale_fill_manual(values = c("non-EU" = "#C02026", "EU" = "#C0BFBF")) + 
  scale_color_manual(values = c("non-EU" = "#C02026", "EU" = "#C0BFBF")) + 
  theme_minimal()
dev.off()
#is.black
Black_Results_C <- bind_rows(Black_Results_0 %>% 
                            mutate(Dependent_Variable = Black_Results_1$Dependent_Variable,
                                   group = "non-Black"), 
                          Black_Results_1 %>%
                            mutate(group = "Black")) %>%
  mutate(Lower = Coefficient - 1.96 * SE, 
         Upper = Coefficient + 1.96 * SE) %>%
  group_by(group)

pdf("plots/forest/Black_plot1.pdf", height = 9, title = "Black Hetero")
Black_Results_C %>%
  filter(Dependent_Variable %in% names(Pass_subset)) %>%
  forestplot(mean = Coefficient,
             lower = Lower,
             upper = Upper,
             labeltext = Dependent_Variable,
             title = "Black Hetero on Passing",
             xlab = "The estimates",
             legend = c("non-Black", "Black"),
             legend_args = fpLegend(
               pos = list("topright"),
               title = "Group",
               r = unit(.1, "snpc"),
               gp = gpar(col = "#CCCCCC", lwd = 1.5)
             )) |>
  fp_set_style(box = c("#C02026", "#C0BFBF"),
               line = c("#C02026", "#C0BFBF"),
               summary = c("darkblue", "red"))
dev.off()

pdf("plots/forest/Black_plot2.pdf", height = 9, title = "Black Hetero")
Black_Results_C %>%
  filter(Dependent_Variable %in% names(Possession_subset)) %>%
  forestplot(mean = Coefficient,
             lower = Lower,
             upper = Upper,
             labeltext = Dependent_Variable,
             title = "Black Hetero on Possession",
             xlab = "The estimates",
             legend = c("non-Black", "Black"),
             legend_args = fpLegend(
               pos = list("topright"),
               title = "Group",
               r = unit(.1, "snpc"),
               gp = gpar(col = "#CCCCCC", lwd = 1.5)
             )) |>
  fp_set_style(box = c("#C02026", "#C0BFBF"),
               line = c("#C02026", "#C0BFBF"),
               summary = c("darkblue", "red"))
dev.off()

pdf("plots/forest/Black_plot3.pdf", height = 9, title = "Black Hetero")
Black_Results_C %>%
  filter(Dependent_Variable %in% names(Shot_subset)) %>%
  forestplot(mean = Coefficient,
             lower = Lower,
             upper = Upper,
             labeltext = Dependent_Variable,
             title = "Black Hetero on Shot",
             xlab = "The estimates",
             legend = c("non-Black", "Black"),
             legend_args = fpLegend(
               pos = list("topright"),
               title = "Group",
               r = unit(.1, "snpc"),
               gp = gpar(col = "#CCCCCC", lwd = 1.5)
             )) |>
  fp_set_style(box = c("#C02026", "#C0BFBF"),
               line = c("#C02026", "#C0BFBF"),
               summary = c("darkblue", "red"))
dev.off()

pdf("plots/forest/Black_plot4.pdf", height = 9, title = "Black Hetero")
Black_Results_C %>%
  ggplot(aes(x = Coefficient)) +
  geom_histogram(aes(y = after_stat(density), fill = group), 
                 alpha = 0.5, stat = "bin", color = "black") +
  geom_density(aes(color = group), alpha = 1, size = 1.2) +
  labs(title = "Distribution of Coefficients by Group", x = "Coefficient", y = "Density") +
  scale_fill_manual(values = c("non-Black" = "#C02026", "Black" = "#C0BFBF")) + 
  scale_color_manual(values = c("non-Black" = "#C02026", "Black" = "#C0BFBF")) + 
  theme_minimal()
dev.off()
#Position
Position_Results <- bind_rows(Position_B %>% 
                               mutate(Dependent_Variable = Position_F$Dependent_Variable,
                                      group = "Back"), 
                              Position_M %>% 
                                mutate(Dependent_Variable = Position_F$Dependent_Variable,
                                       group = "Middle"), 
                             Position_F %>%
                               mutate(group = "Forward")) %>%
  mutate(Lower = Coefficient - 1.96 * SE, 
         Upper = Coefficient + 1.96 * SE) %>%
  group_by(group)


pdf("plots/forest/Position_plot1.pdf", height = 9, title = "Position Hetero")

Position_Results %>%
  filter(Dependent_Variable %in% names(Pass_subset)) %>%
  forestplot(mean = Coefficient,
             lower = Lower,
             upper = Upper,
             labeltext = Dependent_Variable,
             title = "Position Hetero on Passing",
             xlab = "The estimates",
             legend = c("Back", "Middle", "Forward"),
             legend_args = fpLegend(
               pos = list("topright"),
               title = "Group",
               r = unit(.1, "snpc"),
               gp = gpar(col = "#CCCCCC", lwd = 1.5)
             )) |>
  fp_set_style(box = c("#85CEB7", "#A4B3D5", "#FDA481"),
               line = c("#85CEB7", "#A4B3D5", "#FDA481"))
dev.off()

pdf("plots/forest/Position_plot2.pdf", height = 9, title = "Position Hetero")
Position_Results %>%
  filter(Dependent_Variable %in% names(Possession_subset)) %>%
  forestplot(mean = Coefficient,
             lower = Lower,
             upper = Upper,
             labeltext = Dependent_Variable,
             title = "Position Hetero on Possession",
             xlab = "The estimates",
             legend = c("Back", "Middle", "Forward"),
             legend_args = fpLegend(
               pos = list("topright"),
               title = "Group",
               r = unit(.1, "snpc"),
               gp = gpar(col = "#CCCCCC", lwd = 1.5)
             )) |>
  fp_set_style(box = c("#85CEB7", "#A4B3D5", "#FDA481"),
               line = c("#85CEB7", "#A4B3D5", "#FDA481"))
dev.off()

pdf("plots/forest/Position_plot3.pdf", height = 9, title = "Position Hetero")
Position_Results %>%
  filter(Dependent_Variable %in% names(Shot_subset)) %>%
  forestplot(mean = Coefficient,
             lower = Lower,
             upper = Upper,
             labeltext = Dependent_Variable,
             title = "Position Hetero on Shot",
             xlab = "The estimates",
             legend = c("Back", "Middle", "Forward"),
             legend_args = fpLegend(
               pos = list("topright"),
               title = "Group",
               r = unit(.1, "snpc"),
               gp = gpar(col = "#CCCCCC", lwd = 1.5)
             )) |>
  fp_set_style(box = c("#85CEB7", "#A4B3D5", "#FDA481"),
               line = c("#85CEB7", "#A4B3D5", "#FDA481"))
dev.off()

pdf("plots/forest/Position_plot4.pdf", height = 9, title = "Position Hetero")
Position_Results %>%
  ggplot(aes(x = Coefficient)) +
  geom_histogram(aes(y = after_stat(density), fill = group), 
                 alpha = 0.2, stat = "bin", color = "black") +
  geom_density(aes(color = group), alpha = 1, size = 1.2) +
  labs(title = "Distribution of Coefficients by Group", x = "Coefficient", y = "Density") +
  scale_fill_manual(values = c("Back" = "#85CEB7", "Middle" = "#A4B3D5", "Forward" = "#FDA481")) + 
  scale_color_manual(values = c("Back" = "#85CEB7", "Middle" = "#A4B3D5", "Forward" = "#FDA481")) + 
  theme_minimal()
dev.off()
#Club Hetero
CON_Results_C <- bind_rows(CON_Results_0 %>% 
                               mutate(Dependent_Variable = CON_Results_1$Dependent_Variable,
                                      group = "LAB"), 
                             CON_Results_1 %>%
                               mutate(group = "CON")) %>%
  mutate(Lower = Coefficient - 1.96 * SE, 
         Upper = Coefficient + 1.96 * SE) %>%
  group_by(group)

pdf("plots/forest/CON_plot1.pdf", height = 9, title = "CON Hetero")
CON_Results_C %>%
  filter(Dependent_Variable %in% names(Pass_subset)) %>%
  forestplot(mean = Coefficient,
             lower = Lower,
             upper = Upper,
             labeltext = Dependent_Variable,
             title = "CON Hetero on Passing",
             xlab = "The estimates",
             legend = c("LAB", "CON"),
             legend_args = fpLegend(
               pos = list("topright"),
               title = "Group",
               r = unit(.1, "snpc"),
               gp = gpar(col = "#CCCCCC", lwd = 1.5)
             )) |>
  fp_set_style(box = c("#C0BFBF", "#C02026"),
               line = c("#C0BFBF", "#C02026"),
               summary = c("darkblue", "red"))
dev.off()

pdf("plots/forest/CON_plot2.pdf", height = 9, title = "CON Hetero")
CON_Results_C %>%
  filter(Dependent_Variable %in% names(Possession_subset)) %>%
  forestplot(mean = Coefficient,
             lower = Lower,
             upper = Upper,
             labeltext = Dependent_Variable,
             title = "CON Hetero on Possession",
             xlab = "The estimates",
             legend = c("LAB", "CON"),
             legend_args = fpLegend(
               pos = list("topright"),
               title = "Group",
               r = unit(.1, "snpc"),
               gp = gpar(col = "#CCCCCC", lwd = 1.5)
             )) |>
  fp_set_style(box = c("#C0BFBF", "#C02026"),
               line = c("#C0BFBF", "#C02026"),
               summary = c("darkblue", "red"))
dev.off()

pdf("plots/forest/CON_plot3.pdf", height = 9, title = "CON Hetero")
CON_Results_C %>%
  filter(Dependent_Variable %in% names(Shot_subset)) %>%
  forestplot(mean = Coefficient,
             lower = Lower,
             upper = Upper,
             labeltext = Dependent_Variable,
             title = "CON Hetero on Shot",
             xlab = "The estimates",
             legend = c("LAB", "CON"),
             legend_args = fpLegend(
               pos = list("topright"),
               title = "Group",
               r = unit(.1, "snpc"),
               gp = gpar(col = "#CCCCCC", lwd = 1.5)
             )) |>
  fp_set_style(box = c("#C0BFBF", "#C02026"),
               line = c("#C0BFBF", "#C02026"),
               summary = c("darkblue", "red"))
dev.off()

pdf("plots/forest/CON_plot4.pdf", height = 9, title = "CON Hetero")
CON_Results_C %>%
  ggplot(aes(x = Coefficient)) +
  geom_histogram(aes(y = after_stat(density), fill = group), 
                 alpha = 0.5, stat = "bin", color = "black") +
  geom_density(aes(color = group), alpha = 1, size = 1.2) +
  labs(title = "Distribution of Coefficients by Group", x = "Coefficient", y = "Density") +
  scale_fill_manual(values = c("LAB" = "#C0BFBF", "CON" = "#C02026")) + 
  scale_color_manual(values = c("LAB" = "#C0BFBF", "CON" = "#C02026")) + 
  theme_minimal()
dev.off()

