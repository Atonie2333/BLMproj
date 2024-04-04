##Club Heterogeneity##
library(lfe)
library(dplyr)
library(ggplot2)
library(xtable)
#Heterogeneity between CON and LAB.
# Explore hetero across is CON club.
CON_Results_1 <- data.frame(Dependent_Variable = character(),
                              Coefficient = numeric(),
                              Signif.level = character(),
                              SE = numeric(),
                              Obs = numeric(),
                              stringsAsFactors = FALSE)
#Add a intersection term to divide CON players and non-CON players
for (y in c(names(Pass_subset), names(Possession_subset),
            names(Shot_subset))) {
  formula <- as.formula(paste(y, "~ post_Howmany| Player + after"))  
  model <- felm(formula, data = hetero_data %>% filter(CON == 1))
  
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"]
  stars <- ifelse(pvalues<0.001, "***", ifelse(pvalues<0.01, "**", ifelse(pvalues<0.05, "*", "")))
  row <- data.frame(Dependent_Variable = y,
                    Coefficient = coef(model),
                    Signif.level = stars,
                    SE = sqrt(diag(vcov(model))),
                    Obs = length(model$residuals),
                    stringsAsFactors = FALSE)
  
  CON_Results_1 <- rbind(CON_Results_1, row)
}
#Non-CON Clubs
CON_Results_0 <- data.frame(
  Coefficient = numeric(),
  Signif.level = character(),
  SE = numeric(),
  Obs = numeric(),
  stringsAsFactors = FALSE)
#Add a intersection term to divide CON clubs and non-CON clubs
for (y in c(names(Pass_subset), names(Possession_subset), names(Shot_subset))) {
  formula <- as.formula(paste(y, "~ post_Howmany| Player + after"))  
  model <- felm(formula, data = hetero_data %>% filter(CON == 0))
  
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"]
  stars <- ifelse(pvalues<0.001, "***", ifelse(pvalues<0.01, "**", ifelse(pvalues<0.05, "*", "")))
  row <- data.frame(
    Coefficient = coef(model),
    Signif.level = stars,
    SE = sqrt(diag(vcov(model))),
    Obs = length(model$residuals),
    stringsAsFactors = FALSE)
  
  CON_Results_0 <- rbind(CON_Results_0, row)
}

print(xtable(cbind(CON_Results_1, CON_Results_0)), 
      include.rownames=FALSE, 
      hline.after=c(-1,0,nrow(CON_Results_0)), 
      file="D:/Xujianuo/socialmedia paper/BLMproj/BLMCode/tables/CON.tex",
      type="latex")

#Con
con_Results <- data.frame(
  Dependent_Variable = character(),
  Coefficient = numeric(),
  Signif.level = character(),
  SE = numeric(),
  Obs = numeric(),
  stringsAsFactors = FALSE)

for (y in c(names(Pass_subset), names(Possession_subset), names(Shot_subset))) {
  formula <- as.formula(paste(y, "~ post_Howmany*con| Player + after"))  
  model <- felm(formula, data = hetero_data )
  
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"][[3]]
  stars <- ifelse(pvalues<0.001, "***", ifelse(pvalues<0.01, "**", ifelse(pvalues<0.05, "*", "")))
  row <- data.frame(Dependent_Variable = y,
                    Coefficient = coef(model)[[3]],
    Signif.level = stars,
    SE = sqrt(diag(vcov(model)))[[3]],
    Obs = length(model$residuals),
    stringsAsFactors = FALSE)
  
  con_Results <- rbind(con_Results, row)
}

#Lab
lab_Results <- data.frame(
  Dependent_Variable = character(),
  Coefficient = numeric(),
  Signif.level = character(),
  SE = numeric(),
  Obs = numeric(),
  stringsAsFactors = FALSE)

for (y in c(names(Pass_subset), names(Possession_subset), names(Shot_subset))) {
  formula <- as.formula(paste(y, "~ post_Howmany*lab| Player + after"))  
  model <- felm(formula, data = hetero_data )
  
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"][[3]]
  stars <- ifelse(pvalues<0.001, "***", ifelse(pvalues<0.01, "**", ifelse(pvalues<0.05, "*", "")))
  row <- data.frame(Dependent_Variable = y,
                    Coefficient = coef(model)[[3]],
                    Signif.level = stars,
                    SE = sqrt(diag(vcov(model)))[[3]],
                    Obs = length(model$residuals),
                    stringsAsFactors = FALSE)
  
  lab_Results <- rbind(lab_Results, row)
}

# Green.
grn_Results <- data.frame(
  Dependent_Variable = character(),
  Coefficient = numeric(),
  Signif.level = character(),
  SE = numeric(),
  Obs = numeric(),
  stringsAsFactors = FALSE)

for (y in c(names(Pass_subset), names(Possession_subset), names(Shot_subset))) {
  formula <- as.formula(paste(y, "~ post_Howmany*grn| Player + after"))  
  model <- felm(formula, data = hetero_data )
  
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"][[3]]
  stars <- ifelse(pvalues<0.001, "***", ifelse(pvalues<0.01, "**", ifelse(pvalues<0.05, "*", "")))
  row <- data.frame(Dependent_Variable = y,
                    Coefficient = coef(model)[[3]],
                    Signif.level = stars,
                    SE = sqrt(diag(vcov(model)))[[3]],
                    Obs = length(model$residuals),
                    stringsAsFactors = FALSE)
  
  grn_Results <- rbind(grn_Results, row)
}
#Ldm
ldm_Results <- data.frame(
  Dependent_Variable = character(),
  Coefficient = numeric(),
  Signif.level = character(),
  SE = numeric(),
  Obs = numeric(),
  stringsAsFactors = FALSE)

for (y in c(names(Pass_subset), names(Possession_subset), names(Shot_subset))) {
  formula <- as.formula(paste(y, "~ post_Howmany*ldm| Player + after"))  
  model <- felm(formula, data = hetero_data )
  
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"][[3]]
  stars <- ifelse(pvalues<0.001, "***", ifelse(pvalues<0.01, "**", ifelse(pvalues<0.05, "*", "")))
  row <- data.frame(Dependent_Variable = y,
                    Coefficient = coef(model)[[3]],
                    Signif.level = stars,
                    SE = sqrt(diag(vcov(model)))[[3]],
                    Obs = length(model$residuals),
                    stringsAsFactors = FALSE)
  
  ldm_Results <- rbind(ldm_Results, row)
}

rfm_Results <- data.frame(
  Dependent_Variable = character(),
  Coefficient = numeric(),
  Signif.level = character(),
  SE = numeric(),
  Obs = numeric(),
  stringsAsFactors = FALSE)

for (y in c(names(Pass_subset), names(Possession_subset), names(Shot_subset))) {
  formula <- as.formula(paste(y, "~ post_Howmany*rfm| Player + after"))  
  model <- felm(formula, data = hetero_data )
  
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"][[3]]
  stars <- ifelse(pvalues<0.001, "***", ifelse(pvalues<0.01, "**", ifelse(pvalues<0.05, "*", "")))
  row <- data.frame(Dependent_Variable = y,
                    Coefficient = coef(model)[[3]],
                    Signif.level = stars,
                    SE = sqrt(diag(vcov(model)))[[3]],
                    Obs = length(model$residuals),
                    stringsAsFactors = FALSE)
  
  rfm_Results <- rbind(rfm_Results, row)
}

Politics <- bind_rows(lab_Results %>% mutate(group = "lab"), 
                      con_Results %>% mutate(group = "con"),
                      grn_Results %>% mutate(group = "grn"),
                      ldm_Results %>% mutate(group = "ldm"),
                      rfm_Results %>% mutate(group = "rfm")) %>%
  mutate(Lower = Coefficient - 1.96 * SE, 
         Upper = Coefficient + 1.96 * SE)

# Create the pie chart 
pdf("plots/Politics.pdf", width = 12, title = "Club Politics Exploration")
Politics %>%
  ggplot(aes(y = as.factor(group), fill = Signif.level)) +
  geom_bar(alpha = 1, stat = "count", color = "black", position = "stack") +
  labs(title = "Distribution of Significant Level ", x = "Frequecy", y = "Political Support") +
  scale_fill_manual(values = c("#B4B6B6", "#C7E2E4", "#8FC2C7", "#488B87", "#BD514A")) + 
  #scale_color_manual(values = c("#FDF6A4", "#EFE7F3", "#E4F2F3", "#E9EDDB", "#F7E6E1")) + 
  theme_minimal() +
  facet_wrap(~group, scales = "free_y", ncol = 1)
dev.off()

