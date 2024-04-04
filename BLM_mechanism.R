#Mechanism Exploration

#Exponential function form.

#Passing Section
Passing_exp <- data.frame(Dependent_Variable = character(),
                              Coefficient = numeric(),
                              Signif.level = character(),
                              SE = numeric(),
                              R2 = numeric(),
                              Obs = numeric(),
                              stringsAsFactors = FALSE)

for (y in names(Pass_subset)) {
  formula <- as.formula(paste("log(", y, "+1) ~ post_Howmany | Player + after"))
  model <- felm(formula, data = scale_data)
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"]
  stars <- ifelse(pvalues<0.01, "***", 
                  ifelse(pvalues<0.05, "**", 
                         ifelse(pvalues<0.1, "*", "")))
  
  row <- data.frame(Dependent_Variable = y,
                    Coefficient = coef(model),
                    Signif.level = stars,
                    SE = sqrt(diag(vcov(model))),
                    R2 = result$r.squared,
                    Obs = length(model$residuals),
                    stringsAsFactors = FALSE)
  
  Passing_exp <- rbind(Passing_exp, row)
}
print(xtable(caption = "Exponential Form of Passing Specifications", Passing_exp), 
      include.rownames=FALSE, 
      hline.after=c(-1,0,nrow(Passing_exp)), 
      file="D:/Xujianuo/socialmedia paper/BLMproj/BLMCode/tables/Passing_exp.tex",
      type="latex")

#Possession Section
Possession_exp <- data.frame(Dependent_Variable = character(),
                                Coefficient = numeric(),
                                Signif.level = character(),
                                SE = numeric(),
                                R2 = numeric(),
                                Obs = numeric(),
                                stringsAsFactors = FALSE)

for (y in names(Possession_subset)) {
  formula <- as.formula(paste("log(", y, "+1) ~ post_Howmany | Player + after"))
  model <- felm(formula, data = scale_data)
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"]
  stars <- ifelse(pvalues<0.01, "***", 
                  ifelse(pvalues<0.05, "**", 
                         ifelse(pvalues<0.1, "*", "")))
  
  row <- data.frame(Dependent_Variable = y,
                    Coefficient = coef(model),
                    Signif.level = stars,
                    SE = sqrt(diag(vcov(model))),
                    R2 = result$r.squared,
                    Obs = length(model$residuals),
                    stringsAsFactors = FALSE)
  
  Possession_exp <- rbind(Possession_exp, row)
}
print(xtable(Possession_exp,
             caption = "Exponential Form of Possession Specifications"), 
      include.rownames=FALSE, 
      hline.after=c(-1,0,nrow(Possession_exp)), 
      file="D:/Xujianuo/socialmedia paper/BLMproj/BLMCode/tables/Possession_exp.tex",
      type="latex")
#Logrithm function form.

#Passing Section
Passing_log <- data.frame(Dependent_Variable = character(),
                          Coefficient = numeric(),
                          Signif.level = character(),
                          SE = numeric(),
                          R2 = numeric(),
                          Obs = numeric(),
                          stringsAsFactors = FALSE)

for (y in names(Pass_subset)) {
  formula <- as.formula(paste(y, "~ log(post_Howmany + 1) | Player + after"))
  model <- felm(formula, data = scale_data)
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"]
  stars <- ifelse(pvalues<0.01, "***", 
                  ifelse(pvalues<0.05, "**", 
                         ifelse(pvalues<0.1, "*", "")))
  
  row <- data.frame(Dependent_Variable = y,
                    Coefficient = coef(model),
                    Signif.level = stars,
                    SE = sqrt(diag(vcov(model))),
                    R2 = result$r.squared,
                    Obs = length(model$residuals),
                    stringsAsFactors = FALSE)
  
  Passing_log <- rbind(Passing_log, row)
}
print(xtable(caption = "Exponential Form of Passing Specifications", Passing_log), 
      include.rownames=FALSE, 
      hline.after=c(-1,0,nrow(Passing_log)), 
      file="D:/Xujianuo/socialmedia paper/BLMproj/BLMCode/tables/Passing_log.tex",
      type="latex")

#Possession Section
Possession_log <- data.frame(Dependent_Variable = character(),
                             Coefficient = numeric(),
                             Signif.level = character(),
                             SE = numeric(),
                             R2 = numeric(),
                             Obs = numeric(),
                             stringsAsFactors = FALSE)

for (y in names(Possession_subset)) {
  formula <- as.formula(paste(y, "~ log(post_Howmany + 1) | Player + after"))
  model <- felm(formula, data = scale_data)
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"]
  stars <- ifelse(pvalues<0.01, "***", 
                  ifelse(pvalues<0.05, "**", 
                         ifelse(pvalues<0.1, "*", "")))
  
  row <- data.frame(Dependent_Variable = y,
                    Coefficient = coef(model),
                    Signif.level = stars,
                    SE = sqrt(diag(vcov(model))),
                    R2 = result$r.squared,
                    Obs = length(model$residuals),
                    stringsAsFactors = FALSE)
  
  Possession_log <- rbind(Possession_log, row)
}
print(xtable(Possession_log,
             caption = "Exponential Form of Possession Specifications"), 
      include.rownames=FALSE, 
      hline.after=c(-1,0,nrow(Possession_log)), 
      file="D:/Xujianuo/socialmedia paper/BLMproj/BLMCode/tables/Possession_log.tex",
      type="latex")
#Visualize the R-square of different function forms
r_squares_Passing <- data.frame(Model_Type = c(rep("Linear", length(Passing_Results$R2)),
                                               rep("Log-transformed", length(Passing_log$R2)),
                                               rep("Exponential-transformed", length(Passing_exp$R2))),
                                R_squared = c(Passing_Results$R2, Passing_log$R2, Passing_exp$R2))
ggplot(r_squares_Passing, aes(x = Model_Type, y = R_squared, fill = Model_Type)) +
  geom_boxplot() +
  labs(title="Comparison of R-squared Values", x="Model Type", y="R-squared") +
  theme_minimal()

t.test(Passing_Results$R2, Passing_exp$R2)
t.test(Passing_Results$R2, Passing_log$R2)

#Use various post quantile as x variable
quantile_0 <- data.frame(Dependent_Variable = character(),
                         Coefficient = numeric(),
                         Signif.level = character(),
                         SE = numeric(),
                         R2 = numeric(),
                         Obs = numeric(),
                         stringsAsFactors = FALSE)

for (y in c(names(Pass_subset), names(Possession_subset), 
            names(Shot_subset))) {
  formula <- as.formula(paste(y, "~ quantile_0 | Player + after"))
  model <- felm(formula, data = scale_data)
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"]
  stars <- ifelse(pvalues<0.01, "***", 
                  ifelse(pvalues<0.05, "**", 
                         ifelse(pvalues<0.1, "*", "")))
  
  row <- data.frame(Dependent_Variable = y,
                    Coefficient = coef(model),
                    Signif.level = stars,
                    SE = sqrt(diag(vcov(model))),
                    R2 = result$r.squared,
                    Obs = length(model$residuals),
                    stringsAsFactors = FALSE)
  
  quantile_0 <- rbind(quantile_0, row)
}

quantile_1 <- data.frame(Dependent_Variable = character(),
                         Coefficient = numeric(),
                         Signif.level = character(),
                         SE = numeric(),
                         R2 = numeric(),
                         Obs = numeric(),
                         stringsAsFactors = FALSE)

for (y in c(names(Pass_subset), names(Possession_subset), 
            names(Shot_subset))) {
  formula <- as.formula(paste(y, "~ quantile_1 | Player + after"))
  model <- felm(formula, data = scale_data)
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"]
  stars <- ifelse(pvalues<0.01, "***", 
                  ifelse(pvalues<0.05, "**", 
                         ifelse(pvalues<0.1, "*", "")))
  
  row <- data.frame(Dependent_Variable = y,
                    Coefficient = coef(model),
                    Signif.level = stars,
                    SE = sqrt(diag(vcov(model))),
                    R2 = result$r.squared,
                    Obs = length(model$residuals),
                    stringsAsFactors = FALSE)
  
  quantile_1 <- rbind(quantile_1, row)
}

quantile_2 <- data.frame(Dependent_Variable = character(),
                         Coefficient = numeric(),
                         Signif.level = character(),
                         SE = numeric(),
                         R2 = numeric(),
                         Obs = numeric(),
                         stringsAsFactors = FALSE)

for (y in c(names(Pass_subset), names(Possession_subset), 
            names(Shot_subset))) {
  formula <- as.formula(paste(y, "~ quantile_2 | Player + after"))
  model <- felm(formula, data = scale_data)
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"]
  stars <- ifelse(pvalues<0.01, "***", 
                  ifelse(pvalues<0.05, "**", 
                         ifelse(pvalues<0.1, "*", "")))
  
  row <- data.frame(Dependent_Variable = y,
                    Coefficient = coef(model),
                    Signif.level = stars,
                    SE = sqrt(diag(vcov(model))),
                    R2 = result$r.squared,
                    Obs = length(model$residuals),
                    stringsAsFactors = FALSE)
  
  quantile_2 <- rbind(quantile_2, row)
}

quantile_3 <- data.frame(Dependent_Variable = character(),
                              Coefficient = numeric(),
                              Signif.level = character(),
                              SE = numeric(),
                              R2 = numeric(),
                              Obs = numeric(),
                              stringsAsFactors = FALSE)

for (y in c(names(Pass_subset), names(Possession_subset), 
             names(Shot_subset))) {
  formula <- as.formula(paste(y, "~ quantile_3 | Player + after"))
  model <- felm(formula, data = scale_data)
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"]
  stars <- ifelse(pvalues<0.01, "***", 
                  ifelse(pvalues<0.05, "**", 
                         ifelse(pvalues<0.1, "*", "")))
  
  row <- data.frame(Dependent_Variable = y,
                    Coefficient = coef(model),
                    Signif.level = stars,
                    SE = sqrt(diag(vcov(model))),
                    R2 = result$r.squared,
                    Obs = length(model$residuals),
                    stringsAsFactors = FALSE)
  
  quantile_3 <- rbind(quantile_3, row)
}

quantile_4 <- data.frame(Dependent_Variable = character(),
                         Coefficient = numeric(),
                         Signif.level = character(),
                         SE = numeric(),
                         R2 = numeric(),
                         Obs = numeric(),
                         stringsAsFactors = FALSE)

for (y in c(names(Pass_subset), names(Possession_subset), 
             names(Shot_subset))) {
  formula <- as.formula(paste(y, "~ quantile_4 | Player + after"))
  model <- felm(formula, data = scale_data)
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"]
  stars <- ifelse(pvalues<0.01, "***", 
                  ifelse(pvalues<0.05, "**", 
                         ifelse(pvalues<0.1, "*", "")))
  
  row <- data.frame(Dependent_Variable = y,
                    Coefficient = coef(model),
                    Signif.level = stars,
                    SE = sqrt(diag(vcov(model))),
                    R2 = result$r.squared,
                    Obs = length(model$residuals),
                    stringsAsFactors = FALSE)
  
  quantile_4 <- rbind(quantile_4, row)
}

library(ggplot2)

Quantile <- bind_rows(quantile_0 %>% mutate(group = 0), 
                      quantile_1 %>% mutate(group = 1),
                      quantile_2 %>% mutate(group = 2),
                      quantile_3 %>% mutate(group = 3),
                      quantile_4 %>% mutate(group = 4)) %>%
  mutate(Lower = Coefficient - 1.96 * SE, 
         Upper = Coefficient + 1.96 * SE)

# Create the pie chart 
pdf("plots/Quantile.pdf", width = 12, title = "Mechanism Exploration")
Quantile %>%
  ggplot(aes(y = as.factor(group), fill = Signif.level)) +
    geom_bar(alpha = 1, stat = "count", color = "black", position = "stack") +
    labs(title = "Distribution of Significant Level ", x = "Frequecy", y = "Quantile") +
    scale_fill_manual(values = c("#B4B6B6", "#C7E2E4", "#8FC2C7", "#488B87", "#BD514A")) + 
    #scale_color_manual(values = c("#FDF6A4", "#EFE7F3", "#E4F2F3", "#E9EDDB", "#F7E6E1")) + 
    theme_minimal() +
    facet_wrap(~group, scales = "free_y", ncol = 1)
dev.off()
