##DiD Analysis##
library(lfe)
library(gtsummary)
library(broom)
library(xtable)
#Linear

#Standardize all variables
scale_data <- data
scale_data[names(perf_subset)] <- scale(subset(data, select = names(perf_subset)))
  #Passing Section
Passing_Results <- data.frame(Dependent_Variable = character(),
                              Coefficient = numeric(),
                              Signif.level = character(),
                              SE = numeric(),
                              R2 = numeric(),
                              Obs = numeric(),
                              stringsAsFactors = FALSE)

for (y in names(Pass_subset)) {
  formula <- as.formula(paste(y, "~ post_Howmany | Player + after"))
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
  
  Passing_Results <- rbind(Passing_Results, row)
}
print(xtable(Passing_Results), 
      include.rownames=FALSE, 
      hline.after=c(-1,0,nrow(Passing_Results)), 
      file="D:/Xujianuo/socialmedia paper/BLMproj/BLMCode/tables/Passing_Results.tex",
      type="latex")
#Possession Section
Possession_Results <- data.frame(Dependent_Variable = character(),
                              Coefficient = numeric(),
                              Signif.level = character(),
                              SE = numeric(),
                              R2 = numeric(),
                              Obs = numeric(),
                              stringsAsFactors = FALSE)

for (y in names(Possession_subset)) {
  formula <- as.formula(paste(y, "~ post_Howmany | Player + after"))
  model <- felm(formula, data = scale_data)
  
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"]
  stars <- ifelse(pvalues<0.001, "***", ifelse(pvalues<0.01, "**", ifelse(pvalues<0.05, "*", "")))

  row <- data.frame(Dependent_Variable = y,
                    Coefficient = coef(model),
                    Signif.level = stars,
                    SE = sqrt(diag(vcov(model))),
                    R2 = result$r.squared,
                    Obs = length(model$residuals),
                    stringsAsFactors = FALSE)
  
  Possession_Results <- rbind(Possession_Results, row)
}
print(xtable(Possession_Results), 
      include.rownames=FALSE, 
      hline.after=c(-1,0,nrow(Possession_Results)), 
      file="D:/Xujianuo/socialmedia paper/BLMproj/BLMCode/tables/Possession_Results.tex",
      type="latex")
#Defend Section
Defend_Results <- data.frame(Dependent_Variable = character(),
                                 Coefficient = numeric(),
                             Signif.level = character(),
                                 SE = numeric(),
                                 R2 = numeric(),
                                 Obs = numeric(),
                                 stringsAsFactors = FALSE)

for (y in names(Defend_subset)) {
  formula <- as.formula(paste(y, "~ post_Howmany | Player + after"))
  model <- felm(formula, data = scale_data)
  
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"]
  stars <- ifelse(pvalues<0.001, "***", ifelse(pvalues<0.01, "**", ifelse(pvalues<0.05, "*", "")))

  row <- data.frame(Dependent_Variable = y,
                    Coefficient = coef(model),
                    Signif.level = stars,
                    SE = sqrt(diag(vcov(model))),
                    R2 = result$r.squared,
                    Obs = length(model$residuals),
                    stringsAsFactors = FALSE)
  
  Defend_Results <- rbind(Defend_Results, row)
}
print(xtable(Defend_Results), 
      include.rownames=FALSE, 
      hline.after=c(-1,0,nrow(Defend_Results)), 
      file="D:/Xujianuo/socialmedia paper/BLMproj/BLMCode/tables/Defend_Results.tex",
      type="latex")
#Shot Section
Shot_Results <- data.frame(Dependent_Variable = character(),
                                 Coefficient = numeric(),
                           Signif.level = character(),
                                 SE = numeric(),
                                 R2 = numeric(),
                                 Obs = numeric(),
                                 stringsAsFactors = FALSE)

for (y in names(Shot_subset)) {
  formula <- as.formula(paste(y, "~ post_Howmany | Player + after"))
  model <- felm(formula, data = scale_data)
  
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"]
  stars <- ifelse(pvalues<0.001, "***", ifelse(pvalues<0.01, "**", ifelse(pvalues<0.05, "*", "")))

  row <- data.frame(Dependent_Variable = y,
                    Coefficient = coef(model),
                    Signif.level = stars,
                    SE = sqrt(diag(vcov(model))),
                    R2 = result$r.squared,
                    Obs = length(model$residuals),
                    stringsAsFactors = FALSE)
  
  Shot_Results <- rbind(Shot_Results, row)
}
#Export results into LaTex
print(xtable(Shot_Results), 
      include.rownames=FALSE, 
      hline.after=c(-1,0,nrow(Shot_Results)), 
      file="D:/Xujianuo/socialmedia paper/BLMproj/BLMCode/tables/Shot_Results.tex",
      type="latex")
#Dead Section
Dead_Results <- data.frame(Dependent_Variable = character(),
                                 Coefficient = numeric(),
                           Signif.level = character(),
                                 SE = numeric(),
                                 R2 = numeric(),
                                 Obs = numeric(),
                                 stringsAsFactors = FALSE)

for (y in names(Dead_subset)) {
  formula <- as.formula(paste(y, "~ post_Howmany | Player + after"))
  model <- felm(formula, data = scale_data)
  
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"]
  stars <- ifelse(pvalues<0.001, "***", ifelse(pvalues<0.01, "**", ifelse(pvalues<0.05, "*", "")))

  row <- data.frame(Dependent_Variable = y,
                    Coefficient = coef(model),
                    Signif.level = stars,
                    SE = sqrt(diag(vcov(model))),
                    R2 = result$r.squared,
                    Obs = length(model$residuals),
                    stringsAsFactors = FALSE)
  
  Dead_Results <- rbind(Dead_Results, row)
}
print(xtable(Dead_Results), 
      include.rownames=FALSE, 
      hline.after=c(-1,0,nrow(Dead_Results)), 
      file="D:/Xujianuo/socialmedia paper/BLMproj/BLMCode/tables/Dead_Results.tex",
      type="latex")

#Miscellaneous Section
Miscellaneous_Results <- data.frame(Dependent_Variable = character(),
                           Coefficient = numeric(),
                           Signif.level = character(),
                           SE = numeric(),
                           R2 = numeric(),
                           Obs = numeric(),
                           stringsAsFactors = FALSE)

for (y in names(Miscellaneous_subset)) {
  formula <- as.formula(paste(y, "~ post_Howmany | Player + after"))
  model <- felm(formula, data = scale_data)
  
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"]
  stars <- ifelse(pvalues<0.001, "***", ifelse(pvalues<0.01, "**", ifelse(pvalues<0.05, "*", "")))

  row <- data.frame(Dependent_Variable = y,
                    Coefficient = coef(model),
                    Signif.level = stars,
                    SE = sqrt(diag(vcov(model))),
                    R2 = result$r.squared,
                    Obs = length(model$residuals),
                    stringsAsFactors = FALSE)
  
  Miscellaneous_Results <- rbind(Miscellaneous_Results, row)
}
print(xtable(Miscellaneous_Results), 
      include.rownames=FALSE, 
      hline.after=c(-1,0,nrow(Miscellaneous_Results)), 
      file="D:/Xujianuo/socialmedia paper/BLMproj/BLMCode/tables/Miscellaneous_Results.tex",
      type="latex")

