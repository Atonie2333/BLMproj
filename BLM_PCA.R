##Principle Components Analysis (PCA)##
library(ggplot2)

#Passing Section
pr.out_Passing <- perf_subset %>% 
  select(all_of(Selected_list[[1]])) %>%
  select(-c("Ast", "xAG", "KP", "PPA", "CrsPA")) %>%
  prcomp (scale. = T)
#plot PCA result
biplot(pr.out_Passing, scale = 0)

#Plot PVE explained by each component
pr.var_Passing <- pr.out_Passing$sdev^2
pve_Passing <- pr.var_Passing/sum(pr.var_Passing)
pdf("plots/PCA/PCA_Passing.pdf", width = 12, title = "PCA_Passing")
par(mfrow = c(1,2))
PVE_Passing <- plot(pve_Passing, 
                    xlab = "Principal Component", 
                    ylab = "Propotion of Variance Explained", 
                    ylim = c(0, 1), 
                    type = "b"
                    )
Cum_PVE_Passing <- plot(cumsum(pve_Passing), 
                xlab = "Principal Component", 
                ylab = "Cumulative Propotion of Variance Explained", 
                ylim = c(0, 1), 
                type = "b")
dev.off()

#Dead Ball Section
pr.out_Dead <- perf_subset %>% 
  select(all_of(Selected_list[[2]])) %>%
  select(-TI) %>%
  prcomp (scale. = T)
#plot PCA result
biplot(pr.out_Dead, scale = 0)

#Plot PVE explained by each component
pr.var_Dead <- pr.out_Dead$sdev^2
pve_Dead <- pr.var_Dead/sum(pr.var_Dead)
pdf("plots/PCA/PCA_Dead.pdf", width = 12, title = "PCA_Dead")
par(mfrow = c(1,2))
PVE_Dead <- plot(pve_Dead, 
                    xlab = "Principal Component", 
                    ylab = "Propotion of Variance Explained", 
                    ylim = c(0, 1), 
                    type = "b"
)
Cum_PVE_Dead <- plot(cumsum(pve_Dead), 
                        xlab = "Principal Component", 
                        ylab = "Cumulative Propotion of Variance Explained", 
                        ylim = c(0, 1), 
                        type = "b")
dev.off()
pr.out_Possession <- perf_subset %>% 
  select(all_of(Selected_list[[3]])) %>%
  select(-c("Touches_DefPen", "Touches_Mid_3rd", "Touches_Def_3rd", 
            "Touches_Live", "CPA")) %>%
  prcomp (scale. = T)
#plot PCA result
biplot(pr.out_Possession, scale = 0)

#Plot PVE explained by each component
pr.var_Possession <- pr.out_Possession$sdev^2
pve_Possession <- pr.var_Possession/sum(pr.var_Possession)
pdf("plots/PCA/PCA_Possession.pdf", width = 12, title = "PCA_Possession")
par(mfrow = c(1,2))
PVE_Possession <- plot(pve_Possession, 
                    xlab = "Principal Component", 
                    ylab = "Propotion of Variance Explained", 
                    ylim = c(0, 1), 
                    type = "b"
)
Cum_PVE_Possession <- plot(cumsum(pve_Possession), 
                        xlab = "Principal Component", 
                        ylab = "Cumulative Propotion of Variance Explained", 
                        ylim = c(0, 1), 
                        type = "b")
dev.off()

pr.out_Defend <- perf_subset %>% 
  select(all_of(Selected_list[[5]])) %>%
  select(-c("Sh_Blocked", "Pass_Blocked", "Int")) %>%
  prcomp (scale. = T)
#plot PCA result
biplot(pr.out_Defend, scale = 0)

#Plot PVE explained by each component
pr.var_Defend <- pr.out_Defend$sdev^2
pve_Defend <- pr.var_Defend/sum(pr.var_Defend)
pdf("plots/PCA/PCA_Defend.pdf", width = 12, title = "PCA_Defend")
par(mfrow = c(1,2))
PVE_Defend <- plot(pve_Defend, 
                    xlab = "Principal Component", 
                    ylab = "Propotion of Variance Explained", 
                    ylim = c(0, 1), 
                    type = "b"
)
Cum_PVE_Defend <- plot(cumsum(pve_Defend), 
                        xlab = "Principal Component", 
                        ylab = "Cumulative Propotion of Variance Explained", 
                        ylim = c(0, 1), 
                        type = "b")
dev.off()

pr.out_Shot <- perf_subset %>% 
  select(all_of(Selected_list[[4]])) %>%
  select(-c("PK", "PKatt")) %>%
  prcomp (scale. = T)
#plot PCA result
biplot(pr.out_Shot, scale = 0)

#Plot PVE explained by each component
pr.var_Shot <- pr.out_Shot$sdev^2
pve_Shot <- pr.var_Shot/sum(pr.var_Shot)
pdf("plots/PCA/PCA_Shot.pdf", width = 12, title = "PCA_Shot")
par(mfrow = c(1,2))
PVE_Shot <- plot(pve_Shot, 
                    xlab = "Principal Component", 
                    ylab = "Propotion of Variance Explained", 
                    ylim = c(0, 1), 
                    type = "b"
)
Cum_PVE_Shot <- plot(cumsum(pve_Shot), 
                        xlab = "Principal Component", 
                        ylab = "Cumulative Propotion of Variance Explained", 
                        ylim = c(0, 1), 
                        type = "b")
dev.off()

pr.out_Miscellaneous <- perf_subset %>% 
  select(all_of(Selected_list[[6]])) %>%
  prcomp (scale. = T)
#plot PCA result
biplot(pr.out_Miscellaneous, scale = 0)

#Plot PVE explained by each component
pr.var_Miscellaneous <- pr.out_Miscellaneous$sdev^2
pve_Miscellaneous <- pr.var_Miscellaneous/sum(pr.var_Miscellaneous)
pdf("plots/PCA/PCA_Miscellaneous.pdf", width = 12, title = "PCA_Miscellaneous")
par(mfrow = c(1,2))
PVE_Miscellaneous <- plot(pve_Miscellaneous, 
                    xlab = "Principal Component", 
                    ylab = "Propotion of Variance Explained", 
                    ylim = c(0, 1), 
                    type = "b"
)
Cum_PVE_Miscellaneous <- plot(cumsum(pve_Miscellaneous), 
                        xlab = "Principal Component", 
                        ylab = "Cumulative Propotion of Variance Explained", 
                        ylim = c(0, 1), 
                        type = "b")
dev.off()
#Regression with PCs.
pr.out_Passing$x <- pr.out_Passing$x %>%
  as.data.frame() %>%
  rename("Passing_PC1" = "PC1", 
         "Passing_PC2" = "PC2",
         "Passing_PC3" = "PC3" )
pr.out_Dead$x <- pr.out_Dead$x %>%
  as.data.frame() %>%
  rename("Dead_PC1" = "PC1", 
         "Dead_PC2" = "PC2",
         "Dead_PC3" = "PC3" )
pr.out_Possession$x <- pr.out_Possession$x %>%
  as.data.frame() %>%
  rename("Possession_PC1" = "PC1", 
         "Possession_PC2" = "PC2",
         "Possession_PC3" = "PC3" )
pr.out_Shot$x <- pr.out_Shot$x %>%
  as.data.frame() %>%
  rename("Shot_PC1" = "PC1", 
         "Shot_PC2" = "PC2",
         "Shot_PC3" = "PC3" )
pr.out_Defend$x <- pr.out_Defend$x %>%
  as.data.frame() %>%
  rename("Defend_PC1" = "PC1", 
         "Defend_PC2" = "PC2",
         "Defend_PC3" = "PC3" )
PC.data <- scale_data %>%
  cbind(pr.out_Passing$x[ ,1:3], pr.out_Dead$x[ ,1:3], pr.out_Possession$x[ ,1:3],
        pr.out_Shot$x[ ,1:3], pr.out_Defend$x[ ,1:3]
  )


#Regression with PCs' key variables. 
PCs.kv <- c(which.max(abs(pr.out_Passing$rotation[, "PC1"])),
which.max(abs(pr.out_Passing$rotation[, "PC2"])),
which.max(abs(pr.out_Passing$rotation[, "PC3"])),
which.max(abs(pr.out_Dead$rotation[, "PC1"])),
which.max(abs(pr.out_Dead$rotation[, "PC2"])),
which.max(abs(pr.out_Dead$rotation[, "PC3"])),
which.max(abs(pr.out_Possession$rotation[, "PC1"])),
which.max(abs(pr.out_Possession$rotation[, "PC2"])),
which.max(abs(pr.out_Possession$rotation[, "PC3"])),
which.max(abs(pr.out_Defend$rotation[, "PC1"])),
which.max(abs(pr.out_Defend$rotation[, "PC2"])),
which.max(abs(pr.out_Defend$rotation[, "PC3"])),
which.max(abs(pr.out_Shot$rotation[, "PC1"])),
which.max(abs(pr.out_Shot$rotation[, "PC2"])),
which.max(abs(pr.out_Shot$rotation[, "PC3"]))
)
PCs.kv

PCs.kv_Results <- data.frame(Dependent_Variable = character(),
                                    Coefficient = numeric(),
                                    Signif.level = character(),
                                    SE = numeric(),
                                    R2 = numeric(),
                                    Obs = numeric(),
                                    stringsAsFactors = FALSE)

for (y in c("Passing_PC1", "PassAtt",
            "Dead_PC1", "CK", 
            "Possession_PC1", "Carries_TotDist",
            "Defend_PC1", "Tkl",
            "Shot_PC1", "npxG")) {
  formula <- as.formula(paste(y, "~ post_Howmany | Player + after"))  
  model <- felm(formula, data = PC.data)
  
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
  
  PCs.kv_Results <- rbind(PCs.kv_Results, row)
}
print(xtable(PCs.kv_Results), 
      include.rownames=FALSE, 
      hline.after=c(-1,0,nrow(PCs.kv_Results)), 
      file="D:/Xujianuo/socialmedia paper/BLMproj/BLMCode/tables/PCs.kv_Results.tex",
      type="latex")




