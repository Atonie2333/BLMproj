# Identify columns with NAs
columns_with_nas <- colSums(is.na(perf_subset)) > 0
names(columns_with_nas)

#Delete columns with NAs
#perf_subset <- perf_subset[, !columns_with_nas]


#Categorize Variables
Pass_subset <- perf_subset %>%
  select(Cmp:Ast, xAG:Live, TB:Crs, Blocked_Passes) 
Pass_subset %>%
  describe() %>%
  select(n, mean, sd) %>%
  mutate(across(everything(), round, 3)) %>%
  as.data.frame() %>%
  rename(N = n, Mean = mean, St.Dev = sd) %>%
  cbind(Statistic = names(Pass_subset), .) %>%
  gt(caption = "Descriptive Statistic of Passing Specifications") %>%
  gtsave(filename = "Des_Passing.tex", 
         path = "D:/Xujianuo/socialmedia paper/BLMproj/BLMCode/tables")

Dead_subset <- perf_subset %>%
  select(TI:Corner_Str, Dead, FK)
Dead_subset %>%
  describe() %>%
  select(n, mean, sd) %>%
  mutate(across(everything(), round, 3)) %>%
  as.data.frame() %>%
  rename(N = n, Mean = mean, St.Dev = sd) %>%
  cbind(Statistic = names(Dead_subset), .) %>%
  gt(caption = "Descriptive Statistic of Dead Ball Specifications") %>%
  gtsave(filename = "Des_Dead.tex", 
         path = "D:/Xujianuo/socialmedia paper/BLMproj/BLMCode/tables")

Defend_subset <- perf_subset %>%
  select(Tkl:Err) 
Defend_subset %>%
  describe() %>%
  select(n, mean, sd) %>%
  mutate(across(everything(), round, 3)) %>%
  as.data.frame() %>%
  rename(N = n, Mean = mean, St.Dev = sd) %>%
  cbind(Statistic = names(Defend_subset), .) %>%
  gt(caption = "Descriptive Statistic of Defensive Actions Specifications") %>%
  gtsave(filename = "Des_Defend.tex", 
         path = "D:/Xujianuo/socialmedia paper/BLMproj/BLMCode/tables")

Possession_subset <- perf_subset %>%
  select(Touches:PrgR)
Possession_subset %>%
  describe() %>%
  select(n, mean, sd) %>%
  mutate(across(everything(), round, 3)) %>%
  as.data.frame() %>%
  rename(N = n, Mean = mean, St.Dev = sd) %>%
  cbind(Statistic = names(Possession_subset), .) %>%
  gt(caption = "Descriptive Statistic of Possessions Specifications") %>%
  gtsave(filename = "Des_Possession.tex", 
         path = "D:/Xujianuo/socialmedia paper/BLMproj/BLMCode/tables")

Shot_subset <- perf_subset %>%
  select(npxG:SoT, xG) 
Shot_subset %>%
  describe() %>%
  select(n, mean, sd) %>%
  mutate(across(everything(), round, 3)) %>%
  as.data.frame() %>%
  rename(N = n, Mean = mean, St.Dev = sd) %>%
  cbind(Statistic = names(Shot_subset), .) %>%
  gt(caption = "Descriptive Statistic of Shot Specifications") %>%
  gtsave(filename = "Des_Shot.tex", 
         path = "D:/Xujianuo/socialmedia paper/BLMproj/BLMCode/tables")

Miscellaneous_subset <- perf_subset %>%
  select(CrdY:Lost)

##Filter variables whose correlation with others are correlated.
#Correlation Matrix
corrmatrix_Passing <- cor(Pass_subset)
corrmatrix_Dead <- cor(Dead_subset)
corrmatrix_Possession <- cor(Possession_subset)
corrmatrix_Shot <- cor(Shot_subset)
corrmatrix_Defend <- cor(Defend_subset)
corrmatrix_Miscellaneous <- cor(Miscellaneous_subset)
# Find the columns where at least have one corr significant and greater than 0.5.
#Define filter function as conditions.

find_significant_correlations <- function(cor_matrix) {
  p_values <- corr.test(cor_matrix)$p
  significant_cols <- colnames(cor_matrix)[apply(p_values < 0.01, 1, any)]
  return(significant_cols)
}

find_highCorColumns <- function(cor_matrix) {
  highCorColumns <- apply(cor_matrix, 2, function(x) any(abs(x) > 0.5 & abs(x) < 1 & !is.na(x)))
  colnames(cor_matrix)[highCorColumns]
}
  #Function to filter columns at least have one corr significant and greater than 0.5.
find_corr_cols <- function(df) {
  cor_matrix <- cor(df)
  
  # Find cols significant
  significant_cols <- find_significant_correlations(cor_matrix)
  
  # Find cols with corr greater than 0.5
  highCor_columns <- find_highCorColumns(cor_matrix)
  
  # Find cols fit both conditions.
  high_corr_significant_cols <- intersect(significant_cols, highCor_columns)
  
  return(high_corr_significant_cols)
}

#Create a list including all subsets
subset_list <- list(Pass_subset, Dead_subset, Possession_subset, 
                  Shot_subset, Defend_subset, Miscellaneous_subset)

# Apply the function on each subset
Selected_list <- lapply(subset_list, find_corr_cols)

#Correlation Plot

gr <- colorRampPalette(c( "#F3C846", "white", "#4A7298"))

pdf("plots/cor.plot/Corrplot_Passing.pdf", width = 12,
    title = "Correlation Plot of Passing Specifications")
Corrplot_Passing <- perf_subset %>% 
  select(all_of(Selected_list[[1]])) %>%
  cor.plot(
    stars = T,
    cex = 0.75,
    gr = gr,
    upper = F,
    main = "Correlation Plot of Passing Specifications"
  )
dev.off()

pdf("plots/cor.plot/Corrplot_Dead.pdf", width = 12,
    title = "Correlation Plot of Dead Ball Specifications")
Corrplot_Dead <- perf_subset %>% 
  select(all_of(Selected_list[[2]])) %>%
  cor.plot(
    stars = T,
    cex = 1,
    gr = gr,
    upper = F,
    main = "Correlation Plot of Dead Ball Specifications"
  )
dev.off()

pdf("plots/cor.plot/Corrplot_Defend.pdf", width = 12,
    title = "Correlation Plot of Defensive Action Specifications")
Corrplot_Defend <- perf_subset %>% 
  select(all_of(Selected_list[[5]])) %>%
  cor.plot(
    stars = T,
    cex = 1,
    gr = gr,
    upper = F,
    main = "Correlation Plot of Defensive Action Specifications"
  )
dev.off()

pdf("plots/cor.plot/Corrplot_Possession.pdf", width = 12,
    title = "Correlation Plot of Possession Specifications")
Corrplot_Possession <- perf_subset %>% 
  select(all_of(Selected_list[[3]])) %>%
  cor.plot(
    stars = T,
    cex = 0.75,
    gr = gr,
    upper = F,
    main = "Correlation Plot of Possession Specifications"
  )
dev.off()

pdf("plots/cor.plot/Corrplot_Miscellaneous.pdf", width = 12, 
    title = "Correlation Plot of Miscellaneous Specifications")
Corrplot_Miscellaneous <- perf_subset %>% 
  select(all_of(Selected_list[[6]])) %>%
  cor.plot(
    stars = T,
    cex = 1, 
    gr = gr,
    upper = F,
    main = "Correlation Plot of Miscellaneous Specifications"
  )

dev.off()

pdf("plots/cor.plot/Corrplot_Shot.pdf", width = 12,
    title = "Correlation Plot of Shot Specifications")
corrplot_Shot <- perf_subset %>% 
  select(all_of(Selected_list[[4]])) %>%
  cor.plot(
    stars = T,
    cex = 1,
    gr = gr,
    upper = F,
    main = "Correlation Plot of Shot Specifications"
  )
dev.off()

