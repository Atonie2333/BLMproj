#Create the plot with ggplot2

# Create plot individually
library(patchwork)
# Convert the date column to Date type to ensure proper sorting
#df_post_schedule$date <- as.Date(df_post_schedule$date, format="%Y/%m/%d")

# Group the data and calculate the cumulative sum for each group

df_post_schedule <- df_post_schedule %>%
  arrange(club, date) %>%  # Sort by club and date
  group_by(club) %>%  # Group by the club
  mutate(cumulative_posts = cumsum(post))  # Calculate the cumulative number of posts
df_post_schedule$cumulative_posts<-as.integer(df_post_schedule$cumulative_posts)
# Customize the theme to decrease text size for more compact plotting

# Retrieve all unique club names
clubs <- unique(df_post_schedule$club)
p<-NULL
# Create and save a separate plot for each club
for (club in clubs) {
  # Extract data for the current club
  club_data <- df_post_schedule %>% filter(club == !! club)
  
  # Create the plot using ggplot2
  single_plot<-ggplot(club_data, aes(x = date, y = cumulative_posts)) +
    #geom_line() +  # Line for the trend of posts over time
    geom_point() + # Points for each data entry
    theme_minimal() +  # Minimal theme for a nice layout
    
    labs(title = sprintf('Posts for %s', club),
         x = "Date",
         y = "Posts")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  if (is.null(p)) {
    p <- single_plot
  } else {
    p <- p + single_plot
  }
}
p_layout <- p + 
  plot_layout(ncol = 4, guides = "collect") + 
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "lines"), # Adjust the margin around each plot
    strip.text = element_text(size = 3) # Adjust the text size of facet labels if using facet_dwrap or facet_grid
  )
ggsave("post_schedule_individule.png", plot = p_layout,path = "plots"
       , width = 12, height = 9, dpi = 300)

library(gridExtra)

# Create an empty list to store the pie plots
plot_list <- list()
par(mar = c(2, 2, 2, 2), mfrow=c(4,5)) # define sub plot area
colors <- c("tomato",  "lightgreen", "lightgoldenrod1", "pink", "lightskyblue") # define colors
labels <- c("lab", "grn", "ldm", "con", "rfm") # define lables
main <- "Voting Distribution" # Title
for (i in 1:20) {
  slices <- as.numeric(unlist(club_status_nonzero[i, 6:10]))
  club <- club_status_nonzero[i, 1]
  
  # Create a pie plot using ggplot2
  votepie <- ggplot(data.frame(labels, slices), aes(x = "", y = slices, fill = labels)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    labs(title = club, x = "", y = "") +
    scale_fill_manual(values = colors)
  
  # Add the plot to the list
  plot_list[[i]] <- votepie
}

# Arrange the pie plots in a grid layout and save as one graph
combined_votepie <- grid.arrange(grobs = plot_list, ncol = 4)  # 4 columns for 20 plots
ggsave("votepie_plots.png", combined_votepie, width = 16, height = 12, 
       path = "plots")
#################################################################################