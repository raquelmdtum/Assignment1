library(ggplot2)
load("Dtrain.RData")

# Open a new plot window
if (.Platform$OS.type == "windows") {
  x11()  # For Windows
} else if (Sys.info()["sysname"] == "Darwin") {
  quartz()  # For macOS
} else {
  X11()  # For Linux
}

# Exercise 1.1

# Year breaks for x-axis (whole years only)
year_breaks <- seq(floor(min(Dtrain$year)), ceiling(max(Dtrain$year)), by = 1)

p <- ggplot(Dtrain, aes(x = year, y = total)) +
  #geom_line(color = "blue") +  # Line plot
  geom_point(color = "pink") +  # Points
  labs(title = "Number of Vehicles Registered in Denmark",
       x = "Year",
       y = "Total Vehicles (in millions)") +
  theme_minimal() +
  scale_x_continuous(breaks = year_breaks, labels = as.character(year_breaks)) +  # Whole years only
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))  # Keep labels horizontal

#ggsave("exer11.pdf", plot = p, width = 8, height = 5, dpi = 300)
print(p)

# Exercise 1.2
