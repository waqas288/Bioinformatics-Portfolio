
# read in the data
data <- read.table("WG_table.txt", header = TRUE, sep = "\t")

library(ggplot2)

# Create a new column "Ecotype"
data$Ecotype <- ifelse(data$comp == "Tidal", "Tidal", "Seasonal")

# Create a scatter plot of FST between short and long winged ecotypes
ggplot(data, aes(x = comp, y = Fst)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6) +
  labs(x = "comp", y = "Fst", title = "Genetic distance between short and long winged ecotypes")



# Create a scatter plot of FST between ecotypes, colored by country
ggplot(data, aes(x = comp, y = Fst, color = comp)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6) +
  labs(x = "comp", y = "Fst", color = "comp", 
       title = "Genetic distance between short and long winged ecotypes by country")




# Calculate Fst
Fst <- data$Fst

# Calculate quantiles for outlier detection
q95 <- quantile(Fst, 0.95)
q99 <- quantile(Fst, 0.99)

# Identify outlier loci
outliers <- which(Fst > q99)

# Print results
cat("Outlier loci (99% quantile):", outliers, "\n")










