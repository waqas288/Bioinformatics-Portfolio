library(tidyverse) # Load the tidyverse package for data manipulation and visualization
library(MASS) # Load the MASS package for statistical functions

load("assess_data_24-resit.Rdata") # Load the data from the specified Rdata file

idx <- 20 # Set the index for the gene to be analyzed
c_cl <- 15:30 # Select columns 15 to 30 for the analysis (likely representing samples)

boxplot(log2(Y[, c_cl] + 1)) # Create a boxplot of the log2-transformed gene expression data for the selected columns

x <- patient_data$tissue[c_cl] # Extract the tissue type data for the selected columns
z <- patient_data$patient[c_cl] # Extract the patient ID data for the selected columns
tmp <- data.frame(y = Y[idx, c_cl], x = x, z = z, lib_size = colSums(Y[, c_cl])) # Create a data frame with gene expression, tissue type, patient ID, and library size
out <- glm(y ~ x + z + lib_size, data = tmp, family = "poisson") # Fit a Poisson regression model to the data
p_val <- summary(out)$coefficients[2, 4] # Extract the p-value for the tissue type variable from the model summary


# Basic statistics to identify problematic samples
summary_stats <- colSums(Y)
boxplot(summary_stats, main="Boxplot of Column Sums", ylab="Sum of Expression Levels")

# Identify the sample with the maximum or minimum value
problematic_sample <- which.max(summary_stats) # or which.min(summary_stats)

# Plot the problematic sample
plot(summary_stats, main="Sum of Expression Levels per Sample", ylab="Sum of Expression Levels", xlab="Sample Index")
points(problematic_sample, summary_stats[problematic_sample], col="red", pch=19)



# Function to perform Poisson regression and extract p-values with tissue type as covariate
get_p_value_tissue <- function(gene_expression, tissue_type) {
  tmp <- data.frame(y = gene_expression, x = tissue_type)
  out <- glm(y ~ x, data = tmp, family = "poisson")
  p_val <- summary(out)$coefficients[2, 4]  # Extract p-value
  return(p_val)
}


# Extract tissue type for all samples
tissue_type <- patient_data$tissue

# Perform regression for all genes with tissue type as the only covariate
p_values <- apply(Y, 1, get_p_value_tissue, tissue_type = tissue_type)

# Calculate -log10 p-values
log_p_values <- -log10(p_values)

# Plot the histogram of -log10 p-values
hist(log_p_values, main = "Histogram of -log10 p-values with Tissue Covariate",
     xlab = "-log10(p-value)", col = "lightblue", border = "white")




# Extract tissue type and patient ID for all samples
tissue_type <- factor(patient_data$tissue)
patient_id <- patient_data$patient

# Function to perform Poisson regression and extract p-values with only tissue type as a covariate
get_p_value_tissue <- function(gene_expression, tissue_type) {
  tmp <- data.frame(y = gene_expression, x = tissue_type)
  out <- glm(y ~ x, data = tmp, family = "poisson")
  p_val <- summary(out)$coefficients[2, 4]
  return(p_val)
}

# Function to perform Poisson regression with additional patient ID covariate and extract p-values
get_p_value_with_covariate <- function(gene_expression, tissue_type, patient_id) {
  tmp <- data.frame(y = gene_expression, x = tissue_type, z = patient_id, lib_size = colSums(Y))
  out <- glm(y ~ x + z + lib_size, data = tmp, family = "poisson")
  p_val <- summary(out)$coefficients[2, 4]
  return(p_val)
}

# Perform regression for all genes with tissue type as the only covariate
p_values_tissue <- apply(Y, 1, get_p_value_tissue, tissue_type = tissue_type)

# Perform regression for all genes with additional patient ID covariate
p_values_with_covariate <- apply(Y, 1, get_p_value_with_covariate, tissue_type = tissue_type, patient_id = patient_id)

# Convert p-values to -log10 scale
log_p_values_tissue <- -log10(p_values_tissue)
log_p_values_with_covariate <- -log10(p_values_with_covariate)

# Assign colors based on tissue type
colors_tissue <- ifelse(tissue_type == "Tumour", "red", "lightgreen")

# Plot the -log10 p-values for tissue type only with colors
hist(log_p_values_tissue, main="Histogram of -log10 p-values with Tissue Covariate", 
     xlab="-log10(p-value)", col=colors_tissue, breaks=50)
legend("topright", legend=c("Tumour", "Normal"), fill=c("red", "lightgreen"), bty="n")

# Assign colors based on tissue type and patient ID
colors_covariate <- ifelse(tissue_type == "Tumour", "red", "lightgreen")

# Plot the -log10 p-values for both tissue type and patient ID as covariates with colors
hist(log_p_values_with_covariate, main="Histogram of -log10 p-values with Tissue and Patient Covariates", 
     xlab="-log10(p-value)", col=colors_covariate, breaks=50)
legend("topright", legend=c("Tumour", "Normal"), fill=c("red", "lightgreen"), bty="n")
# Assign colors based on tissue type
colors <- ifelse(tissue_type == "Tumour", "red", "blue")

# Comparison plot with tumor and normal samples indicated
plot(log_p_values_tissue, log_p_values_with_covariate, main="Comparison of -log10 p-values",
     xlab="-log10(p-values) with Tissue Covariate", ylab="-log10(p-values) with Tissue and Patient Covariates", col=colors)
abline(0, 1, col="black") # Add a reference line

# Calculate the difference in p-values
comparison <- data.frame(
  Gene = 1:nrow(Y),
  P_Value_Without_Covariate = log_p_values_tissue,
  P_Value_With_Covariate = log_p_values_with_covariate
)
comparison$P_Value_Difference <- log_p_values_tissue - log_p_values_with_covariate

# Plot the differences with tumor and normal samples indicated
plot(comparison$Gene, comparison$P_Value_Difference, main="Difference in -log10(p-values)",
     xlab="Gene Index", ylab="Difference in -log10(p-values)", col=colors)
legend("topright", legend=c("Tumor Samples", "Normal Samples"), col=c("red", "blue"), pch=1)




# Function to perform Poisson regression and extract p-values with tissue type as covariate
get_p_value_tissue <- function(gene_expression, tissue_type) {
  tmp <- data.frame(y = gene_expression, x = tissue_type)
  out <- glm(y ~ x, data = tmp, family = "poisson")
  p_val <- summary(out)$coefficients[2, 4]  # Extract p-value
  return(p_val)
}

# Assuming Y is your gene expression matrix and tissue_type is extracted from patient_data
p_values <- apply(Y, 1, get_p_value_tissue, tissue_type = tissue_type)

log_p_values <- -log10(p_values)

plot_data <- data.frame(log_p_value = log_p_values, tissue_type = tissue_type)

library(ggplot2)  # Load ggplot2 for advanced plotting

# Plot histogram with distinction between tumor and normal tissues
ggplot(plot_data, aes(x = log_p_value, fill = tissue_type)) +
  geom_histogram(position = "identity", bins = 30, alpha = 0.7, color = "black") +
  labs(title = "Histogram of -log10 p-values with Tissue Covariate",
       x = "-log10(p-value)", y = "Frequency") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Normal", "Tumor")) +
  theme_minimal()

