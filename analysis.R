# Importing data

my_data <- read.table("assignment_Data (1).tsv", header=TRUE, sep="\t")
head(my_data)

# CHECK DATA TYPES

str(my_data)





# perform a statistical test to identify if the variants in BMX or BRCA2 are associated with the patient phenotype

# Create a 2x2 contingency table for the BMX variant and patient phenotype
bmx_table <- table(my_data$Phenotype, my_data$BMX_Genotype)

# Perform the chi-squared test of independence for the BMX variant
bmx_test <- chisq.test(bmx_table)

# Print the results of the BMX chi-squared test
print(bmx_test)

# Create a 2x2 contingency table for the BRCA2 variant and patient phenotype
brca2_table <- table(my_data$Phenotype, my_data$BRCA2_Genotype)

# Perform the chi-squared test of independence for the BRCA2 variant
brca2_test <- chisq.test(brca2_table)

# Print the results of the BRCA2 chi-squared test
print(brca2_test)





#  Perform a statistical test to identify if the expression of BMX is affected by rs139052738

# Load the data
assignment_data <- read.delim("assignment_data (1).tsv")

# column names

colnames(assignment_data)


# Subset the data to only include BMX expression and rs139052738 genotype
bmx_data <- subset(assignment_data, select = c("Expression.BMX", "rs139052738"))

# Convert the genotype column to a factor variable
bmx_data$rs139052738_genotype <- factor(bmx_data$rs139052738)

# Fit a linear regression model to the data
lm_bmx <- lm(Expression.BMX ~ rs139052738, data = bmx_data)

# Print the results of the linear regression model
summary(lm_bmx)





#  Perform a statistical test to identify if the expression of BRCA2 is affected by rs886040801

# subset the data with BRCA2 expression and rs886040801 genotypes
brca2_data <- subset(assignment_data, select = c("Expression.BRCA2", "rs886040801"))

# fit linear regression model
model <- lm(Expression.BRCA2 ~ rs886040801, data = brca2_data)

# summarize the model results
summary(model)





# Perform a statistical test to identify if expression of BMX affects expression of BRCA2

# Subset the data to only include Expression.BMX and Expression.BRCA2 columns
bmx_brca2_data <- subset(assignment_data, select = c("Expression.BMX", "Expression.BRCA2"))

# Fit a linear regression model
model <- lm(Expression.BRCA2 ~ Expression.BMX, data = bmx_brca2_data)

# Print the model summary
summary(model)




# The linear regression model results suggest that there is no significant association between 
# the expression of BMX and the rs139052738 genotype. The p-value for the rs139052738C/C genotype 
# is 0.49, which is not significant at the conventional alpha level of 0.05. Additionally, the
# p-value for the rs139052738C/T genotype is 0.093, which is close to the significance level but 
# still not significant. The other genotypes (rs139052738T, rs139052738T/C, and rs139052738T/T) 
# also have p-values above 0.05, suggesting no significant association between the genotype and 
# the expression of BMX. The R-squared value of 0.028 suggests that only a small proportion of the 
# variation in BMX expression is explained by the rs139052738 genotype.

# In conclusion, based on the linear regression model results, there is no significant evidence of 
# an association between the rs139052738 genotype and the expression of BMX.




