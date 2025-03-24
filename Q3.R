#Q3 analysis: 

#To investigate potential differences in metabolic profiles between the sexes, we will use PCA. Here, it’s important to ensure that all data we use is numerical and appropriately scaled. Using the summary of our PCA results we plan to create a scatter plot  of the first two PCs to visualize, by sex, how patients are distributed along the PCs. In this plot, we should be able to determine whether or not male vs. female patients are clustered separately, and thus determine whether or not sex plays a role in metabolite profiling. 


# Load required libraries
library(tidyverse)
library(ggplot2)

# Load dataset
dat <- "/Users/emmamitchell/Downloads/RawData.csv"
df <- read.csv(dat, stringsAsFactors = FALSE)

# Remove non-numeric columns except "Sex"
df_clean <- df %>%
  select(-c(Sample.Name, Batch.Number, Class.name)) %>%  # Remove non-relevant columns
  filter(!is.na(Sex)) %>%  # Keep rows with "Sex"
  mutate(Sex = as.factor(Sex))  # Convert "Sex" to a factor

# Convert "< 0" and other non-numeric values to NA
df_clean[df_clean == "< 0"] <- NA

# Convert all columns (except "Sex") to numeric
df_clean <- df_clean %>%
  mutate(across(where(is.character), as.numeric))

# Remove columns with more than 20% missing values
df_clean <- df_clean %>%
  select(where(~ mean(is.na(.)) < 0.2))

# Remove rows with any remaining missing values
df_clean <- na.omit(df_clean)

# Extract numerical features for PCA
X <- df_clean %>% select(-Sex)

# Remove constant (zero-variance) columns
X <- X[, apply(X, 2, var) > 0]

# Standardize the data
X_scaled <- scale(X)

# Perform PCA
pca_result <- prcomp(X_scaled, center = TRUE, scale. = TRUE)

# Create a dataframe with first two principal components
pca_df <- data.frame(PC1 = pca_result$x[,1],
                     PC2 = pca_result$x[,2],
                     Sex = df_clean$Sex)

# Visualize PCA results
plot <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Sex)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(title = "PCA of Metabolite Profiles by Sex",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme(legend.title = element_blank())

plot




