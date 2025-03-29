library(dplyr)
library(tidyr)
library(caret)
library(MASS)  
url <- "https://raw.githubusercontent.com/emmaamitchyy/BIOL432_Gr3_Final/refs/heads/main/Data_Input/RawData.csv"
df <- read.csv(url)

df$Age <- as.numeric(df$Age)
df$CT <- as.numeric(df$CT)
df$Age[is.na(df$Age)] <- mean(df$Age, na.rm = TRUE) 
head(df)

# Step 1: Load required libraries 
library(randomForest)
library(ggplot2)
library(dplyr)


# Step 2: Convert special entries to NA
df[df == "< 0"] <- NA
df[df == "No Peak"] <- NA

# Step 3: Create COVID vs Non-COVID label
df$Label <- ifelse(grepl("COVID19", df$Class.name, ignore.case = TRUE), "COVID", "Non-COVID")
df$Label <- as.factor(df$Label)

# Step 4: Drop metadata columns
drop_cols <- c("Sample.Name", "Batch.Number", "Sex", "Age", "CT", "Class.name")
df <- df[, !(names(df) %in% drop_cols)]

# Step 5: Convert all feature columns to numeric
df[, names(df) != "Label"] <- lapply(df[, names(df) != "Label"], function(x) as.numeric(as.character(x)))

# Step 6: Drop columns with >20% missing values
threshold <- 0.2 * nrow(df)
df <- df[, colSums(is.na(df)) < threshold]

# Step 7: Drop rows with missing Label (if any)
df <- df[!is.na(df$Label), ]

# Step 8: Impute missing values using column median
for (col in names(df)) {
  if (col != "Label" && is.numeric(df[[col]])) {
    df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
  }
}

# Step 9: Separate features and labels
X <- df[, names(df) != "Label"]
y <- df$Label

# Step 10: Fit Random Forest model
set.seed(123)
rf_model <- randomForest(x = X, y = y, importance = TRUE)

# Step 11: Extract importance (Mean Decrease Accuracy)
importance_df <- importance(rf_model, type = 1)
importance_df <- data.frame(Metabolite = rownames(importance_df),
                            MeanDecreaseAccuracy = importance_df[, 1])

# Step 12: Get top 20 important metabolites
top20 <- importance_df %>%
  arrange(desc(MeanDecreaseAccuracy)) %>%
  slice(1:20)

# Step 13: Plot
ggplot(top20, aes(x = MeanDecreaseAccuracy,
                  y = reorder(Metabolite, MeanDecreaseAccuracy))) +
  geom_col(fill = "steelblue") +
  labs(title = "Top 20 Metabolites Differentiating COVID vs Non-COVID",
       x = "Mean Decrease Accuracy",
       y = "Metabolite") +
  theme_minimal()



library(randomForest)
library(ggplot2)
library(gridExtra)  # **Needed for side-by-side plots**

# Load dataset
url <- "https://raw.githubusercontent.com/emmaamitchyy/BIOL432_Gr3_Final/refs/heads/main/Data_Input/RawData.csv"
df <- read.csv(url)

# Convert relevant columns to numeric
df$Age <- as.numeric(df$Age)
df$CT <- as.numeric(df$CT)
df$Age[is.na(df$Age)] <- mean(df$Age, na.rm = TRUE)

# Convert special entries to NA
df[df == "< 0"] <- NA
df[df == "No Peak"] <- NA

# Create COVID vs Non-COVID label
df$Label <- ifelse(grepl("COVID19", df$Class.name, ignore.case = TRUE), "COVID", "Non-COVID")
df$Label <- as.factor(df$Label)

# Drop metadata columns
drop_cols <- c("Sample.Name", "Batch.Number", "Sex", "Age", "CT", "Class.name")
df <- df[, !(names(df) %in% drop_cols)]

# Convert all feature columns to numeric
df[, names(df) != "Label"] <- lapply(df[, names(df) != "Label"], function(x) as.numeric(as.character(x)))

# Drop columns with >20% missing values
threshold <- 0.2 * nrow(df)
df <- df[, colSums(is.na(df)) < threshold]

# Drop rows with missing Label
df <- df[!is.na(df$Label), ]

# Impute missing values using column median
for (col in names(df)) {
  if (col != "Label" && is.numeric(df[[col]])) {
    df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
  }
}

# Separate features and labels
X <- df[, names(df) != "Label"]
y <- df$Label

# Fit Random Forest model
set.seed(123)
rf_model <- randomForest(x = X, y = y, importance = TRUE)

# Extract feature importance
importance_df <- importance(rf_model)
importance_df <- data.frame(
  Metabolite = rownames(importance_df),
  MeanDecreaseAccuracy = importance_df[, 1],  # Accuracy-based importance
  MeanDecreaseGini = importance_df[, 2]  # **Gini-based importance added**
)

# Get top 20 important metabolites
top20_accuracy <- importance_df %>%
  arrange(desc(MeanDecreaseAccuracy)) %>%
  slice(1:20)

top20_gini <- importance_df %>%
  arrange(desc(MeanDecreaseGini)) %>%
  slice(1:20)

# Create plots
p1 <- ggplot(top20_accuracy, aes(x = MeanDecreaseAccuracy, y = reorder(Metabolite, MeanDecreaseAccuracy))) +
  geom_point() +
  labs(title = "Feature Importance (Mean Decrease Accuracy)", x = "MeanDecreaseAccuracy", y = "Metabolite") +
  theme_minimal()

p2 <- ggplot(top20_gini, aes(x = MeanDecreaseGini, y = reorder(Metabolite, MeanDecreaseGini))) +
  geom_point() +
  labs(title = "Feature Importance (Mean Decrease Gini)", x = "MeanDecreaseGini", y = "Metabolite") +
  theme_minimal()

# Arrange plots side-by-side
grid.arrange(p1, p2, ncol = 2)  # **Combine both plots**
