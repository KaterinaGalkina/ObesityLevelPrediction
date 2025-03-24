library(sna)
library(psych)
library(corrplot)
library(dplyr)
library(factoextra)
library(class)
library(plotly) # For 3-D representation
library(gmodels) # For cross table

# Data import
path <- file.choose()
data <- read.table(path, sep = ",", header = TRUE, stringsAsFactors = FALSE)
data[1:10, 1:17]

#-------------------------------------------------------------------------
# Question 2 - c - Convert categorical variables to numeric
#-------------------------------------------------------------------------

data <- data %>%
  mutate(
    Gender = ifelse(Gender == "Female", 1, 0),
    
    family_history_with_overweight = ifelse(family_history_with_overweight == "yes", 1, 0),
    
    FAVC = ifelse(FAVC == "yes", 1, 0),
    
    CAEC = case_when(
      CAEC == "no" ~ 0,
      CAEC == "Sometimes" ~ 1,
      CAEC == "Frequently" ~ 2,
      CAEC == "Always" ~ 3
    ),
    
    SMOKE = ifelse(SMOKE == "yes", 1, 0),
    
    SCC = ifelse(SCC == "yes", 1, 0),
    
    CALC = case_when(
      CALC == "no" ~ 0,
      CALC == "Sometimes" ~ 1,
      CALC == "Frequently" ~ 2,
      CALC == "Always" ~ 3
    ),
    
    MTRANS = case_when(
      MTRANS == "Automobile" ~ 0,
      MTRANS == "Motorbike" ~ 1,
      MTRANS == "Public_Transportation" ~ 2,
      MTRANS == "Bike" ~ 3,
      MTRANS == "Walking" ~ 4
    ),
    
    NObeyesdad = case_when(
      NObeyesdad == "Obesity_Type_III" ~ 6,
      NObeyesdad == "Obesity_Type_II" ~ 5,
      NObeyesdad == "Obesity_Type_I" ~ 4,
      NObeyesdad == "Overweight_Level_II" ~ 3,
      NObeyesdad == "Overweight_Level_I" ~ 2,
      NObeyesdad == "Normal_Weight" ~ 1,
      NObeyesdad == "Insufficient_Weight" ~ 0
    )
  )

data[1:10, 1:17]

# Normalization functions
normalize <- function(x) {((x-min(x))/(max(x) - min(x)))}

zscore <- function(x){(x-mean(x))/sd(x)}


data.norm <- as.data.frame(lapply(data[,1:17], normalize))
data.norm[1:10,]

data.znorm <- as.data.frame(lapply(data[,1:17], zscore))
data.znorm[1:10,]


# Question 1
#-----------------------------
# 2D Plots
#-----------------------------

# First we print the whole matrix of pairwise plotting, but it is not very 
# usefull as everything is too small to perceive possible linearity 
plot(data)

# Let's plot each of th 16 values with the 17 to see possible linearity 
# But still not very visible
plot_titles <- colnames(data)[1:16]  # Titles of the first 16 columns 
for (i in 1:16) {
  plot(data[, i], data[, 17], main = paste("Correlation between", plot_titles[i], "and Obesity Level"),
       xlab = plot_titles[i], ylab = "Obesity Level", col = rgb(0, 0, 0, 0.2), pch = 16)
}

# Change the column name to see better
colnames(data.znorm)[colnames(data.znorm) == "family_history_with_overweight"] <- "family_history"

# Here we can better see possible correlation 
cor_matrix <- cor(data.znorm)

# Initial way to plot
corrplot(cor_matrix)

# With some modifications to better visualize 
corrplot(cor_matrix,
         tl.srt = 45, addCoef.col = "black")

#-----------------------------
# 3D Plots
#-----------------------------

# Age, family_history, and Obesity Level
plot_ly(data, x = ~Age, y = ~family_history_with_overweight, z = ~NObeyesdad, color = ~NObeyesdad, colors = c('#636EFA', 'green', 'red')) %>%
  add_markers() %>%
  layout(scene = list(
    xaxis = list(title = 'Age'),
    yaxis = list(title = 'family_history_with_overweight'),
    zaxis = list(title = 'Obesity Level')))

# FAVC, family_history_with_overweight, and Obesity Level
plot_ly(data, x = ~FAVC, y = ~family_history_with_overweight, z = ~NObeyesdad, color = ~NObeyesdad, colors = c('#636EFA', 'green', 'red')) %>%
  add_markers() %>%
  layout(scene = list(
    xaxis = list(title = 'FAVC'),
    yaxis = list(title = 'family_history_with_overweight'),
    zaxis = list(title = 'Obesity Level')))

# Example 3: FAVC, FAF, and Obesity Level
plot_ly(data, x = ~CAEC, y = ~FAF, z = ~NObeyesdad, color = ~NObeyesdad, colors = c('#636EFA', 'green', 'red')) %>%
  add_markers() %>%
  layout(scene = list(
    xaxis = list(title = 'CAEC'),
    yaxis = list(title = 'FAF'),
    zaxis = list(title = 'Obesity Level')))


# Question 2

#-----------------------------
# a - Data - Investigation
#-----------------------------

summary(data)

describe(data)

#-----------------------------
# b - Sub-setting the data
#-----------------------------

# For this we are simply going to choose values which revealed the most 
# correlation in the question 1 with the level of obesity, namely :

# weight : 0.91
# family_history : 0.51
# Age : 0.28
# FAVC : 0.25
# CALC : 0.15

# We are not including these attributes, as the correlation is little : 
# CH2O : 0.13
# Height : 0.13

# Creating the subset of the data set with the most correlated attributes
subset_data <- data %>%
  select(Weight, family_history_with_overweight, Age, FAVC, CALC, NObeyesdad)
subset_data[1:10,]

#---------------------------------------------
# d - Creating training and testing sets
#---------------------------------------------

# Normalizing our initial subset 
subset_data.norm <- as.data.frame(lapply(subset_data, normalize))

# Dividing into training and testing sets
subset_data.norm.rows = nrow(subset_data.norm)

# First training and testing sets with 70-30% division 
subset_data.norm.sample1 = 0.7
subset_data.rows1 = subset_data.norm.sample1 * subset_data.norm.rows

subset_data.train.index1 = sample(subset_data.norm.rows, subset_data.rows1)

subset_data.train1 = subset_data.norm[subset_data.train.index1,]
subset_data.test1 = subset_data.norm[-subset_data.train.index1,]

# Second training and testing sets with 60-40% division 
subset_data.norm.sample2 = 0.6
subset_data.rows2 = subset_data.norm.sample2 * subset_data.norm.rows

subset_data.train.index2 = sample(subset_data.norm.rows, subset_data.rows2)

subset_data.train2 = subset_data.norm[subset_data.train.index2,]
subset_data.test2 = subset_data.norm[-subset_data.train.index2,]

# Third training and testing sets with 50-50% division 
subset_data.norm.sample3 = 0.5
subset_data.rows3 = subset_data.norm.sample3 * subset_data.norm.rows

subset_data.train.index3 = sample(subset_data.norm.rows, subset_data.rows3)

subset_data.train3 = subset_data.norm[subset_data.train.index3,]
subset_data.test3 = subset_data.norm[-subset_data.train.index3,]


# Question 4

# Let's see what number of clusters is the best : 
wssplot(subset_data.norm, nc = 15)
# We can see that 6 is the best one 

#-----------------------------------------------------------------------
#   First prediction using Weight, family_history_with_overweight, Age, 
#   FAVC and CALC attributes 
#   Training and Testing sets division : 70-30%
#-----------------------------------------------------------------------

subset_data.train1.glm = glm(NObeyesdad ~ Weight + family_history_with_overweight + Age + FAVC + CALC, 
                            family = gaussian, data = subset_data.train1)

summary(subset_data.train1.glm)

plot(subset_data.train1.glm)

confint(subset_data.train1.glm)

subset_data.train1.glm.anova = anova(subset_data.train1.glm, test = "Chisq")
subset_data.train1.glm.anova

subset_data.test1.pred = predict(subset_data.train1.glm, newdata = subset_data.test1)
subset_data.test1.pred

summary(subset_data.test1.pred)

subset_data.test1.pred.k6 = kmeans(subset_data.test1.pred, centers = 6)
subset_data.test1.kmeans.k6 = kmeans(subset_data.test1, centers = 6)

subset_data.test1.ct.k6 = CrossTable(subset_data.test1.pred.k6$cluster, 
                                    subset_data.test1.kmeans.k6$cluster, 
                                    prop.chisq = TRUE)

confusion_matrix1 <- subset_data.test1.ct.k6$t

print_measurements <- function(confusion_matrix) {
  matrix_values <- as.matrix(confusion_matrix)
  
  num_clusters <- nrow(matrix_values)
  
  TP <- numeric(num_clusters)
  FP <- numeric(num_clusters)
  FN <- numeric(num_clusters)
  TN <- numeric(num_clusters)
  
  for (i in 1:num_clusters) {
    TP[i] <- matrix_values[i, i]
    FP[i] <- sum(matrix_values[, i]) - TP[i]
    FN[i] <- sum(matrix_values[i, ]) - TP[i]
    TN[i] <- sum(matrix_values) - TP[i] - FP[i] - FN[i]
  }
  
  accuracy <- (TP + TN) / (TP + FP + FN + TN)
  error <- (FP + FN) / (TP + FP + FN + TN)
  sensivity <- TP / (TP + FN)
  specificity <- TN / (FP + TN)
  precision <- TP / (TP + FP) 
  f_measure <- 2 * (precision * sensivity) / (precision + sensivity)
  
  cat("False Positive: ", FP, "\n")
  cat("False Negative: ", FN, "\n")
  cat("True Positive: ", TP, "\n")
  cat("True Negative: ", TN, "\n")
  
  cat("Accuracy: ", accuracy, "\n")
  cat("Error: ", error, "\n")
  cat("Sensivity: ", sensivity, "\n")
  cat("Specificity: ", specificity, "\n")
  cat("Precision: ", precision, "\n")
  cat("F - measure: ", f_measure, "\n")
}

print_measurements(confusion_matrix1)

