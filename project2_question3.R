path <- file.choose()

data <- read.table(path, sep = ",", header = TRUE, stringsAsFactors = FALSE)

str(data)

data[1:5,]

# Converting categorical variables to numeric
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

data[1:5,]

summary(data)

# As there are no NAs values we do not have to run na.omit(data)

describe(data)

# Normalization functions
normalize <- function(x) {((x-min(x))/(max(x) - min(x)))}

zscore <- function(x){(x-mean(x))/sd(x)}

data.norm <- as.data.frame(lapply(data, normalize))
data.norm[1:10,]

data.znorm <- as.data.frame(lapply(data, zscore))
data.znorm[1:10,]

colnames(data)[colnames(data) == "family_history_with_overweight"] <- "family_history"
cor_matrix <- cor(data)
corrplot(cor_matrix)
colnames(data)[colnames(data) == "family_history"] <- "family_history_with_overweight"

# Clustering on the whole data set :
object <- kmeans(data.norm, centers = 3, iter.max = 10, nstart = 25)

str(object)

object

# Get cluster centers for each point
fitted(object, method = "centers")

# Get cluster assignments (which cluster each point belongs to)
fitted(object, method = "classes")

factoextra::fviz_cluster(object, data.norm)

object$centers

extract_cluster <- function(kmeans_object, cluster_number, data_norm) {
  clusters <- kmeans_object$cluster
  data_in_cluster <- data_norm[clusters == cluster_number, ]
  return(data_in_cluster)
}

cluster_1_data <- extract_cluster(object, 1, data.norm)
cluster_1_data[1:10,]

wssplot <- function(data, nc = 15, seed = 1234) {
  wss <- numeric(nc) 
  wss[1] <- (nrow(data) - 1) * sum(apply(data, 2, var)) 
  
  for (i in 2:nc) {
    set.seed(seed)
    kmeans_result <- kmeans(data, centers = i)
    wss[i] <- kmeans_result$tot.withinss
  }
  
  plot(1:nc, wss, type = "b", 
       xlab = "Number of clusters", 
       ylab = "Within groups sum of squares")
}

wssplot(data.norm, nc = 15)

factoextra::fviz_nbclust(data.norm, FUNcluster = kmeans, method = "wss", k.max = 20, verbose = TRUE)
# From this we can see that the optimal number of clusters is around 9 or 10

# Trying with 10 clusters
object2 <- kmeans(data.norm, centers = 10, iter.max = 15, nstart = 30)
object2
factoextra::fviz_cluster(object2, data.norm)

object2$centers
