### Conflicing claims based feature importance methods
### 

install.packages("faraway")
install.packages("tidyverse")
install.packages("dplr")
install.packages("GameTheory")
install.packages("rgl")
install.packages("Hmisc")
install.packages("xtable")
#install.packages("dplyr")

library(faraway)
library(tidyverse)
#library(dplyr)
library(GameTheory)
install.packages("CoopGame")
library(CoopGame)
library(Hmisc)
library(xtable)

################################### SEATPOS DATA ANALYSIS #######################
View(seatpos) 
head(seatpos)
str(seatpos)
hist(seatpos$Age)
hist(seatpos$Weight)
hist(seatpos$HtShoes)
hist(seatpos)

#### SHAPLEY VALUE FOR ALL VARIABLES ##############################
data <- read.csv('r_squared_results_seatpos.csv', header = TRUE)
View(data)
r_squaredCoalitions <- c(data$r_squared)
r_squaredCoalitions
features <- DefineGame(8, r_squaredCoalitions)
summary(features)
length(features)
View(features)
feature_names <- c("Age", "Weight", "HtShoes", "Ht", "Seated", "Arm", "Thigh", "Leg")
length(feature_names)
ShapleyValueCalculation <- ShapleyValue(features, feature_names)
summary(ShapleyValueCalculation)


### check the performance of the models based on the top-N features
shapley_val <- data.frame(
  Feature = c("Age", "Weight", "HtShoes", "Ht", "Seated", "Arm", "Thigh", "Leg"),
  Shapley_values = c(0.03, 0.06, 0.12, 0.12, 0.09, 0.058, 0.057, 0.132)
)          

shap_ordered <- shapley_val[order(- shapley_val$Shapley_values), ]

m1 <- lm(hipcenter ~  Leg, data = seatpos)
v1_s<- summary(m1)$r.squared
v1_s

m2 <- lm(hipcenter ~  Leg + HtShoes, data = seatpos)
v2_s <-summary(m2)$r.squared
v2_s

m3 <- lm(hipcenter ~  Ht + HtShoes + Leg, data = seatpos)
v3_s <- summary(m3)$r.squared
v3_s

m4 <- lm(hipcenter ~  Ht + HtShoes + Leg + Seated, data = seatpos)
v4_s <- summary(m4)$r.squared
v4_s


shapley_val <- data.frame(
  Feature = c("Age", "Weight", "HtShoes", "Ht", "Seated", "Arm", "Thigh", "Leg"),
  Shapley_values = c(0.03, 0.06, 0.12, 0.12, 0.09, 0.058, 0.057, 0.132)
) 

shap_ordered <- shapley_val[order(- shapley_val$Shapley_values), ]

seatpos_sfi <- ggplot(shapley_val, aes(x = reorder(row.names(shapley_val), Shapley_values), y = Shapley_values, fill = factor(Feature))) +
  scale_fill_manual(values = c("#008B8B", "#00468B", "#59C7EB", "#9B0056", "#FEA090", "#8B0046", "#8B8B00", "#8B4500")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Shapley Values") +
  xlab("Features") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size = 1, fill = NA),
        legend.position = "top",
        legend.justification = "center",
        legend.box = "horizontal",
        legend.key.size = unit(0.5, "lines"), # Adjust the size as needed
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 10))

# Save as .eps
ggsave("seatpos_shapley_original.eps", seatpos_sfi, device = "eps", dpi = 400, width = 8, height = 4)


seatpos_sfi


####### PRIME approach 
# Step 1: Permute the data
#

################ Permuting data  ################
install.packages("faraway")
library(faraway)
library(dplyr)

View(seatpos)
set.seed(123)

################################# Permuting data ########################
# Number of permutations
n_permutations <- 30  # here I have tried 10, 20, 30, and 60

# Perform permutations and save shuffled features columns in the same files
for (i in 1:n_permutations) {
  shuffled_Leg <- sample(seatpos$Leg)
  
  # Save the shuffled "Leg" column to a new variable
  # permute all the features within dataset
  # you can save all the permuted files in a sinlge file or in seperate files
  var_name <- paste("Leg", i, sep = "")
  seatpos[var_name] <- shuffled_Leg
}

# Save the updated dataset with shuffled "feature" columns to a CSV file
write.csv(seatpos, "All_shuffled_data.csv", row.names=FALSE)


##########################################################
# Step 2

# take each permuted feature and find the Shapley feature importance values
### Shapley values
########################################
# Create an empty list to store the data frames
#start_time <- Sys.time()

list_comb <- list()

# Define the number of files to read
num_files <- 30

for (i in 1:num_files) {
  # Read each CSV file and add it to the list
  data <- read.csv(paste0("Age", i, "_r_squared_results.csv"), header = TRUE)
  list_comb[[i]] <- data
}


ShapleyValueDataFrame <- data.frame()
ShapleyValueDataFrames <- list()

for (i in list_comb) {
  r_squaredCoalitions <- c(i$r_squared)
  features <- DefineGame(8, r_squaredCoalitions)
  feature_names <- c("Age", "Weight", "HtShoes", "Ht", "Seated", "Arm", "Thigh", "Leg")
  ShapleyValueCalculation <- ShapleyValue(features, feature_names)
  
  # Data frame with Shapley values for the current iteration
  ShapleyValueDataFrame <- data.frame(
    ShapleyValue = ShapleyValueCalculation$SV
  )
  
  # Add it to the list of data frames
  ShapleyValueDataFrames[[length(ShapleyValueDataFrames) + 1]] <- ShapleyValueDataFrame
}

# Combine all data frames into one
CombinedDataFrame <- do.call(cbind, ShapleyValueDataFrames)

# Transpose
CombinedDataFrame <- t(CombinedDataFrame)

# Save 
write.csv(CombinedDataFrame, "Age_combined_results.csv", row.names = TRUE)
##########

#end_time <- Sys.time()
#execution_time <- end_time - start_time
#cat("Execution time: ", execution_time, " min/seconds\n")

###############################################################

######### Conflicting claims based feature importance evaluation
#################################################################################
# Here instead of using the R-squared values generated from various combinations
# of models, the R-squared values from a single feature models are used.

features_seatpos <- c(0.04209562, 0.41002632, 0.63456583, 0.63828503, 0.53473195, 0.34233614, 0.34951927, 0.61963425)
feature_names <- c('Age', 'Weight', 'HtShoes', 'Ht','Seated','Arm','Thigh','Leg')
features_df <- data.frame(Name = feature_names, Seatpos = features_seatpos)
sorted_features_df <- features_df %>% arrange(Seatpos)
print(sorted_features_df)

print(sorted_features_df)

# Below I have altered the threshold values from 0.1 to 1
# summary() shows the feature importance values observed from various 
# conflicting claims based solutions
all_rules_values <- AllRules(0.1, sorted_features_df$Seatpos, sorted_features_df$Name)
summary(all_rules_values)

#all <- data.frame(summary(all_rules_values))

# the results below are not included in the NeurIPS2024 paper
plot(all_rules_values, 1)
plot(all_rules_values, 2)
plot(all_rules_values, 3)
plot(all_rules_values, 4)
plot(all_rules_values, 5)
plot(all_rules_values, 6)
plot(all_rules_values, 7)
plot(all_rules_values, 8)
LorenzRules(all_rules_values)


########################################################################
########################################################################
########################################################################
########################################################################
######################## Adult data analysis ################


# Initial data analsis
# choose the adult.data 
adult <- read.table("adult.data", sep = ",", header = FALSE,stringsAsFactors=T)
View(adult)
names(adult) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")
head(adult)
null_positions <- adult == " ?"
is.na(adult) <- null_positions
# Now dropping the null values
adult1 <- na.omit(adult)
# Now dropping the duplicates
adult2 <- adult1 %>% distinct()
# Removing factor levels which doesn't occur (i.e., zero frequency)
adult2 <- droplevels(adult2)
# Type of Attributes in the data
names(adult2)

###########
adult2$capital_gain<-NULL
adult2$capital_loss<-NULL
adult2$fnlwgt<-NULL
adult2$education_num<-NULL
adult2$relationship<-NULL
# Summary of all the attributes in the data
View(adult2)
summary(adult2)
write.csv(adult2, "adult2.csv", row.names = FALSE)
read.csv("adult2.csv")


##### Permuting the data
# Number of permutations
n_permutations <- 60  

# Perform permutations and save permuted features columns in the same files
for (i in 1:n_permutations) {
  shuffled_native_country <- sample(adult2$native_country) #example - native_country
  
  # Save the shuffled "native_country" column to a new feature
  var_name <- paste("native_country", i, sep = "")
  adult2[var_name] <- shuffled_native_country
}

# Save the updated dataset with shuffled "feature" columns to a CSV file
write.csv(adult2, "Adult_native_country_shuffled_data.csv", row.names=FALSE)

adult2 <- read.csv("adult_permuted_subset.csv", header=TRUE, stringsAsFactors=T)
data <- read.csv("adult2.csv", header=TRUE, stringsAsFactors=T)

data$workclass <- as.factor(data$workclass)
data$education <- as.factor(data$education)
data$marital_status <- as.factor(data$marital_status)
data$occupation <- as.factor(data$occupation)
data$race <- as.factor(data$race)
data$sex <- as.factor(data$sex)
data$native_country <- as.factor(data$native_country)
data$income <- as.factor(data$income)

# Fitting a logistic regression model
model <- glm(income ~ ., data=data, family=binomial)
# Display the summary of the model
summary(model)


# create an empty data frame to store the results
# This generates different McFadden R-squared values using the permuted data
start_time <- Sys.time()
rsquared_df <- data.frame(combination = character(),
                          R_squared = numeric(),
                          stringsAsFactors = FALSE)

null.model <- function(model) {
  null <- glm(income ~ 1, data = model$data, family = "binomial")
  return(null)
}

variables <- c('age', 'workclass', 'education', 'marital_status', 'occupation', 'race', 'sex', 'hours_per_week', 'native_country') 

for (i in 1:length(variables)) {
  combs <- combn(variables, i, simplify = FALSE)
  for (j in 1:length(combs)) {
    fmla <- paste("income ~", paste(combs[[j]], collapse = " + "))
    
    model <- glm(fmla, data = adult2, family = "binomial")
    
    R_squared <- 1 - (logLik(model) / logLik(null.model(model)))
    
    # store the results in the data frame
    rsquared_df <- rsquared_df %>% 
      add_row(combination = paste(combs[[j]], collapse = ", "),
              R_squared = as.numeric(R_squared))
  }
}

end_time <- Sys.time()
execution_time <- end_time - start_time
cat("Execution time: ", execution_time, " seconds\n")


#### After executing the McFadden R-squared values ###
### Shapley feature importance values can be extracted

list_comb <- list()
# Define the number of files to read
# If the dataset contains 9 featues and each feature was permuted 60 times
# the new dataset will contain 549 features in total 
num_files <- 60

for (i in 1:num_files) {
  # Read each CSV file and add it to the list
  data <- read.csv(paste0("adult_education", i, "_rsquared.csv"), header = TRUE)
  list_comb[[i]] <- data
}


ShapleyValueDataFrame <- data.frame()
ShapleyValueDataFrames <- list()

for (i in list_comb) {
  r_squaredCoalitions <- c(i$R_squared)
  features <- DefineGame(9, r_squaredCoalitions)
  feature_names <- c('age', 'workclass', 'education', 'marital_status', 'occupation', 'race', 'sex', 'hours_per_week', 'native_country')
  ShapleyValueCalculation <- ShapleyValue(features, feature_names)
  # Data frame with Shapley values for the current iteration
  ShapleyValueDataFrame <- data.frame(
    ShapleyValue = ShapleyValueCalculation$SV
  )
  
  # Add it to the list of data frames
  ShapleyValueDataFrames[[length(ShapleyValueDataFrames) + 1]] <- ShapleyValueDataFrame
}

###################################################################
###################################################################
###################################################################
#### Adult Income data conflicting claims feature importance methods ###

features_adult <- c(0.051358828, 0.021110661, 0.115388744, 0.19438796, 0.115120543, 0.010259564, 0.046178169, 0.047779554, 0.01145082)
feature_names <- c("age", "workclass", "education", "marital_status", "occupation", "race", "sex", "hoursPerWeek",
                   "nativeCountry")
features_df <- data.frame(Name = feature_names, adult = features_adult)
sorted_features_df <- features_df %>% arrange(adult)
print(sorted_features_df)

all_rules_values <- AllRules(0.1, sorted_features_df$adult, sorted_features_df$Name)
summary(all_rules_values)

#all <- data.frame(summary(all_rules_values))

# the results below are not included in the NeurIPS2024 paper
plot(all_rules_values, 1)
plot(all_rules_values, 2)
plot(all_rules_values, 3)
plot(all_rules_values, 4)
plot(all_rules_values, 5)
plot(all_rules_values, 6)
plot(all_rules_values, 7)
plot(all_rules_values, 8)
LorenzRules(all_rules_values)

# Combine all data frames into one
CombinedDataFrame <- do.call(cbind, ShapleyValueDataFrames)
# Transpose
CombinedDataFrame <- t(CombinedDataFrame)
# Save 
write.csv(CombinedDataFrame, "Adult_age_combined_results.csv", row.names = TRUE)

##################################
rsquared_df <- data.frame(combination = character(),
                          R_squared = numeric(),
                          stringsAsFactors = FALSE)

null.model <- function(model) {
  null <- glm(income ~ 1, data = model$data, family = "binomial")
  return(null)
}

# Get all possible combinations of features
variables <- c('age1', 'workclass', 'education', 'marital_status', 'occupation', 'race', 'sex', 'hours_per_week', 'native_country')

for (age in 1:5) {
  age_variable <- paste0("age", age)
  
  for (i in 1:length(variables)) {
    combs <- combn(variables, i, simplify = FALSE)
    for (j in 1:length(combs)) {
      # Create a formula string for the current combination of features
      fmla <- paste("income ~", paste(c(age_variable, combs[[j]]), collapse = " + "))
      # Remove duplicate age variables
      fmla <- gsub(paste0("age", age, "\\s*\\+\\s*age", age), paste0("age", age), fmla)
      
      # Fit the logistic regression model
      model <- glm(fmla, data = adult2, family = "binomial")
      
      # Calculate the McFadden R-squared
      R_squared <- 1 - (logLik(model) / logLik(null.model(model)))
      
      # Store the results in the data frame
      rsquared_df <- rsquared_df %>% 
        add_row(combination = paste(c(age_variable, combs[[j]]), collapse = ", "),
                R_squared = as.numeric(R_squared))
    }
  }
}

# View the data frame
View(rsquared_df)

################################################################################
################################################################################
################################################################################
# Visualize Adult Income data Shapley feature importance method
shapley_val <- data.frame(
  Feature = c("marital status", "education", "occupation", "age", "hours per week", "sex", "native country", "workclass", "race"), 
  Shapley_values = c(0.14, 0.07, 0.06, 0.03, 0.02, 0.018, 0.015, 0.01, 0.003) # SFI
)  


adult_sfi <- ggplot(shapley_val, aes(x = reorder(row.names(shapley_val), Shapley_values), y = Shapley_values, fill = factor(Feature))) +
  scale_fill_manual(values = c( "#FEA090", "#008B45", "#008B8B", "#00468B", "#59C7EB", "#8B0046", "#8B8B00", "#8B4500", "#0000FF")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Shapley Values")+
  xlab("Features")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1, fill= NA),
        legend.title = element_blank(),
        axis.text.y = element_blank())

ggsave("adult_sfi.png", adult_sfi, dpi = 350, width = 8, height = 6, units = "in")

adult_sfi
