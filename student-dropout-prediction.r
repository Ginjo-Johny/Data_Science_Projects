#####################################################################
#                                                                    #
#         Importing and loading the required libraries               #
#                                                                    #
######################################################################

# Load the viridis package to get colour blindness 
# Load reshape2 package to reshape the corr_matrix to a format understood by ggplot
# Load required libraries

required_libraries <- c("ggrepel", "dplyr",  "corrplot", "cluster", "factoextra", "moments",
                        "viridis", "caret","reshape2", "keras", "gridExtra","plyr","ROCR",
                        "gmodels","caTools","class","psych","nFactors","GPArotation","e1071","pROC","glmnet")

for (lib in required_libraries) {
  # Install and load the libraries if not already installed
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib)
    library(lib, character.only = TRUE)
  }
  # Load the libraries if already installed
  else {
    library(lib, character.only = TRUE)
  }
}
# Remove required_libraries and lib from memory
rm("required_libraries","lib")

library("tidyverse")
library("skimr")

#################################################################
#                                                               #
#             Data Preparation and Quality Check                #
#                                                               #
#################################################################

### Loading the dataset ### 


# set data directory

setwd(dirname(file.choose()))
getwd()

# read in data from csv file
students.data <- read.csv("students_dropout.csv", stringsAsFactors = FALSE,header = T, sep = ",")
head(students.data)    # Inspect top rows of the data
str(students.data)
### Data Quality Check ### 

# Checking dimensions of the data
dim(students.data)

# Getting the variable names
names(students.data)


# getting a summary of the data
#   skim() is same as summary(), but more cleaner and in tabular form
skim(students.data)

# Eyeballing the data
head(students.data)
tail(students.data)

# Get no of Unique values in every column
for(col in names(students.data)) {
  # Get the number of unique values in the column
  unique_values <- unique(students.data[[col]])
  length_unique <- length(unique_values)
  
  # If no of unique values is less, e.g. 10 supposedly, print those values
  if(length_unique < 10) {
    print(paste(col, "has", length_unique,"unique values; Repeated values are ", toString(sort(unique_values))))
  }
  else{
    print(paste(col, "has", length_unique,"values"))
  }
}

# Counting NA values
setNames(as.data.frame(colSums(is.na(students.data))), "Count of NA")

# Count the number of duplicate rows in the dataset
sum(duplicated(students.data))

# defining categorical and numerical columns based on the above checks and metadata
categorical_cols <- c("Marital.status", "Application.mode", "Application.order", "Course", "Daytime.evening.attendance", "Previous.qualification", "Nacionality", "Mother.s.qualification", "Father.s.qualification", "Mother.s.occupation", "Father.s.occupation", "Displaced", "Educational.special.needs", "Debtor", "Tuition.fees.up.to.date", "Gender", "Scholarship.holder", "International", "Student.status")
numerical_cols <- setdiff(names(students.data), categorical_cols)


# defining custom function for categorical variables
# to show frequency table, barplots and pie charts
categorical_fn <- function(col_name){
  # Frequency Table sorted in descending order
  freq_table_df <- as.data.frame(table(students.data[,col_name]))
  colnames(freq_table_df) <- c(col_name, "Frequency")
  freq_table_df <- freq_table_df %>% arrange(desc(Frequency))
  
  # Bar Plot
  bar_plot <- ggplot(freq_table_df, 
                     aes(x = !!sym(col_name), y = Frequency, fill = !!sym(col_name))) + 
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(100*Frequency/sum(Frequency),2), "%"),
                  y = Frequency + 50),
              size = 3, color = "black") +  # Add percentages as text
    scale_fill_viridis(discrete = TRUE) +
    labs(title = paste("Bar Plot for", col_name, "distribution"), 
         x = col_name, y = "Frequency") +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(color = "black")) 
  
  # Combine plots and table in a grid
  table_grob <- tableGrob(freq_table_df, 
                          rows = NULL,
                          theme = ttheme_minimal())
  
  grid.arrange(table_grob,bar_plot,
               ncol = 2,
               widths = c(1, 5))
  
  # Ask the user to press a key to view the next variable
  readline(prompt = "Press any key to continue")
}



# apply custom function to categorical columns
#   Saving to a variable suppresses duplicate results being displayed by lapply()
categorical_results <- lapply(categorical_cols, categorical_fn)


# defining custom function for numerical variables
# to show numerical summary, box plot and histogram
numerical_fn <- function(col_name){ 
  # Numerical Summary
  summary_table <- data.frame(t(summary(students.data[,col_name])))[,2:3]
  names(summary_table) <- c("Statistics", col_name)
  
  # Box Plot
  box_plot <- ggplot(students.data, aes(x = "", y = !!sym(col_name))) +
    geom_boxplot(aes(fill = "viridis")) +
    labs(title = paste("Box Plot for", col_name), y = col_name) +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
  
  # Histogram
  hist_plot <- ggplot(students.data, aes(x = !!sym(col_name), fill = ..count..)) +
    geom_histogram(binwidth = diff(range(students.data[, col_name]))/30, alpha = 0.7) +
    labs(title = paste("Histogram for", col_name, "distribution"), 
         x = col_name, y = "Frequency") +
    scale_fill_viridis() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Print the visualizations
  print(paste("Numerical Summary for", col_name))
  print(summary_table)
  print(paste("Skewness for",col_name,"is",round(skewness(students.data[,col_name]),2)))
  print(hist_plot)
  print(box_plot)
  grid.arrange(hist_plot, box_plot, ncol = 2)
  
  # Ask the user to press a key to view the next variable
  readline(prompt = "Press any key to continue")
}


# apply custom function to numerical columns
#   Saving to a variable suppresses duplicate results being displayed by lapply
numerical_results <- lapply(numerical_cols, numerical_fn)


### ERRORS FOUND ### 
# Application order 0 has a frequency of 1, which does not match the metadata
####### Many of the variables are defined as categorical, which might not suit our model.

#################################################################
#                                                               #
#                       Data Cleaning                           #
#                                                               #
#################################################################

## Copy the dataset
students.data.cleaned <- students.data

## Correcting Application order
#   Find the mode of the Application.order column
#   and replace 0 values with the mode
mode_value <- as.numeric(names(sort(-table(students.data.cleaned$Application.order)))[1])
students.data.cleaned$Application.order[students.data.cleaned$Application.order == 0] <- mode_value
skim(students.data.cleaned$Application.order)

# Define the minimum frequency for combining categories
min_freq <- 150

# Combine categories with low frequency
combined_cats_dict <- list()
for (col in categorical_cols) {
  
  #Check if the variable is not binary
  if (length(unique(students.data.cleaned[, col])) > 2) {
    # Count the frequency of each category
    counts <- table(students.data.cleaned[, col])
    # Identify the categories with low frequency
    low_freq_cats <- names(counts[counts < min_freq])
    # Only combine categories if at least 2 categories have frequency less than min_freq
    if (length(low_freq_cats) >= 2) {
      # Combine the low frequency categories into a single "Other" category represented as -1
      students.data.cleaned[, col] <- ifelse(students.data.cleaned[, col] %in% low_freq_cats, -1, students.data.cleaned[, col])
      # Identify the categories that were combined
      combined_cats_dict[[col]] <- paste(low_freq_cats, collapse = ", ")
      other_percent <- length(students.data.cleaned[students.data.cleaned[, col] == -1, col]) / nrow(students.data) * 100
      cat_msg <- paste("For", col, "variable, -1 (Other) constitutes", other_percent, "% & combines categories", combined_cats_dict[[col]])
      print(cat_msg)
    }
  }
}

# loop through categorical columns and create frequency table for each
for (col in categorical_cols) {
  freq_table <- as.data.frame(table(students.data.cleaned[, col]))
  freq_table$Percent <- freq_table$Freq / nrow(students.data) * 100
  freq_table$Percent <- sprintf("%.2f%%", freq_table$Percent)
  freq_table <- data.frame(col = col, freq_table)
  freq_table <- freq_table[, c("col", "Var1", "Freq", "Percent")]
  colnames(freq_table) <- c("Variable", "Category", "Frequency", "Percent")
  print(as.data.frame(t(freq_table), stringsAsFactors = FALSE))
}


## Check the structure of the dataset
str(students.data.cleaned)

# Normalizing data

# A function that normalizes numeric data
data_norm <- function(x){return((x - min(x))/(max(x) - min(x)))}

for(col in colnames(students.data.cleaned)){
  if(is.numeric(students.data.cleaned[[col]])){
    students.data.cleaned[[col]] <- data_norm(students.data.cleaned[[col]])
  }
}

#################################################################
#                                                               #
#                   Exploratory Data Analysis                   #
#                                                               #
#################################################################


## Analysing independent variables with Student.status Variable

# Student.status variable - categorical
table(students.data.cleaned$Student.status)

# Visualize the distribution of the Student.status variable
ggplot(data = students.data, aes(x = Student.status)) +
  geom_bar(aes(fill = after_stat(count)), color = "black") +
  scale_fill_viridis() +
  labs(title = "Distribution of Student.status Variable",
       x = "Student.status", y = "Count")

# Define a custom function to create
#   2-way bar plots and frequency tables for categorical variables versus Student.status Variable
#   and 2-way boxplots and numerical summaries for numerical variables
#     USING AES_STRING INSTEAD OF AES TO PASS COLUMN NAMES AS ARGUMENT
create_plot_summary <- function(data, x, y = NULL) {
  
  # If y is not provided, create a bar plot
  if(is.null(y)) {
    # Create a stacked bar plot of x and Student.status variables
    plot1 <- ggplot(data, aes_string(x = x, fill = "Student.status")) +
      geom_bar(position = "dodge") +
      scale_fill_viridis(discrete = TRUE) +
      labs(title = paste("Relationship between Student.status Variable and", x))
    
    # Creating a stacked bar plot to visualise proportions of Student.status variable in x
    plot2 <- ggplot(data = data, aes_string(x = x, fill="Student.status")) +
      geom_bar(position="fill") +
      scale_fill_viridis(discrete = TRUE) +
      labs(title = paste("Distribution of Student.status variable by", x), y="Proportion")
    
    # Create a frequency table of x and Student.status variables
    freq_table <- table(data[, x], data$Student.status)
    colnames(freq_table) <- paste0("Count.", colnames(freq_table))
    rownames(freq_table) <- paste0(x, ".", rownames(freq_table))
    cat(paste0("Frequency table for '", x, "' variable:\n"))
    print(freq_table)
    
    # Display all plots in one page
    grid.arrange(plot1, plot2, ncol = 2)
    
  } else {
    
    # Create a boxplot of y and Student.status variables
    plot1 <- ggplot(data, aes_string(x = "Student.status", y = y, fill = "Student.status")) +
      geom_boxplot() +
      scale_fill_viridis(discrete = TRUE) +
      labs(title = paste("Relationship between Student.status Variable and", y))
    
    # Print summary statistics of y by Student.status variable
    summary_table <- aggregate(as.formula(paste(y," ~ Student.status")), data = data, FUN = summary)
    colnames(summary_table) <- c("Student.status", sub("\\..*", "", names(summary_table[2:length(summary_table)])))
    cat(paste0("Summary table for '", y, "' variable:\n"))
    print(summary_table)
    
    # Display both plot and table in one page
    grid.arrange(plot1)
    
  }
  readline(prompt = "Press any key to continue")
}

# Selecting all independent variables (all variables except Student.status)
vars <- names(students.data[, -which(names(students.data) == "Student.status")])

# Use lapply() or loops to call the function for different variables
results <- lapply(vars, function(x){
  if(is.element(x, categorical_cols)){
    #create_plot_summary(students.data.cleaned, x)
  }else{
    create_plot_summary(students.data.cleaned, "Student.status", x)
  }
})



## For numerical variables

# Select only the numerical columns
num_vars <- students.data.cleaned[, numerical_cols]

# Scaling numerical variables
num_vars_scaled <- scale(num_vars)

# Calculate the correlation matrix
corr_matrix <- cor(num_vars_scaled)

# Plot basic heatmap
 heatmap(corr_matrix)
# 
# # Plot the full heatmap using ggplot2
 ggplot(data = melt(corr_matrix), aes(x = Var1, y = Var2, fill = value)) + 
   geom_tile() +
   scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", na.value = "white") +
   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
   labs(title = "Correlation Heatmap")
 
##############################################################################
 
     # Factor Analysis and Feature Selection 
 
 ##############################################################################

 
# basic correlation matrix using spearman correlaton
cor(students.data.cleaned, method = "spearman")
student_cor <- cor(students.data.cleaned, method = "spearman")
round(student_cor, digits = 2)

# after checking correlation matrix some variables are omitted  zero and low correlations with the target variable student status.
# Define the columns to remove included dependent variable student.status 
columns_to_remove <- c(
  "Marital.status", "Nationality", "Mother.s.qualification", "Educational.special.needs", 
  "International","Father.s.qualification","Mother.s.occupation","Father.s.occupation",
  "Curricular.units.1st.sem..evaluations.", "Curricular.units.1st.sem..credited.",
  "Curricular.units.1st.sem..without.evaluations.","Curricular.units.2nd.sem..evaluations.",
  "Curricular.units.2nd.sem..without.evaluations.", "Unemployment.rate", "Inflation.rate", "GDP"
)

# Remove the specified columns from the data frame
student_data_new <- students.data.cleaned[, !names(students.data.cleaned) %in% columns_to_remove]
str(student_data_new)

# basic correlation matrix for the new data set with relevant variables
cor(student_data_new, method = "spearman")
student_cor <- cor(student_data_new, method = "spearman")
round(student_cor, digits = 2)

heatmap(student_cor)

# # Plot the full heatmap using ggplot2
ggplot(data = melt(student_cor), aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", na.value = "white") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Correlation Heatmap with relevant variables")

#--------------- ----- --  Factor Analysis -------------------------------------


# Kaiser-Meyer-Olkin statistics: if overall MSA > 0.6, proceed to factor analysis
KMO(cor(student_data_new))

# Determine Number of Factors to Extract

# get eigenvalues: eigen() uses a correlation matrix
ev <- eigen(cor(student_data_new))
ev$values
# plot a scree plot of eigenvalues
plot(ev$values, type="b", col="blue", xlab="variables")

# calculate cumulative proportion of eigenvalue and plot
ev.sum<-0
for(i in 1:length(ev$value)){
  ev.sum<-ev.sum+ev$value[i]
}
ev.list1<-1:length(ev$value)
for(i in 1:length(ev$value)){
  ev.list1[i]=ev$value[i]/ev.sum
}
ev.list2<-1:length(ev$value)
ev.list2[1]<-ev.list1[1]
for(i in 2:length(ev$value)){
  ev.list2[i]=ev.list2[i-1]+ev.list1[i]
}
plot (ev.list2, type="b", col="red", xlab="number of components", ylab ="cumulative proportion")


# Varimax Rotated Principal Components
# retaining 'nFactors' components

# principal() uses a data frame or matrix of correlations
fit <- principal(student_data_new, nfactors=4, rotate="varimax")
fit

# weed out further variables after first factor analysis
myvars <- names(student_data_new) %in% c("Application.mode", "Application.order","Course","Gender","Curricular.units.1st.sem..approved.","Curricular.units.2nd.sem..approved.")
student_data_factor <- student_data_new[!myvars]
str(student_data_factor)
rm(myvars)

# get eigenvalues
ev <- eigen(cor(student_data_factor))
ev$values
# plot a scree plot of eigenvalues
plot(ev$values, type = "b", col = "blue", xlab = "variables")

fit <- principal(student_data_factor, nfactors = 4, rotate = "varimax")
fit

# create four variables to represent the rorated components
fit$scores
fit.data <- data.frame(fit$scores)

# check new variables are uncorrelated
cor.matrix2 <-cor(fit.data, method = "pearson")
cor.df2 <- as.data.frame(cor.matrix2)
round(cor.df2, 2)


#################################################################
#                                                               #
#                       Machine Learning                        #
#                                                               #
#################################################################



# changed  variables to component variables or factors

student_data_factor[c("Daytime.evening.attendance","Previous.qualification","Displaced",
                   "Debtor","Tuition.fees.up.to.date", "Scholarship.holder", "Age.at.enrollment","Curricular.units.1st.sem..enrolled.",
                   "Curricular.units.1st.sem..grade.","Curricular.units.2nd.sem..credited.",
                   "Curricular.units.2nd.sem..enrolled.","Curricular.units.2nd.sem..grade.","Student.status")] <-
 lapply(student_data_factor[c("Daytime.evening.attendance","Previous.qualification","Displaced",
                   "Debtor","Tuition.fees.up.to.date", "Scholarship.holder", "Age.at.enrollment","Curricular.units.1st.sem..enrolled.",
                   "Curricular.units.1st.sem..grade.","Curricular.units.2nd.sem..credited.",
                   "Curricular.units.2nd.sem..enrolled.","Curricular.units.2nd.sem..grade.","Student.status")], factor)



#####################################################################

# Support vector machine algorithms (SVM)

####################################################################

#library(e1071)

# Assuming you have a dataset named 'data' containing features and target variable 'dropout'
# where 'dropout' is binary (1 for dropout, 0 for non-dropout)

# Split the data into training and testing sets
set.seed(123) # for reproducibility
train_index <- sample(1:nrow(student_data_factor), 0.7*nrow(student_data_factor)) # 70% for training
train_data <- student_data_factor[train_index, ]
test_data <- student_data_factor[-train_index, ]

# Train the SVM model
svm_model <- svm(Student.status ~ ., data = train_data, kernel = "linear", probability = TRUE)

# Predict on the test set
svm_pred <- predict(svm_model, newdata = test_data, probability = TRUE)

print(svm_pred)

#  Calculate the confusion matrix
cm <- confusionMatrix(data = svm_pred, test_data$Student.status)
#  Print the confusion matrix
print("Confusion Matrix:")
print(cm)

# Extract predicted probabilities for the positive class
svm_prob_pos <- attr(svm_pred, "probabilities")[, 2]

# Create ROC curve
roc_obj <- roc(test_data$Student.status, svm_prob_pos)

# Plot ROC curve
plot(roc_obj, main = "ROC Curve for SVM Model", col = "blue")
legend("bottomright", legend = paste("AUC =", round(auc(roc_obj), 2)), col = "blue", lty = 1, cex = 0.8)



##############################################################


## =================== Logistic Regression ===================

set.seed(123) # for reproducibility
train_index <- createDataPartition(student_data_factor$Student.status, p = 0.7, list = FALSE)
train_data <- student_data_factor[train_index, ]
test_data <- student_data_factor[-train_index, ]


# Train the logistic regression model
#library(glmnet)
Student.status <- as.factor(train_data$Student.status)

logit_model <- cv.glmnet(x = as.matrix(train_data), y = Student.status, family = "binomial")

# Print the cross-validated performance metrics
print(logit_model)

# Plot the cross-validated performance metrics
plot(logit_model)

# Make predictions on the training data
probabilities <- predict(logit_model, newx = as.matrix(train_data), type = "response")

# Obtain the predicted class labels
predicted_classes <- ifelse(probabilities > 0.5, 1, 0)

# Create confusion matrix
conf_matrix <- table(predicted_classes, Student.status)
print(conf_matrix)

cm_log <- confusionMatrix(data = probabilities, test_data$Student.status)
#  Print the confusion matrix
print("Confusion Matrix:")
print(cm_log)

# Calculate ROC curve
roc_lg <- roc(Student.status, probabilities)
plot(roc_lg, main = "ROC Curve")

# Plot ROC curve
plot(roc_lg, main = "ROC Curve for Logistic Regression Model", col = "blue")
legend("bottomright", legend = paste("AUC =", round(auc(roc_lg), 2)), col = "blue", lty = 1, cex = 0.8)


#-----  KNN Model -------------------------------------------


sample <- sample.int(n = nrow(student_data_factor), size = floor(.8*nrow(student_data_factor)), replace = F)
train.new <- student_data_factor[sample, ]
test.new  <- student_data_factor[-sample, ]

print("Train Columns:")
sapply(train.new, class)

print("Test Columns:")
sapply(test.new, class)

set.seed(2018)
split <- sample.split(train.new$Student.status, SplitRatio = 0.8)
train.train <- subset(train.new, split == TRUE)
train.test <- subset(train.new, split == FALSE)

# Codifying categorical variables

train.train.new <- data.frame(model.matrix(Student.status ~ .-1, data = train.train))
train.train.new <- cbind(train.train[["Student.status"]], train.train.new)
colnames(train.train.new)[1] <- c("Student.status")

train.test.new <- data.frame(model.matrix(Student.status ~ .-1, data = train.test))
train.test.new <- cbind(train.test[["Student.status"]], train.test.new)
colnames(train.test.new)[1] <- c("Student.status")

# Changing the train funciton to use cross-validation

train_control <- trainControl(method = "repeatedcv", number = 5, repeats = 3, 
                              classProbs = TRUE, summaryFunction = twoClassSummary, 
                              savePredictions = TRUE, verboseIter = TRUE)


# KNN 

start_time <- Sys.time()

set.seed(2018)

# Making the model

# Check unique levels of the target variable
unique_levels <- unique(train.train.new$Student.status)
print(unique_levels)

# Rename levels if necessary
valid_levels <- make.names(unique_levels)
levels(train.train.new$Student.status) <- valid_levels
levels(train.test.new$Student.status) <- valid_levels


# Train the model
knn.model <- caret::train(Student.status ~ ., 
                          data = train.train.new, 
                          method = "knn", 
                          trControl = train_control, 
                          tuneLength = 1, 
                          metric = "ROC")


# Predict on the test set
test_pred <- predict(knn.model, newdata = train.test.new, type = "prob")

# Create ROC curve
roc_curve <- roc(train.test.new$Student.status, test_pred$X1)

print(roc_curve)

# Plot ROC curve
plot(roc_curve, main = "ROC Curve for KNN Model", col = "blue")
legend("bottomright", legend = paste("AUC =", round(auc(roc_curve), 2)), col = "blue", lty = 1, cex = 0.8)


# remove all variables from the environment

rm(list=ls())
