# -------------------- Code for HW 6 Question 2 -----------------------------
# Clear environment

rm(list = ls())

# Setting the random number generator seed so that our results are reproducible
# (Your solution doesn't need this, but it's usually good practice to do)

set.seed(1)

# ---------------------------- Data manipulation -------------------------------------

# First, read in the data

data <- read.table("breast-cancer-wisconsin.data.txt", stringsAsFactors = FALSE, header = FALSE, sep=",")



# data <- read.table("breast-cancer-wisconsin.data.txt", stringsAsFactors = FALSE, header = FALSE, na.strings = "?", sep=",")

# Add column names
 colnames(data) <- c("SampleNo", 
                    "Thickness", 
                    "SizeUniform", 
                    "ShapeUniform", 
                    "Adhesion", 
                    "SE_CellSize", 
                    "BareNuclei", 
                    "BlandChromatin", 
                    "NormalNucleoli", 
                    "Mitoses", 
                    "Class")


# Optional check to make sure the data is read correctly
#

head(data)

# Try to find the missing value
for (i in 2:11){
  print(paste0("V",i))
  print(table(data[,i]))
}

# show the missing data
data[which(data$V7 =="?"),]

# calculate the missing %
nrow(data[which(data$V7 =="?"),])/nrow(data)

missing<-which(data$V7 =="?", arr.ind = TRUE)
missing

################### Mean & Mode imputation   ######################

# creat a mode function 
getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Find the mode for v7
mode_v7 <-as.numeric(getmode(data[-missing,"V7"]))
mode_v7

data_mode_imp<- data
data_mode_imp[missing,]$V7<- mode_v7
data_mode_imp$V7<- as.integer(data_mode_imp$V7)

typeof(data_mode_imp$V7)


# install packages
install.packages("mice")


########################## Regression Imputation #####################

# Not to include the response variable in regression imputation
data_modified <- data[-missing, 2:10]
data_modified$V7 <- as.integer(data_modified$V7)


#---- Linear regression Imputation --------

model <- lm(V7~V2+V3+V4+V5+V6+V7+V8+V9+V10, data_modified)
summary(model)

step(model)

model2<- lm(V7~V2+V4+V5+V8, data_modified)
summary(model2)

# predict V7
V7_hat <- predict(model2, newdata = data[missing,])

data_reg_imp <- data
data_reg_imp[missing,]$V7 <-V7_hat
data_reg_imp$V7 <- as.numeric(data_reg_imp$V7)

#################regression with perturbation to impute values #####################

V7_hat_pert <- rnorm(length(missing), V7_hat, sd(V7_hat))
V7_hat_pert

data_reg_pert_imp <- data
data_reg_pert_imp[missing,]$V7 <- V7_hat_pert
data_reg_pert_imp$V7 <- as.numeric(data_reg_pert_imp$V7)

