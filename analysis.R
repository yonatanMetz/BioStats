Dataset <- read.csv("Dataset.csv", header=TRUE)

Dataset <- Dataset[Dataset$Assignment %in% c("G","S"),]
Dataset <- Dataset[Dataset$Therapeutic.Guidances %in% c("CT","EG","TL"),]
Dataset <- Dataset[!is.na(Dataset$LOS)]

Dataset$GENDER <- as.factor(Dataset$GENDER)
Dataset$RACE.ETHNICITY <- as.factor(Dataset$RACE.ETHNICITY)
Dataset$Diagnosis <- as.factor(Dataset$Diagnosis)
Dataset$MD <- as.factor(Dataset$MD)
Dataset$Assignment <- as.factor(Dataset$Assignment)
Dataset$EMR <- as.factor(Dataset$EMR)
Dataset$Therapeutic.Guidances <- as.factor(Dataset$Therapeutic.Guidances)
Dataset$RAR <- as.factor(Dataset$RAR)

# Create drug dataframe
drugs <- data.frame(
  Name = c("A", "B", "C", "D", "E", "F", "G", "H", 
           "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", 
           "S", "T", "U", "V", "W", "X", "Y", "Z", "AA", "AB", "AC", "AD"),
  Color = c("Red", "Red", "Green", "Yellow", "Red", "Yellow", 
            "Red", "Red", "Red", "Red", "Green", "Red", "Red", 
            "Red", "Green", "Red", "Red", "Green", "Yellow", 
            "Red", "Red", "Yellow", "Yellow", "Red", "Yellow", "Red", 
            "Yellow", "Red", "Red", "Green")
)

# Create list for each type of drug
major_cols <- drugs$Name[drugs$Color == "Red"]
minor_cols <- drugs$Name[drugs$Color == "Yellow"]
non_cols <- drugs$Name[drugs$Color == "Green"]

# Create a column for each type of drug with the amount taken by each obs 
Dataset$major = rowSums(Dataset[, major_cols])
Dataset$minor = rowSums(Dataset[, minor_cols])
Dataset$non = rowSums(Dataset[, non_cols])

# Join all drugs to "green" for EG patients in both experimental and control groups
EG_patients <- Dataset$Therapeutic.Guidances == "EG"
Dataset[EG_patients, "green"] <- rowSums(Dataset[EG_patients, c("major", "minor", "non")])
Dataset[EG_patients, "red"] <- 0
Dataset[EG_patients, "yellow"] <- 0

# Join "minor" and "non" drugs to "green" for CT patients in the experimental group
# (we have CT only in experimental group)
CT_patients <- Dataset$Therapeutic.Guidances == "CT"
Dataset[CT_patients, "green"] <- rowSums(Dataset[CT_patients, c("minor", "non")])
Dataset[CT_patients, "yellow"] <- 0
Dataset[CT_patients, "red"] <- Dataset[CT_patients, "major"]

# Create "red", "yellow", "green" columns for TL patients in the experimental group
# (we have CT only in experimental group)
TL_patients <- Dataset$Therapeutic.Guidances == "TL"
Dataset[TL_patients, "green"] <- Dataset[TL_patients, "non"]
Dataset[TL_patients, "yellow"] <- Dataset[TL_patients, "minor"]
Dataset[TL_patients, "red"] <- Dataset[TL_patients, "major"]

#Remove doctors that have less than 10 patients

busyMD <- Dataset[summary(Dataset$MD)[Dataset$MD] > 10,]

library(caret)

# Define the training control with 10-fold cross-validation
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

# Fit a linear model using cross-validation
linear_model <- train(LOS ~ GENDER + AGE + RACE.ETHNICITY 
                       + MD + Assignment + Therapeutic.Guidances + Diagnosis + EMR
                       + red/X..Administrations 
                       + green/X..Administrations, data=busyMD[busyMD$LOS > 72, ],
                      method = "lm", trControl = train_control)

# Print the results
print(linear_model)

library(tidymodels)
library(multilevelmod)

lmer_spec <- 
  linear_reg() %>% 
  set_engine("lmer")

folds <- vfold_cv(busyMD, v = 10)
folds

rf_wf <- 
  workflow() %>%
  add_model(lmer_spec) %>%
  add_formula(log(LOS) ~ GENDER + AGE + RACE.ETHNICITY 
              + (1|MD) + Assignment + Therapeutic.Guidances + Diagnosis + EMR
              + red/X..Administrations 
              + green/X..Administrations)

set.seed(456)
rf_fit_rs <- 
  rf_wf %>% 
  fit_resamples(folds)

rf_fit <- 
  lmer_spec %>% 
  fit(log(LOS) ~ GENDER + AGE + RACE.ETHNICITY 
      + (1|MD) + Assignment + Therapeutic.Guidances + Diagnosis + EMR
      + red/X..Administrations 
      + green/X..Administrations, data=busyMD[busyMD$LOS > 72, ])

rf_fit


fit(lmer_wflow, data = riesby)

mixed_model_intercept <- train(LOS ~ GENDER + AGE + RACE.ETHNICITY 
                     + (1|MD) + Assignment + Therapeutic.Guidances + Diagnosis + EMR
                     + red/X..Administrations 
                     + green/X..Administrations, data=busyMD[busyMD$LOS > 72, ],
                      method = "lmer", trControl = train_control)

print(mixed_model_intercept)

library(dplyr)
# compute the proportion of red pills administered for each category in MD
result <- busyMD[busyMD$Over3==1 & busyMD$Therapeutic.Guidances != "EG",] %>%
  group_by(MD) %>%
  summarize(proportion = (sum(red) / sum(X..Administrations)), 
            total_cases = n(),
            total_drugs = sum(X..Administrations),
            sd = mean(red/X..Administrations))

library(dplyr)
test<-df %>%
  group_by(Diagnosis, MD) %>%
  summarize(count = n())
