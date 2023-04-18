Dataset <- read.csv("Dataset.csv", stringsAsFactors=TRUE, header=TRUE)

Dataset <- Dataset[Dataset$Assignment != "",]

View(Dataset)

# Create drug dataframe
drugs <- data.frame(
  Name = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "AA", "AB", "AC", "AD"),
  Color = c("Red", "Red", "Green", "Yellow", "Red", "Yellow", "Red", "Red", "Red", "Red", "Green", "Red", "Red", "Red", "Green", "Red", "Red", "Green", "Yellow", "Red", "Red", "Yellow", "Yellow", "Red", "Yellow", "Red", "Yellow", "Red", "Red", "Green")
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

# Group by LOS: <72, >72
Dataset$Over3 <- ifelse(Dataset$LOS > 72, 1, 0)

linear_model <- lm(LOS ~ GENDER + AGE + RACE.ETHNICITY + MD + Assignment + Therapeutic.Guidances + red/X..Administrations + yellow/X..Administrations + green/X..Administrations, data=Dataset[Dataset$Over3 == 1, ])
summary(linear_model)

library(dplyr)
# compute the proportion of red pills administered for each category in MD
result <- Dataset[Dataset$Over3==1,] %>%
  group_by(MD) %>%
  summarize(proportion = (sum(red) / sum(X..Administrations)), 
            total_cases = n())

# view the result
barplot(result$proportion, names.arg = result$MD)
mean(Dataset$red[Dataset$LOS>96])
mean(Dataset$red[Dataset$LOS<96])
