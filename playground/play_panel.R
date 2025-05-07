setwd('../causalwiz')
source("main.R")


data <- read.csv('cali-smoking.csv')
data$X <- NULL  # removing X column

# fill out these by hand
# these variables are important for summary plots and analysis
outcome.var <- "cigsale"
predictors <- c("lnincome", "beer", "age15to24", "retprice") # if any
time.var <- c("year")
unit.var <- c("state")
treatment.year <- 1989
treated.unit <- 3
pretreat.period <- c(1970:1988)
time.period <- c(1970:2000)
control.units <- c(1, 2, 4:39)

# if using special predictors which are
# certain pretreatment years of the outcome variable used to 
# more accurately predict the synthetic unit
special.years <- c(1975, 1980, treatment.year)
special.predictors <- list(        
  list("outcome", special.years[1], c("mean")), 
  list("outcome", special.years[2], c("mean")),
  list("outcome", special.years[3], c("mean"))
)

# rename variables in the dataset
data <- data %>% rename(outcome = !!sym(outcome.var),
                        time = !!sym(time.var),
                        unit = !!sym(unit.var))
# now the outcome, time, and unit variables are:
outcome.var <- "outcome"
time.var <- c("time")
unit.var <- c("unit")

allvars <- c("outcome", predictors)


# Data Setup for Synthetic Diff-in-Diff (synthdid package requires us to change the data structure)
# set up empty dataframe
data.sdid <- data.frame()

# first row = numbers for each unit
data.sdid <- data.frame(unit.no = unique(data$unit))

# next is covariate data = predictors and special predictors
# predictors
# will save each dataset later
for (i in 1:length(predictors)){
  covariate_column <- data %>% 
    group_by(unit) %>%
    summarize(predictor_mean = mean(!!sym(predictors[i]), na.rm = T)) %>% 
    dplyr::select(predictor_mean)
  data.sdid <- cbind(data.sdid, covariate_column)
}

# special.predictors
special_predictors_data <- data %>%
  dplyr::filter(time %in% special.years) %>%
  dplyr::select(unit, time, outcome)
# convert from long to wide dataset
special_predictors_data <- spread(special_predictors_data, time, outcome)[,-1]
data.sdid <- cbind(data.sdid, special_predictors_data)

# next is the outcome variable for each state in the time period
outcome_data <- data %>% dplyr::select(unit, time, outcome)
outcome_data <- spread(outcome_data, time, outcome)[,-1]
data.sdid <- cbind(data.sdid, outcome_data)

# transpose data
data.sdid <- t(data.sdid)

# add other data setup variables for SDID
UNIT <- data.sdid[1,] # unit numbers
X.attr <- t(data.sdid[2:8,]) # covariate data
Y <- t(data.sdid[9:39,]) # outcome variable data
colnames(Y) <- time.period # colname = year
rownames(Y) <- UNIT # rowname = unit number
units <- function(...) { which(UNIT %in% c(...)) }
Y <- Y[c(setdiff(1:nrow(Y), units(treated.unit)), units(treated.unit)), ] # make sure treated unit is in the last row
T0 <- length(pretreat.period) # number of pre-treatment years
N0 <- nrow(Y)-1