###Creating of the base functions to compute the outcome of the simulation later on

# install the necessary libraries, if not already done
install.packages("ggplot2")
install.packages("dplyr")
install.packages("multcomp")

# load the necessary libraries
library(ggplot2)
library(dplyr)
library(multcomp)


#' p1 Compute the contingency between input and outcome
#' 
#' If ID and SA are given, CON is, independent of CT, 1. 
#' If ID and/or SA are not given, CON decreases with increasing CT, 
#' whereby the nth CT has less effect than the (1-n)th one.
#' 
#' @param CT number of people who are also viewed as targets of social pressure. Ranges from 0 to infinite (discrete)
#' @param ID identifiability of individual scores/outputs. Binary with 0 (no identifiability) and 1 (identifiability)
#' @param SA standard available for comparison. Binary with 0 (no standard available) and 1 (standard available)
#' 
#' @return (CON) contingency between input and outcome. Ranges from 0 to 1 (continuous)

get_CON <- function(CT, ID, SA) {
  n <- CT + 1
  CON <- ID * SA + (1 - ID * SA) * 1/n
  return(CON)
}

# (example) check vectorized form:
CT <- c(0, 1, 5)
ID <- c(0, 1)
SA <- c(0, 1)
CON <- get_CON(CT, ID, SA)
CON

# (example) create data for CT, ID, SA, S, and MC 
plot_data <- expand.grid(
  CT = c(0, 1, 5),
  ID = c(0, 1),
  SA = c(0, 1),
  S = c(1, 4, 7),
  MC = c(9, 10, 13)
)

# (example) compute CON from the CT, ID, and SA data
plot_data$CON <- get_CON(
  CT = plot_data$CT,
  ID = plot_data$ID,
  SA = plot_data$SA
)

# (example) create an extra column for the plotting - label
plot_data$ID_SA_label <- paste("Identifiability:", plot_data$ID, "Standards:", plot_data$SA)
plot_data$ID_SA_label <- ifelse(
  plot_data$ID == 1 & plot_data$SA == 1,
  "Identifiability and Standard: Yes",
  "Identifiability and/or Standard: No")

# (example) plot a visual graph for the p1-function
ggplot(plot_data, aes(x = CT, y = CON)) +
  geom_point(shape = 8) +
  facet_wrap(~plot_data$ID_SA_label) +
  labs(title = "Contingency between input and outcome", subtitle = "Dependent on Cotargets, Identifiability, and Standards", y = "Contingency", x = "Number of Cotargets") +
  scale_x_continuous(limits = c(0, 5), breaks = c(0, 1, 5)) +
  theme_classic() +
  geom_line()


#' p2 compute the social pressure on each given target member of the group
#' 
#' P drops with increasing CT and grows with increasing S, 
#' whereby the nth CT and/or S has less effect than the (1-n)th one.
#' 
#' @param CT number of people who are also viewed as targets of social impact. Ranges from 0 to infinite (discrete)
#' @param S number of people viewed as source of social pressure. Ranges from 0 to infinite (discrete)
#' 
#' @return (P) social pressure. Ranges from 0 to 1 (continuous)

get_P <- function (CT, S) {
  P <- S/(S+CT+1)
  return(P)
}

# (example) check vectorized form:
CT <- c(0, 1, 5)
S <- c(1, 4, 7)
P <- get_P(CT, S)
P

# (example) data for CT and S 
plot_data

# (example) compute pressure from CT and S
plot_data$P <- get_P(
  CT = plot_data$CT,
  S = plot_data$S
)

# (example) create an extra column for the plotting
plot_data$S_label <- paste("Number of Sources:", plot_data$S)

# (example) plot a visual graph for the p2-function
ggplot(plot_data, aes(x = CT, y = P)) +
  geom_point(shape = 8) +
  facet_wrap(~plot_data$S_label) +
  labs(title = "Pressure", subtitle = "Dependent on Cotargets and Sources", y = "Pressure", x = "Number of Cotargets") +
  scale_x_continuous(limits = c(0, 5), breaks = c(0, 1, 5)) +
  theme_classic() +
  geom_line()


#' t1 compute the incentive to give full effort
#' 
#' INC equals CON. If ID and SA are given, CON and therefore INC are always 1. 
#' If ID and/or SA are not given, CON and INC can vary between 0 and 1.
#' 
#' @param CON number of people who are also viewed as targets of social impact. Ranges from 0 to infinite (discrete)
#' 
#' @return (INC) incentive to give full effort. Ranges from 0 to 1 (continuous)

get_INC <- function(CON) {
  INC <- CON
  return(INC)
}

# (example) check vectorized form:
INC <- get_INC(CON)
INC

# (example) data for CT and ID
plot_data

# (example) compute the incentive from CT (and ID)
plot_data$INC <- get_INC(
  CON = plot_data$CON
)

# (example) plot a visual graph for the t1-function
ggplot(plot_data, aes(x = CON, y = INC)) +
  geom_point(shape = 8) +
  facet_wrap(~plot_data$ID_SA_label) +
  labs(title = "Incentive to give full effort", y = "Incentive", x = "Contingency") +
  scale_x_continuous(limits = c(min(plot_data$CON), max(plot_data$CON)), breaks = round(plot_data$CON, digits = 2)) +
  scale_y_continuous(limits = c(min(plot_data$INC), max(plot_data$INC)), breaks = round(plot_data$INC, digits = 2)) +
  theme_classic() +
  geom_line()


#' p3 compute the individual effort 
#' 
#' IE grows with both increasing INC and increasing P. If ID and SA are given, 
#' INC is always 1, which is why IE is sort of only dependent on P. 
#' If ID and/or SA are not given, INC and P both vary.
#' 
#' @param INC incentive to give full effort. Ranges from 0 to 1 (continuous)
#' @param P social pressure. Ranges from 0 to 1 (continuous)
#' 
#' @return (IE) individual effort. Ranges from 0 to 1 (continuous)

get_IE <- function(INC, P) {
  IE <- 0.5 * INC + 0.5 * P
  return(IE)
}

# (example) check vectorized form:
IE <- get_IE(INC, P)
IE

# (example) data for INC (dependent on ID) and P
plot_data

# (example) compute the individual effort from INC and P
plot_data$IE <- get_IE(
  INC = plot_data$INC,
  P = plot_data$P
)

# (example) plot a visual graph for the p3-function
ggplot(plot_data, aes(x = P, y = IE, linetype = factor(round(plot_data$INC, digits = 2)))) +
  geom_point(shape = 8) +
  facet_wrap(~plot_data$ID_SA_label) +
  labs(title = "Individual effort", subtitle = "Dependent on Pressure, Incentive, Identifiability, and Standard", y = "Individual effort", x = "Pressure", linetype = "Incentive") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  theme_classic() +
  geom_line()


#' Superfunction - compute the individual effort with some noise (through all the previous functions)
#' 
#' @param CT number of people who are also viewed as targets of social pressure. Ranges from 0 to infinite (discrete)
#' @param S number of people viewed as source of social pressure. Ranges from 0 to infinite (discrete)
#' @param ID identifiability of individual scores/outputs. Binary with 0 (no identifiability) and 1 (identifiability)
#' @param SA standard available for comparison. Binary with 0 (no standard available) and 1 (standard available)
#' 
#' @return (IE) individual effort. Ranges from 0 to 1 (continuous)

get_super_IE <- function(CT, S, ID, SA) {
  CON <- get_CON(CT, ID, SA) 
  P <- get_P(CT, S)
  INC <- get_INC(CON)
  IE <- get_IE(INC, P)
  IE <- IE + rnorm(length(IE), mean = 0, sd = 0.2)
  IE[IE > 1] <- 1
  IE[IE < 0] <- 0
  return(IE)
}


#' t2-function (manifest) - compute the individual outcome
#' 
#' IO grows with both increasing MC and increasing IE. If ID and SA are given, 
#' IE is always at least 0.5 because CON and therefore INC are always 1. 
#' If ID and/or SA are not given, IE can vary between 0 and 1.
#' 
#' @param MC maximum capacity to scream. Ranges from 0 to 15 (continuous)
#' @param IE individual effort Ranges from 0 to 1 (continuous)
#' 
#' @return (IO) individual outcome. Ranges from 0 to inf. (continuous)

get_IO <- function(MC, IE) {
  IO <- MC * IE   
  return (IO)
}

# (example) check vectorized form:
MC <- c(9, 10, 13)
IO <- get_IE(MC, IE)
IO

# (example) data for MC and IE
plot_data

# (example) compute the individual effort from MC and IE
plot_data$IO <- get_IO(
  MC = plot_data$MC,
  IE = plot_data$IE
)

# (example) plot a visual graph for the t2-function
ggplot(plot_data, aes(x = IE, y = IO, linetype = factor(plot_data$MC))) +
  geom_point(shape = 8) +
  facet_wrap(~plot_data$ID_SA_label) +
  labs(title = "Individual output", subtitle = "Dependent on Maximum, Individual effort, Identifiability, and Standard", y = "Individual output", x = "Individual Effort", linetype = "Individual Maximum Capacity") +
  scale_x_continuous(limits = c(min(plot_data$IE), max(plot_data$IE)), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  scale_y_continuous(limits = c(min(plot_data$IO), max(plot_data$IO)), breaks = pretty(plot_data$IO)) +
  theme_classic() +
  geom_line()


#' Superfunction - compute the individual outcome (through all the previous functions - with some noise because the Superfunction for IE is used)
#' 
#' In theory without noise: IO drops with increasing CT and grows with increasing S, whereby the nth CT and/or S has less effect 
#' than the (1-n)th one. If ID and SA are given, CON is always 1, and therefore IO is higher via INC and IE. 
#' If ID and/or SA are not given, IO is usually lower. 
#' 
#' @param CT number of people who are also viewed as targets of social pressure. Ranges from 0 to infinite (discrete)
#' @param S number of people viewed as source of social pressure. Ranges from 0 to infinite (discrete)
#' @param ID identifiability of individual scores/outputs. Binary with 0 (no identifiability) and 1 (identifiability)
#' @param SA standard available for comparison. Binary with 0 (no standard available) and 1 (standard available)
#' @param MC maximum capacity to scream. Ranges from 0 to 15 (continuous)
#' 
#' @return (IO) individual outcome. Ranges from 0 to inf (continuous)

get_super_IO <- function(CT, S, ID, SA, MC){
  IE <- get_super_IE(CT, S, ID, SA)
  IO <- IE * MC
  return (IO)
}

# (example) set seed because of random noise
set.seed(324)

# (example) create data for CT, S, ID, SA, and MC 
df_test <- expand.grid(
  CT = c(0, 1, 5),
  S = c(1, 4, 7), 
  ID = c(0, 1),
  SA = c(0, 1),
  MC = c(9)
)

# compute the manifest outcome variable (IO)
df_test$IO <- get_super_IO(
  CT = df_test$CT,
  S = df_test$S,
  ID = df_test$ID,
  SA = df_test$SA,
  MC = df_test$MC
)

# (example) create extra columns for the plotting - label
df_test$ID_label <- paste("Identifiability:", df_test$ID)
df_test$ID_label <- ifelse(
  df_test$ID == 1,
  "Identifiability: Yes",
  "Identifiability: No")

df_test$SA_label <- paste("Standards:", df_test$SA)
df_test$SA_label <- ifelse(
  df_test$SA == 1,
  "Standard: Yes",
  "Standard: No")

# (example) plot a visual graph for the Superfunction for IO (with random noise) - 4 panels 
ggplot(df_test, aes(x = CT, y = IO, linetype = factor(df_test$S))) +
  geom_point(shape = 8) +
  facet_wrap(~df_test$ID_label + ~df_test$SA_label) +
  labs(title = "Individual output", subtitle = "Dependent on Cotargets, Sources, Identifiability, and Standard", y = "Individual output", x = "Number of Cotargets", linetype = "Number of Sources") +
  scale_x_continuous(limits = c(0, 5), breaks = c(0, 1, 5)) +
  scale_y_continuous(limits = c(0, 13), breaks = c(0, 2, 4, 6, 8, 10, 12)) +
  theme_classic() +
  geom_line()



# -----
# small insert: if you want to plot the Superfunction for IO without random noise
# Superfunction for IE without random noise
get_super_IE_without <- function(CT, S, ID, SA) {
CON <- get_CON(CT, ID, SA) 
P <- get_P(CT, S)
INC <- get_INC(CON)
IE <- get_IE(INC, P)
IE <- IE 
IE[IE > 1] <- 1
IE[IE < 0] <- 0
return(IE)
}

# Superfunction for IO without random noise
get_super_IO_without <- function(CT, S, ID, SA, MC){
  IE <- get_super_IE_without(CT, S, ID, SA)
  IO <- IE * MC
  return (IO)
}

# compute the manifest outcome variable (IO) without random noise
df_test$IO_without <- get_super_IO_without(
  CT = df_test$CT,
  S = df_test$S,
  ID = df_test$ID,
  SA = df_test$SA,
  MC = df_test$MC
)

# (example) plot a visual graph for the Superfunction for IO (without random noise) - 4 panels 
ggplot(df_test, aes(x = CT, y = IO_without, linetype = factor(df_test$S))) +
  geom_point(shape = 8) +
  facet_wrap(~df_test$ID_label + ~df_test$SA_label) +
  labs(title = "Individual output", subtitle = "Dependent on Cotargets, Sources, Identifiability, and Standard", y = "Individual output", x = "Number of Cotargets", linetype = "Number of Sources") +
  scale_x_continuous(limits = c(0, 5), breaks = c(0, 1, 5)) +
  scale_y_continuous(limits = c(0, 13), breaks = c(0, 2, 4, 6, 8, 10, 12)) +
  theme_classic() +
  geom_line()

# (example) create an extra column for the plotting - label
df_test$ID_SA_label <- paste("Identifiability:", df_test$ID, "Standards:", df_test$SA)
df_test$ID_SA_label <- ifelse(
  df_test$ID == 1 & df_test$SA == 1,
  "Identifiability and Standard: Yes",
  "Identifiability and/or Standard: No")

# (example) plot a visual graph for the Superfunction for IO (without random noise) - 2 panels 
ggplot(df_test, aes(x = CT, y = IO_without, linetype = factor(df_test$S), color = df_test$ID_SA_label)) +
  geom_point(shape = 8) +
  labs(title = "Individual output", subtitle = "Dependent on Cotargets, Sources, Identifiability, and Standard", y = "Individual output", x = "Number of Cotargets", linetype = "Number of Sources", color = "Identifiability and Standard") +
  scale_x_continuous(limits = c(0, 5), breaks = c(0, 1, 5)) +
  scale_y_continuous(limits = c(0, 13), breaks = c(0, 2, 4, 6, 8, 10, 12)) +
  theme_classic() +
  geom_line()

# -----


### Final Simulation

## Replicate the original study of Latané et al. (1979)

# Simulate virtual experiments from the model.
# ---------------------------------------------------------------------------
# We assume that the model is the true data generating model.
# As it is a deterministic model, any virtual participant
# will have on average the same individual effort based on 
# the number of Cotargets (CT) and Sources (S) of Social Pressure.
# The identifiability of the individual output (ID) and a standard for 
# comparison (SA) are not part of Latané et al. (1979) but an extension
# based on Latané et al.'s (1979) suggestion and Harkins & Jackson (1985).


# Variability comes into the experiment by:
# (a) Different trait values for the maximum noise each person can produce
# (b) Randomness from the noise we compute in the superfuncion - due to, e.g., 
#     faitque, personality, mental state etc. during the experiment

# You can run the script repeatedly to simulate new replication studies,
# and observe the variability in outcomes due to sampling variability.
# However, the simulation must then be run without set.seed.

set.seed(324)

# set sample size of participants (we assume a within-person design)
# derived  from experiment 2 - the pseudo-groups (Latané et al., 1979)
n <- 36

# experimental design: Latané et al. (1979)
# create data for CT, S, ID, and SA

df <- expand.grid(
  id = 1:n, 
  CT = c(0, 1, 5),
  S = c(2), 
  ID = c(0, 1),
  SA = c(0, 1)
)

# reduce the data set to only the two relevant conditions (ID = 0 and SA = 0; ID = 1 and SA = 1)
df <- df %>% 
  filter((ID==0 & SA ==0) | (ID==1 & SA==1))

# determine the variables for a simulation of the exogenous variables
phi <- 5
alpha <- 0.922*phi
beta <- (1-0.922)*phi

scale_factor <- 12.29 / 0.922

# interindividual variability in exogenous variable ("maximum capacity")
max_per_person <- data.frame(
  id = 1:n,
  maximum = scale_factor * rbeta(n, alpha, beta)
)

mean(max_per_person$maximum)

# merge into df
df <- merge(df, max_per_person, by = "id")

# compute the psychological outcome variable (IE)
df$IE <- get_super_IE(
  CT = df$CT,
  S = df$S,
  ID = df$ID,
  SA = df$SA
)

# compute the manifest outcome variable (IO)
df$IO <- get_super_IO(
  CT = df$CT,
  S = df$S,
  ID = df$ID,
  SA = df$SA,
  MC = df$maximum
)

# look at the simulated data 
# View(df)

#compute the means of IO for each experimental group
m0_00 <- mean(df$IO[df$CT == 0 & df$ID == 0 & df$SA == 0], na.rm = TRUE)
m1_00 <- mean(df$IO[df$CT == 1 & df$ID == 0 & df$SA == 0], na.rm = TRUE)
m5_00 <- mean(df$IO[df$CT == 5 & df$ID == 0 & df$SA == 0], na.rm = TRUE)

m0_11 <- mean(df$IO[df$CT == 0 & df$ID == 1 & df$SA == 1], na.rm = TRUE)
m1_11 <- mean(df$IO[df$CT == 1 & df$ID == 1 & df$SA == 1], na.rm = TRUE)
m5_11 <- mean(df$IO[df$CT == 5 & df$ID == 1 & df$SA == 1], na.rm = TRUE)

# create data frame for those means of each experimental group
data_final <- data.frame(
  ID = c(0, 0, 0, 1, 1, 1),
  SA = c(0, 0, 0, 1, 1, 1),
  groups = c(0, 1, 5),
  means = c(m0_00, m1_00, m5_00, m0_11, m1_11, m5_11)
)

# alternatively, you could have computed the means like this
# data_final <- df %>% 
#   group_by(ID, SA, CT) %>%
#   summarise(mean_IO = mean(IO, na.rm = TRUE))

# create extra columns for the plotting - label
data_final$ID_label <- paste("Identifiability:", data_final$ID)
data_final$ID_label <- ifelse(
  data_final$ID == 1,
  "Identifiability: Yes",
  "Identifiability: No")

data_final$SA_label <- paste("Standards:", data_final$SA)
data_final$SA_label <- ifelse(
  data_final$SA == 1,
  "Standard: Yes",
  "Standard: No")

# plot the visual graph for the mean individual outcome in the different conditions - two graphs
plot_final <- ggplot(data_final, aes(x = groups, y = means)) +
  geom_point(shape = 8) +
  facet_grid(~ID_label + ~SA_label) +
  labs(title = "Mean Individual Output", subtitle = "Dependent on experimental group (S = 2)", y = "Sound Pressure in dyn per cm^2", x = "Conditions (Number of Cotargets)") +
  scale_x_continuous(limits = c(0, 5), breaks = c(0, 1, 5)) +
  scale_y_continuous(limits = c(0, max(data_final$means)), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)) +
  theme_classic() +
  geom_line()
plot_final

# plot the visual graph for the mean individual outcome in the different conditions - one graph
plot_final <- ggplot(data_final, aes(x = groups, y = means, linetype = factor(ID_label), color = factor(SA_label))) +
  geom_point(shape = 8) +
  labs(title = "Mean Individual Output", subtitle = "Dependent on experimental group (S = 2)", y = "Sound Pressure in dyn per cm^2", x = "Conditions (Number of Cotargets)", color = "Standard", linetype = "Identifiability") +
  scale_x_continuous(limits = c(0, 5), breaks = c(0, 1, 5)) +
  scale_y_continuous(limits = c(0, max(data_final$means)), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)) +
  theme_classic() +
  geom_line()
plot_final


# create extra columns for the plotting - label
df$ID_label <- paste("Identifiability:", df$ID)
df$ID_label <- ifelse(
  df$ID == 1,
  "Identifiability: Yes",
  "Identifiability: No")

df$SA_label <- paste("Standards:", df$SA)
df$SA_label <- ifelse(
  df$SA == 1,
  "Standard: Yes",
  "Standard: No")

# Boxplots to visualise simulated raw data
ggplot(df, aes(x = factor(CT), y = IO)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 1.5) +
  facet_grid(~ID_label + ~SA_label) +
  labs(title = "Simulated Raw Data", y = "Sound Pressure in dyn per cm^2", x = "Number of Cotargets") +
  scale_y_continuous(limits = c(0, max(df$IO)), breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)) +
  theme_classic()


# split data set for ANOVA
df_noID_noSA <- df %>% filter (ID==0 & SA==0)
df_ID_SA <- df %>% filter (ID==1 & SA==1)



# Variance analysis for Latané et al.'s (1979) experiment (ID = 0, SA = 0)
df_noID_noSA$CT_factor <- factor(df_noID_noSA$CT,
                       levels = c(0,1,5),
                       labels = c("CT0","CT1","CT5"))

# save aov-object
fit_aov1 <- aov(IO ~ CT_factor, data = df_noID_noSA)

# confidence intervals
# define null hypotheses
hyp1c <- "CT1 - CT5 == 0"
hyp2c <- "CT0 - CT1 == 0"

# summarise these hypotheses in a character-vector and save it
hypsc <- c(hyp1c, hyp2c)

# create contrast-object and save it
kontraste1c <- mcp(CT_factor = hypsc)

# create glht-object and save it
fit1c <- glht(fit_aov1, linfct = kontraste1c)

# 95% confidence intervals
confint(fit1c, level = 0.95, calpha = univariate_calpha())

# hypothesis testing
# define null hypotheses
hyp1 <- "CT1 - CT5 <= 0"
hyp2 <- "CT0 - CT1 <= 0"

# summarise these hypotheses in a character-vector and save it
hyps <- c(hyp1, hyp2)

# create contrast-object and save it
kontraste1 <- mcp(CT_factor = hyps)

# create glht-object and save it
fit1 <- glht(fit_aov1, linfct = kontraste1)

# test results and save it
result1 <- summary(fit1, test = univariate())
result1

# extract p-values and save and name them
p_values1 <- result1$test$pvalues
p_CT1_CT5_1 <- p_values1["CT1 - CT5"]
p_CT0_CT1_1 <- p_values1["CT0 - CT1"]



# Variance analysis for extension of Latané et al.'s (1979) experiment (ID = 1, SA = 1)
df_ID_SA$CT_factor <- factor(df_ID_SA$CT,
                                 levels = c(0,1,5),
                                 labels = c("CT0","CT1","CT5"))

# save aov-object
fit_aov2 <- aov(IO ~ CT_factor, data = df_ID_SA)

# confidence intervals
# define null hypotheses
hyp1c <- "CT1 - CT5 == 0"
hyp2c <- "CT0 - CT1 == 0"

# summarise these hypotheses in a character-vector and save it
hypsc <- c(hyp1c, hyp2c)

# create contrast-object and save it
kontraste2c <- mcp(CT_factor = hypsc)

# create glht-object and save it
fit2c <- glht(fit_aov2, linfct = kontraste2c)

# 95% confidence intervals
confint(fit2c, level = 0.95, calpha = univariate_calpha())

# hypothesis testing
# define null hypotheses
hyp1 <- "CT1 - CT5 <= 0"
hyp2 <- "CT0 - CT1 <= 0"

# summarise these hypotheses in a character-vector and save it
hyps <- c(hyp1, hyp2)

# create contrast-object and save it
kontraste2 <- mcp(CT_factor = hyps)

# create glht-object and save it
fit2 <- glht(fit_aov2, linfct = kontraste2)

# test results and save it
result2 <- summary(fit2, test = univariate())
result2

# extract p-values and save and name them
p_values2 <- result2$test$pvalues
p_CT1_CT5_2 <- p_values2["CT1 - CT5"]
p_CT0_CT1_2 <- p_values2["CT0 - CT1"]


# create a data frame for p-values regarding the two variance analyses
pvalues_df <- data.frame(
  experiment = c("Latané_original", "Latané_original",
                 "Extension", "Extension"),
  contrast = c("CT1-CT5", "CT0-CT1",
               "CT1-CT5", "CT0-CT1"),
  p_value = c(p_CT1_CT5_1, p_CT0_CT1_1,
              p_CT1_CT5_2, p_CT0_CT1_2)
)

# correct the p-values with the help of the Bonferroni-method and save them in the data frame
pvalues_df$p_korrigiert <- pvalues_df$p_value*2

# instead of multiplying the p-values, you can also divide the significance level, which is why 0.0025 (0.005/2) was used here
# test decisions
decision1 <- if (p_CT0_CT1_1 < 0.0025 & p_CT1_CT5_1 < 0.0025) {
  Entscheidung = "H1: The phenomenon of social loafing can be found in the condition without identifiability and a standard in this study"
} else {
  Entscheidung = "H0: The phenomenon of social loafing can not be found in the condition without identifiability and a standard in this study"
}
decision1

decision2 <- if (p_CT0_CT1_2 < 0.0025 & p_CT1_CT5_2 < 0.0025) {
  Entscheidung = "H1: The phenomenon of social loafing can be found in the condition with identifiability and a standard in this study"
} else {
  Entscheidung = "H0: The phenomenon of social loafing can not be found in the condition with identifiability and a standard in this study"
}
decision2


# compute effect size for the first experiment (ID = 0, SA = 0)
anova_table1 <- summary(fit_aov1)[[1]]
SS_between1 <- anova_table1["CT_factor", "Sum Sq"]
SS_total1 <- sum(anova_table1[, "Sum Sq"])
eta2_1 <- SS_between1/SS_total1
eta2_1

# compute effect size for the second experiment (ID = 1, SA = 1)
anova_table2 <- summary(fit_aov2)[[1]]
SS_between2 <- anova_table2["CT_factor", "Sum Sq"]
SS_total2 <- sum(anova_table2[, "Sum Sq"])
eta2_2 <- SS_between2/SS_total2
eta2_2


