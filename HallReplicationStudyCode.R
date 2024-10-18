# Data preparation

# Load packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(car))
suppressPackageStartupMessages(library(stargazer))
suppressPackageStartupMessages(library(here)) # for ease of replication

# Load data (all quarterly)
pce_serv <- read.csv(file = here("Data/PCESV.csv"))
pce_nondur <- read.csv(file = here("Data/PCND.csv"))
pce_price_index <- read.csv(file = here("Data/PCECTPI.csv"))
pop <- read.csv(file = here("Data/CNP16OV.csv"))
nom_dpi <- read.csv(file = here("Data/DSPI.csv"))

# Create single dataframe with all variables,
# change some names for convenience
hall_data_approx <- data.frame(pce_serv$DATE,
                               pce_serv$PCESV,
                               pce_nondur$PCND,
                               pce_price_index$PCECTPI,
                               pop$CNP16OV,
                               nom_dpi$DSPI) %>%
  rename("date" = pce_serv.DATE,
         "pce_serv" = pce_serv.PCESV,
         "pce_nondur" = pce_nondur.PCND,
         "pce_price_index" = pce_price_index.PCECTPI,
         "pop" = pop.CNP16OV,
         "nom_dpi" = nom_dpi.DSPI) %>%
  mutate(date = as_date(date))

# Nominal PCE for services and non-durable goods
hall_data_approx$nom_pce_serv_nondur <- hall_data_approx$pce_serv +
  hall_data_approx$pce_nondur

# Real consumption of services and non-durable goods
hall_data_approx$real_serv_nondur <- hall_data_approx$nom_pce_serv_nondur/
  (hall_data_approx$pce_price_index/100)

# Real disposable personal income
hall_data_approx$real_dpi <- hall_data_approx$nom_dpi/
  (hall_data_approx$pce_price_index/100)

# Real per-capita consumption of services and non-durable goods
hall_data_approx$real_serv_nondur_pc_lag0 <- (hall_data_approx$real_serv_nondur/
                                                hall_data_approx$pop)*1000000

# Real per-capita disposable personal income
hall_data_approx$real_dpi_pc_lag0 <- (hall_data_approx$real_dpi/
                                        hall_data_approx$pop)*1000000


# Time series for real per-capita disposable personal income
# along with consumption of services and non-durable goods
ggplot(aes(x = date), data = hall_data_approx) +
  geom_line(aes(y = real_serv_nondur_pc_lag0), color = "blue") +
  geom_line(aes(y = real_dpi_pc_lag0), color = "red") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "          Per-Capita Real Disposable Personal Income (red)
       and Real Consumption of Services and Non-Durables (blue)",
       subtitle = "1960 Q1 to 2024 Q2",
       x = "Date",
       y = "Per-Capita USD") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


# Perform regression and conduct F tests

# Iteratively create variables for real lagged per-capita consumption
# of services and non-durable goods. Note the loss of data entries
# in these lagged variables (we lack data prior to 1960 Q1)

# Reorder columns to aid in following for loop
hall_data_approx <- hall_data_approx[, c(1:9, 11, 10)]

for(i in 1:4){
  # Calculate lag, add to dataframe
  hall_data_approx[, i+11] <- lag(hall_data_approx[, i+10])

  # Rename new variable
  new_varname <- paste0("real_serv_nondur_pc_lag", i)
  names(hall_data_approx)[i+11] <- new_varname
}

# Create linear model
consum_mod <- lm(real_serv_nondur_pc_lag0 ~
                   real_serv_nondur_pc_lag1 + real_serv_nondur_pc_lag2 +
                   real_serv_nondur_pc_lag3 + real_serv_nondur_pc_lag4,
                 data = hall_data_approx)

# Perform appropriate hypothesis test
ftest_consum_mod <- car::linearHypothesis(consum_mod, c("real_serv_nondur_pc_lag1 = 1",
                                                        "real_serv_nondur_pc_lag2 = 0",
                                                        "real_serv_nondur_pc_lag3 = 0",
                                                        "real_serv_nondur_pc_lag4 = 0"))

######################################################################################################################

# Iteratively create variables for real lagged per-capita disposable
# income. Note again the loss of data entries in these lagged variables
# (we still lack data prior to 1960 Q1)

# Reorder columns to aid in following for loop
hall_data_approx <- hall_data_approx[, c(1:9, 11:15, 10)]

for(i in 1:4){
  # Calculate lag, add to dataframe
  hall_data_approx[, i+15] <- lag(hall_data_approx[, i+14])

  # Rename new variable
  new_varname <- paste0("real_dpi_pc_lag", i)
  names(hall_data_approx)[i+15] <- new_varname
}

# Create linear model
consum_incom_mod <- lm(real_serv_nondur_pc_lag0 ~
                         real_serv_nondur_pc_lag1 +
                         real_dpi_pc_lag1 + real_dpi_pc_lag2 +
                         real_dpi_pc_lag3 + real_dpi_pc_lag4,
                       data = hall_data_approx)

# Perform appropriate hypothesis test
ftest_consum_incom_mod <- car::linearHypothesis(consum_incom_mod, c("real_serv_nondur_pc_lag1 = 1",
                                                                    "real_dpi_pc_lag1 = 0",
                                                                    "real_dpi_pc_lag2 = 0",
                                                                    "real_dpi_pc_lag3 = 0",
                                                                    "real_dpi_pc_lag4 = 0"))


# Display regression output and F test results

# Build tables using stargazer package
# Regression output

`(2)` <- consum_mod # renaming models to make table easier to interpret
`(3)` <- consum_incom_mod
stargazer(`(2)`, `(3)`,
          title = "Regression Results for Models of Real Consumption (CNS) and Real Income (DPI)",
          align = TRUE, dep.var.labels.include = FALSE,
          column.labels = "CNS in Current Period", column.separate = 2,
          object.names = TRUE, model.numbers = FALSE,
          covariate.labels = c("CNS Lagged 1 Period",
                               "CNS Lagged 2 Periods",
                               "CNS Lagged 3 Periods",
                               "CNS Lagged 4 Periods",
                               "DPI Lagged 1 Period",
                               "DPI Lagged 2 Periods",
                               "DPI Lagged 3 Periods",
                               "DPI Lagged 4 Periods",
                               "Intercept"),
          header = FALSE,
          notes = c('Consumption is per-capita and is of non-durables and services',
                    'Income is per-capita disposable personal income'))

# F test results
stargazer(ftest_consum_mod, ftest_consum_incom_mod,
          title = c("F Test Results for Lagged Consumption Regression",
                    "F Test Results for Lagged Consumption and Income Regression"),
          header = FALSE,
          notes = c('The results in Table 2 apply to Equation 2',
                    'The results in Table 3 apply to Equation 3'))

# Create .csv file of full dataset used
write.csv(hall_data_approx, file = "econometrics_lab_1_data.csv")

