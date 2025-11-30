library(tidyverse)
library(mice)
library(doParallel)
raw_data = read.csv(here::here("Main RCT analyses/analytic_raw_ns_data.csv"))

raw_data$Treatment[raw_data$Treatment == 1] = "Treatment"
raw_data$Treatment[raw_data$Treatment == 2] = "Control"
raw_data$Treatment <- factor(raw_data$Treatment,
                                  levels = c("Control",
                                             "Treatment"),
                                  ordered = TRUE)
# raw_data |> glimpse()

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

# imp <- mice(raw_data, m = 5, method = "pmm", seed = 123)
imp <- parlmice(raw_data, m = 5, method = "pmm", seed = 123)
stopCluster(cl)
summary(imp)
completed_data <- complete(imp, 1)

saveRDS(completed_data, file = here::here("Main RCT analyses/imputed_data.rds"))
saveRDS(raw_data, file = here::here("Main RCT analyses/raw_data.rds"))
