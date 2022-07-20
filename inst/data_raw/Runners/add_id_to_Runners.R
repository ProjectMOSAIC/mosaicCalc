# add a unique ID to each runner.
# This relies on the ordering of the original <Runners>
library(mosaicCalc)
# Runners is without ID.
load("Runners.rda")
#put an ID number
firsts <- Runners %>% filter(previous==0) %>% mutate(id = paste0("A", row_number()))

all_id <- with(firsts, rep(id, nruns))

Runners <- Runners %>% mutate(id = all_id)

# save(Runners, file="data/Runners.rda")