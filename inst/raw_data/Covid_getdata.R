# Get Covid data

Covid <- COVID19::covid19(
  country = "US",
  level = 1,
  start = "2019-01-01")

save(Covid_US, file="data/Covid_US.rda")
