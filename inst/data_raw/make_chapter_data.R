# Make the chapter-by-chapter data

Coal_prices <- tibble::tribble(
  ~ year, ~ price,
  1980, 31.36,
  1981, 30.11,
  1983, 27.46,
  1987, 22.25
)
Birth_weight <- tibble::tribble(
  ~ week, ~ weight,
  25, 777,
  26, 888,
  30, 1435,
  31, 1633,
  33, 2058
)
Prozac <- tibble::tribble(
  ~ day, ~ concentration,
  0, 79,
  5, 40,
  10, 19.6,
  22, 4.3,
  27, 2.5
)
Unemployment <- tibble::tribble(
  ~ year, ~ rate,
  2008, 5,
  2009, 7.8,
  2010, 9.7,
  2012, 8.2,
  2013, 7.9
)

Twitter_users <- tibble::tribble(
  ~ quarter,  ~ users_M,
  11, 68,
  11.25, 85,
  11.5, 101,
  11.75, 117,
  12.74, 185
)

Health_expend <- tibble::tribble(
  ~ year, ~ frac_gdp,
  2009, 17.71,
  2010, 17.66,
  2011, 17.86,
  2012, 17.91
)

Bachelors_debt <- tibble::tribble(
  ~ year, ~ debt_K,
  2001, 20.4,
  2003, 20.9,
  2005, 21.5,
  2006, 21.8
)

save(Prozac, Birth_weight, Coal_prices,
     Unemployment, Twitter_users, Health_expend,
     Bachelors_debt,
     file="data/Chapter1.rda")
