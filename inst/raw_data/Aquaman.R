# read the Aquaman data from BoxOfficeMojo

# Aquaman
BethsURL <- "https://www.boxofficemojo.com/release/rl3108800001/weekend/?ref_=bo_rl_tab"
BethsPage <- read_html(BethsURL)
BethsPage %>%
  html_nodes("title") %>%
  html_text()
BethsTable <- BethsPage %>%
  html_nodes("table") %>%
  extract2(1) %>%
  html_table()
names(BethsTable)[3] <- "Earnings"

Aquaman <- BethsTable %>%
  select(Date, Earnings, Weekend) %>%
  mutate(Earnings = readr::parse_number(Earnings),
         Weekend = readr::parse_number(Weekend))

save(Aquaman, file="Aquaman.rda")
