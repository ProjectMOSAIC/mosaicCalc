# Constructing the `Fly_ball` data and functions
#
# The data come from a simulation which is presented as an Excel spreadsheet.
# (See reference in documentation for `Fly_ball`.)

file_name <- here::here("inst/raw_data/Fly_ball/TrajectoryCalculator-new-3D.rds")
Fly_ball <- readRDS(file_name) %>%
  select(t, y, z) %>%
  filter(z >= 0)

# save(Fly_ball, file=here::here("data/Fly_ball.rda"))

# MENTION THESE FUNCTIONS IN THE DOCUMENTATION FOR Fly_ball.rda
# # Construct the functions
# # Original versions
# ball_z <- lm(z ~ ns(t, 15), data = Fly_ball) %>% makeFun()
# ball_y <- lm(z ~ ns(t, 15), data = Fly_ball) %>% makeFun()
# # New versions
# ball_z <- mosaic::connector(z ~ t, data = Fly_ball)
# ball_y <- mosaic::connector(y ~ t, data = Fly_ball)

# save(ball_z, ball_y, file=here::here("data/Fly_ball.rda"))
