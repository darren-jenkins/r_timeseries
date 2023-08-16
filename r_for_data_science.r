# https://otexts.com/fpp3/
 
library(tidyverse)
library(fpp3)
library(ggplot2)

library(seasonal)

library(feasts)
library(fracdiff)

library(GGally)

# load("H:/DS/r_for_data_science.RData")

ggplot(mpg, aes(x = displ, y = hwy)) + 
   geom_point(aes(color = class))

mpg

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

# left
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# right
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE
  )

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()


ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median
  )

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))


ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))


ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")
ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")


ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()

library(maps)
nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()


bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()


seq(1, 10)

library(tidyverse)
library(nycflights13)
flights

filter(flights, month == 1, day == 1)
jan1 <- filter(flights, month == 1, day == 1)
(dec25 <- filter(flights, month == 12, day == 25))


sqrt(2) ^ 2 == 2
near(sqrt(2) ^ 2,  2)

filter(flights, month == 11 | month == 12)

nov_dec <- filter(flights, month %in% c(11, 12))

x <- NA
is.na(x)

df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)

arrange(flights, year, month, day)

arrange(flights, desc(dep_delay))


# Select columns by name
select(flights, year, month, day)

# Select all columns between year and day (inclusive)
select(flights, year:day)

# Select all columns except those from year to day (inclusive)
select(flights, -(year:day))


rename(flights, tail_num = tailnum)


flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60
)

View(flights_sml)


summarize(flights, delay = mean(dep_delay, na.rm = TRUE))


by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20, dest != "HNL")

# It looks like delays increase with distance up to ~750 miles 
# and then decrease. Maybe as flights get longer there's more 
# ability to make up delays in the air?
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
#> 

# There's another way to tackle the same problem with the pipe, %>%:
delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )

ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)

# Convert to a tibble so it prints nicely
batting <- as_tibble(Lahman::Batting)

batters <- batting %>% 
  group_by(playerID) %>% 
  summarise(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )

batters %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() + 
  geom_smooth(se = FALSE)
#> `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
#> 
#> 
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

diamonds %>% 
  count(cut)


ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)


diamonds %>% 
  count(cut_width(carat, 0.5))


smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)


ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)


ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))


unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  select(price, x, y, z) %>%
  arrange(y)
unusual


diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point()
#> Warning: Removed 9 rows containing missing values (`geom_point()`).
#> 
#> 
nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
    geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)

ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

ggplot(diamonds) + 
  geom_bar(mapping = aes(x = cut))

# display density, which is the count standardised so that the area under each frequency polygon is one.
ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)


ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()


ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

diamonds %>% 
  count(color, cut)

diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))

ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price), alpha = 1 / 100)

ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))

# install.packages("hexbin")
ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price))


ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))


ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))


ggplot(data = diamonds) +
  geom_point(mapping = aes(x = x, y = y)) +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))

library(modelr)

mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

ggplot(data = diamonds2) + 
  geom_point(mapping = aes(x = carat, y = resid))


diamonds %>% 
  count(cut, clarity) %>% 
  ggplot(aes(clarity, cut, fill = n)) + 
  geom_tile()

setwd("H:/DS/")



ggplot(diamonds, aes(carat, price)) + 
  geom_hex()
ggsave("diamonds.pdf")

write_csv(diamonds, "diamonds.csv")

iris
iris_tibble <- as_tibble(iris)


tibble(
  x = 1:5, 
  y = 1, 
  z = x ^ 2 + y
)

tb <- tibble(
  `:)` = "smile", 
  ` ` = "space",
  `2000` = "number"
)
tb

# a 'tribble' is a transposed tribble
tribble(
  ~x, ~y, ~z,
  #--|--|----
  "a", 2, 3.6,
  "b", 1, 8.5
)

# read_csv is about 10 times faster than read.csv
heights <- read_csv("data/heights.csv")


challenge <- read_csv(readr_example("challenge.csv"))

write_csv(challenge, "challenge.csv")


library(feather)
write_feather(challenge, "challenge.feather")
read_feather("challenge.feather")

airlines
airports
planes
weather


planes %>% 
  count(tailnum) %>% 
  filter(n > 1)

weather %>% 
  count(year, month, day, hour, origin) %>% 
  filter(n > 1)

# section 3.1
global_economy |>
  filter(Country=="Australia") |>
  autoplot(GDP/Population) + 
  labs(title= "GDP per Capita", y = "$US")

print_retail <- aus_retail |>
  filter(Industry == "Newspaper and book retailing") |>
  group_by(Industry) |>
  index_by(Year = year(Month)) |>
  summarise(Turnover = sum(Turnover))
aus_economy <- global_economy |>
  filter(Code == "AUS")

print_retail |>
  left_join(aus_economy, by = "Year") |>
  mutate(Adjusted_turnover = Turnover / CPI * 100) |>
  pivot_longer(c(Turnover, Adjusted_turnover),
               values_to = "Turnover") |>
  mutate(name = factor(name,
                       levels=c("Turnover","Adjusted_turnover"))) |>
  ggplot(aes(x = Year, y = Turnover)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(title = "Turnover: Australian print media industry",
       y = "$AU")

lambda <- aus_production |>
  features(Gas, features = guerrero) |>
  pull(lambda_guerrero)
aus_production |>
  autoplot(box_cox(Gas, lambda)) +
  labs(y = "",
       title = "Transformed gas production with lambda = ", lambda)

# section 3.2
us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade") |>
  select(-Series_ID)
autoplot(us_retail_employment, Employed) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")

dcmp <- us_retail_employment |>
  model(stl = STL(Employed))
components(dcmp)

components(dcmp) |>
  as_tsibble() |>
  autoplot(Employed, colour="gray") +
  geom_line(aes(y=trend), colour = "#D55E00") +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

components(dcmp) |> autoplot()

components(dcmp) |>
  as_tsibble() |>
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y=season_adjust), colour = "#0072B2") +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")

# section 3.3
# choose the m value of the moving average that removes seasonality

global_economy |>
  filter(Country == "Australia") |>
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Total Australian exports")

aus_exports <- global_economy |>
  filter(Country == "Australia") |>
  mutate(
    `5-MA` = slider::slide_dbl(Exports, mean,
                               .before = 2, .after = 2, .complete = TRUE)
  )

aus_exports |>
  autoplot(Exports) +
  geom_line(aes(y = `5-MA`), colour = "#D55E00") +
  labs(y = "% of GDP",
       title = "Total Australian exports") +
  guides(colour = guide_legend(title = "series"))

us_retail_employment_ma <- us_retail_employment |>
  mutate(
    `12-MA` = slider::slide_dbl(Employed, mean,
                                .before = 5, .after = 6, .complete = TRUE),
    `2x12-MA` = slider::slide_dbl(`12-MA`, mean,
                                  .before = 1, .after = 0, .complete = TRUE)
  )
us_retail_employment_ma |>
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y = `2x12-MA`), colour = "#D55E00") +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")

# section 3.4
us_retail_employment |>
  model(
    classical_decomposition(Employed, type = "additive")
  ) |>
  components() |>
  autoplot() +
  labs(title = "Classical additive decomposition of total
                  US retail employment")

# section 3.5
x11_dcmp <- us_retail_employment |>
  model(x11 = X_13ARIMA_SEATS(Employed ~ x11())) |>
  components()
autoplot(x11_dcmp) +
  labs(title =
         "Decomposition of total US retail employment using X-11.")

x11_dcmp |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Employed, colour = "Data")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail") +
  scale_colour_manual(
    values = c("gray", "#0072B2", "#D55E00"),
    breaks = c("Data", "Seasonally Adjusted", "Trend")
  )

x11_dcmp |>
  gg_subseries(seasonal)

seats_dcmp <- us_retail_employment |>
  model(seats = X_13ARIMA_SEATS(Employed ~ seats())) |>
  components()
autoplot(seats_dcmp) +
  labs(title =
         "Decomposition of total US retail employment using SEATS")

# 3.6
us_retail_employment |>
  model(
    STL(Employed ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)) |>
  components() |>
  autoplot()

# section 4.1
tourism |>
  features(Trips, list(mean = mean)) |>
  arrange(mean)

tourism |> features(Trips, quantile)

# section 4.2
tourism |> features(Trips, feat_acf)

# section 4.3
tourism |>
  features(Trips, feat_stl)

tourism |>
  features(Trips, feat_stl) |>
  ggplot(aes(x = trend_strength, y = seasonal_strength_year,
             col = Purpose)) +
  geom_point() +
  facet_wrap(vars(State))

tourism |>
  features(Trips, feat_stl) |>
  filter(
    seasonal_strength_year == max(seasonal_strength_year)
  ) |>
  left_join(tourism, by = c("State", "Region", "Purpose"), multiple = "all") |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State, Region, Purpose))

# section 4.5
tourism_features <- tourism |>
  features(Trips, feature_set(pkgs = "feasts"))
tourism_features

library(glue)
tourism_features |>
  select_at(vars(contains("season"), Purpose)) |>
  mutate(
    seasonal_peak_year = seasonal_peak_year +
      4*(seasonal_peak_year==0),
    seasonal_trough_year = seasonal_trough_year +
      4*(seasonal_trough_year==0),
    seasonal_peak_year = glue("Q{seasonal_peak_year}"),
    seasonal_trough_year = glue("Q{seasonal_trough_year}"),
  ) |>
  GGally::ggpairs(mapping = aes(colour = Purpose))

library(broom)
pcs <- tourism_features |>
  select(-State, -Region, -Purpose) |>
  prcomp(scale = TRUE) |>
  augment(tourism_features)
pcs |>
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Purpose)) +
  geom_point() +
  theme(aspect.ratio = 1)

outliers <- pcs |>
  filter(.fittedPC1 > 10) |>
  select(Region, State, Purpose, .fittedPC1, .fittedPC2)
outliers
outliers |>
  left_join(tourism, by = c("State", "Region", "Purpose"), multiple = "all") |>
  mutate(Series = glue("{State}", "{Region}", "{Purpose}", .sep = "\n\n")) |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(Series ~ ., scales = "free") +
  labs(title = "Outlying time series in Principal Component space")

#section 5.1
gdppc <- global_economy |>
  mutate(GDP_per_capita = GDP / Population)

gdppc |>
  filter(Country == "Sweden") |>
  autoplot(GDP_per_capita) +
  labs(y = "$US", title = "GDP per capita for Sweden")

fit <- gdppc |>
  model(trend_model = TSLM(GDP_per_capita ~ trend()))

fit

fit |> forecast(h = "3 years")

fit |>
  forecast(h = "3 years") |>
  filter(Country == "Sweden") |>
  autoplot(gdppc) +
  labs(y = "$US", title = "GDP per capita for Sweden")

# section 5.2
bricks <- aus_production |>
  filter_index("1970 Q1" ~ "2004 Q4") |>
  select(Bricks)

bricks |> model(MEAN(Bricks))
bricks |> model(NAIVE(Bricks))
bricks |> model(SNAIVE(Bricks ~ lag("year")))
bricks |> model(RW(Bricks ~ drift()))

# Set training data from 1992 to 2006
train <- aus_production |>
  filter_index("1992 Q1" ~ "2006 Q4")
# Fit the models
beer_fit <- train |>
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer)
  )
# Generate forecasts for 14 quarters
beer_fc <- beer_fit |> forecast(h = 14)
# Plot forecasts against actual values
beer_fc |>
  autoplot(train, level = NULL) +
  autolayer(
    filter_index(aus_production, "2007 Q1" ~ .),
    colour = "black"
  ) +
  labs(
    y = "Megalitres",
    title = "Forecasts for quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))


# Re-index based on trading days
google_stock <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) >= 2015) |>
  mutate(day = row_number()) |>
  update_tsibble(index = day, regular = TRUE)
# Filter the year of interest
google_2015 <- google_stock |> filter(year(Date) == 2015)
# Fit the models
google_fit <- google_2015 |>
  model(
    Mean = MEAN(Close),
    `Naïve` = NAIVE(Close),
    Drift = NAIVE(Close ~ drift())
  )
# Produce forecasts for the trading days in January 2016
# google_jan_2016 <- google_stock |>
#   filter(yearmonth(Date) == yearmonth("2016 Jan"))

google_jan_2016 <- google_stock[253:271,]

############
# str(yearmonth(google_stock$Date[254])) == str(yearmonth("2016 Feb"))
# 
# yearmonth(google_stock$Date[254]) - yearmonth("2016 Feb")
# 
# print(typeof(yearmonth(google_stock$Date[254]) - yearmonth("2016 Feb")))
#############
google_fc <- google_fit |>
  forecast(new_data = google_jan_2016)
# Plot the forecasts
google_fc |>
  autoplot(google_2015, level = NULL) +
  autolayer(google_jan_2016, Close, colour = "black") +
  labs(y = "$US",
       title = "Google daily closing stock prices",
       subtitle = "(Jan 2015 - Jan 2016)") +
  guides(colour = guide_legend(title = "Forecast"))


# section 5.3
augment(beer_fit)

# section 5.4
autoplot(google_2015, Close) +
  labs(y = "$US",
       title = "Google daily closing stock prices in 2015")

aug <- google_2015 |>
  model(NAIVE(Close)) |>
  augment()
autoplot(aug, .innov) +
  labs(y = "$US",
       title = "Residuals from the naïve method")

aug |>
  ggplot(aes(x = .innov)) +
  geom_histogram() +
  labs(title = "Histogram of residuals")

aug |>
  ACF(.innov) |>
  autoplot() +
  labs(title = "Residuals from the naïve method")

google_2015 |>
  model(NAIVE(Close)) |>
  gg_tsresiduals()

aug |> features(.innov, box_pierce, lag = 10)

aug |> features(.innov, ljung_box, lag = 10)

fit <- google_2015 |> model(RW(Close ~ drift()))
tidy(fit)

augment(fit) |> features(.innov, ljung_box, lag=10)

# section 5.5
google_2015 |>
  model(NAIVE(Close)) |>
  forecast(h = 10) |>
  hilo()

google_2015 |>
  model(NAIVE(Close)) |>
  forecast(h = 10) |>
  autoplot(google_2015) +
  labs(title="Google daily closing stock price", y="$US" )

fit <- google_2015 |>
  model(NAIVE(Close))
sim <- fit |> generate(h = 30, times = 5, bootstrap = TRUE)
sim

google_2015 |>
  ggplot(aes(x = day)) +
  geom_line(aes(y = Close)) +
  geom_line(aes(y = .sim, colour = as.factor(.rep)),
            data = sim) +
  labs(title="Google daily closing stock price", y="$US" ) +
  guides(colour = "none")

fc <- fit |> forecast(h = 30, bootstrap = TRUE)
fc

autoplot(fc, google_2015) +
  labs(title="Google daily closing stock price", y="$US" )

google_2015 |>
  model(NAIVE(Close)) |>
  forecast(h = 10, bootstrap = TRUE, times = 1000) |>
  hilo()

#section 5.6
prices |>
  filter(!is.na(eggs)) |>
  model(RW(log(eggs) ~ drift())) |>
  forecast(h = 50) |>
  autoplot(prices |> filter(!is.na(eggs)),
           level = 80, point_forecast = lst(mean, median)
  ) +
  labs(title = "Annual egg prices",
       y = "$US (in cents adjusted for inflation) ")

# section 5.7
us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade")
dcmp <- us_retail_employment |>
  model(STL(Employed ~ trend(window = 7), robust = TRUE)) |>
  components() |>
  select(-.model)
dcmp |>
  model(NAIVE(season_adjust)) |>
  forecast() |>
  autoplot(dcmp) +
  labs(y = "Number of people",
       title = "US retail employment")

fit_dcmp <- us_retail_employment |>
  model(stlf = decomposition_model(
    STL(Employed ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)
  ))
fit_dcmp |>
  forecast() |>
  autoplot(us_retail_employment)+
  labs(y = "Number of people",
       title = "US retail employment")

fit_dcmp |> gg_tsresiduals()

# section 5.8
aus_production |> filter(year(Quarter) >= 1995)

# extracts the last 20 observations (5 years)
aus_production |>
  slice(n()-19:0)

# 
recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)
beer_train <- recent_production |>
  filter(year(Quarter) <= 2007)

beer_fit <- beer_train |>
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer),
    Drift = RW(Beer ~ drift())
  )

beer_fc <- beer_fit |>
  forecast(h = 10)

beer_fc |>
  autoplot(
    aus_production |> filter(year(Quarter) >= 1992),
    level = NULL
  ) +
  labs(
    y = "Megalitres",
    title = "Forecasts for quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

accuracy(beer_fc, recent_production)


google_fit <- google_2015 |>
  model(
    Mean = MEAN(Close),
    `Naïve` = NAIVE(Close),
    Drift = RW(Close ~ drift())
  )

google_fc <- google_fit |>
  forecast(google_jan_2016)

google_fc |>
  autoplot(bind_rows(google_2015, google_jan_2016),
           level = NULL) +
  labs(y = "$US",
       title = "Google closing stock prices from Jan 2015") +
  guides(colour = guide_legend(title = "Forecast"))

# section 5.9
google_fc |>
  filter(.model == "Naïve") |>
  autoplot(bind_rows(google_2015, google_jan_2016), level=80)+
  labs(y = "$US",
       title = "Google closing stock prices")

google_fc |>
  filter(.model == "Naïve", Date == "2016-01-04") |>
  accuracy(google_stock, list(qs=quantile_score), probs=0.10)

google_fc |>
  filter(.model == "Naïve", Date == "2016-01-04") |>
  accuracy(google_stock,
           list(winkler = winkler_score), level = 80)

google_fc |>
  accuracy(google_stock, list(crps = CRPS))

google_fc |>
  accuracy(google_stock, list(skill = skill_score(CRPS)))

# section 5.10
# Time series cross-validation accuracy
google_2015_tr <- google_2015 |>
  stretch_tsibble(.init = 3, .step = 1) |>
  relocate(Date, Symbol, .id)
google_2015_tr

# TSCV accuracy
google_2015_tr |>
  model(RW(Close ~ drift())) |>
  forecast(h = 1) |>
  accuracy(google_2015)
# Training set accuracy
google_2015 |>
  model(RW(Close ~ drift())) |>
  accuracy()

#The code below evaluates the forecasting performance of 1- to 8-step-ahead drift forecasts. 
#The plot shows that the forecast error increases as the forecast horizon increases, as we would expect.
google_2015_tr <- google_2015 |>
  stretch_tsibble(.init = 3, .step = 1)
fc <- google_2015_tr |>
  model(RW(Close ~ drift())) |>
  forecast(h = 8) |>
  group_by(.id) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(response = "Close", distribution = Close)
fc |>
  accuracy(google_2015, by = c("h", ".model")) |>
  ggplot(aes(x = h, y = RMSE)) +
  geom_point()


# section 7.1
us_change |>
  pivot_longer(c(Consumption, Income), names_to="Series") |>
  autoplot(value) +
  labs(y = "% change")

us_change |>
  ggplot(aes(x = Income, y = Consumption)) +
  labs(y = "Consumption (quarterly % change)",
       x = "Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

us_change |>
  model(TSLM(Consumption ~ Income)) |>
  report()

us_change |>
  select(-Consumption, -Income) |>
  pivot_longer(-Quarter) |>
  ggplot(aes(Quarter, value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  guides(colour = "none") +
  labs(y="% change")

us_change |>
  GGally::ggpairs(columns = 2:6)

# section 7.2
fit_consMR <- us_change |>
  model(tslm = TSLM(Consumption ~ Income + Production +
                      Unemployment + Savings))
report(fit_consMR)

augment(fit_consMR) |>
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Consumption, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Percent change in US consumption expenditure"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

augment(fit_consMR) |>
  ggplot(aes(x = Consumption, y = .fitted)) +
  geom_point() +
  labs(
    y = "Fitted (predicted values)",
    x = "Data (actual values)",
    title = "Percent change in US consumption expenditure"
  ) +
  geom_abline(intercept = 0, slope = 1)

# section 7.3
fit_consMR |> gg_tsresiduals()

augment(fit_consMR) |>
  features(.innov, ljung_box, lag = 10)

us_change |>
  left_join(residuals(fit_consMR), by = "Quarter") |>
  pivot_longer(Income:Unemployment,
               names_to = "regressor", values_to = "x") |>
  ggplot(aes(x = x, y = .resid)) +
  geom_point() +
  facet_wrap(. ~ regressor, scales = "free_x") +
  labs(y = "Residuals", x = "")

augment(fit_consMR) |>
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() + labs(x = "Fitted", y = "Residuals")

fit <- aus_airpassengers |>
  filter(Year <= 2011) |>
  left_join(guinea_rice, by = "Year") |>
  model(TSLM(Passengers ~ Production))
report(fit)

fit |> gg_tsresiduals()

# section 7.4
recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)
recent_production |>
  autoplot(Beer) +
  labs(y = "Megalitres",
       title = "Australian quarterly beer production")

fit_beer <- recent_production |>
  model(TSLM(Beer ~ trend() + season()))
report(fit_beer)

augment(fit_beer) |>
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Beer, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")
  ) +
  labs(y = "Megalitres",
       title = "Australian quarterly beer production") +
  guides(colour = guide_legend(title = "Series"))

augment(fit_beer) |>
  ggplot(aes(x = Beer, y = .fitted,
             colour = factor(quarter(Quarter)))) +
  geom_point() +
  labs(y = "Fitted", x = "Actual values",
       title = "Australian quarterly beer production") +
  geom_abline(intercept = 0, slope = 1) +
  guides(colour = guide_legend(title = "Quarter"))

fourier_beer <- recent_production |>
  model(TSLM(Beer ~ trend() + fourier(K = 2)))
report(fourier_beer)

# section 7.5
glance(fit_consMR) |>
  select(adj_r_squared, CV, AIC, AICc, BIC)

# section 7.6
recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)
fit_beer <- recent_production |>
  model(TSLM(Beer ~ trend() + season()))
fc_beer <- forecast(fit_beer)
fc_beer |>
  autoplot(recent_production) +
  labs(
    title = "Forecasts of beer production using regression",
    y = "megalitres"
  )

fit_consBest <- us_change |>
  model(
    lm = TSLM(Consumption ~ Income + Savings + Unemployment)
  )
future_scenarios <- scenarios(
  Increase = new_data(us_change, 4) |>
    mutate(Income=1, Savings=0.5, Unemployment=0),
  Decrease = new_data(us_change, 4) |>
    mutate(Income=-1, Savings=-0.5, Unemployment=0),
  names_to = "Scenario")

fc <- forecast(fit_consBest, new_data = future_scenarios)

us_change |>
  autoplot(Consumption) +
  autolayer(fc) +
  labs(title = "US consumption", y = "% change")

# section 7.7
boston_men <- boston_marathon |>
  filter(Year >= 1924) |>
  filter(Event == "Men's open division") |>
  mutate(Minutes = as.numeric(Time)/60)

fit_trends <- boston_men |>
  model(
    linear = TSLM(Minutes ~ trend()),
    exponential = TSLM(log(Minutes) ~ trend()),
    piecewise = TSLM(Minutes ~ trend(knots = c(1950, 1980)))
  )
fc_trends <- fit_trends |> forecast(h = 10)

boston_men |>
  autoplot(Minutes) +
  geom_line(data = fitted(fit_trends),
            aes(y = .fitted, colour = .model)) +
  autolayer(fc_trends, alpha = 0.5, level = 95) +
  labs(y = "Minutes",
       title = "Boston marathon winning times")

# section 8.1
algeria_economy <- global_economy |>
  filter(Country == "Algeria")
algeria_economy |>
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Exports: Algeria")

# Estimate parameters
fit <- algeria_economy |>
  model(ETS(Exports ~ error("A") + trend("N") + season("N")))
fc <- fit |>
  forecast(h = 5)
fc |>
  autoplot(algeria_economy) +
  geom_line(aes(y = .fitted), col="#D55E00",
            data = augment(fit)) +
  labs(y="% of GDP", title="Exports: Algeria") +
  guides(colour = "none")

report(fit)

# section 8.2
aus_economy <- global_economy |>
  filter(Code == "AUS") |>
  mutate(Pop = Population / 1e6)
autoplot(aus_economy, Pop) +
  labs(y = "Millions", title = "Australian population")

fit <- aus_economy |>
  model(
    AAN = ETS(Pop ~ error("A") + trend("A") + season("N"))
  )
fc <- fit |> forecast(h = 10)

report(fit)

aus_economy |>
  model(
    `Holt's method` = ETS(Pop ~ error("A") +
                            trend("A") + season("N")),
    `Damped Holt's method` = ETS(Pop ~ error("A") +
                                   trend("Ad", phi = 0.9) + season("N"))
  ) |>
  forecast(h = 15) |>
  autoplot(aus_economy, level = NULL) +
  labs(title = "Australian population",
       y = "Millions") +
  guides(colour = guide_legend(title = "Forecast"))

www_usage <- as_tsibble(WWWusage)
www_usage |> autoplot(value) +
  labs(x="Minute", y="Number of users",
       title = "Internet usage per minute")

www_usage |>
  stretch_tsibble(.init = 10) |>
  model(
    SES = ETS(value ~ error("A") + trend("N") + season("N")),
    Holt = ETS(value ~ error("A") + trend("A") + season("N")),
    Damped = ETS(value ~ error("A") + trend("Ad") +
                   season("N"))
  ) |>
  forecast(h = 1) |>
  accuracy(www_usage)

fit <- www_usage |>
  model(
    Damped = ETS(value ~ error("A") + trend("Ad") +
                   season("N"))
  )
# Estimated parameters:
tidy(fit)

fit |>
  forecast(h = 10) |>
  autoplot(www_usage) +
  labs(x="Minute", y="Number of users",
       title = "Internet usage per minute")

# section 8.3
aus_holidays <- tourism |>
  filter(Purpose == "Holiday") |>
  summarise(Trips = sum(Trips)/1e3)
fit <- aus_holidays |>
  model(
    additive = ETS(Trips ~ error("A") + trend("A") +
                     season("A")),
    multiplicative = ETS(Trips ~ error("M") + trend("A") +
                           season("M"))
  )
fc <- fit |> forecast(h = "3 years")
fc |>
  autoplot(aus_holidays, level = NULL) +
  labs(title="Australian domestic tourism",
       y="Overnight trips (millions)") +
  guides(colour = guide_legend(title = "Forecast"))

sth_cross_ped <- pedestrian |>
  filter(Date >= "2016-07-01",
         Sensor == "Southern Cross Station") |>
  index_by(Date) |>
  summarise(Count = sum(Count)/1000)
sth_cross_ped |>
  filter(Date <= "2016-07-31") |>
  model(
    hw = ETS(Count ~ error("M") + trend("Ad") + season("M"))
  ) |>
  forecast(h = "2 weeks") |>
  autoplot(sth_cross_ped |> filter(Date <= "2016-08-14")) +
  labs(title = "Daily traffic: Southern Cross",
       y="Pedestrians ('000)")

# section 8.6
aus_holidays <- tourism |>
  filter(Purpose == "Holiday") |>
  summarise(Trips = sum(Trips)/1e3)
fit <- aus_holidays |>
  model(ETS(Trips))
report(fit)

components(fit) |>
  autoplot() +
  labs(title = "ETS(M,N,A) components")

# section 8.7
fit |>
  forecast(h = 8) |>
  autoplot(aus_holidays)+
  labs(title="Australian domestic tourism",
       y="Overnight trips (millions)")

# section 9.1
google_2015 <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) == 2015)
google_2015 |> ACF(Close) |>
  autoplot() + labs(subtitle = "Google closing stock price")
google_2015 |> ACF(difference(Close)) |>
  autoplot() + labs(subtitle = "Changes in Google closing stock price")

google_2015 |>
  mutate(diff_close = difference(Close)) |>
  features(diff_close, ljung_box, lag = 10)

PBS |>
  filter(ATC2 == "H02") |>
  summarise(Cost = sum(Cost)/1e6) |>
  transmute(
    `Sales ($million)` = Cost,
    `Log sales` = log(Cost),
    `Annual change in log sales` = difference(log(Cost), 12),
    `Doubly differenced log sales` =
      difference(difference(log(Cost), 12), 1)
  ) |>
  pivot_longer(-Month, names_to="Type", values_to="Sales") |>
  mutate(
    Type = factor(Type, levels = c(
      "Sales ($million)",
      "Log sales",
      "Annual change in log sales",
      "Doubly differenced log sales"))
  ) |>
  ggplot(aes(x = Month, y = Sales)) +
  geom_line() +
  facet_grid(vars(Type), scales = "free_y") +
  labs(title = "Corticosteroid drug sales", y = NULL)

google_2015 |>
  mutate(diff_close = difference(Close)) |>
  features(diff_close, ljung_box, lag = 10)

google_2015 |>
  features(Close, unitroot_kpss)

google_2015 |>
  mutate(diff_close = difference(Close)) |>
  features(diff_close, unitroot_kpss)

google_2015 |>
  features(Close, unitroot_ndiffs)

aus_total_retail <- aus_retail |>
  summarise(Turnover = sum(Turnover))
aus_total_retail |>
  mutate(log_turnover = log(Turnover)) |>
  features(log_turnover, unitroot_nsdiffs)

aus_total_retail |>
  mutate(log_turnover = difference(log(Turnover), 12)) |>
  features(log_turnover, unitroot_ndiffs)

# section 9.5
global_economy |>
  filter(Code == "EGY") |>
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Egyptian exports")

fit <- global_economy |>
  filter(Code == "EGY") |>
  model(ARIMA(Exports))
report(fit)

fit |> forecast(h=10) |>
  autoplot(global_economy) +
  labs(y = "% of GDP", title = "Egyptian exports")

global_economy |>
  filter(Code == "EGY") |>
  ACF(Exports) |>
  autoplot()

global_economy |>
  filter(Code == "EGY") |>
  PACF(Exports) |>
  autoplot()

global_economy |>
  filter(Code == "EGY") |>
  gg_tsdisplay(
    y = Exports,
    plot_type = "partial",
    lag_max = NULL
  )


fit2 <- global_economy |>
  filter(Code == "EGY") |>
  model(ARIMA(Exports ~ pdq(4,0,0)))
report(fit2)

# section 9.7
global_economy |>
  filter(Code == "CAF") |>
  autoplot(Exports) +
  labs(title="Central African Republic exports",
       y="% of GDP")

global_economy |>
  filter(Code == "CAF") |>
  gg_tsdisplay(difference(Exports), plot_type='partial')

caf_fit <- global_economy |>
  filter(Code == "CAF") |>
  model(arima210 = ARIMA(Exports ~ pdq(2,1,0)),
        arima013 = ARIMA(Exports ~ pdq(0,1,3)),
        stepwise = ARIMA(Exports),
        search = ARIMA(Exports, stepwise=FALSE))

caf_fit |> pivot_longer(!Country, names_to = "Model name",
                        values_to = "Orders")

glance(caf_fit) |> arrange(AICc) |> select(.model:BIC)

caf_fit |>
  select(search) |>
  gg_tsresiduals()

augment(caf_fit) |>
  filter(.model=='search') |>
  features(.innov, ljung_box, lag = 10, dof = 3)

caf_fit |>
  forecast(h=5) |>
  filter(.model=='search') |>
  autoplot(global_economy)

gg_arma(caf_fit |> select(Country, search))

# section 9.9
leisure <- us_employment |>
  filter(Title == "Leisure and Hospitality",
         year(Month) > 2000) |>
  mutate(Employed = Employed/1000) |>
  select(Month, Employed)
autoplot(leisure, Employed) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")

leisure |>
  gg_tsdisplay(difference(Employed, 12),
               plot_type='partial', lag=36) +
  labs(title="Seasonally differenced", y="")

leisure |>
  gg_tsdisplay(difference(Employed, 12) |> difference(),
               plot_type='partial', lag=36) +
  labs(title = "Double differenced", y="")

fit <- leisure |>
  model(
    arima012011 = ARIMA(Employed ~ pdq(0,1,2) + PDQ(0,1,1)),
    arima210011 = ARIMA(Employed ~ pdq(2,1,0) + PDQ(0,1,1)),
    auto = ARIMA(Employed, stepwise = FALSE, approx = FALSE)
  )
fit |> pivot_longer(everything(), names_to = "Model name",
                    values_to = "Orders")
glance(fit) |> arrange(AICc) |> select(.model:BIC)

fit |> select(auto) |> gg_tsresiduals(lag=36)

augment(fit) |>
  filter(.model == "auto") |>
  features(.innov, ljung_box, lag=24, dof=4)

forecast(fit, h=36) |>
  filter(.model=='auto') |>
  autoplot(leisure) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")

h02 <- PBS |>
  filter(ATC2 == "H02") |>
  summarise(Cost = sum(Cost)/1e6)
h02 |>
  mutate(log(Cost)) |>
  pivot_longer(-Month) |>
  ggplot(aes(x = Month, y = value)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(y="", title="Corticosteroid drug scripts (H02)")

h02 |> gg_tsdisplay(difference(log(Cost), 12),
                    plot_type='partial', lag_max = 24)

fit <- h02 |>
  model(ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,2)))
fit |> gg_tsresiduals(lag_max=36)

augment(fit) |>
  features(.innov, ljung_box, lag = 36, dof = 6)

h02 |>
  model(ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,2))) |>
  forecast() |>
  autoplot(h02) +
  labs(y=" $AU (millions)",
       title="Corticosteroid drug scripts (H02) sales")

# section 9.10
aus_economy <- global_economy |>
  filter(Code == "AUS") |>
  mutate(Population = Population/1e6)

aus_economy |>
  slice(-n()) |>
  stretch_tsibble(.init = 10) |>
  model(
    ETS(Population),
    ARIMA(Population)
  ) |>
  forecast(h = 1) |>
  accuracy(aus_economy) |>
  select(.model, RMSE:MAPE)

aus_economy |>
  model(ETS(Population)) |>
  forecast(h = "5 years") |>
  autoplot(aus_economy |> filter(Year >= 2000)) +
  labs(title = "Australian population",
       y = "People (millions)")

cement <- aus_production |>
  select(Cement) |>
  filter_index("1988 Q1" ~ .)
train <- cement |> filter_index(. ~ "2007 Q4")

fit_arima <- train |> model(ARIMA(Cement))
report(fit_arima)
fit_arima |> gg_tsresiduals(lag_max = 16)

augment(fit_arima) |>
  features(.innov, ljung_box, lag = 16, dof = 5)

fit_ets <- train |> model(ETS(Cement))
report(fit_ets)
fit_ets |>
  gg_tsresiduals(lag_max = 16)

augment(fit_ets) |>
  features(.innov, ljung_box, lag = 16)

bind_rows(
  fit_arima |> accuracy(),
  fit_ets |> accuracy(),
  fit_arima |> forecast(h = 10) |> accuracy(cement),
  fit_ets |> forecast(h = 10) |> accuracy(cement)
) |>
  select(-ME, -MPE, -ACF1)

cement |>
  model(ARIMA(Cement)) |>
  forecast(h="3 years") |>
  autoplot(cement) +
  labs(title = "Cement production in Australia",
       y = "Tonnes ('000)")

# section 10.2
ARIMA(y ~ x + pdq(1,1,0))

us_change |>
  pivot_longer(c(Consumption, Income),
               names_to = "var", values_to = "value") |>
  ggplot(aes(x = Quarter, y = value)) +
  geom_line() +
  facet_grid(vars(var), scales = "free_y") +
  labs(title = "US consumption and personal income",
       y = "Quarterly % change")

fit <- us_change |>
  model(ARIMA(Consumption ~ Income))
report(fit)

bind_rows(
  `Regression residuals` =
    as_tibble(residuals(fit, type = "regression")),
  `ARIMA residuals` =
    as_tibble(residuals(fit, type = "innovation")),
  .id = "type"
) |>
  mutate(
    type = factor(type, levels=c(
      "Regression residuals", "ARIMA residuals"))
  ) |>
  ggplot(aes(x = Quarter, y = .resid)) +
  geom_line() +
  facet_grid(vars(type))

fit |> gg_tsresiduals()

augment(fit) |>
  features(.innov, ljung_box, dof = 3, lag = 8)

# section 10.3
us_change_future <- new_data(us_change, 8) |>
  mutate(Income = mean(us_change$Income))
forecast(fit, new_data = us_change_future) |>
  autoplot(us_change) +
  labs(y = "Percentage change")

vic_elec_daily <- vic_elec |>
  filter(year(Time) == 2014) |>
  index_by(Date = date(Time)) |>
  summarise(
    Demand = sum(Demand) / 1e3,
    Temperature = max(Temperature),
    Holiday = any(Holiday)
  ) |>
  mutate(Day_Type = case_when(
    Holiday ~ "Holiday",
    wday(Date) %in% 2:6 ~ "Weekday",
    TRUE ~ "Weekend"
  ))

vic_elec_daily |>
  ggplot(aes(x = Temperature, y = Demand, colour = Day_Type)) +
  geom_point() +
  labs(y = "Electricity demand (GW)",
       x = "Maximum daily temperature")

vic_elec_daily |>
  pivot_longer(c(Demand, Temperature)) |>
  ggplot(aes(x = Date, y = value)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") + ylab("")

fit <- vic_elec_daily |>
  model(ARIMA(Demand ~ Temperature + I(Temperature^2) +
                (Day_Type == "Weekday")))
fit |> gg_tsresiduals()

augment(fit) |>
  features(.innov, ljung_box, dof = 6, lag = 14)

vic_elec_future <- new_data(vic_elec_daily, 14) |>
  mutate(
    Temperature = 26,
    Holiday = c(TRUE, rep(FALSE, 13)),
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"
    )
  )
forecast(fit, vic_elec_future) |>
  autoplot(vic_elec_daily) +
  labs(title="Daily electricity demand: Victoria",
       y="GW")

# save.image("H:/DS/r_for_data_science.RData")