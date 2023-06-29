#**************************************************************************************#
#**************************************************************************************#
#
#                 Taller de Introducción a las Series de Tiempo con R                        
#                        Sociedad Ecuatoriana de Estadística
#
#     Fecha de elaboración:   25/06/2023
#     Última actualización:   29/06/2023
#     Actualizado por:        Andrés Peña M.               
#     Contacto:               Andrés Peña M. (agpena@colmex.mx)
#     Organización:           R Users Group - Ecuador
#                             
#
#**************************************************************************************#
#**************************************************************************************#


# Joint Hypothesis Tests

# Suppose that a researcher had estimated the first 5 autocorrelation coefficients 
# using a series of length 100 observations, and found them to be (from 1 to 5): 
# 0.207, -0.013, 0.086, 0.005, -0.022.
# Test each of the individual coefficient for significance, and use both the 
# Box-Pierce and Ljung-Box tests to establish whether they are jointly significant.

T <- 100
m <- 5
tau <- c(0.207, -0.013, 0.086, 0.005, -0.022)

Q <- T*(T+2)*sum(tau^2/(T-1:5))

qchisq(p = 0.95, df = 5)



# Moving average processes 

th1 <- -0.5
th2 <- 0.25

tau0 <- 1
tau1 <- (th1+th1*th2)/(1+th1^2+th2^2)
tau2 <- th2/(1+th1^2+th2^2)
tau3 <- 0
tau4 <- 0
tau5 <- 0
tau6 <- 0


plot(c(tau0, tau1, tau2, tau3, tau4, tau5, tau6))




# # Comandos E-views
# genr = dlog(variable)
# 
# ls dlog(pib) c ar(2) ma(4)
# ls dlog(pib_sa) c ar(1) ma(1)
# 
# smpl @last-8 @last
# smpl @all
# 
# # Corrección de error
# ls var1 c var2 var3 
# ls d(var1) c d(var2) d(var3) resid(-1)


library(fpp3) 

y <- tsibble(
  Year = 2015:2019,
  Observation = c(123, 39, 78, 52, 110),
  index = Year
)


olympic_running |> distinct(Sex)


# Sales data on pharmaceutical products in Australia.
PBS

PBS |>
  filter(ATC2 == "A10")

PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost)

PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
  summarise(TotalC = sum(Cost))

# change the units from dollars to millions of dollars
PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
  summarise(TotalC = sum(Cost)) |>
  mutate(Cost = TotalC/1e6)

# saving
PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
  summarise(TotalC = sum(Cost)) |>
  mutate(Cost = TotalC / 1e6) -> a10

# Read a csv file and convert to a tsibble

prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")
prison <- prison |>
  mutate(Quarter = yearquarter(Date)) |>
  select(-Date) |>
  as_tsibble(key = c(State, Gender, Legal, Indigenous),
             index = Quarter)

prison



# Time plots

melsyd_economy <- ansett |>
  filter(Airports == "MEL-SYD", Class == "Economy") |>
  mutate(Passengers = Passengers/1000)
autoplot(melsyd_economy, Passengers) +
  labs(title = "Ansett airlines economy class",
       subtitle = "Melbourne-Sydney",
       y = "Passengers ('000)")


autoplot(a10, Cost) +
  labs(y = "$ (millions)",
       title = "Australian antidiabetic drug sales")


a10 |>
  gg_season(Cost, labels = "both") +
  labs(y = "$ (millions)",
       title = "Seasonal plot: Antidiabetic drug sales")


# Scatterplot 
visitors <- tourism |>
  group_by(State) |>
  summarise(Trips = sum(Trips))

visitors |>
  pivot_wider(values_from=Trips, names_from=State) |>
  GGally::ggpairs(columns = 2:9)


# Autocorrelation
recent_production <- aus_production |>
  filter(year(Quarter) >= 2000)

recent_production |> ACF(Beer, lag_max = 9)

recent_production |>
  ACF(Beer) |>
  autoplot() + labs(title="Australian beer production")

a10 |>
  ACF(Cost, lag_max = 48) |>
  autoplot() +
  labs(title="Australian antidiabetic drug sales")


# White noise
set.seed(30)
y <- tsibble(sample = 1:50, wn = rnorm(50), index = sample)
y |> autoplot(wn) + labs(title = "White noise", y = "")

y |>
  ACF(wn) |>
  autoplot() + labs(title = "White noise")





# Stationarity and differencing

google_2015 <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) == 2015)

google_2015 |> ACF(Close) |>
  autoplot() + labs(subtitle = "Google closing stock price")

google_2015 |> ACF(difference(Close)) |>
  autoplot() + labs(subtitle = "Changes in Google closing stock price")



google_2015 |>
  mutate(diff_close = difference(Close)) |>
  features(diff_close, ljung_box, lag = 10)



# Unit root tests
google_2015 |>
  features(Close, unitroot_kpss)

google_2015 |>
  mutate(diff_close = difference(Close)) |>
  features(diff_close, unitroot_kpss)

# appropriate number of first differences
google_2015 |>
  features(Close, unitroot_ndiffs)

# seasonal differencing
aus_total_retail <- aus_retail |>
  summarise(Turnover = sum(Turnover))

aus_total_retail |>
  mutate(log_turnover = log(Turnover)) |>
  features(log_turnover, unitroot_nsdiffs)

aus_total_retail |>
  mutate(log_turnover = difference(log(Turnover), 12)) |>
  features(log_turnover, unitroot_ndiffs)



# Non-seasonal ARIMA models

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