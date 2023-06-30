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

rm(list = ls())

library(fpp3)

setwd("C:\\Users\\LENOVO\\OneDrive - El Colegio de México A.C\\DEP_22_26\\2do semestre\\Series de Tiempo\\bdd")

ipc <- read.csv("ipc.csv")
ipc$t <-as.Date(ipc$t, format="%d/%m/%Y")


ipc <- ipc |>
  mutate(month = yearmonth(t)) |>
  as_tsibble(index = month) |>
  filter_index("2003 mar." ~ "2023 mar.") |>
  mutate(infl = c(NA, diff(log(ipc)))*100,
         infl_a = c(rep(NA,12), diff(log(ipc), 12))*100,
         infl_m = c(NA, diff(ipc))/lag(ipc)*100)


autoplot(ipc, infl)

plot(ipc$t, ipc$infl, type = "l")
lines(ipc$t, ipc$infl_m, col="yellow")


ipc |>
  gg_season(infl, labels = "both", period = "year") +
  labs(y = "Index",
       title = "Seasonal plot: IPC")

ipc |>
  gg_subseries(infl) +
  labs(
    y = "Index",
    title = "Seasonal plot: IPC"
  )


# ACF y PACF
ipc |>
  ACF(infl, lag_max = 48) |>
  autoplot() + labs(title="Inflación mensual")

ipc |>
  PACF(infl, lag_max = 48) |>
  autoplot() + labs(title="Inflación mensual")



# Decomposición de la inflación usando X-11.
x11_dcmp <- ipc |>
  model(x11 = X_13ARIMA_SEATS(infl ~ x11())) |>
  components() 
  # |> select(-.model)

autoplot(x11_dcmp) +
  labs(title =
         "Decomposición de la inflación usando X-11.")

x11_dcmp |>
  ggplot(aes(x = month)) +
  geom_line(aes(y = infl, colour = "Data")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Persons (thousands)",
       title = "Inflación mensual") +
  scale_colour_manual(
    values = c("gray", "#0072B2", "#D55E00"),
    breaks = c("Data", "Seasonally Adjusted", "Trend")
  )


ipc |>
  features(infl, unitroot_kpss)


ipc |>
  mutate(diff_close = difference(infl)) |>
  features(diff_close, unitroot_kpss)

x11_dcmp |>
  mutate(diff_close = difference(season_adjust)) |>
  features(diff_close, unitroot_kpss)



ipc |> ACF(difference(infl)) |>
  autoplot() + labs(subtitle = "IPC") 

ipc |> PACF(difference(infl)) |>
  autoplot() + labs(subtitle = "IPC") 



ipc |>
  features(infl, unitroot_ndiffs)

x11_dcmp |>
  features(season_adjust, unitroot_ndiffs)



ipc |>
  features(infl, unitroot_nsdiffs)


ipc <- bind_cols(ipc, x11_dcmp[ ,"season_adjust"])    



fit_sa <- x11_dcmp |>
  model(ARIMA(season_adjust, trace = T,
              stepwise = F, approximation = F)
  ) 
report(fit_sa)


x11_dcmp |>
  model(ARIMA(season_adjust, trace = F,
              stepwise = F, approximation = F)) |>
  forecast() |>
  autoplot(x11_dcmp) +
  labs(y = "Number of people",
       title = "US retail employment")


fit_dcmp <- ipc[-1,] |>
  model(stlf = decomposition_model(
    X_13ARIMA_SEATS(infl ~ x11()),
    ARIMA(infl, trace = T,
          stepwise = F, approximation = F)
  ))

fitted <- c(NA, ipc$infl[-1]-residuals(fit_dcmp)[[3]])
ipc <- bind_cols(ipc, fitted=fitted)

fit_dcmp |>
  forecast(h=36) |>
  autoplot(ipc)+
  geom_line(aes(y = fitted), color = "blue") +
  labs(y = "Inflación",
       title = "Inflación mensual - Ecuador") 


accuracy(fit)
accuracy(fit_dcmp)


fit |> forecast(h=36) |>
  autoplot(ipc) +
  labs(y = "% of GDP", title = "IPC Ecuador")

augment(fit_dcmp) |>
  features(.innov, ljung_box, lag = 24, dof = 4) 

fit_dcmp |>
  gg_tsresiduals(lag = 36)



ipc |>
  gg_tsdisplay(infl, plot_type='partial', lag_max = 48)


ipc |>
  gg_tsdisplay(difference(infl), plot_type='partial',
               lag_max = 48)

x11_dcmp |>
  gg_tsdisplay(difference(season_adjust), plot_type='partial',
               lag_max = 48)

ipc |>
  gg_tsdisplay(difference(infl, 12) |> difference(), 
               plot_type='partial', lag_max = 36)