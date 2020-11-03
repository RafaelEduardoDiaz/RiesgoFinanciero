rm(list = ls()); gc(T)

## Cargo los paquetes
library(quantmod)
library(highcharter)
library(DT)
library(PerformanceAnalytics)
library(tidyquant)
library(highcharter)
library(plotly)
library(gridExtra)
library(stargazer)

#---------------------------------------------------------------------------------------------------------------------------------------------#

# -------------------- #
# -- Renta Variable -- #
# -------------------- #

## Leo los archivos
nikkei225 <- read.csv(file = "Nikkei225.csv")
canon <- read.csv(file = "Canon.csv") #7751
mazda <- read.csv(file = "Mazda.csv") #7261
bandai <- read.csv(file = "Bandai.csv") #7832
sony <- read.csv(file = "Sony.csv") #6758
yamaha <- read.csv(file = "Yamaha.csv") #7272

## Convierto los datos en series
Nikkei225 <- as.xts(x = nikkei225[,2:5], order.by = as.Date(nikkei225$Date))
Canon <- as.xts(x = canon[,2:5], order.by = as.Date(canon$Date))
Mazda <- as.xts(x = mazda[,2:5], order.by = as.Date(mazda$Date))
Bandai <- as.xts(x = bandai[,2:5], order.by = as.Date(bandai$Date))
Sony <- as.xts(x = sony[,2:5], order.by = as.Date(sony$Date))
Yamaha <- as.xts(x = yamaha[,2:5], order.by = as.Date(yamaha$Date))

## Tabla
Acciones <- as.data.frame(cbind(Nikkei225[,4], Bandai[,4], Canon[,4], Mazda[,4], Sony[,4], Yamaha[,4]))
colnames(Acciones) <- c("Nikkei","Bandai","Canon","Mazda","Sony","Yamaha")
DT::datatable(data = Acciones, rownames = TRUE, class = 'cell-border stripe', extensions = 'Buttons',
              options = list(pageLength = 20, dom = 'Btip', buttons = c('copy', 'csv', 'excel', 'pdf')))

## Graficas
p1 <- hchart(Nikkei225, name = "Nikkei225", color = "green") %>% hc_title(text = "Nikkei 225 index - OSA")
p2 <- hchart(Bandai, name = "Bandai", color = "violet") %>% hc_title(text = "Bandai Namco Entertainment Inc.  Equity - JPX")
p3 <- hchart(Canon, name = "Canon", color = "red") %>% hc_title(text = "Canon Inc. Equity - JPX")
p4 <- hchart(Mazda, name = "Mazda", color = "blue") %>% hc_title(text = "Mazda motor corporation Equity - JPX")
p5 <- hchart(Sony, name = "Sony", color = "cyan") %>% hc_title(text = "Sony corporation Equity - JPX")
p6 <- hchart(Yamaha, name = "Yamaha", color = "pink") %>% hc_title(text = "Yamaha motor Co., Ltd.. Equity - JPX")

hw_grid(list(p1, p2, p3, p4, p5, p6), ncol = 3)

retornos <- Return.calculate(prices = Acciones, method = "log")
DT::datatable(data = round(retornos,7), colnames = paste("Retornos",colnames(retornos)), rownames = TRUE, 
              class = 'cell-border stripe', extensions = 'Buttons',
              options = list(pageLength = 20, dom = 'Btip', buttons = c('copy', 'csv', 'excel', 'pdf')))

## Rendimientos
`Retorno Promedio` <- colMeans(x = retornos, na.rm = TRUE)
`Retorno EA` <- ((1 + `Retorno Promedio`)^nrow(retornos)-1)
Varianza <- apply(X = retornos, MARGIN = 2, FUN = function(x)var(x,na.rm = T))
Volatilidad <- apply(X = retornos, MARGIN = 2, FUN = function(x)sd(x,na.rm = T))
covarianzas <- cov(retornos[-1,])[1,]
Beta <-  covarianzas / var(retornos[-1,1])
estadisticas <-rbind(`Retorno Promedio`, `Retorno EA`, Varianza, Volatilidad, covarianzas, Beta)

datatable(t(estadisticas)) %>% 
  formatPercentage(c("Retorno Promedio", "Retorno EA", "Volatilidad"), 2) %>%
  formatRound(c("Varianza"), digits = 4)

p1 <- ggplotly(na.omit(retornos) %>%
                 ggplot(aes(x = Nikkei, y = Bandai)) + geom_point(color = palette_light()[[1]], alpha = 0.5) +
                 geom_smooth(method = "lm", formula = 'y ~ x') + labs(title = "Rendimientos de las acciones vs Nikkei225") +theme_tq())

p2 <- ggplotly(na.omit(retornos) %>%
                 ggplot(aes(x = Nikkei, y = Canon)) + geom_point(color = palette_light()[[1]], alpha = 0.5) +
                 geom_smooth(method = "lm", formula = 'y ~ x') + labs(title = "Rendimientos de las acciones vs Nikkei225") + theme_tq())

p3 <- ggplotly(na.omit(retornos) %>%
                 ggplot(aes(x = Nikkei, y = Mazda)) + geom_point(color = palette_light()[[1]], alpha = 0.5) +
                 geom_smooth(method = "lm", formula = 'y ~ x') + labs(title = "Rendimientos de Mazda vs Nikkei225") +theme_tq())

p4 <- ggplotly(na.omit(retornos) %>%
                 ggplot(aes(x = Nikkei, y = Sony)) + geom_point(color = palette_light()[[1]], alpha = 0.5) +
                 geom_smooth(method = "lm", formula = 'y ~ x') + labs(title = "Rendimientos de Sony vs Nikkei225") +theme_tq())

p5 <- ggplotly(na.omit(retornos) %>%
                 ggplot(aes(x = Nikkei, y = Yamaha)) + geom_point(color = palette_light()[[1]], alpha = 0.5) +
                 geom_smooth(method = "lm", formula = 'y ~ x') + labs(title = "Rendimientos de las acciones vs Nikkei225") +theme_tq())

subplot(list(p1, p2, p3, p4, p5), nrows = 1, margin = 0.03, shareX = TRUE, shareY = FALSE, titleY = TRUE)

mod1 <- lm(formula = Bandai ~ Nikkei, data = retornos)
mod2 <- lm(formula = Canon ~ Nikkei, data = retornos)
mod3 <- lm(formula = Mazda ~ Nikkei, data = retornos)
mod4 <- lm(formula = Sony ~ Nikkei, data = retornos)
mod5 <- lm(formula = Yamaha ~ Nikkei, data = retornos)

stargazer(mod1, mod2, mod3, mod4, mod5, type = "text", single.row = TRUE)


# Name	                     Exp Return
# Investable Universe (225)	       14.9
# BANDAI NAMCO HOLDINGS INC	    -4.5136
# CANON INC	                     2.3305
# MAZDA MOTOR CORP	            -1.1866
# SONY CORP	                     29.156
# YAMAHA MOTOR CO LTD	          19.2481

resultado <- data.frame(Retornos = c(14.9,0.03,-4.5136,2.3305,-1.1866,29.156,19.2481), Betas = c(Beta[1], 0, Beta[-1]))
rownames(resultado)[1:2] <- c("Retornos mercado", "TES")
resultado$Retorno_Requerido <- round(c(NA_integer_, NA_integer_, resultado[2,1] + resultado$Betas[-c(1:2)] * (resultado[1,1] - resultado[2,1])), 2)
resultado$Evaluacion <- ifelse(resultado$Retorno_Requerido < resultado$Retornos,"Subvalorada", "Sobrevalorada")
resultado$Alfa <- resultado$Retornos - resultado$Retorno_Requerido
nombres <- rownames(resultado)
resultado <- resultado %>% mutate(Retornos = Retornos / 100, Retorno_Requerido = Retorno_Requerido / 100, Alfa = Alfa / 100)
rownames(resultado) <- nombres
resultado

datatable(resultado, rownames = TRUE) %>% 
  formatPercentage(c("Retornos", "Retorno_Requerido", "Alfa"), 2) %>% formatRound("Betas", digits = 4)

m <- (resultado$Retornos[1] - resultado$Retornos[2]) / (1 - 0)
y <- function(x){return(m * x + resultado$Retornos[2])}

plot_ly(data = resultado, x = resultado$Betas, y = resultado$Retornos, text = rownames(resultado)) %>%
  add_markers() %>% add_text(textfont = list(family = "sans serif", size = 14, color = toRGB("grey50")), textposition = "top right") %>% 
  layout(showlegend = FALSE, title = 'Capital Asset Price Model CAPM',xaxis = list(title = 'Beta'), yaxis = list(title = 'Expected return')) %>%
  add_segments(x = 1, xend = 1, y = 0, yend = resultado$Retornos[1], line = list(color = "rgb(46, 62, 204)")) %>%
  add_segments(x = 0, xend = 1, y = resultado$Retornos[1], yend = resultado$Retornos[1], line = list(color = "rgb(46, 62, 204)")) %>%
  add_segments(x = 0, xend = 1, y = resultado$Retornos[2], yend = resultado$Retornos[1], line = list(color = "rgb(231, 76, 60)")) %>%
  add_segments(x = 1, xend = 1.7, y = resultado$Retornos[1], yend = y(1.7), line = list(color = "rgb(231, 76, 60)"))

# -------------------------- #
# -- Portafolio Eficiente -- #
# -------------------------- #

# https://www.codingfinance.com/post/2018-05-31-portfolio-opt-in-r/

rm(list = ls())
library(tidyquant) # To download the data
library(plotly) # To create interactive charts
library(timetk) # To manipulate the data series
library(tidyr)
library(tibble)
library(data.table)
library(scales)
library(corrplot)

## Descargar los datos desde Yahoo Finance para las acciones:

# Apple Inc (AAPL)
# Amazon (AMZN)
# Netflix (NFLX)
# Exxon Mobil (XOM)
# AT&T (T)
# Facebook (FB)

price_data <- tq_get(c('AMZN','AAPL','FB','NFLX','T'),
                     from = '2014-01-01', to = '2019-12-31',
                     get = 'stock.prices')

DT::datatable(data = price_data, rownames = TRUE, class = 'cell-border stripe', extensions = 'Buttons',
              options = list(pageLength = 20, dom = 'Btip', buttons = c('copy', 'csv', 'excel', 'pdf')))

plot1 <- price_data %>%
  group_by(symbol) %>%
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line(size = 1) +
  labs(title = "Daily Stock Prices", x = "", y = "Adjusted Prices", color = "") +
  facet_wrap(~ symbol, ncol = 3, scales = "free_y") +
  scale_y_continuous(labels = scales::dollar) +
  theme_tq() + scale_color_tq()

ggplotly(plot1)

## Calcular los retornos
log_ret_tidy <- price_data %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'yearly',
               col_rename = 'ret',
               type = 'log', leading = T)


plot2 <- log_ret_tidy %>%
  ggplot(aes(x = date, y = ret, fill = symbol)) +
  geom_col() +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "FANG: Annual Returns",
       subtitle = "Get annual returns quickly with tq_transmute!",
       y = "Annual Returns", x = "") + 
  facet_wrap(~ symbol, ncol = 3, scales = "free_y") +
  theme_tq() + scale_fill_tq()
ggplotly(plot2)

## Se convierte en formato ancho (dcast) y lueg en un time series object
Retornos <- log_ret_tidy %>% mutate(Year = year(date)) %>% 
  group_by(symbol, Year) %>% summarise(mean_ret = mean(ret)) %>% spread(symbol, value = mean_ret) %>% 
  column_to_rownames(var = "Year")

corrplot(cor(Retornos), method="number", diag = T, type="upper")

colMeans(Retornos)
apply(Retornos,2,sd)

Portafolio.min.var <- function(Acciones){
  R <- colMeans(Acciones)
  n <- ncol(Acciones)
  Sigma <- cov(Acciones)
  Z0 <- rbind(cbind(2 * Sigma, rep(1,n)), c(rep(1,n),0))
  Q0 <- c(rep(0,n),1)
  W0 <- solve(Z0) %*% Q0
  
  Rp <- t(W0[1:n]) %*% R
  Sigma2 <- t(W0[1:n]) %*% Sigma %*% W0[1:n]
  return(list(Pesos =   round(W0[1:n,], 5), Rendimientos = round(Rp[1], 5), Riesgo = round(sqrt(Sigma2[1]), 5)))
}

Frontera.eficiente <- function(Acciones, TES = 0){
  R <- colMeans(Acciones)
  n <- ncol(Acciones)
  Sigma <- cov(Acciones)
  Z <- cbind(rbind(2 * Sigma, R, rep(1,n)), c(R, 0, 0), c(rep(1,n),0, 0))
  
  rk <- seq(from = min(R), to = max(R) + 0.11, length.out = 100)
  m <- length(rk)
  mat <- matrix(data = NA, nrow = m, ncol = ncol(Acciones) + 2)
  colnames(mat) <- c(colnames(Acciones), "Rendimientos", "Riesgo")
  for(i in seq_along(rk)){
    Q <- c(rep(0,n), rk[i], 1)
    W <- solve(Z) %*% Q
    Rp <- t(W[1:n]) %*% R
    SigmaP <- sqrt(t(W[1:n]) %*% Sigma %*% W[1:n])
    mat[i,] <- c(W[1:n], Rp, SigmaP)
  }
  resultado <- as.data.frame(mat)
  resultado$Ratio <- (resultado$Rendimiento - TES) / resultado$Riesgo
  return(round(resultado, 5))
}

min_var <- as.data.frame(t(unlist(Portafolio.min.var(Acciones = Retornos))))

valores_portafolio <- Frontera.eficiente(Acciones = Retornos, TES = 0.015)

max_sr <- filter(valores_portafolio, Ratio == max(Ratio))


plot_port <- valores_portafolio %>%
  ggplot(aes(x = Riesgo, y = Rendimientos, color = Ratio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Riesgo anualizado',
       y = 'Retornos anualizados',
       title = "Optimización del portafolio & Frontera eficiente") +
  geom_point(aes(x = Riesgo,
                 y = Rendimientos), data = min_var, color = 'red') +
  geom_point(aes(x = Riesgo,
                 y = Rendimientos), data = max_sr, color = 'red') +
  annotate('text', x = 0.12, y = 0.42, label = "Portafolio Tangente") +
  annotate('text', x = 0.11, y = 0.1, label = "Portafolio de mínima varianza") +
  annotate(geom = 'segment', x = 0.1, xend = 0.08908,  y = 0.11, 
           yend = 0.247, color = 'red', arrow = arrow(type = "open")) +
  annotate(geom = 'segment', x = 0.12, xend = 0.09525,  y = 0.415, 
           yend = 0.287, color = 'red', arrow = arrow(type = "open"))

ggplotly(plot_port)
