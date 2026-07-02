#################################################
### PROJETO CO2 - MAUNA LOA
### ETAPA: IMPORTACAO DOS DADOS
#################################################

library(fpp3)
library(forecast)
library(tseries)
library(patchwork)

data("co2")

dados_co2 =
  co2

dados_co2_tsibble =
  as_tsibble(dados_co2)


#################################################
### ANALISE EXPLORATORIA
#################################################

library(fpp3)

head(dados_co2)

tail(dados_co2)

summary(dados_co2)

start(dados_co2)

end(dados_co2)

frequency(dados_co2)

layout(
  matrix(
    c(1,1,
      2,3),
    nrow = 2,
    byrow = TRUE
  )
)

plot(
  dados_co2,
  main = "Serie temporal (CO2)",
  ylab = "CO2"
)

acf(
  dados_co2,
  main = "ACF"
)

pacf(
  dados_co2,
  main = "PACF"
)

glimpse(dados_co2_tsibble)

summary(
  dados_co2_tsibble$value
)

dados_mensais =
  as.data.frame(
    dados_co2_tsibble
  )

dados_mensais$MES =
  lubridate::month(
    dados_mensais$index
  )

estatisticas_mensais =
  dados_mensais |>
  group_by(MES) |>
  summarise(
    media = mean(value),
    sd = sd(value)
  )

estatisticas_mensais$MES =
  factor(
    estatisticas_mensais$MES,
    levels = 1:12,
    labels = c(
      "Jan","Fev","Mar","Abr",
      "Mai","Jun","Jul","Ago",
      "Set","Out","Nov","Dez"
    )
  )

estatisticas_mensais

#################################################
### TESTES DE ESTACIONARIEDADE
#################################################

library(tseries)

adf.test(
  dados_co2
)

kpss.test(
  dados_co2
)

dados_co2_diff2 =
  diff(
    diff(dados_co2),
    lag = 12
  )

ggtsdisplay(
  dados_co2_diff2,
  lag.max = 48,
  points = FALSE,
  main = ""
)

adf.test(
  dados_co2_diff2
)

kpss.test(
  dados_co2_diff2
)

#################################################
### DECOMPOSICAO STL
#################################################

library(fpp3)

# Decomposicao classica aditiva

decomp =
  
  dados_co2_tsibble %>%
  
  model(
    
    classical_decomposition(
      
      value,
      
      type = "additive"
    )
  ) %>%
  
  components()



# Removendo tendencia

decomp$sem_tendencia =
  
  decomp$value -
  
  decomp$trend



# Criando base para heatmap

dados_heatmap =
  
  data.frame(
    
    data = as.Date(decomp$index),
    
    valor = decomp$sem_tendencia
  )



# Extraindo ano

dados_heatmap$ano =
  
  format(
    
    dados_heatmap$data,
    
    "%Y"
  )



# Extraindo mes

dados_heatmap$mes =
  
  factor(
    
    format(
      
      dados_heatmap$data,
      
      "%m"
    ),
    
    levels = c(
      
      "01","02","03","04",
      "05","06","07","08",
      "09","10","11","12"
    ),
    
    labels = c(
      
      "Jan","Fev","Mar","Abr",
      "Mai","Jun","Jul","Ago",
      "Set","Out","Nov","Dez"
    )
  )



# Heatmap sem tendencia

ggplot(
  
  dados_heatmap,
  
  aes(
    
    x = ano,
    
    y = mes,
    
    fill = valor
  )
) +
  
  geom_tile(
    
    color = "white",
    
    linewidth = 0.2
  ) +
  
  scale_y_discrete(
    
    limits =
      
      rev(
        
        levels(
          
          dados_heatmap$mes
        )
      )
  ) +
  
  scale_fill_gradient(
    
    low = "white",
    
    high = "orangered4"
  ) +
  
  labs(
    
    title = "",
    
    x = "",
    
    y = "",
    
    fill = "CO2"
  ) +
  
  theme_minimal()



# Decomposicao STL

modelo_stl_co2 =
  
  dados_co2_tsibble %>%
  
  model(
    
    STL(
      
      value ~ trend() +
        
        season(
          
          window = "periodic"
        ),
      
      robust = TRUE
    )
  )



# Componentes STL

componentes_stl_co2 =
  
  modelo_stl_co2 %>%
  
  components()



# Grafico dos componentes

componentes_stl_co2 %>%
  
  autoplot() +
  
  labs(
    
    x = "",
    
    y = ""
  ) +
  
  theme_bw()



# Residuos STL

componentes_stl_co2 %>%
  
  autoplot(
    
    remainder
  ) +
  
  geom_hline(
    
    yintercept = 0,
    
    colour = "red",
    
    linetype = "dashed"
  ) +
  
  theme_bw()



# FAC dos residuos STL

componentes_stl_co2 %>%
  
  ACF(
    
    remainder
  ) %>%
  
  autoplot() +
  
  theme_bw()

#################################################
### MODELOS CANDIDATOS
#################################################

library(fpp3)

# Divisao treino e teste

treino_co2 =
  
  dados_co2_tsibble %>%
  
  filter(
    year(index) <= 1996
  )

teste_co2 =
  
  dados_co2_tsibble %>%
  
  filter(
    year(index) > 1996
  )



# Ajuste dos modelos

modelos_candidatos_co2 =
  
  treino_co2 %>%
  
  model(
    
    snaive =
      SNAIVE(
        value ~ lag(12)
      ),
    
    ets =
      ETS(
        value
      ),
    
    arima =
      ARIMA(
        value
      ),
    
    sarima =
      ARIMA(
        value ~ pdq() + PDQ()
      )
  )

#################################################
### CRITERIOS DE INFORMACAO
#################################################

library(fpp3)

criterios_info_co2 =
  
  modelos_candidatos_co2 %>%
  
  glance() %>%
  
  dplyr::select(
    
    .model,
    
    AIC,
    
    AICc,
    
    BIC
  )

criterios_info_co2

#======================================
# BACKTESTING DOS MODELOS
#======================================

library(fpp3)

#################################################
### DIVISAO TREINO E TESTE
#################################################

treino_co2 =
  
  dados_co2_tsibble %>%
  
  filter(
    year(index) <= 1996
  )



teste_co2 =
  
  dados_co2_tsibble %>%
  
  filter(
    year(index) > 1996
  )



#################################################
### AJUSTANDO MODELOS CANDIDATOS
#################################################

modelos_candidatos_co2 =
  
  treino_co2 %>%
  
  model(
    
    snaive = SNAIVE(
      value ~ lag(12)
    ),
    
    ets = ETS(
      value
    ),
    
    arima = ARIMA(
      value
    ),
    
    sarima = ARIMA(
      value ~ pdq() + PDQ()
    )
  )



#################################################
### PREVISOES NO CONJUNTO TESTE
#################################################

fc_co2 =
  
  modelos_candidatos_co2 %>%
  
  forecast(
    h = nrow(teste_co2)
  )



#################################################
### TABELA DE CRITERIOS DE PREVISAO
#################################################

criterios_previsao_co2 =
  
  accuracy(
    fc_co2,
    teste_co2
  ) %>%
  
  mutate(
    
    EQM = RMSE^2,
    
    EAM = MAE,
    
    EPM = MPE,
    
    EPAM = MAPE
  ) %>%
  
  dplyr::select(
    
    .model,
    
    EPM,
    
    EQM,
    
    EAM,
    
    EPAM
  )



#################################################
### RESULTADO
#################################################

criterios_previsao_co2



#################################################
### CODIGO UTILIZADO APENAS PARA CONFERENCIA
#################################################

# accuracy(fc_co2, teste_co2)

#======================================
# EXTRAINDO MODELO ETS AJUSTADO
#======================================

fit_ets_co2 =
  
  modelos_candidatos_co2 %>%
  
  dplyr::select(ets)

#======================================
# GRAFICO DOS RESIDUOS
#======================================

graf_res_ets_co2 =
  
  fit_ets_co2 %>%
  
  residuals() %>%
  
  autoplot(.resid) +
  
  labs(
    x = "",
    y = "Residuos",
    title = ""
  ) +
  
  theme_bw()

#======================================
# HISTOGRAMA DOS RESIDUOS
#======================================

graf_hist_ets_co2 =
  
  fit_ets_co2 %>%
  
  residuals() %>%
  
  ggplot(
    aes(x = .resid)
  ) +
  
  geom_histogram(
    bins = 20,
    color = "black",
    fill = "gray90"
  ) +
  
  labs(
    x = "Residuos",
    y = "Frequencia",
    title = ""
  ) +
  
  theme_bw()

#======================================
# FAC DOS RESIDUOS
#======================================

graf_acf_ets_co2 =
  
  fit_ets_co2 %>%
  
  residuals() %>%
  
  ACF() %>%
  
  autoplot() +
  
  labs(
    x = "Lag",
    y = "FAC",
    title = ""
  ) +
  
  theme_bw()

#======================================
# QQ-PLOT DOS RESIDUOS
#======================================

graf_qq_ets_co2 =
  
  fit_ets_co2 %>%
  
  residuals() %>%
  
  ggplot(
    aes(sample = .resid)
  ) +
  
  stat_qq(size = 0.9) +
  
  stat_qq_line() +
  
  labs(
    x = "Quantis teoricos",
    y = "Quantis amostrais",
    title = ""
  ) +
  
  theme_bw()

#======================================
# GRAFICOS COMBINADOS
#======================================

(graf_res_ets_co2) /
  (
    graf_acf_ets_co2 |
      graf_hist_ets_co2 |
      graf_qq_ets_co2
  )

#======================================
# TESTE DE LJUNG-BOX - ETS
#======================================

fit_ets_co2 %>%
  
  augment() %>%
  
  features(
    .innov,
    ljung_box,
    lag = 24,
    dof = 0
  )

#======================================
# RESIDUOS ETS
#======================================

res_ets_co2 =
  
  augment(
    fit_ets_co2
  )$.innov

n =
  
  length(
    res_ets_co2
  )

acf_ets_co2 =
  
  acf(
    res_ets_co2,
    lag.max = 24,
    plot = FALSE
  )

#======================================
# BOX-PIERCE ETS
#======================================

qmBP_ets_co2 = 0

pvBP_ets_co2 = 0

for(i in 2:25){
  
  qmBP_ets_co2[i-1] =
    
    n *
    
    sum(
      acf_ets_co2$acf[2:i]^2
    )
  
  pvBP_ets_co2[i-1] =
    
    1 -
    
    pchisq(
      qmBP_ets_co2[i-1],
      i - 1
    )
}

#======================================
# LJUNG-BOX ETS
#======================================

qmLB_ets_co2 = 0

pvLB_ets_co2 = 0

for(i in 2:25){
  
  qmLB_ets_co2[i-1] =
    
    n *
    
    (n + 2) *
    
    sum(
      acf_ets_co2$acf[2:i]^2 /
        ((n - 1):(n - i + 1))
    )
  
  pvLB_ets_co2[i-1] =
    
    1 -
    
    pchisq(
      qmLB_ets_co2[i-1],
      i - 1
    )
}

#======================================
# GRAFICO DOS VALORES-P
#======================================

ggplot() +
  
  geom_line(
    aes(
      x = 1:24,
      y = pvBP_ets_co2,
      colour = "Box-Pierce"
    ),
    linewidth = 1
  ) +
  
  geom_line(
    aes(
      x = 1:24,
      y = pvLB_ets_co2,
      colour = "Ljung-Box"
    ),
    linewidth = 1
  ) +
  
  geom_hline(
    yintercept = 0.05,
    linetype = "dashed",
    colour = "red"
  ) +
  
  scale_colour_manual(
    values = c(
      "Box-Pierce" = "black",
      "Ljung-Box" = "green"
    )
  ) +
  
  scale_y_continuous(
    breaks = c(
      0.05,
      0.25,
      0.50,
      0.75,
      1.00
    )
  ) +
  
  labs(
    x = "Lag",
    y = "Valor-p para o teste de Portmanteau",
    colour = "",
    title = ""
  ) +
  
  theme_bw()

library(fpp3)

library(tseries)

library(patchwork)

#======================================

# EXTRAINDO MODELO SARIMA AJUSTADO

#======================================

fit_sarima_co2 =
  
  modelos_candidatos_co2 %>%
  
  dplyr::select(sarima)

#======================================

# GRAFICO DOS RESIDUOS

#======================================

graf_res_sarima_co2 =
  
  fit_sarima_co2 %>%
  
  residuals() %>%
  
  autoplot(.resid) +
  
  labs(
    x = "",
    y = "Residuos",
    title = ""
  ) +
  
  theme_bw()

#======================================

# HISTOGRAMA DOS RESIDUOS

#======================================

graf_hist_sarima_co2 =
  
  fit_sarima_co2 %>%
  
  residuals() %>%
  
  ggplot(
    aes(x = .resid)
  ) +
  
  geom_histogram(
    bins = 20,
    color = "black",
    fill = "gray90"
  ) +
  
  labs(
    x = "Residuos",
    y = "Frequencia",
    title = ""
  ) +
  
  theme_bw()

#======================================

# FAC DOS RESIDUOS

#======================================

graf_acf_sarima_co2 =
  
  fit_sarima_co2 %>%
  
  residuals() %>%
  
  ACF() %>%
  
  autoplot() +
  
  labs(
    x = "Lag",
    y = "FAC",
    title = ""
  ) +
  
  theme_bw()

#======================================

# QQ-PLOT DOS RESIDUOS

#======================================

graf_qq_sarima_co2 =
  
  fit_sarima_co2 %>%
  
  residuals() %>%
  
  ggplot(
    aes(sample = .resid)
  ) +
  
  stat_qq(size = 0.9) +
  
  stat_qq_line() +
  
  labs(
    x = "Quantis teoricos",
    y = "Quantis amostrais",
    title = ""
  ) +
  
  theme_bw()

#======================================

# GRAFICOS COMBINADOS

#======================================

(graf_res_sarima_co2) /
  (
    graf_acf_sarima_co2 |
      graf_hist_sarima_co2 |
      graf_qq_sarima_co2
  )

#======================================

# TESTE DE LJUNG-BOX - SARIMA

#======================================

fit_sarima_co2 %>%
  
  augment() %>%
  
  features(
    .innov,
    ljung_box,
    lag = 24,
    dof = 5
  )

#======================================

# RESIDUOS SARIMA

#======================================

res_sarima_co2 =
  
  augment(
    fit_sarima_co2
  )$.innov

n =
  
  length(
    res_sarima_co2
  )

acf_sarima_co2 =
  
  acf(
    res_sarima_co2,
    lag.max = 24,
    plot = FALSE
  )

#======================================

# BOX-PIERCE SARIMA

#======================================

qmBP_sarima_co2 = 0

pvBP_sarima_co2 = 0

for(i in 2:25){
  
  qmBP_sarima_co2[i-1] =
    
    n *
    
    sum(
      acf_sarima_co2$acf[2:i]^2
    )
  
  
  pvBP_sarima_co2[i-1] =
    1 -
    
    pchisq(
      qmBP_sarima_co2[i-1],
      i - 1 - 5
    )
  
}

#======================================

# LJUNG-BOX SARIMA

#======================================

qmLB_sarima_co2 = 0

pvLB_sarima_co2 = 0

for(i in 2:25){
  
  qmLB_sarima_co2[i-1] =
    
    n *
    
    (n + 2) *
    
    sum(
      acf_sarima_co2$acf[2:i]^2 /
        ((n - 1):(n - i + 1))
    )
  
  pvLB_sarima_co2[i-1] =
    
    1 -
    
    pchisq(
      qmLB_sarima_co2[i-1],
      i - 1 - 5
    )
  
}

ggplot() +
  
  geom_line(
    aes(
      x = 1:24,
      y = pvBP_sarima_co2,
      colour = "Box-Pierce"
    ),
    linewidth = 1
  ) +
  
  geom_line(
    aes(
      x = 1:24,
      y = pvLB_sarima_co2,
      colour = "Ljung-Box"
    ),
    linewidth = 1
  ) +
  
  geom_hline(
    yintercept = 0.05,
    linetype = "dashed",
    colour = "red"
  ) +
  
  theme_bw()


library(fpp3)

library(dplyr)



#################################################

### MODELO FINAL - ETS

#################################################

fit_final_ets_co2 =
  
  dados_co2_tsibble %>%
  
  model(
    
    ETS(
      value
    )
  )



#################################################
### PREVISOES FUTURAS - ETS
#################################################

fc_final_ets_co2 =
  
  fit_final_ets_co2 %>%
  
  forecast(
    h = "3 years"
  )



#################################################
### VALORES AJUSTADOS
#################################################

ajuste_ets_co2 =
  
  fitted(
    fit_final_ets_co2
  )



#################################################
### UNINDO AJUSTE + PREVISAO
#################################################

linha_modelo_ets_co2 =
  
  bind_rows(
    
    ajuste_ets_co2 %>%
      
      as_tibble() %>%
      
      dplyr::select(
        
        index,
        
        valor = .fitted
      ),
    
    
    fc_final_ets_co2 %>%
      
      as_tibble() %>%
      
      dplyr::select(
        
        index,
        
        valor = .mean
      )
  )



#################################################
### GRAFICO FINAL
#################################################

autoplot(
  
  dados_co2_tsibble,
  
  value
) +
  
  geom_line(
    
    data = linha_modelo_ets_co2,
    
    aes(
      
      x = index,
      
      y = valor
    ),
    
    colour = "red",
    
    linetype = "dashed",
    
    linewidth = 0.8
  ) +
  
  labs(
    
    x = "",
    
    y = "Concentração de CO2",
    
    title = ""
  ) +
  
  theme_bw()



#verificando qual estrutura do ETS escolhida
report(fit_final_ets_co2)


library(fpp3)

library(dplyr)

#======================================

#MODELO FINAL SNAIVE

#======================================

fit_final_snaive_co2 =
  
  dados_co2_tsibble %>%
  
  model(
    SNAIVE(
      value ~ lag(12)
    )
  )

fc_final_snaive_co2 =
  
  fit_final_snaive_co2 %>%
  
  forecast(
    h = "3 years"
  )

ajuste_snaive_co2 =
  
  fitted(
    fit_final_snaive_co2
  )

linha_modelo_snaive_co2 =
  
  bind_rows(
    
    ajuste_snaive_co2 %>%
      as_tibble() %>%
      dplyr::select(
        index,
        valor = .fitted
      ),
    
    fc_final_snaive_co2 %>%
      as_tibble() %>%
      dplyr::select(
        index,
        valor = .mean
      )
    
  )

autoplot(
  dados_co2_tsibble,
  value
) +
  
  geom_line(
    data = linha_modelo_snaive_co2,
    aes(
      x = index,
      y = valor
    ),
    colour = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  labs(
    
    x = "",
    
    y = "Concentração de CO2",
    
    title = ""
  ) +
  
  theme_bw()

library(fpp3)

library(dplyr)


#################################################
### MODELO FINAL - SARIMA
#################################################

fit_final_sarima_co2 =
  
  dados_co2_tsibble %>%
  
  model(
    
    ARIMA(
      
      value ~ pdq(1,1,1) +
        
        PDQ(1,1,2)
    )
  )



#################################################
### PREVISOES FUTURAS - SARIMA
#################################################

fc_final_sarima_co2 =
  
  fit_final_sarima_co2 %>%
  
  forecast(
    h = "3 years"
  )


#################################################
### VALORES AJUSTADOS
#################################################

ajuste_sarima_co2 =
  
  fitted(
    fit_final_sarima_co2
  )


#################################################
### UNINDO AJUSTE + PREVISAO
#################################################

linha_modelo_sarima_co2 =
  
  bind_rows(
    
    ajuste_sarima_co2 %>%
      
      as_tibble() %>%
      
      dplyr::select(
        
        index,
        
        valor = .fitted
      ),
    
    
    fc_final_sarima_co2 %>%
      
      as_tibble() %>%
      
      dplyr::select(
        
        index,
        
        valor = .mean
      )
  )


#################################################
### GRAFICO FINAL
#################################################

autoplot(
  
  dados_co2_tsibble,
  
  value
) +
  
  geom_line(
    
    data = linha_modelo_sarima_co2,
    
    aes(
      
      x = index,
      
      y = valor
    ),
    
    colour = "red",
    
    linetype = "dashed",
    
    linewidth = 0.8
  ) +
  
  labs(
    
    x = "",
    
    y = "Concentração de CO2",
    
    title = ""
  ) +
  
  theme_bw()



# visualizando modelo ajustado
report(
  fit_final_sarima_co2)
