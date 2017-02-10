# Server ------------------------------------------------------------------

require(shiny)
require(tidyverse)
require(readxl)
require(stringr)
require(FinCal)
require(scales)
require(purrr)

Demanda <- read_excel("Datos/Intermodal/Ingresos.xlsx", sheet = "Demanda")
Tarifas <- read_excel("Datos/Intermodal/Ingresos.xlsx", sheet = "Tarifas")
Costos <- read_excel("Datos/Intermodal/Costos e Inversiones.xlsx", sheet = "Costos")
Inversiones <- read_excel("Datos/Intermodal/Costos e Inversiones.xlsx", sheet = "Inversiones")
Ingresos.poliducto <- read_excel("Datos/Intermodal/Ingresos.xlsx", sheet = "Ingresos")



modelo.base <- function(n                 = 30,
                        n.concesion.multi = 50,
                        n.concesion.poli  = 30,
                        r                 = 0.05, ## Tasa financiamiento
                        r.ice             = 0.0777, ## Tasa de valuación canones
                        tasa.descuento    = 0.08,  # Tasa de descuento manual
                        pct.financiado    = 0.6,
                        tipo.demanda      = "min",
                        isr               = 0.07,
                        isr2              = 0.25,
                        horizonte         = 2062,
                        regimen.fiscal    = TRUE, ## en donde FALSE indica regimen sobre ingresos
                        split.puertos     = 0.5,
                        pct.royalty       = 0.05,
                        pct.gastos.comercializacion = 0.025,
                        pct.pago.anticipado = 0.15,
                        pg                = 5,
                        tasa.retorno.manual = FALSE,
                        Demanda = Demanda,
                        Tarifas = Tarifas,
                        Costos  = Costos,
                        Inversiones = Inversiones,
                        Ingresos.poliducto = Ingresos.poliducto) {
  
  
  # Demanda -----------------------------------------------------------------
  
  Demanda <-   Demanda %>%
    gather(Año,TEUS,-Escenario) %>%
    replace_na(demanda,replace = list(TEUS = 0)) %>%
    spread(Escenario,TEUS)
  
  names(Demanda)[2] <- "Ramp.Up" # Utilizo un punto en el nombre de la columna para hacer mas facil la referencia a esta columna
  
  
  # Tarifas -----------------------------------------------------------------
  
  Tarifas <- Tarifas %>%
    gather(Año,Tarifa.dols, -Sistema,-Elemento) %>%
    filter(!is.na(Tarifa.dols))
  
  
  
  # Ingresos ----------------------------------------------------------------
  # En el caso del ferrocarril, se asume que el 100% de los TEUS disponibles seran movilizados
  Tarifas$pct.mercado <- ifelse(Tarifas$Elemento != "Ferrocarril",split.puertos,1)
  Tarifas$pct.mercado[Tarifas$Elemento == "San Jorge"] <- 1 - split.puertos
  
  
  # Tabla principal para modelar los ingresos
  Ingresos <- left_join(Tarifas, Demanda) %>%
    mutate(Año = as.factor(Año))
  
  
  # Seleccion de tipo de demanda para modelar los ingresos del sistema y elementos multimodales
  
  if (tipo.demanda == "min") {
    Ingresos <- Ingresos %>%
      mutate( Mercado = round(pct.mercado*Ramp.Up*MIN,0),
              Mercado = if_else(Elemento == "Ferrocarril", 
                                lag(Mercado,1) + lag(Mercado,2),
                                Mercado),
              Ingresos.Brutos = if_else(Elemento != "Ferrocarril", 
                                        Tarifa.dols*Mercado*2, 
                                        Tarifa.dols*Mercado),
              Royalty = Ingresos.Brutos*pct.royalty,
              IVAxPagar = Ingresos.Brutos*0.12) 
    
  }else{
    Ingresos <- Ingresos %>%
      mutate( Mercado = round(pct.mercado*Ramp.Up*MAX,0),
              Mercado = if_else(Elemento == "Ferrocarril", 
                                lag(Mercado,1) + lag(Mercado,2),
                                Mercado),
              Ingresos.Brutos = if_else(Elemento != "Ferrocarril",
                                        Tarifa.dols*Mercado*2,
                                        Tarifa.dols*Mercado),
              Royalty = Ingresos.Brutos*pct.royalty,
              IVAxPagar = Ingresos.Brutos*0.12) 
    
  }
  
  Ingresos <- Ingresos %>%
    mutate(Elemento = as.factor(Elemento),
           Sistema = as.factor(Sistema)) %>%
    select(Sistema, Elemento, Año, Ingresos.Brutos, Royalty, IVAxPagar)
  
  
  Ingresos.poliducto <- Ingresos.poliducto %>%
    gather(Año, Ingresos.Brutos, -Sistema,-Elemento ) %>%
    mutate(Año = as.factor(Año),
           Sistema = as.factor(Sistema),
           Elemento = as.factor(Elemento),
           Royalty = Ingresos.Brutos*pct.royalty,
           IVAxPagar = Ingresos.Brutos*0.12) 
  
  Ingresos <- Ingresos %>% 
    bind_rows(Ingresos.poliducto) %>%
    mutate(Año = as.factor(Año),
           Sistema = as.factor(Sistema),
           Elemento = as.factor(Elemento))
  
  
  segmentar_ingreso <- function(df = Ingresos, type = "Sistema"){
    
    switch(type,
           Sistema = Ingresos %>%
             split(Ingresos$Sistema) %>%
             map(select, -Elemento, -Sistema) %>%
             map(group_by, Año) %>%
             map(summarise_all, sum) %>% 
             map(mutate, ISR = Ingresos.Brutos*isr),
           Elemento = Ingresos %>%
             split(Ingresos$Elemento) %>%
             map(select, -Elemento, -Sistema) %>%
             map(group_by, Año) %>%
             map(summarise_all, sum) %>% 
             map(mutate, ISR = Ingresos.Brutos*isr))
  }
  
  ingresos.sistema11 <- segmentar_ingreso(type = "Sistema")
  ingresos.elemento11 <- segmentar_ingreso(type = "Elemento")
  
  primer.año <- Ingresos$Año %>% as.character( ) %>% as.numeric() %>% min()
  
  # Inversiones y canones de infraestructura y concesión -------------------------------------------------------------
  
  
  ## Funcion que sirve para obtener los data frame para los distintos escenarios de negociación
  segmentar_inversion <- function(df = Inversiones, type) {
    require(dplyr)
    require(purrr)
    
    switch(type,
           SF = df %>% 
             mutate(Sistema = as.factor(Sistema)) %>%
             split(df$Sistema) %>%
             map(gather, Año, Valor, -c(Sistema:Fase)) %>%
             map(mutate, Año = as.factor(Año)) %>%
             map(group_by, Año) %>%
             map(summarise, Inversion = sum(Valor)) %>% 
             map(mutate, IVAxCobrar = Inversion*0.12),
           ST = df %>% 
             mutate(Sistema = as.factor(Sistema)) %>%
             split(df$Sistema) %>%
             map(gather, Año, Valor, -c(Sistema:Fase)) %>%
             map(mutate, Año = as.factor(Año)) %>%
             map(group_by, Año, Componente) %>%
             map(summarise, Inversion = sum(Valor)) %>% 
             map(mutate, IVAxCobrar = Inversion*0.12),
           EF = df %>% 
             mutate(Elemento = as.factor(Elemento)) %>%
             split(df$Elemento) %>%
             map(gather, Año, Valor, -c(Sistema:Fase)) %>%
             map(mutate, Año = as.factor(Año)) %>%
             map(group_by, Año) %>%
             map(summarise, Inversion = sum(Valor)) %>% 
             map(mutate, IVAxCobrar = Inversion*0.12),
           ET = df %>% 
             mutate(Elemento = as.factor(Elemento)) %>%
             split(df$Elemento) %>%
             map(gather, Año, Valor, -c(Sistema:Fase)) %>%
             map(mutate, Año = as.factor(Año)) %>%
             map(group_by, Año, Componente) %>%
             map(summarise, Inversion = sum(Valor)) %>% 
             map(mutate, IVAxCobrar = Inversion*0.12))
    
  }
  inversiones.sistema11 <- segmentar_inversion(Inversiones, type = "SF")
  inversiones.sistema10 <- segmentar_inversion(Inversiones, type = "ST")
  inversiones.elemento11 <- segmentar_inversion(Inversiones, type = "EF")
  inversiones.elemento10 <- segmentar_inversion(Inversiones, type = "ET")
  
  
  ## Funcion que sirve para calcular los anticipos y las anualidades de cada canon
  
  canon <- function(valor = 5500000,
                    tasa.descuento.ice = 0.04,
                    n.canon = 30,
                    pct.pago.anticipado = 0.1,
                    pg = 0){
    
    
    pago.anticipado <- -pmt(r = tasa.descuento.ice,
                            n = n.canon - pg,
                            pv = valor,
                            fv = 0)*n.canon*pct.pago.anticipado
    
    valor <- valor*(1 - pct.pago.anticipado)
    pago <- -pmt(r = tasa.descuento.ice,
                 n = n.canon - pg,
                 pv = valor,
                 fv = 0)
    res <- data.frame(pago = pago, pago.anticipado = pago.anticipado)
    res
  }
  
  # Costos ------------------------------------------------------------------
  
  
  segmentar_costos <- function(df = Costos, type) {
    require(dplyr)
    require(purrr)
    
    switch(type,
           SF = df %>% 
             mutate(Sistema = as.factor(Sistema)) %>%
             split(df$Sistema) %>%
             map(gather, Año, Valor, -c(Sistema:Componente)) %>%
             map(mutate, Año = as.factor(Año)) %>%
             map(group_by, Año) %>%
             map(summarise, Costos = sum(Valor)) %>% 
             map(mutate, IVAxCobrar = Costos*0.12),
           ST = df %>% 
             mutate(Sistema = as.factor(Sistema)) %>%
             split(df$Sistema) %>%
             map(gather, Año, Valor, -c(Sistema:Componente)) %>%
             map(mutate, Año = as.factor(Año)) %>%
             map(group_by, Año, Componente) %>%
             map(summarise, Costos = sum(Valor)) %>% 
             map(mutate, IVAxCobrar = Costos*0.12),
           EF = df %>% 
             mutate(Elemento = as.factor(Elemento)) %>%
             split(df$Elemento) %>%
             map(gather, Año, Valor, -c(Sistema:Componente)) %>%
             map(mutate, Año = as.factor(Año)) %>%
             map(group_by, Año) %>%
             map(summarise, Costos = sum(Valor)) %>% 
             map(mutate, IVAxCobrar = Costos*0.12),
           ET = df %>% 
             mutate(Elemento = as.factor(Elemento)) %>%
             split(df$Elemento) %>%
             map(gather, Año, Valor, -c(Sistema:Componente)) %>%
             map(mutate, Año = as.factor(Año)) %>%
             map(group_by, Año, Componente) %>%
             map(summarise, Costos = sum(Valor)) %>% 
             map(mutate, IVAxCobrar = Costos*0.12))
    
  }
  
  costos.sistema11 <- segmentar_costos(type = "SF")
  costos.sistema10 <- segmentar_costos(type = "ST")
  costos.elemento11 <- segmentar_costos(type = "EF")
  costos.elemento10 <- segmentar_costos(type = "ET")
  
  
  # Financiamiento ----------------------------------------------------------
  
  
  amortizacion <- function(df,pct.financiado = 1, n = 30, r = 0.07 ){
    require(tidyverse)
    
    n.inversiones <- df %>% filter(Inversion != 0) %>% nrow()
    
    primer.ingreso <- filter(df, Ingresos.Brutos != 0)
    primer.ingreso <- primer.ingreso$Año %>%  as.character() %>% as.numeric() %>% min()
    
    df <- df %>% filter(Inversion != 0)
    inversion.input <- df[1,]
    inversion.input$Inversion <- inversion.input$Inversion*pct.financiado
    año.inversion <- inversion.input$Año %>% as.character() %>% as.numeric()
    
    ### Definicion periodo de gracia
    if (año.inversion < primer.ingreso ) {
      pg = primer.ingreso - año.inversion
    }else{
      pg = 0 
    }
    
    # creacion de escenario inversión -----------------------------------------
    ## al momento de crear el escenario sumo n+pg por el periodo de gracia de cada pago
    escenario <- data_frame(Año = numeric(n + pg), 
                            Saldo = numeric(n + pg),
                            Intereses = numeric(n + pg),
                            Amortizacion = numeric(n + pg))
    escenario$Año <- seq_len(n + pg) + año.inversion - 1 # Llenado de los años correspondientes al escenario
    escenario$Amortizacion <- inversion.input$Inversion/n # Calculo de los pagos de amortización anuales
    
    
    if (pg == 0) {
      
      escenario <- escenario %>% 
        mutate(Saldo = inversion.input$Inversion - cumsum(escenario$Amortizacion),
               Intereses = (Saldo + Amortizacion)*r)
      
    }else {
      escenario$Amortizacion[1:pg] <- 0 # Set de periodo de gracia
      escenario <- escenario %>% 
        mutate(Saldo = inversion.input$Inversion - cumsum(escenario$Amortizacion),
               Intereses = (Saldo + Amortizacion)*r)
    }
    
    
    if(exists("Componente",df)) {
      escenario  <- escenario %>% mutate(Pago = Intereses + Amortizacion)
      escenario$Componente <- inversion.input$Componente
    } else {
      escenario  <- escenario %>% mutate(Pago = Intereses + Amortizacion)
    }
    
    for (i in 2:n.inversiones) {
      inversion.input <- df[i,]
      inversion.input$Inversion <- inversion.input$Inversion*pct.financiado
      año.inversion <- inversion.input$Año %>% as.character() %>% as.numeric()
      
      ### Definicion periodo de gracia
      if (año.inversion < primer.ingreso ) {
        pg = primer.ingreso - año.inversion
      }else{
        pg = 0 
      }
      
      # creacion de escenario inversión -----------------------------------------
      ## al momento de crear el escenario sumo n+pg por el periodo de gracia de cada pago
      tmp <- data_frame(Año = numeric(n + pg), 
                        Saldo = numeric(n + pg),
                        Intereses = numeric(n + pg),
                        Amortizacion = numeric(n + pg))
      tmp$Año <- seq_len(n + pg) + año.inversion - 1 # Llenado de los años correspondientes al escenario
      tmp$Amortizacion <- inversion.input$Inversion/n # Calculo de los pagos de amortización anuales
      
      
      if (pg == 0) {
        
        tmp <- tmp %>% 
          mutate(Saldo = inversion.input$Inversion - cumsum(tmp$Amortizacion),
                 Intereses = (Saldo + Amortizacion)*r)
        
      }else {
        tmp$Amortizacion[1:pg] <- 0 # Set de periodo de gracia
        tmp <- tmp %>% 
          mutate(Saldo = inversion.input$Inversion - cumsum(tmp$Amortizacion),
                 Intereses = (Saldo + Amortizacion)*r)
      }
      
      if (exists("Componente",df)) {
        tmp  <- tmp %>% mutate(Pago = Intereses + Amortizacion)
        tmp$Componente <- inversion.input$Componente
      } else {
        tmp  <- tmp %>% mutate(Pago = Intereses + Amortizacion)
      }
      
      escenario <- rbind(escenario, tmp)
    }
    
    if (exists("Componente",df)) {
      escenario <- escenario %>% 
        group_by(Año, Componente) %>% 
        summarise_all(sum)
      escenario 
    }else{
      escenario <- escenario %>% 
        group_by(Año) %>% 
        summarise_all(sum)
      escenario$Año <- as.factor(escenario$Año)
      escenario
    }
    escenario$Año <- as.factor(escenario$Año)
    escenario
  }
  
  financiamiento.sistema11 <- inversiones.sistema11 %>% 
    map2(ingresos.sistema11, left_join) %>% 
    map(amortizacion,pct.financiado = pct.financiado, n = n, r = r)
  
  financiamiento.elemento11 <- inversiones.elemento11 %>%
    map2(ingresos.elemento11, left_join) %>% 
    map(amortizacion,pct.financiado = pct.financiado, n = n, r = r)
  
  # IVA e ISR ---------------------------------------------------------------------
  
  Impuestos.sistema11 <- map2(ingresos.sistema11,inversiones.sistema11,left_join) %>%
    map2(costos.sistema11, left_join, by = "Año") %>%
    map(mutate, IVAxCobrar = IVAxCobrar.x + IVAxCobrar.y) %>%
    map(select, -Inversion, -Royalty, -Ingresos.Brutos, -IVAxCobrar.x, -IVAxCobrar.y, -Costos ) %>% 
    map(mutate, 
        IVA.aux1 = cumsum(IVAxCobrar - IVAxPagar),
        IVA.aux2 = IVAxCobrar - IVAxPagar,
        IVA.Neto = if_else(lag(IVA.aux1) < 0 ,IVA.aux2, IVA.aux1)) 
  
  
  impuestos.elemento11 <- map2(ingresos.elemento11,inversiones.elemento11,left_join) %>%
    map2(costos.elemento11, left_join, by = "Año") %>%
    map(mutate, IVAxCobrar = IVAxCobrar.x + IVAxCobrar.y) %>%
    map(select, -Inversion, -Royalty, -Ingresos.Brutos, -IVAxCobrar.x, -IVAxCobrar.y, -Costos ) %>% 
    map(mutate, 
        IVA.aux1 = cumsum(IVAxCobrar - IVAxPagar),
        IVA.aux2 = IVAxCobrar - IVAxPagar,
        IVA.Neto = if_else(lag(IVA.aux1) < 0 ,IVA.aux2, IVA.aux1)) %>% 
    map(select, -contains("aux"))
  
  
  # Valorizacion ------------------------------------------------------------
  
  if (tasa.retorno.manual == TRUE) {
    expected.return <- tasa.descuento
  } else{
    require(rvest)
    url <- "http://pages.stern.nyu.edu/~adamodar/New_Home_Page/datafile/totalbeta.html"
    url2 <- "http://pages.stern.nyu.edu/~adamodar/New_Home_Page/datafile/histretSP.html"
    
    safe_read <- safely(read_html)
    prueba <- safe_read(url)
    
    
    if (is_null(prueba$result)) {
      betas <- readRDS("Datos/Intermodal/betas2017-01-25.rds")
      tasas <- readRDS("Datos/Intermodal/tasas2017-01-25.rds")
      
    } else{
      betas <- read_html(url) %>% 
        html_nodes(xpath = "/html/body/table") %>% 
        html_table()
      
      fecha.consulta <- Sys.Date()
      flname <- paste0("Datos/Intermodal/betas",fecha.consulta,".rds")
      saveRDS(object = betas ,file = flname)
      
      tasas <- read_html(url2) %>% 
        html_table()
      
      flname <- paste0("Datos/Intermodal/tasas",fecha.consulta,".rds")
      saveRDS(object = tasas ,file = flname)
    }
    
    
    betas <- as.data.frame(betas)
    names(betas) <- betas[1,]
    betas <- betas[c(91,92),c(1,3)]
    beta <- betas$`Average Unlevered Beta` %>% 
      as.numeric() %>% 
      mean()
    
    tasas <- as.data.frame(tasas)
    names(tasas) <- tasas[2,]
    
    tasas  <- tasas[3:nrow(tasas),] 
    tasas.promedio <- tail(tasas,9) %>% head(4)
    nombres <- c("periodo","SP500","3month.bill","10year.bond")
    tasas <- tasas.promedio[1:4] 
    names(tasas) <- nombres
    tasas <- tasas[2:nrow(tasas),]
    
    rm <- tasas[3,2]
    rf <- tasas[3,4]
    rm <- rm %>% str_replace("%","")
    rm <- as.numeric(rm)/100
    
    rf <- rf %>% str_replace("%","")
    rf <- as.numeric(rf)/100
    expected.return <- rf + beta*(rm - rf)
    financial.data <- data.frame(expected.return, rf, rm,beta)
    financial.data
    expected.return <- financial.data[["expected.return"]]
  }
  
 
  r.ice <- expected.return
  ### Creación de data frames que sirven de input para el calculo del VP base del 
  ### proyecto y VP del financiamiento en las modalidades: 
  ### Sistema completo, portipo de sistema y por elemento del sistema
  df.sistema11 <- ingresos.sistema11 %>%
    map2(costos.sistema11, full_join) %>%
    map(select, -contains("IVA")) %>%
    map2(inversiones.sistema11, full_join) %>%
    map(select, -contains("IVA")) %>%
    map2(financiamiento.sistema11, full_join) %>%
    map(select, -Saldo, -Amortizacion) %>%
    map2(Impuestos.sistema11, full_join) %>%
    map(select, -contains("IVAx")) %>%
    map(replace_na, replace = list(Ingresos.Brutos = 0,
                                   Royalty = 0,
                                   ISR = 0,
                                   Costos = 0,
                                   Inversion = 0,
                                   Pago = 0,
                                   IVA.Neto = 0,
                                   Intereses = 0))
  
  df.sistema.completo11 <- df.sistema11 %>% 
    bind_rows() %>% 
    group_by(Año) %>% 
    summarise_each("sum") 
  
  df.elemento11 <- ingresos.elemento11 %>%
    map2(costos.elemento11, full_join) %>%
    map(select, -contains("IVA")) %>%
    map2(inversiones.elemento11, full_join) %>%
    map(select, -contains("IVA")) %>%
    map2(financiamiento.elemento11, full_join) %>%
    map(select, -Saldo, -Amortizacion) %>%
    map2(impuestos.elemento11, full_join) %>%
    map(select, -contains("IVAx")) %>%
    map(replace_na, replace = list(Ingresos.Brutos = 0,
                                   Royalty = 0,
                                   ISR = 0,
                                   Costos = 0,
                                   Inversion = 0,
                                   Pago = 0,
                                   IVA.Neto = 0,
                                   Intereses = 0))
  
  ### Funciones que calculan vpn base y de financiamiento respectivamente
  calcular_fen <- function(df,r,horizonte = 2062, res = 1, regimen.fiscal = FALSE){
    
    
    df$regimen.fiscal <- regimen.fiscal
    df <-  df %>% 
      mutate(IVA.Neto = if_else(IVA.Neto > 0,0,IVA.Neto),
             ISR2 = if_else(regimen.fiscal == FALSE, ISR, (Ingresos.Brutos - Costos)*isr2),
             FEN      =  Ingresos.Brutos
             - ISR2
             - Costos
             - Inversion
             # - Pago
             + IVA.Neto,
             Año = as.numeric(Año)) %>% 
      filter(Año <= horizonte) %>% 
      arrange(Año)
    
    irr_safe <- safely(irr)
    flujo.base <- sum(df$FEN)
    vp.base <- npv(r = r, cf = df$FEN)
    tir <- ifelse(is.null(irr_safe(df$FEN)$result), NA, irr_safe(df$FEN)$result)
    
    if (res == 1) {
      res <- data.frame(flujo.base, vp.base, tir)
    } else {
      df
    }
    
  }
  calcular_vpnf <- function(df,r,horizonte = 2062, res = 1, regimen.fiscal = FALSE,
                            pct.financiado = pct.financiado){
    
    df$regimen.fiscal <- regimen.fiscal
    
    
    df <-  df %>% 
      mutate(FEN = Inversion*pct.financiado - Pago + Intereses*isr2,
             Año = as.numeric(Año)) %>% 
      filter(Año <= horizonte) %>% 
      arrange(Año)
    
    flujo.fin <- sum(df$FEN)
    vp.fin <- npv(r = r, cf = df$FEN)
    
    if (res == 1) {
      res <- data.frame(flujo.fin, vp.fin)
    } else {
      df
    }
  }
  
  
  ### Parametros del modelo
  n.multi <- primer.año + n.concesion.multi
  n.poli <- primer.año + n.concesion.poli
  r.input <- list(expected.return)
  horizonte.input <- list(n.multi)
  res.input <- list(1)
  regimen.fiscal.input <-  list(regimen.fiscal)
  
  
  params.completo <- list(df = list(df.sistema.completo11), r = r.input, horizonte = horizonte.input, res = res.input, regimen.fiscal = regimen.fiscal.input)
  
  data <- df.sistema11
  horizonte.input <- list(n.poli, n.multi)
  params.sistema <- list(df = data, r = r.input, horizonte = horizonte.input, res = res.input, regimen.fiscal = regimen.fiscal.input)
  
  data <- df.elemento11
  horizonte.input <- list(n.multi, n.poli, n.multi, n.multi)
  params.elemento <- list(df = data, r = r.input, horizonte = horizonte.input, res = res.input, regimen.fiscal = regimen.fiscal.input)
  
  
  ### Calculo de vp base para cada modalidad
  
  valor.sistema.completo <- pmap(params.completo, calcular_fen)
  valor.sistema11 <- pmap(params.sistema, calcular_fen)
  valor.elemento11 <- pmap(params.elemento, calcular_fen)
  
  valor.sistema11 <- bind_rows(valor.sistema11, .id = "Sistema")
  valor.elemento11 <- bind_rows(valor.elemento11, .id = "Elemento")
  
  ### Parametros para financiamiento 
  
  n.multi <- primer.año + n.concesion.multi
  n.poli <- primer.año + n.concesion.poli
  r.input <- list(r)
  horizonte.input <- list(n.multi)
  res.input <- list(1)
  regimen.fiscal.input <-  list(regimen.fiscal)
  
  params.completo.fin <- list(df = list(df.sistema.completo11), r = r.input, horizonte = horizonte.input, res = res.input, regimen.fiscal = regimen.fiscal.input, pct.financiado = list(pct.financiado))
  
  data <- df.sistema11
  horizonte.input <- list(n.poli, n.multi)
  params.sistema.fin <- list(df = data, r = r.input, horizonte = horizonte.input, res = res.input, regimen.fiscal = regimen.fiscal.input, pct.financiado = list(pct.financiado))
  
  data <- df.elemento11
  horizonte.input <- list(n.multi, n.poli, n.multi, n.multi)
  params.elemento.fin <- list(df = data, r = r.input, horizonte = horizonte.input, res = res.input, regimen.fiscal = regimen.fiscal.input, pct.financiado = list(pct.financiado))
  
  
  ### Calculo de vp del financiamiento para cada modalidad
  
  
  
  valor.sistema.completo.fin <- pmap(params.completo.fin, calcular_vpnf)
  valor.sistema11.fin <- pmap(params.sistema.fin, calcular_vpnf)
  valor.elemento11.fin <- pmap(params.elemento.fin, calcular_vpnf)
  valor.sistema11.fin <- bind_rows(valor.sistema11.fin, .id = "Sistema")
  valor.elemento11.fin <- bind_rows(valor.elemento11.fin, .id = "Elemento")
  
  ### DF para cada variedad
  valor.completo <-   bind_cols(valor.sistema.completo,
                                valor.sistema.completo.fin) %>% 
    mutate(vp.total = vp.base + vp.fin)
  
  valor.sistema <- left_join(valor.sistema11, valor.sistema11.fin) %>% 
    mutate(vp.total = vp.base + vp.fin)
  
  valor.elemento <- left_join(valor.elemento11, valor.elemento11.fin) %>% 
    mutate(vp.total = vp.base + vp.fin)
  

  
  resultado <- list(Completo = valor.completo, 
                    Sistema  = valor.sistema, 
                    Elemento = valor.elemento)
  
  resultado <- resultado %>% 
    map(select, -flujo.base, -flujo.fin) %>% 
    map(mutate, 
        vp.base = formattable::currency(vp.base, digits = 0),
        vp.fin  = formattable::currency(vp.fin, digits = 0),
        vp.total  = formattable::currency(vp.total, digits = 0),
        tir = formattable::percent(tir))
  
  

  # Calculo de canones Concesión e Infraestructura-------------------------------------------------
  
  ### Calculo valor de concesion
  
  concesion.completo <- valor.completo$vp.base %>%   
    canon(tasa.descuento.ice = r.ice, 
          n.canon = n.concesion.multi, 
          pg = pg, 
          pct.pago.anticipado = pct.pago.anticipado)
  
  concesion.completo$n <- n.concesion.multi  
  
  concesion.sistema  <-  valor.sistema$vp.base %>% 
    canon(tasa.descuento.ice = r.ice, 
          n.canon = c(n.concesion.poli ,n.concesion.multi),
          pg = pg,
          pct.pago.anticipado = pct.pago.anticipado)
  
  concesion.sistema$Sistema <- valor.sistema$Sistema  
  concesion.sistema <- concesion.sistema %>% 
    mutate(n = if_else(Sistema == "Energia", 
                       n.concesion.poli, 
                       n.concesion.multi))
  
  concesion.elemento <-  valor.elemento$vp.base %>% 
    canon(tasa.descuento.ice = r.ice, 
          n.canon = c(n.concesion.multi,n.concesion.poli,n.concesion.multi,n.concesion.multi),
          pg = pg,
          pct.pago.anticipado = pct.pago.anticipado)
  
  concesion.elemento$Elemento <- valor.elemento$Elemento
  concesion.elemento <- concesion.elemento %>% 
    mutate(n = if_else(Elemento == "Poliductos", 
                       n.concesion.poli, 
                       n.concesion.multi))
  
  
  ### Calculo uso infraestructuras
  
  canon.infra.completo <- inversiones.sistema10 %>% 
    bind_rows(.id = "Sistema") %>% 
    filter(Componente == "Infraestructura") %>% 
    summarize(Inversion = sum(Inversion)) %>% 
    .$Inversion %>% 
    npv(r = r.ice) %>% 
    canon(tasa.descuento.ice = r.ice,
          n.canon = n.concesion.multi,
          pg = pg,
          pct.pago.anticipado = pct.pago.anticipado)
  
  canon.infra.completo$n <- n.concesion.multi
  
  canon.infra.sistema <- inversiones.sistema10  %>% 
    map(filter, Componente == "Infraestructura") %>% 
    map(ungroup) %>% 
    map(select, Inversion) %>% 
    map(function(df) npv(r = r.ice, cf = df$Inversion)) %>% 
    bind_rows() %>% 
    gather(Sistema, Canon.ICE) %>% 
    .$Canon.ICE %>% 
    canon(tasa.descuento.ice = r.ice, 
          n.canon = c(n.concesion.poli,n.concesion.multi),
          pct.pago.anticipado = pct.pago.anticipado,
          pg = pg)
  
  canon.infra.sistema$Sistema <- valor.sistema$Sistema  
  canon.infra.sistema <- canon.infra.sistema %>% 
    mutate(n = if_else(Sistema == "Energia", n.concesion.poli, n.concesion.multi))
  
  
  canon.infra.elemento <- inversiones.elemento10  %>% 
    map(filter, Componente == "Infraestructura") %>% 
    map(ungroup) %>% 
    map(select, Inversion) %>% 
    map(function(df) npv(r = r.ice, cf = df$Inversion)) %>% 
    bind_rows() %>% 
    gather(Elemento, Canon.ICE) %>% 
    .$Canon.ICE %>% 
    canon(tasa.descuento.ice = r.ice, 
          n.canon = c(n.concesion.multi,
                      n.concesion.poli,
                      n.concesion.multi,
                      n.concesion.multi), 
          pct.pago.anticipado = pct.pago.anticipado, 
          pg = pg)
  
  canon.infra.elemento$Elemento <- valor.elemento$Elemento
  
  canon.infra.elemento <- canon.infra.elemento %>% 
    mutate(n = if_else(Elemento == "Poliductos", n.concesion.poli, n.concesion.multi))
  
  
  
  #### Funcion que crea df "costos.adicionales" que usare para la evaluacion de fen para sistema y un tercero 
  
  calcular.costos.adicionales <- function(df, nombre, pg){
    costos.adicionales <- data.frame(Año = numeric(df$n),
                                     Anticipo = numeric(df$n),
                                     Pago = numeric(df$n))
    costos.adicionales[1,] <- c(2012, df$pago.anticipado, 0)
    costos.adicionales$Año <- seq(1:df$n) + 2012 - 1
    costos.adicionales$Año <- costos.adicionales$Año %>% as.factor()
    if (pg == 0) {
      costos.adicionales$Pago[2:df$n] <- df$pago
    } else{
      costos.adicionales$Pago[2:(2 + pg)] <- 0
      costos.adicionales$Pago[(3 + pg):df$n] <- df$pago
    }
    costos.adicionales <- costos.adicionales %>% 
      mutate(IVAxCobrar = (Anticipo + Pago)*0.12)
    names(costos.adicionales)[2:3] <- paste0(names(costos.adicionales)[2:3],".",nombre)
    costos.adicionales
  }
  
  ### Data Frames costos adicionales para cada nivel
  
  valores.concesion.completo <- calcular.costos.adicionales(concesion.completo, "concesion", pg = pg)
  valores.canon.infr.completo <- calcular.costos.adicionales(canon.infra.completo, "canon.infra", pg = pg)
  
  valores.concesion.sistema <- concesion.sistema %>% 
    split(.$Sistema) %>% 
    map(calcular.costos.adicionales, "concesion", pg = pg)
  
  valores.canon.infr.sistema <- canon.infra.sistema %>% 
    split(.$Sistema) %>% 
    map(calcular.costos.adicionales, "canon.infra", pg = pg)
  
  valores.concesion.elemento <- concesion.elemento %>% 
    split(.$Elemento) %>% 
    map(calcular.costos.adicionales, "concesion", pg = pg)
  
  valores.canon.infr.elemento <- canon.infra.elemento %>% 
    split(.$Elemento) %>% 
    map(calcular.costos.adicionales, "canon.infra", pg = pg)
  
  
  # Separacion costos e inversiones 3ro -------------------------------------
  
  ### Separacion de costos e inversiones de 3ro y propios
  
  #### Sistema
  costos.sistema.3ro <- costos.sistema10 %>% 
    map(filter, Componente != "Infraestructura") %>% 
    map(group_by, Año) %>%
    map(summarise_each, "sum", -Componente)
  
  inversiones.sistema.3ro <- inversiones.sistema10 %>% 
    map(filter, Componente != "Infraestructura") %>% 
    map(group_by, Año) %>%
    map(summarise_each, "sum", -Componente)
  
  costos.sistema.3ro <- costos.sistema.3ro %>% 
    map2(valores.concesion.sistema, left_join, by = "Año") %>% 
    map(mutate, IVAxCobrar = IVAxCobrar.x + IVAxCobrar.y,
        Costos = Costos + Anticipo.concesion + Pago.concesion) %>% 
    map(select, -IVAxCobrar.x, -IVAxCobrar.y, -Anticipo.concesion, -Pago.concesion) %>% 
    map2(valores.canon.infr.sistema, left_join, by = "Año") %>% 
    map(mutate, IVAxCobrar = IVAxCobrar.x + IVAxCobrar.y,
        Costos = Costos + Anticipo.canon.infra + Pago.canon.infra) %>% 
    map(select, -IVAxCobrar.x, -IVAxCobrar.y, -Anticipo.canon.infra, -Pago.canon.infra)
  
  
  #### Elemento
  costos.elemento.3ro <- costos.elemento10 %>% 
    map(filter, Componente != "Infraestructura") %>% 
    map(group_by, Año) %>%
    map(summarise_each, "sum", -Componente)
  
  inversiones.elemento.3ro <- inversiones.elemento10 %>% 
    map(filter, Componente != "Infraestructura") %>% 
    map(group_by, Año) %>%
    map(summarise_each, "sum", -Componente)
  
  costos.elemento.3ro <- costos.elemento.3ro %>% 
    map2(valores.concesion.elemento, left_join, by = "Año") %>% 
    map(mutate, IVAxCobrar = IVAxCobrar.x + IVAxCobrar.y,
        Costos = Costos + Anticipo.concesion + Pago.concesion) %>% 
    map(select, -IVAxCobrar.x, -IVAxCobrar.y, -Anticipo.concesion, -Pago.concesion) %>% 
    map2(valores.canon.infr.elemento, left_join, by = "Año") %>% 
    map(mutate, IVAxCobrar = IVAxCobrar.x + IVAxCobrar.y,
        Costos = Costos + Anticipo.canon.infra + Pago.canon.infra) %>% 
    map(select, -IVAxCobrar.x, -IVAxCobrar.y, -Anticipo.canon.infra, -Pago.canon.infra)
  
  
  #### Calculo de ingresos y costos propios
  
  ingresos.sistema.propios <- map2(ingresos.sistema11, valores.concesion.sistema, full_join, by = "Año") %>% 
    map2(valores.canon.infr.sistema, left_join, by = "Año") %>% 
    map(select, Año, Royalty, contains("Anticipo"), contains("Pago")) %>% 
    map(replace_na, replace = list(Royalty = 0, Anticipo.concesion = 0, Anticipo.canon.infra = 0,
                                   Pago.concesion = 0, Pago.canon.infra = 0)) %>% 
    map(mutate, Ingresos.Brutos = Royalty + Anticipo.concesion + Anticipo.canon.infra +
          Pago.concesion + Pago.canon.infra) %>% 
    map(select, Año, Ingresos.Brutos) %>% 
    map(mutate, IVAxPagar = Ingresos.Brutos*0.12,
        Año = as.numeric(as.character(Año))) %>% 
    map(arrange, Año)
  
  ingresos.elemento.propios <- map2(ingresos.elemento11, valores.concesion.elemento, full_join, by = "Año") %>% 
    map2(valores.canon.infr.elemento, left_join, by = "Año") %>% 
    map(select, Año, Royalty, contains("Anticipo"), contains("Pago")) %>% 
    map(replace_na, replace = list(Royalty = 0, Anticipo.concesion = 0, Anticipo.canon.infra = 0,
                                   Pago.concesion = 0, Pago.canon.infra = 0)) %>% 
    map(mutate, Ingresos.Brutos = Royalty + Anticipo.concesion + Anticipo.canon.infra +
          Pago.concesion + Pago.canon.infra) %>% 
    map(select, Año, Ingresos.Brutos) %>% 
    map(mutate, IVAxPagar = Ingresos.Brutos*0.12,
        Año = as.numeric(as.character(Año))) %>% 
    map(arrange, Año)
  
  
  costos.sistema.propios <- costos.sistema10 %>% 
    map(filter, Componente == "Infraestructura")  %>% 
    map(group_by, Año) %>%
    map(summarise_each, "sum", -Componente) %>% 
    map(ungroup) %>% 
    map(mutate, Año = as.numeric(as.character(Año)))
  
  costos.sistema.propios <- map2(costos.sistema.propios, ingresos.sistema.propios, full_join) %>%
    map(replace_na, list(Costos = 0, IVAxCobrar = 0)) %>% 
    map(mutate, Gastos.comerc = pct.gastos.comercializacion*Ingresos.Brutos,
        Costos = Costos + Gastos.comerc,
        IVAxCobrar = Costos*0.12) %>% 
    map(select, -IVAxPagar, -Gastos.comerc, -Ingresos.Brutos)
  
  inversiones.sistema.propios <- inversiones.sistema10 %>% 
    map(filter, Componente == "Infraestructura") %>% 
    map(group_by, Año) %>%
    map(summarise_each, "sum", -Componente) %>% 
    map(ungroup) %>% 
    map(mutate, Año = as.numeric(as.character(Año)))
  
  costos.elemento.propios <- costos.elemento10 %>% 
    map(filter, Componente == "Infraestructura")  %>% 
    map(group_by, Año) %>%
    map(summarise_each, "sum", -Componente) %>% 
    map(ungroup) %>% 
    map(mutate, Año = as.numeric(as.character(Año)))
  
  costos.elemento.propios <- map2(costos.elemento.propios, ingresos.elemento.propios, full_join) %>% 
    map(replace_na,  list(Costos = 0, IVAxCobrar = 0)) %>% 
    map(mutate, Gastos.comerc = pct.gastos.comercializacion*Ingresos.Brutos,
        Costos = Costos + Gastos.comerc,
        IVAxCobrar = Costos*0.12) %>% 
    map(select, -IVAxPagar, -Gastos.comerc, -Ingresos.Brutos)
  
  inversiones.elemento.propios <- inversiones.elemento10 %>% 
    map(filter, Componente == "Infraestructura") %>% 
    map(group_by, Año) %>%
    map(summarise_each, "sum", -Componente) %>% 
    map(ungroup) %>% 
    map(mutate, Año = as.numeric(as.character(Año)))
  
  
  
  # Financiamiento 3ro y propio ----------------------------------------------
  
  
  financiamiento.sistema.3ro <- inversiones.sistema.3ro %>% 
    map2(ingresos.sistema11, left_join) %>% 
    map(amortizacion, 
        pct.financiado = pct.financiado,
        n = n,
        r = r)
  
  financiamiento.elemento.3ro <- inversiones.elemento.3ro %>% 
    map2(ingresos.elemento11, left_join) %>% 
    map(amortizacion,
        pct.financiado = pct.financiado,
        n = n,
        r = r)
  
  
  financiamiento.sistema.propios <- inversiones.sistema.propios %>% 
    map2(ingresos.sistema.propios, left_join) %>% 
    map(amortizacion,
        pct.financiado = pct.financiado,
        n = n,
        r = r)  %>% 
    map(ungroup) %>% 
    map(mutate, Año = as.numeric(as.character(Año)))
  
  
  financiamiento.elemento.propios <- inversiones.elemento.propios %>% 
    map2(ingresos.elemento.propios, left_join) %>% 
    map(amortizacion,
        pct.financiado = pct.financiado,
        n = n,
        r = r) %>% 
    map(ungroup) %>% 
    map(mutate, Año = as.numeric(as.character(Año)))
  
  
  # IVA 3ro y propios -------------------------------------------------------
  
  impuestos.sistema.3ro <- map2(ingresos.sistema11, inversiones.sistema.3ro, left_join) %>% 
    map2(costos.sistema.3ro, left_join, by = "Año") %>% 
    map(select, -Inversion, -Royalty, -Ingresos.Brutos, -Costos ) %>% 
    map(mutate,
        IVAxCobrar = IVAxCobrar.x + IVAxCobrar.y,
        IVA.aux1 = cumsum(IVAxCobrar - IVAxPagar),
        IVA.aux2 = IVAxCobrar - IVAxPagar,
        IVA.Neto = if_else(lag(IVA.aux1) < 0 ,IVA.aux2, IVA.aux1)) %>% 
    map(select, -IVAxCobrar.x, -IVAxCobrar.y)
  
  
  impuestos.elemento.3ro <- map2(ingresos.elemento11, inversiones.elemento.3ro, left_join) %>% 
    map2(costos.elemento.3ro, left_join, by = "Año") %>% 
    map(select, -Inversion, -Royalty, -Ingresos.Brutos, -Costos ) %>% 
    map(mutate,
        IVAxCobrar = IVAxCobrar.x + IVAxCobrar.y,
        IVA.aux1 = cumsum(IVAxCobrar - IVAxPagar),
        IVA.aux2 = IVAxCobrar - IVAxPagar,
        IVA.Neto = if_else(lag(IVA.aux1) < 0 ,IVA.aux2, IVA.aux1)) %>% 
    map(select, -IVAxCobrar.x, -IVAxCobrar.y)
  
  
  
  impuestos.sistema.propios <- map2(ingresos.sistema.propios, inversiones.sistema.propios, left_join) %>% 
    map2(costos.sistema.propios, left_join, by = "Año") %>% 
    map(select, -Inversion, -Ingresos.Brutos, -Costos ) %>% 
    map(mutate,
        IVAxCobrar = IVAxCobrar.x + IVAxCobrar.y,
        IVA.aux1 = cumsum(IVAxCobrar - IVAxPagar),
        IVA.aux2 = IVAxCobrar - IVAxPagar,
        IVA.Neto = if_else(lag(IVA.aux1) < 0 ,IVA.aux2, IVA.aux1)) %>% 
    map(select, -IVAxCobrar.x, -IVAxCobrar.y)
  
  impuestos.elemento.propios <- map2(ingresos.elemento.propios, inversiones.elemento.propios, left_join) %>% 
    map2(costos.elemento.propios, left_join, by = "Año") %>% 
    map(select, -Inversion, -Ingresos.Brutos, -Costos ) %>% 
    map(mutate,
        IVAxCobrar = IVAxCobrar.x + IVAxCobrar.y,
        IVA.aux1 = cumsum(IVAxCobrar - IVAxPagar),
        IVA.aux2 = IVAxCobrar - IVAxPagar,
        IVA.Neto = if_else(lag(IVA.aux1) < 0 ,IVA.aux2, IVA.aux1)) %>% 
    map(select, -IVAxCobrar.x, -IVAxCobrar.y)
  
  
  
  # Calculo VPN escenario 10  ------------------------------------------------------------
  
  ### Creación de data frames que sirven de input para el calculo del VP base del 
  ### proyecto y VP del financiamiento en las modalidades: 
  ### Sistema completo, portipo de sistema y por elemento del sistema
  df.sistema.3ro <- ingresos.sistema11 %>%
    map2(costos.sistema.3ro, full_join) %>%
    map(select, -contains("IVA")) %>%
    map2(inversiones.sistema.3ro, full_join) %>%
    map(select, -contains("IVA")) %>%
    map2(financiamiento.sistema.3ro, full_join) %>%
    map(select, -Saldo, -Amortizacion) %>%
    map2(impuestos.sistema.3ro, full_join) %>%
    map(select, -contains("IVAx")) %>%
    map(replace_na, replace = list(Ingresos.Brutos = 0,
                                   Royalty = 0,
                                   ISR = 0,
                                   Costos = 0,
                                   Inversion = 0,
                                   Pago = 0,
                                   IVA.Neto = 0,
                                   Intereses = 0))
  
  
  df.sistema.propios <- ingresos.sistema.propios %>%
    map2(costos.sistema.propios, full_join) %>%
    map(select, -contains("IVA")) %>%
    map2(inversiones.sistema.propios, full_join) %>%
    map(select, -contains("IVA")) %>%
    map2(financiamiento.sistema.propios, full_join) %>%
    map(select, -Saldo, -Amortizacion) %>%
    map2(impuestos.sistema.propios, full_join) %>%
    map(select, -contains("IVAx")) %>% 
    map(mutate, ISR = Ingresos.Brutos*isr) %>% 
    map(arrange, Año) %>% 
    map(replace_na, list(Ingresos.Brutos = 0,
                         Costos = 0,
                         Inversion = 0,
                         Intereses = 0,
                         Pago = 0,
                         IVA.Neto = 0))
  
  
  df.elemento.3ro <- ingresos.elemento11 %>%
    map2(costos.elemento.3ro, full_join) %>%
    map(select, -contains("IVA")) %>%
    map2(inversiones.elemento.3ro, full_join) %>%
    map(select, -contains("IVA")) %>%
    map2(financiamiento.elemento.3ro, full_join) %>%
    map(select, -Saldo, -Amortizacion) %>%
    map2(impuestos.elemento.3ro, full_join) %>%
    map(select, -contains("IVAx")) %>%
    map(replace_na, replace = list(Ingresos.Brutos = 0,
                                   Royalty = 0,
                                   ISR = 0,
                                   Costos = 0,
                                   Inversion = 0,
                                   Pago = 0,
                                   IVA.Neto = 0,
                                   Intereses = 0))
  
  df.elemento.propios  <- ingresos.elemento.propios %>%
    map2(costos.elemento.propios, full_join) %>%
    map(select, -contains("IVA")) %>%
    map2(inversiones.elemento.propios, full_join) %>%
    map(select, -contains("IVA")) %>%
    map2(financiamiento.elemento.propios, full_join) %>%
    map(select, -Saldo, -Amortizacion) %>%
    map2(impuestos.elemento.propios, full_join) %>%
    map(select, -contains("IVAx")) %>% 
    map(mutate, ISR = Ingresos.Brutos*isr) %>% 
    map(arrange, Año) %>% 
    map(replace_na, list(Ingresos.Brutos = 0,
                         Costos = 0,
                         Inversion = 0,
                         Intereses = 0,
                         Pago = 0,
                         IVA.Neto = 0))
  
  params.sistema$df <- df.sistema.3ro
  params.elemento$df <- df.elemento.3ro
  valor.sistema.10.3ro <-  pmap(params.sistema, calcular_fen)
  valor.elemento.10.3ro <- pmap(params.elemento, calcular_fen)
  
  params.sistema$df <- df.sistema.propios
  params.elemento$df <- df.elemento.propios
  valor.sistema.10.propio <- pmap(params.sistema, calcular_fen)
  valor.elemento.10.propio <- pmap(params.elemento, calcular_fen)
  
  params.sistema.fin$r <- list(r)
  params.elemento.fin$r <- list(r)
  params.sistema.fin$df <- df.sistema.3ro
  params.elemento.fin$df <- df.elemento.3ro
  
  valor.sistema.fin.3ro <- pmap(params.sistema.fin, calcular_vpnf)
  valor.elemento.fin.3ro <-  pmap(params.elemento.fin, calcular_vpnf)
  
  params.sistema.fin$df <- df.sistema.propios
  params.elemento.fin$df <- df.elemento.propios
  valor.sistema.fin.propio <- pmap(params.sistema.fin, calcular_vpnf)
  valor.elemento.fin.propio <- pmap(params.elemento.fin, calcular_vpnf)
  
  
  
  valor.sistema.10  <- map2(valor.sistema.10.3ro, valor.sistema.fin.3ro, bind_cols) %>% 
    bind_rows(.id = "Sistema")
  
  valor.elemento.10 <- map2(valor.elemento.10.3ro, valor.elemento.fin.3ro, bind_cols) %>% 
    bind_rows(.id = "Elemento")
  
  valor.3ro <- list(valor.sistema.10, valor.elemento.10 )
  
  valor.3ro <- valor.3ro %>% 
    map(select, -flujo.base, -flujo.fin) %>% 
    map(mutate, 
        vp.base = formattable::currency(vp.base, digits = 0),
        vp.fin = formattable::currency(vp.fin, digits = 0),
        vp.total = vp.base + vp.fin,
        tir = formattable::percent(tir))
  
  
  valor.sistema.10.sigsa  <- map2(valor.sistema.10.propio, valor.sistema.fin.propio, bind_cols) %>% 
    bind_rows(.id = "Sistema")
  
  valor.elemento.10.sigsa  <- map2(valor.elemento.10.propio, valor.elemento.fin.propio, bind_cols) %>% 
    bind_rows(.id = "Elemento")
  
  
  valor.SIGSA <- list(valor.sistema.10.sigsa, valor.elemento.10.sigsa)
  
  
  valor.SIGSA <- valor.SIGSA %>% 
    map(select, -flujo.base, -flujo.fin, -tir) %>% 
    map(mutate, 
        vp.base = formattable::currency(vp.base, digits = 0),
        vp.fin = formattable::currency(vp.fin, digits = 0),
        vp.total = vp.base + vp.fin)
  
  lista <- list(resultado, valor.3ro, valor.SIGSA)
  
  
  return(lista)
}


server <- shinyServer( function(input, output) {
  r <- reactive({
    withProgress(message = 'Armando la función',
                 detail = 'Resultados en un momento...', value = 0, {
                   for (i in 1:15) {
                     incProgress(1/5)
                     Sys.sleep(0.2)
                   }
                 })
    
    
    modelo.base(n                                = input$años.financiamiento,
                     n.concesion.multi           = input$cons.multi,
                     n.concesion.poli            = input$cons.poli, 
                     r.ice                       = 0.0777, 
                     r                           = input$tasa.financiamiento,
                     tasa.descuento              = input$tasa.descuento,
                     pct.financiado              = input$pct.financiado,
                     tipo.demanda                = input$tipo.demanda,
                     isr                         = input$isr,
                     isr2                        = input$isr2,
                     horizonte                   = input$horizonte,
                     regimen.fiscal              = input$regimen.fiscal,
                     split.puertos               = 0.5,
                     pct.royalty                 = 0.05,
                     pct.gastos.comercializacion = 0.025,
                     tasa.retorno.manual         = input$tasa.retorno.manual, 
                     pct.pago.anticipado         = 0.15,
                     pg                          = 5,
                     Demanda                     = Demanda,
                     Tarifas                     = Tarifas,
                     Costos                      = Costos,
                     Inversiones                 = Inversiones,
                     Ingresos.poliducto          = Ingresos.poliducto)
  })
output$total <- renderDataTable({

  r()[[1]][[1]] %>% 
    mutate( 
        vp.base = formattable::currency(vp.base, digits = 0),
        vp.fin  = dollar(vp.fin),
        vp.total  = dollar(vp.total),
        tir = formattable::percent(tir)) 
  
  
  })
output$sistema <- renderDataTable({
  withProgress(message = 'Calculando:',
               detail = 'VPN sistema...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/5)
                   Sys.sleep(0.02)
                 }
               })
  r()[[1]][[2]]})  
output$elemento <- renderDataTable({
  withProgress(message = 'Calculando:',
               detail = 'VPN elemento...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/5)
                   Sys.sleep(0.02)
                 }
               })
  r()[[1]][[3]]})
output$tercero.sistema <- renderDataTable({
  withProgress(message = 'Calculando análisis infraestructura',
               detail = 'VPNs 3ro...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/5)
                   Sys.sleep(0.02)
                 }
               })
  r()[[2]][[1]]})
output$tercero.elemento <- renderDataTable({r()[[2]][[2]]})
output$sigsa.sistema <- renderDataTable({
  withProgress(message = 'Calculando análisis infraestructura',
               detail = 'VPNs SIGSA...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/5)
                   Sys.sleep(0.15)
                 }
               })
  
  r()[[3]][[1]]})
output$sigsa.elemento <- renderDataTable({r()[[3]][[2]]})

}
)