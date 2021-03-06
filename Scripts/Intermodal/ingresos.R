source("Scripts/Intermodal/demanda.R")
# Ingresos ----------------------------------------------------------------
# Se asume que la cantidad de TEUS movidos por cada puerto sera exactamente la mitad de la demanda disponible para cada puerto.
# En el caso del ferrocarril, se asume que el 100% de los TEUS disponibles seran movilizados
tarifas$pct.mercado <- ifelse(tarifas$Sistema!="Ferrocarril",0.5,1)
# Creao una tabla distinta para modelar los ingresos
ingresos <- left_join(tarifas,demanda)
ingresos$tipo.demanda <- tipo.demanda

# Desde este momento se utilizara la demanda minima para modelar los ingresos
# Considerar que luego debo de tener una forma de cambiar la columna de demanda, la cual debera de ser influenciada por el modelo a crear.

##Seleccion de tipo de demanda

if(tipo.demanda=="min"){
  ingresos <- ingresos %>%
    mutate(Ingresos.Brutos = if_else(Sistema!="Ferrocarril",Tarifa.dols*pct.mercado*Ramp.Up*MIN*2,Tarifa.dols*pct.mercado*Ramp.Up*MIN),
           Mercado=pct.mercado*Ramp.Up*MIN) 
  
}else{
  ingresos <- ingresos %>%
    mutate(Ingresos.Brutos = if_else(Sistema!="Ferrocarril",Tarifa.dols*pct.mercado*Ramp.Up*MAX*2,Tarifa.dols*pct.mercado*Ramp.Up*MAX),
           Mercado=pct.mercado*Ramp.Up*MAX) 
  
}



ingresos$Infraestructura <- ifelse(ingresos$Sistema!="Ferrocarril","Puertos","Ferrocarril")

ingresos <- ingresos %>% 
  select(Ingresos.Brutos,Year,Infraestructura) %>%
  group_by(Year,Infraestructura)%>%
  summarise_all(sum)

ingresos <- replace_na(ingresos, replace = list(Ingresos.Brutos=0))

