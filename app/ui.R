

# Interfaz gráfica --------------------------------------------------------

ui <- navbarPage(
  title = "Analisis Financiero",
  inverse = TRUE,
  footer = div("  Elaborado por:  ", img(src = "axon2.png")),
  tabPanel("Valor Base",
           sidebarLayout(
             sidebarPanel(h3("Inputs para evaluacion del modelo"),
               sliderInput(
                 "años.financiamiento",
                 label = "Años financiamiento",
                 value = 30,
                 max = 30,
                 min = 5
               ),
               sliderInput(
                 "cons.multi",
                 "Periodo concesion Multimodal",
                 value = 50,
                 max = 70,
                 min = 20,
                 step = 2
               ),
               sliderInput(
                 "cons.poli",
                 "Periodo concesion Energia",
                 value = 30,
                 max = 70,
                 min = 20,
                 step = 2
               ),
               sliderInput(
                 "tasa.financiamiento",
                 "Tasa financiamiento",
                 value = 0.05,
                 max = 0.1,
                 min = 0.01,
                 step = 0.005
               ),
               sliderInput(
                 "pct.financiado",
                 "Porcentaje Inversion Financiada",
                 min   = 0,
                 max = 1,
                 value = 0.6
               ),
               radioButtons(
                 "tasa.retorno.manual",
                 "Tasa de retorno",
                 choices = list("Automatica" = FALSE, "Manual" =
                                  TRUE),
                 selected = TRUE
               ),
               sliderInput(
                 "tasa.descuento",
                 "Tasa de Descuento",
                 value = 0.08,
                 max = 0.2,
                 min = 0.05,
                 step = 0.005
               ),
               helpText(
                 "Esta es la tasa a la que se evalúa el proyecto para el promotor y el operador"
               ),
               radioButtons(
                 "tipo.demanda",
                 "Tipo demanda",
                 choices = list("Demanda Minima" = "min", "Demanda Maxima" = "max"),
                 selected = "min"
               ),
               radioButtons(
                 "regimen.fiscal",
                 "Regimen Fiscal",
                 choices = list("Sobre Ingresos" = FALSE, "Sobre Utilidades" =
                                  TRUE),
                 selected = TRUE
               ),
               sliderInput(
                 "isr",
                 "Impuesto sobre la renta: Sobre ingresos",
                 min = 0.01,
                 max = 0.1,
                 value = 0.07,
                 step = 0.005
               ),
               sliderInput(
                 "isr2",
                 "Impuesto sobre la renta: Sobre utilidades",
                 min = 0.25,
                 max = 0.4,
                 value = 0.25,
                 step = 0.05
               )
             ),
             mainPanel(h1("Evaluación Base"),
                                  h2("Sistema Completo"),
               tabPanel("VPN Proyecto completo", dataTableOutput("total")),
               h2("VPN por sistema"),
               tabPanel("VPN por Sistema", dataTableOutput("sistema")),
               h2("VPN por elemento"),
               tabPanel("VPN por Elemento", dataTableOutput("elemento")),
               h1("Evaluación construcción Infraestructura por parte de SIGSA"),
               h2("VPN 3ro por sistema"),
               tabPanel("VPN Tercero, Sistema", dataTableOutput("tercero.sistema")),
               h2("VPN 3ro por elemento"),
               tabPanel("VPN Tercero, Elemento", dataTableOutput("tercero.elemento")),
               h2("VPN SIGSA por Sistema"),
               tabPanel("VPN SIGSA, Sistema", dataTableOutput("sigsa.sistema")),
               h2("VPN SIGSA por Elemento"),
               tabPanel("VPN SIGSA, Elemento", dataTableOutput("sigsa.elemento"))
             )
           ))
)
