library(bslib)
library(DT)
library(ggplot2)
library(plotly)
library(readxl)
library(shiny)

tpx.h <- read_excel("data/VANUs 2025.xlsx", sheet = "Sobrevivencia H")
tpx.m <- read_excel("data/VANUs 2025.xlsx", sheet = "Sobrevivencia M")

tpx.h <- tpx.h[12:(nrow(tpx.h)), 3:ncol(tpx.h)]
tpx.m <- tpx.m[12:(nrow(tpx.m)), 3:ncol(tpx.m)]

colnames(tpx.h) <- 1:ncol(tpx.h)
colnames(tpx.m) <- 1:ncol(tpx.m)

r <- 0.036
monto.min <- 31938.44

calculo.vanu <- function(x, n, m, tpx){
  
  # Se inicializa el vector que contendrá las VANUs
  vanu <- c()
  
  # Se itera sobre cada edad
  for(edad in x){
    
    # Se calcula el VANU mensual para la edad correspondiente
    vanu <- c(vanu, 12 * (sum(tpx[1:n, edad+1] / (1 + r)^(0:(n-1))) - 
                            ((m-1) / (2*m) * (1-(tpx[n+1, edad+1]/((1+r)^n))))))
    
  }
  
  # Se retorna el vector de VANUs
  return(as.numeric(vanu))
  
}

vanu.h <- calculo.vanu(c(0:115), 115, 12, tpx.h)
vanu.m <- calculo.vanu(c(0:115), 115, 12, tpx.m)

retiro.prgrmd <- function(monto.acum, sexo, edad){
  
  # Se selecciona el vector de VANUs dependiendo del sexo
  if(sexo == "Hombre" | sexo == "Masculino"){
    
    vanu <- vanu.h
    
  } else if(sexo == "Mujer" | sexo == "Femenino"){
    
    vanu <- vanu.m
    
  } else{
    
    stop("El sexo digitado no es válido")
    
  }
  
  # Se inicializa el vector con las edades
  vector.edades <- c(edad)
  
  # Se calcula el monto de la pensión bajo esta modalidad
  pension <- ifelse((monto.acum / vanu[edad+1]) < monto.min, 
                    monto.min, monto.acum / vanu[edad+1])
  
  # Se inicializa el vector que almacenará la pensión mensual cada año
  vector.pensiones <- c(pension)
  
  # Se calculan los rendimientos
  rends <- monto.acum * r - 12 * pension * r / 2
  
  # Se actualiza el monto acumulado (osea la reserva al final del año)
  monto.acum <- ifelse((monto.acum + rends - 12*pension) < 0, 
                       0, monto.acum + rends - 12*pension)
  
  # Se inicializa el vector con la reserva al final de cada año
  vector.reservas <- c(monto.acum)
  
  # Se repite el proceso iterando año a año hasta que se agote el fondo acumulado 
  # o el afiliado llegue a los 115 años
  while(monto.acum > 0 & edad <= 115){
    
    edad <- edad + 1
    
    vector.edades <- c(vector.edades, edad)
    
    pension <- ifelse((monto.acum / vanu[edad+1]) < monto.min, 
                      monto.min, monto.acum / vanu[edad+1])
    
    vector.pensiones <- c(vector.pensiones, pension)
    
    rends <- monto.acum * r - 12 * pension * r / 2
    
    monto.acum <- ifelse((monto.acum + rends - 12*pension) < 0, 
                         0, monto.acum + rends - 12*pension)
    
    vector.reservas <- c(vector.reservas, monto.acum)
    
  }
  
  # Se retorna en un DataFrame la edad y la pensión año a año
  data.frame(edad = vector.edades, pension = vector.pensiones, 
             reserva.final = vector.reservas)
  
}

renta.prmnt <- function(monto.acum, edad, rends.3a){
  
  # Se inicializa el vector que almacenará las pensiones mensuales
  vector.pensiones <- c()
  
  # Se inicializa el vector con la reserva al final de cada año
  vector.reservas <- c()
  
  # Variable auxiliar para controlar la edad durante la simulación
  edad.aux <- edad
  
  # Se itera la edad en bloques de 3 años hasta los 115 años
  while(edad.aux <= 115){
    
    # Se calcula el monto de la pensión bajo esta modalidad
    pension <- max((sum(rends.3a) / 36), monto.min)
    
    # Se agregan 3 años con la misma pensión al vector de pensiones
    vector.pensiones <- c(vector.pensiones, rep(pension, 3))
    
    # Se reinicia el vector con los rendimientos de los últimos 3 años
    rends.3a <- c()
    
    # Se calculan los rendimientos y se actualiza el monto acumulado para el primer año
    rends <- monto.acum * r - 12 * pension * r / 2
    rends.3a <- c(rends.3a, rends)
    monto.acum <- monto.acum + rends - 12 * pension
    
    # Se guarda la reserva al final del primer año
    vector.reservas <- c(vector.reservas, monto.acum)
    
    # Se repite el proceso para el segundo año
    rends <- monto.acum * r - 12 * pension * r / 2
    rends.3a <- c(rends.3a, rends)
    monto.acum <- monto.acum + rends - 12 * pension
    vector.reservas <- c(vector.reservas, monto.acum)
    
    # Y finalmente se repite el proceso para el tercer año
    rends <- monto.acum * r - 12 * pension * r / 2
    rends.3a <- c(rends.3a, rends)
    monto.acum <- monto.acum + rends - 12 * pension
    vector.reservas <- c(vector.reservas, monto.acum)
    
    # Se incrementa la edad en 3 años
    edad.aux <- edad.aux + 3
    
  }
  
  # Se devuelve un DataFrame con las edades (de inicio a 115) y sus respectivas pensiones
  data.frame(edad = edad:115, pension = vector.pensiones[1:(116-edad)],
             reserva.final = vector.reservas[1:(116-edad)])
  
}

renta.tmprl <- function(capital.disp, sexo, edad) {
  
  # Selección de tabla de supervivencia
  if (sexo == "Hombre" | sexo == "Masculino") {
    tpx <- tpx.h
  } else if (sexo == "Mujer" | sexo == "Femenino") {
    tpx <- tpx.m
  } else {
    stop("El sexo digitado no es válido")
  }
  
  esperanza.restante <- ceiling(as.numeric(tpx[118, edad + 1]))
  
  vector.edades <- edad:(edad + esperanza.restante)
  vector.pensiones <- numeric(length(vector.edades))
  vector.capitales <- numeric(length(vector.edades))
  vector.rendimientos <- numeric(length(vector.edades))
  
  for (i in 1:(esperanza.restante + 1)) {
    
    edad.actual <- vector.edades[i]
    
    if (capital.disp <= 0) {
      vector.pensiones[i] <- 0
      vector.capitales[i] <- 0
      next
    }
    
    # VANU y pensión
    n <- esperanza.restante - i + 1
    vanu <- calculo.vanu(edad.actual, n, 12, tpx)
    
    pension.por.renta.temp <- capital.disp / vanu
    pension <- ifelse(pension.por.renta.temp < monto.min, monto.min, pension.por.renta.temp)
    vector.pensiones[i] <- pension
    
    # cálculo de rendimiento
    rendimiento <- capital.disp * r - 12 * pension * (r / 2)
    vector.rendimientos[i] <- rendimiento
    
    # nuevo capital disponible
    nuevo.capital <- capital.disp + rendimiento - 12 * pension
    capital.disp <- max(0, nuevo.capital)
    vector.capitales[i] <- capital.disp
  }
  
  # si la persona sobrevive más de la esperanza de vida:
  capital.restante <- vector.capitales[length(vector.capitales)]
  if (capital.restante > 0) {
    edad.extra <- edad + esperanza.restante + 1
    vector.edades <- c(vector.edades, edad.extra)
    vector.pensiones <- c(vector.pensiones, capital.restante / 12)
    vector.capitales <- c(vector.capitales, 0)
    vector.rendimientos <- c(vector.rendimientos, (capital.restante)*r-12*(capital.restante / 12)*r/2)
  }
  
  # Se quita el 0 final
  vector.edades <- vector.edades[-length(vector.edades)]
  vector.pensiones <- vector.pensiones[-length(vector.pensiones)]
  vector.rendimientos <- vector.rendimientos[-length(vector.rendimientos)]
  vector.capitales <- vector.capitales[-length(vector.capitales)]
  
  return(data.frame(
    edad = vector.edades,
    pension = vector.pensiones,
    reserva.final = vector.capitales
  ))
}



ui <- fluidPage(
  titlePanel("ROP - Modalidades de Retiro"),
  
  theme = bs_theme(
    bg = "#F2FBFC",
    fg = "#08783D",
    primary = "#9ED9CC",
    base_font = font_google("Roboto")
  ),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("monto", "Monto acumulado:", value = 30000000, min = 16137),
      numericInput(
        "edad",
        "Edad actual:",
        value = 65,
        min = 15,
        max = 115
      ),
      selectInput("sexo", "Sexo:", choices = c("Hombre", "Mujer")),
      selectInput(
        "modalidad",
        "Modalidad de retiro:",
        choices = c("Retiro Programado", "Renta Permanente", "Renta Temporal")
      ),
      conditionalPanel(
        condition = "input.modalidad == 'Renta Permanente'",
        numericInput("rend1", "Rendimiento Año 1:", value = 2500000),
        numericInput("rend2", "Rendimiento Año 2:", value = 2700000),
        numericInput("rend3", "Rendimiento Año 3:", value = 2900000)
      )
    ),
    
    mainPanel(tabsetPanel(
      tabPanel("Tabla", dataTableOutput("tabla_resultados")),
      tabPanel(
        "Gráficos",
        plotlyOutput("grafico_pension"),
        plotlyOutput("grafico_reserva")
      )
    ))
  )
)

server <- function(input, output, session) {
  observe({
    if (!is.null(input$edad) &&
        !is.na(input$edad) && is.numeric(input$edad)) {
      if (input$edad < 15) {
        updateNumericInput(session, "edad", value = 15)
      } else if (input$edad > 115) {
        updateNumericInput(session, "edad", value = 115)
      }
    } else{
      updateNumericInput(session, "edad", value = 65)
    }
  })
  
  observe({
    if (!is.null(input$monto) &&
        !is.na(input$monto) && is.numeric(input$monto)) {
      if (input$monto < 0) {
        updateNumericInput(session, "monto", value = 0)
      }
    } else{
      updateNumericInput(session, "monto", value = 30000000)
    }
  })
  
  resultado <- reactive({
    tryCatch({
      if (input$modalidad == "Retiro Programado") {
        retiro.prgrmd(input$monto, input$sexo, input$edad)
      } else if (input$modalidad == "Renta Permanente") {
        observe({
          if (!is.null(input$rend1) &&
              !is.na(input$rend1) && is.numeric(input$rend1) && !is.null(input$rend2) &&
              !is.na(input$rend2) && is.numeric(input$rend2) && !is.null(input$rend3) &&
              !is.na(input$rend3) && is.numeric(input$rend3)) {
            if (input$rend1 < 0) {
              updateNumericInput(session, "rend1", value = 0)
            } else if (input$rend2 < 0) {
              updateNumericInput(session, "rend2", value = 0)
            } else if (input$rend3 < 0) {
              updateNumericInput(session, "rend3", value = 0)
            }
          } else if (is.null(input$rend1) ||
                     is.na(input$rend1) || is.numeric(input$rend1)) {
            updateNumericInput(session, "rend1", value = 3000000)
          } else if (is.null(input$rend2) ||
                     is.na(input$rend2) || is.numeric(input$rend2)) {
            updateNumericInput(session, "rend1", value = 3000000)
          } else {
            updateNumericInput(session, "rend3", value = 3000000)
          }
        })
        
        renta.prmnt(input$monto,
                    input$edad,
                    c(input$rend1, input$rend2, input$rend3))
      } else {
        renta.tmprl(input$monto, input$sexo, input$edad)
      }
    }, error = function(e) {
      data.frame(
        edad = numeric(0),
        pension = numeric(0),
        reserva.final = numeric(0)
      )
    })
  })
  
  output$tabla_resultados <- renderDataTable({
    req(resultado)
    df <- resultado()
    
    names(df) <- c("Edad", "Pensión", "Reserva final")
    
    datatable(df,
              rownames = FALSE,
              extensions = "Buttons",
              options = list(
                dom = 'Blfrtip',
                buttons = c("excel"),
                language = list(url = '//cdn.datatables.net/plug-ins/1.13.4/i18n/es-ES.json')
              ))
  })
  
  output$grafico_pension <- renderPlotly({
    df <- resultado()
    
    fig <- ggplot(df, aes(x = edad, y = pension)) +
      geom_line(color = "#18A777", linewidth = 1.2) +
      labs(title = "Evolución de la Pensión Mensual", x = "Edad", y = "Pensión")
    
    ggplotly(fig)
  })
  
  output$grafico_reserva <- renderPlotly({
    df <- resultado()
    
    fig <- ggplot(df, aes(x = edad, y = reserva.final)) +
      geom_line(color = "#18A777", linewidth = 1.2) +
      labs(title = "Evolución de la Reserva", x = "Edad", y = "Reserva final")
    
    ggplotly(fig)
  })
}

shinyApp(ui = ui, server = server)