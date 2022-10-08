
library(shiny)
library(ggplot2)

# Funciones 
powerg <- function(n = 36, mu0 = 30, mu1 = 31, sd = 6, alpha = 0.10, 
                   alternative = c("two.sided", "less", "greater"))
{# Límites 
  ll <- min(mu0, mu1) - 3.4*sd/sqrt(n)
  ul <- max(mu0, mu1) + 3.4*sd/sqrt(n)
  
  p <- ggplot(data = data.frame(x = c(ll, ul)), aes(x = x))
  
  # Parametro de tipo de hipotesis alternativa
  alternative <- match.arg(alternative)
  
  # Calcula el test para varianza conocida para cada tipo de hipotesis alternativa
  
  if(alternative == "less")
  {
    dnorm_fun1 <- function(x){
      y <- dnorm(x, mu0, sd/sqrt(n))
      y[x > qnorm(alpha, mu0, sd/sqrt(n))] <- NA
      y
    }
    dnorm_fun2 <- function(x){
      y <- dnorm(x, mu1, sd/sqrt(n))
      y[x > qnorm(alpha, mu0, sd/sqrt(n))] <- NA
      y
    }
    dnorm_fun3 <- function(x){
      y <- dnorm(x, mu1, sd/sqrt(n))
      y[x < qnorm(alpha, mu0, sd/sqrt(n))] <- NA
      y
    }
    
    POWER <- round(pnorm(qnorm(alpha, mu0, sd/sqrt(n)), mu1, sd/sqrt(n)), 4)
    BETA <- round(1- POWER, 4)
    
  }else if(alternative == "greater")
  {
    dnorm_fun1 <- function(x){
      y <- dnorm(x, mu0, sd/sqrt(n))
      y[x < qnorm(1 - alpha, mu0, sd/sqrt(n))] <- NA
      y
    }
    dnorm_fun2 <- function(x){
      y <- dnorm(x, mu1, sd/sqrt(n))
      y[x < qnorm(1 - alpha, mu0, sd/sqrt(n))] <- NA
      y
    }
    dnorm_fun3 <- function(x){
      y <- dnorm(x, mu1, sd/sqrt(n))
      y[x > qnorm(1 - alpha, mu0, sd/sqrt(n))] <- NA
      y
    }
    POWER <- round(pnorm(qnorm(1- alpha, mu0, sd/sqrt(n)), mu1, sd/sqrt(n), lower.tail = FALSE), 4)
    BETA <- round(1- POWER, 4)
    
  } else if(alternative == "two.sided") 
  {
    dnorm_fun1 <- function(x){
      y <- dnorm(x, mu0, sd/sqrt(n))
      y[x > qnorm(alpha/2, mu0, sd/sqrt(n)) & x < qnorm(1 - alpha/2, mu0, sd/sqrt(n))] <- NA
      y
    }
    dnorm_fun2 <- function(x){
      y <- dnorm(x, mu1, sd/sqrt(n))
      y[x > qnorm(alpha/2, mu0, sd/sqrt(n)) & x < qnorm(1 - alpha/2, mu0, sd/sqrt(n))] <- NA
      y
    }
    
    dnorm_fun3 <- function(x){
      y <- dnorm(x, mu1, sd/sqrt(n))
      y[x > qnorm(1 - alpha/2, mu0, sd/sqrt(n)) | x < qnorm(alpha/2, mu0, sd/sqrt(n))] <- NA
      y
    }
    
    POWER <- round(pnorm(qnorm(alpha/2, mu0, sd/sqrt(n)), mu1, sd/sqrt(n)) + pnorm(qnorm(1- alpha/2, mu0, sd/sqrt(n)), mu1, sd/sqrt(n), lower.tail = FALSE), 4)
    BETA <- round(1- POWER, 4)
  }
  
  
  p + stat_function(fun = dnorm_fun1, geom = "area", n = 1500, fill = "red", alpha = 0.95, aes(colour = "Hipotetica")) +
    stat_function(fun = dnorm_fun2, geom = "area", n = 1500, fill = "blue", alpha = 0.4, aes(colour = "Verdadera")) +
    stat_function(fun = dnorm_fun3, geom = "area", n = 1500, fill = "green", alpha = 0.1) +
    stat_function(fun = dnorm, args = list(mu0, sd/sqrt(n)), n = 1500, color = "red") +
    stat_function(fun = dnorm, args = list(mu1, sd/sqrt(n)), n = 1500, color = "blue") +
    geom_hline(yintercept = 0) +
    scale_colour_manual("Distribución", values = c("red", "blue")) +
    theme_bw(base_size = 16) +
    labs(x = "", y = "", title = paste0("Potencia ","(",POWER,") es la suma del area azul y del area roja"),
         caption = paste0("El error de tipo II ","(",BETA,") es el area verde")) +
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(plot.caption = element_text(hjust = 0.5))  
}



# Primero definimos un Interfaz de usuario (UI)
ui <- fluidPage(
  withMathJax(),
  # Estética de los recuadros de texto
  tags$head(
    tags$style(HTML("
            code {
                display:block;
                padding:9.5px;
                margin:0 0 10px;
                margin-top:10px;
                font-size:13px;
                line-height:20px;
                word-break:break-all;
                word-wrap:break-word;
                white-space:pre-wrap;
                background-color:#F5F5F5;
                border:1px solid rgba(0,0,0,0.15);
                border-radius:4px; 
                font-family:monospace;
            }"))),
  
  # Titulo
  titlePanel("Potencia de Test para la Media de una Población Normal con Varianza Conocida"),
  
  sidebarPanel(
  radioButtons(inputId = "Tipo", label = "Seleccionar hipotesis alternativa:", 
               choices = c("\\(H_1:\\mu \\neq \\mu_0 \\)" = "two.sided",
                           "\\(H_1:\\mu > \\mu_0 \\)" = "greater",
                           "\\(H_1:\\mu < \\mu_0 \\)" = "less"),
               selected = "two.sided"),
  
  numericInput(inputId = "N", label = "Tamaño de muestra (\\(n\\)):", value = 25, min = 3, max = 10000, step = 1),
  numericInput(inputId = "mu0", label = "Media propuesta \\( \\mu_0 \\):", value = 30, min = -10000, max = 10000, step = 1),
  numericInput(inputId = "mu1", label = "Media real \\( \\mu_1  \\):", value = 31, min = -10000, max = 10000, step = 1),
  numericInput(inputId = "Sigma", label = "Desvio estandar poblacional \\((\\sigma)\\):", value = 1, min = 0.001, max = 10000, step = 1),
  sliderInput(inputId = "Alpha", label = "Nivel de significancia \\((\\alpha)\\):", min = 0.001, max = 0.25, value = 0.05, step = 0.001),

  ),
# Mostramos un grafico con la potencia
mainPanel( 
  code("La función de potencia de un test es la probabilidad de rechazar la
hipótesis nula cuando es falsa."),
  code("También se puede ver como la probabilidad de no cometer un error de tipo II."),
  
  plotOutput("power"), 
  # Lineas en caso de querer ingresar texto a mano.
  #textInput("caption", "Ejercicio", "Se desea testear..."),
  #verbatimTextOutput("value"), 
  )
)



# Definimos la funcion server
server <- function(input, output) {
  
  output$power <- renderPlot({
    powerg(n = input$N,
           sd = input$Sigma,
           mu0 = input$mu0,
           mu1 = input$mu1,
           alpha = input$Alpha,
           alternative = input$Tipo)})
  #output$value <- renderText({ input$caption })
  
}

# Ejecutamos la aplicacion. 
shinyApp(ui = ui, server = server)