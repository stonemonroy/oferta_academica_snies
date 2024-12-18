dashboardPage(
  dashboardHeader(
    title = "Oferta académica datos SNIES"
  ),
  
  ## Sidebar -----------------------------------------------------------------
  
  #https://fontawesome.com/ pagina para iconos
  
  dashboardSidebar(
    sidebarMenu(
      style = "position: fixed;",
      #menuItem("Percepción del servicio", tabName = "dashboardPercepcion", icon = icon("user"), startExpanded = TRUE),
      menuItem("   Programas", tabName = "dashboardOferta", icon = icon("book"), startExpanded = TRUE)
      )
  ), #  Cierra sidebarmenu
  
  ## Dashboard Body ----------------------------------------------------------
  dashboardBody(
    tags$head(includeCSS('www/style.css')),
    
    tabItems(
      
      ### Dashboard Salas -------------------------------------------------------
      
      tabItem(
        tabName = "dashboardOferta",
        
        div(
          class = "filtros",
          fluidRow(
            column(
              width = 6,
              offset = 3,
              box(
                width = 8,
                style = "margin-top: 2%",
                background = "light-blue",
                align = "center",
                column(
                  width = 6,
                  offset = 3,
                  selectInput(
                    inputId = "select_anio_oferta",
                    label = "Seleccione un año",
                    choices = c("2019" = "2019", "2020" = "2020", "2021" = "2021",
                                "2022" = "2022", "2023" = "2023"),
                    selected = "2023"
                  )
                ) 
              )
            )
          )
        ),
        
        
        br(),
        br(),
        
        #### Encabezado ----------------------------------------------------------
        div(
          class = "contenido",
          
          fluidRow(
            column(
              width=12,
              #offset = 1,
              align = "center",
              div(style="display: inline-block; margin-right: 30px;", img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/1/1e/Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg/1200px-Universidad_Pedag%C3%B3gica_Nacional_(Colombia)_logo.svg.png", height=104, width=120)),
              div(style="display: inline-block; vertical-align: middle;", h1("Oferta académica SNIES",
                                                                             style = "font-family: 'Source Sans Pro';
                                                                                      color: #fff; text-align: center;
                                                                                      background-image: url('https://raw.githubusercontent.com/rstudio/shiny-examples/main/081-widgets-gallery/www/texturebg.png');
                                                                                      padding: 20px")
              )),        
          ),
          
          
          #### Texto introducción -------------------------------------------------------------
          
          fluidRow(
            column(
              width = 12,
              offset = 1,
              box(
                width = 10,
                style = "margin-top: 2%",
                background = "light-blue",
                align = "center",
                fluidRow(
                  
                  box(
                    title = "Introducción", width = 6, background = "light-blue",
                    "Este tablero presenta el análisis de la oferta académica de la Universidad Pedagógica Nacional en programas del campo amplio Educación tanto en pregrado como en posgrado, a través de gráficas y tablas se puede visualizar la participación de la UPN en la oferta de estos programas tanto a nivel nacional como a nivel municipal (Bogotá). "
                  ),
                  
                  column(
                    #offset = 1,
                    width = 3,
                    uiOutput("value_box_nacional") %>% withSpinner(type = 8, size = 0.5)
                  ),
                  column(
                    #offset = 1,
                    width = 3,
                    uiOutput("value_box_upn") %>% withSpinner(type = 8, size = 0.5)
                  )
                )
              )
            )
          ),
          
          
          # fluidRow(
          #   align = "center",
          #   div(
          #     style = "max-width: 900px; margin: 0 auto;",
          #     HTML("<h5 style='color: #393939;'><strong>El presente tablero muestra una descarga provisional de los informes descriptivos</strong></h5>")
          #   )
          # ),
          
          
          br(),
          
          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Número de programas pregrado - Participación de la UPN</strong></h3>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 10,
              offset = 1,
              plotOutput("plot_programas_pregrado") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
          
          
          br(),

          fluidRow(
            align = "center",
            HTML("<h3 style = 'color: #00609d'><strong>Número de programas posgrado - Participación de la UPN</strong></h3>"),
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 10,
              offset = 1,
              plotOutput("plot_programas_posgrado") %>% withSpinner(type = 8, size = 0.5)
            )
          ),

          br(),

          fluidRow(
           align= "center",
           HTML("<h3 style = 'color: #00609d'><strong> Modalidades de los programas </strong></h3>"),
          ),
          
          br(),
          
          div(
            fluidRow(
              column(
                width = 10,
                offset = 1,
                box(
                  width = 12,
                  style = "margin-top: 2%",
                  background = "light-blue",
                  align = "center",
                  column(
                    width = 12,
                    pickerInput(
                      inputId = "select_nivel_academico",
                      # options = list(actions-box = TRUE,
                      #                deselect-all-text = "Deseleccionar todo",
                      #                none-selected-text = "Nada seleccionado",
                      #                size = 7),
                      multiple = F,
                      label = "Seleccione el nivel académico a analizar",
                      choices = c(
                        "PREGRADO", "POSGRADO"),
                      selected = "PREGRADO"
                    )
                  )
                )
              )
            )
          ),
          
          fluidRow(
            align = 'center',
            uiOutput("html_texto_nivel_academico")
          ),

          # fluidRow(
          #   align = "center",
          #   HTML("<h3 style = 'color: #00609d'>Número de programas de pregrado en modalidad presencial</h3>"),
          # ),
          
          br(),
          
          fluidRow(
            column(
              width = 10,
              offset = 1,
              plotOutput("plot_modalidades_pregrado") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
          
          br(),
          
          fluidRow(
            align = 'center',
            uiOutput("html_texto_nivel_academico2")
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 10,
              offset = 1,
              plotOutput("plot_modalidades_pregrado_virtual") %>% withSpinner(type = 8, size = 0.5)
            )
          ),

          br(),
          
          
          fluidRow(
            align = 'center',
            uiOutput("html_texto_modalidades")
          ),
          
          br(),

          fluidRow(
            column(
              width = 6,
              plotOutput("plot_modalidad_actual") %>% withSpinner(type = 8, size = 0.5)
            ),
            column(
              width = 6,
              plotOutput("plot_modalidad_bogota") %>% withSpinner(type = 8, size = 0.5)
            )
          ),
          
          br(),
          
          
          fluidRow(
            align = 'center',
            uiOutput("html_texto_modalidades2")
          ),
          
          
          br(),
          
          
          fluidRow(
            column(
              width = 10,
              offset= 1,
              uiOutput("ft_modalidades_pregrado") %>% withSpinner(type = 8, size = 0.5)
              )
          ),
          
          br(),
          

      #     
      #     br(),
      #     br(),
      #     
      #     fluidRow(
      #       align = "center",
      #       HTML("<h3 style = 'color: #00609d'><strong>Claridad del funcionario que atendió</strong></h3>"),
      #     ),
      #     
      #     br(),
      #     
      #     fluidRow(
      #       column(
      #         width = 6,
      #         uiOutput("ft_gestion_calidad") %>% withSpinner(type = 8, size = 0.5)
      #       ),
      #       column(
      #         width = 6,
      #         plotOutput("plot_gestion_calidad") %>% withSpinner(type = 8, size = 0.5)
      #       )),
      #     
      #     br(),
      #     br(),
      #     
        )
      ) ### Cierra dashboardSalas --------------------

      
    )# Cierra tabItems
  )# Cierra dashboard body
)# Cierra dashboard page
