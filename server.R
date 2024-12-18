server <- function(input, output, session) {
  theme_set(theme_fivethirtyeight())

  

  
programas_filtred <- reactive({
    anios_seleccionados <-  input$select_anio_oferta
    Data_Primer_Curso%>% 
      filter(ANO %in% anios_seleccionados)
  })
  

programas_nacional_filtred <- reactive({
  anios_seleccionados <-  input$select_anio_oferta
  Data_Primer_Curso %>% 
    filter(ANO %in% anios_seleccionados) %>% 
    filter(DESC_CINE_CAMPO_AMPLIO == "EDUCACIÓN" | DESC_CINE_CAMPO_AMPLIO == "Educación")
  
})

programas_upn_filtred <- reactive({
  anios_seleccionados <-  input$select_anio_oferta
  Data_Primer_Curso %>% 
    filter(ANO %in% anios_seleccionados) %>% 
    filter(DESC_CINE_CAMPO_AMPLIO == "EDUCACIÓN" | DESC_CINE_CAMPO_AMPLIO == "Educación") %>% 
    filter(INSTITUCION_DE_EDUCACION_SUPERIOR_IES == "UNIVERSIDAD PEDAGOGICA NACIONAL")
})

texto_presencial_html <- function(){
  texto <- paste0(
    "<h3 style='color: #00609d;'>",
    "Número de programas de ", tolower(input$select_nivel_academico), " en modalidad presencial",
    "</h2>"
  )
  # Lo devolvemos como HTML
  HTML(texto)
}


texto_no_presencial_html <- function(variable){
  texto <- paste0(
    "<h3 style='color: #00609d;'>",
    "Número de programas de ", tolower(input$select_nivel_academico), " en modalidad no presencial",
    "</h2>"
  )
  # Lo devolvemos como HTML
  HTML(texto)
}

texto_modalidades_html <- function(variable){
  texto <- paste0(
    "<h3 style='color: #00609d;'>",
    "Porcentaje de participación de la UPN en la oferta de programas de ", tolower(input$select_nivel_academico), " en el campo amplio de educación",
    "</h2>"
  )
  # Lo devolvemos como HTML
  HTML(texto)
}

texto_oferta_modalidades_html <- function(variable){
  texto <- paste0(
    "<h3 style='color: #00609d;'>",
    "Oferta de programas de ", tolower(input$select_nivel_academico), " en el campo amplio de educación por tipo de modalidad",
    "</h2>"
  )
  # Lo devolvemos como HTML
  HTML(texto)
}
  ## Numero de programas ---------------------------------------------------------------
  
    ### Valuebox ----------------------------------------------------------------
  
    output$value_box_nacional <- renderUI({
      fluidRow(
        column(
          width = 12,
          splitLayout(
            summaryBox2(
              title = "Programas nacional",
              value = nrow(programas_nacional_filtred() %>% 
                             distinct()),
              style = "info",
              width = 12
            ),
          )
        )
      )
    })

  output$value_box_upn <- renderUI({
    fluidRow(
      column(
        width = 12,
        splitLayout(
          summaryBox2(
            title = "Programas UPN",
            value = nrow(programas_upn_filtred() %>% 
                           distinct()),
            style = "success",
            width = 12
          ),
        )
      )
    )
  })
  
  output$plot_programas_pregrado <- renderPlot({
   
    Data_Primer_Curso %>%  
    filter(ANO >= fecha - 5) %>% 
    filter(NIVEL_ACADEMICO == "PREGRADO") %>%
      filter(INSTITUCION_DE_EDUCACION_SUPERIOR_IES == "UNIVERSIDAD PEDAGOGICA NACIONAL") %>% 
      group_by(ANO) %>% 
      summarise(UPN = n_distinct(CODIGO_SNIES_DEL_PROGRAMA)) %>% 
      left_join(
        Data_Primer_Curso %>% 
          filter(DESC_CINE_CAMPO_AMPLIO == "EDUCACIÓN" | DESC_CINE_CAMPO_AMPLIO == "Educación") %>%
          filter(ANO >= fecha - 5) %>%
          filter(NIVEL_ACADEMICO == "PREGRADO") %>%
          group_by(ANO) %>% 
          summarise(NACIONAL = n_distinct(CODIGO_SNIES_DEL_PROGRAMA)),
        by = "ANO"
      ) %>% 
      left_join(
        Data_Primer_Curso %>% 
          filter(DESC_CINE_CAMPO_AMPLIO == "EDUCACIÓN" | DESC_CINE_CAMPO_AMPLIO == "Educación") %>%
          filter(ANO >= fecha - 5) %>%
          filter(NIVEL_ACADEMICO == "PREGRADO") %>%
          filter(MUNICIPIO_DE_OFERTA_DEL_PROGRAMA == "BOGOTA, D.C.") %>% 
          group_by(ANO) %>% 
          summarise(BOGOTA = n_distinct(CODIGO_SNIES_DEL_PROGRAMA)),
        by = "ANO"
      ) %>% 
      mutate(
        "UPN - Nacional pregrado" = round((UPN / NACIONAL) * 100,2),
        "UPN - Bogotá pregrado" = round((UPN / BOGOTA) * 100,2)
      ) %>% 
      pivot_longer(cols = c("UPN - Nacional pregrado", "UPN - Bogotá pregrado"),
                   names_to = "Categoria",
                   values_to = "n") %>% 
    
      
      ggplot(aes(x=ANO,y=n,color=Categoria))+
      geom_line()+
      geom_point()+
      labs(
        x = "Año",   # Cambiar el nombre del eje X
        y = "Número de programas",  # Cambiar el nombre del eje Y
        color = NULL
      ) +
      scale_y_continuous(
        limits = c(0, 25),  # Set a fixed range from 0 to 20
        breaks = seq(0, 20, by = 5),  # Add breaks every 2 units
        labels = function(x) paste0(x, "%")
      ) +  
      geom_text_repel(aes(label= n), size=3, show.legend = FALSE)+
      theme_minimal() +
      theme(legend.position = "top")  # This moves the legend to the top
    
  })
  
  output$plot_programas_posgrado <- renderPlot({
      
    Data_Primer_Curso %>% 
      filter(ANO >= fecha - 5) %>%
      filter(NIVEL_ACADEMICO == "POSGRADO") %>%
      filter(INSTITUCION_DE_EDUCACION_SUPERIOR_IES == "UNIVERSIDAD PEDAGOGICA NACIONAL") %>% 
      group_by(ANO) %>% 
      summarise(UPN = n_distinct(CODIGO_SNIES_DEL_PROGRAMA)) %>% 
      left_join(
        Data_Primer_Curso %>% 
          filter(DESC_CINE_CAMPO_AMPLIO == "EDUCACIÓN" | DESC_CINE_CAMPO_AMPLIO == "Educación") %>%
          filter(ANO >= fecha - 5) %>%
          filter(NIVEL_ACADEMICO == "POSGRADO") %>%
          group_by(ANO) %>% 
          summarise(NACIONAL = n_distinct(CODIGO_SNIES_DEL_PROGRAMA)),
        by = "ANO"
      ) %>% 
      left_join(
        Data_Primer_Curso %>% 
          filter(DESC_CINE_CAMPO_AMPLIO == "EDUCACIÓN" | DESC_CINE_CAMPO_AMPLIO == "Educación") %>%
          # filter(ANO == "2020")
          filter(ANO >= fecha - 5) %>%
          filter(NIVEL_ACADEMICO == "POSGRADO") %>%
          filter(MUNICIPIO_DE_OFERTA_DEL_PROGRAMA == "BOGOTA, D.C.") %>% 
          group_by(ANO) %>% 
          summarise(BOGOTA = n_distinct(CODIGO_SNIES_DEL_PROGRAMA)),
        by = "ANO"
      ) %>% 
      mutate(
        "UPN - Nacional posgrado" = round((UPN / NACIONAL) * 100,2),
        "UPN - Bogotá posgrado" = round((UPN / BOGOTA) * 100,2)
      ) %>% 
      pivot_longer(cols = c("UPN - Nacional posgrado", "UPN - Bogotá posgrado"),
                   names_to = "Categoria",
                   values_to = "n") %>% 
      
      ggplot(aes(x=ANO,y=n,color=Categoria))+
      geom_line()+
      geom_text_repel(aes(label= n), size=3, show.legend = FALSE)+
      geom_point()+
      labs(
        x = "Año",   # Cambiar el nombre del eje X
        y = "Número de programas",  # Cambiar el nombre del eje Y
        color = NULL
      ) +
      scale_y_continuous(
        limits = c(0, 15),  # Set a fixed range from 0 to 20
        breaks = seq(0, 20, by = 5),  # Add breaks every 2 units
        labels = function(x) paste0(x, "%")
      ) +  
      theme_minimal() +
      theme(legend.position = "top")  # This moves the legend to the top
      
  })
  
  ##EMPIEZA MODALIDADES --------------------------------------------
  
  output$html_texto_nivel_academico <- renderUI({
    texto_presencial_html()
  })
  
  output$html_texto_nivel_academico2 <- renderUI({
     texto_no_presencial_html()
  })
  
  output$plot_modalidades_pregrado <- renderPlot({

    MODALIDADES_TABLA %>% 
      filter(NIVEL_ACADEMICO == input$select_nivel_academico) %>%
      filter(MODALIDAD == "PRESENCIAL") %>% 
      mutate(CATEGORIA = paste("Presencial - ", CATEGORIA)) %>% 
      group_by(ANO, CATEGORIA) %>%
      summarise(total = sum(n, na.rm = TRUE)) %>% 
      ggplot(aes(x=ANO,y=total,color=CATEGORIA))+
      geom_line()+
      geom_text_repel(aes(label= total), size=3, nudge_y = 2, show.legend = FALSE)+
      geom_point()+
      labs(
        x = "Año",   # Cambiar el nombre del eje X
        y = "Número de programas",  # Cambiar el nombre del eje Y
        color = NULL
      ) +
      theme_minimal() +
      theme(legend.position = "top")  # This moves the legend to the top

  })
  

  
  output$plot_modalidades_pregrado_virtual <- renderPlot({
    
    MODALIDADES_TABLA %>% 
      filter(NIVEL_ACADEMICO == input$select_nivel_academico) %>%
      filter(MODALIDAD == "OTRAS MODALIDADES") %>%
      mutate(CATEGORIA = paste("Otras modalidades - ", CATEGORIA)) %>% 
      group_by(ANO, CATEGORIA) %>%
      summarise(total = sum(n, na.rm = TRUE)) %>% 
      ggplot(aes(x=ANO,y=total,color=CATEGORIA))+
      geom_line()+
      geom_text_repel(aes(label= total), size=3, nudge_y = 2, show.legend = FALSE)+
      geom_point()+
      labs(
        x = "Año",   # Cambiar el nombre del eje X
        y = "Número de programas",  # Cambiar el nombre del eje Y
        color = NULL
      ) +
      theme_minimal() +
      theme(legend.position = "top")  # This moves the legend to the top
    
  })
  
  output$html_texto_modalidades <- renderUI({
    texto_modalidades_html()
  })


  output$plot_modalidad_actual <- renderPlot({

    MODALIDADES_TABLA %>% 
      filter(NIVEL_ACADEMICO == input$select_nivel_academico) %>%
      filter(ANO ==  input$select_anio_oferta) %>% 
      filter(CATEGORIA == "Nacional" | CATEGORIA == "UPN") %>% 
      pivot_wider(names_from = CATEGORIA, values_from = n) %>% 
      mutate("Participación - UPN" = ifelse(is.na(UPN), 0,round((UPN / Nacional) * 100,2))) %>% 
      pivot_longer(cols = c("Nacional", "UPN"),
                   names_to = "CATEGORIA",
                   values_to = "n") %>% 
      filter(CATEGORIA == "Nacional") %>% 
      ggplot(aes(x = METODOLOGIA, y = n)) +
      geom_bar(stat = "identity", alpha = 0.5, width = 0.5) +
      geom_line(aes(y = `Participación - UPN`, group = CATEGORIA), color = "black", size = 0.7) +
      geom_point(aes(y = `Participación - UPN`, group = CATEGORIA))+
      geom_text_repel(
        aes(
          y =`Participación - UPN`, 
          label = `Participación - UPN`, 
          group = CATEGORIA
        ), 
        size = 3, 
        box.padding = 0.5, 
        min.segment.length = 0
      ) +
      
      # Escala para las barras (porcentaje o valores absolutos)
      scale_y_continuous(
        name = "Número de programas",
      ) +
      labs(title = "Participación nacional") + 
      theme(plot.title.position = "plot")

  })
  
  
  output$plot_modalidad_bogota <- renderPlot({
    
    MODALIDADES_TABLA %>% 
      filter(NIVEL_ACADEMICO == input$select_nivel_academico) %>%
      filter(ANO ==  input$select_anio_oferta) %>% 
      filter(CATEGORIA == "Bogotá" | CATEGORIA == "UPN") %>% 
      pivot_wider(names_from = CATEGORIA, values_from = n) %>% 
      mutate("Participación - UPN" = ifelse(is.na(UPN), 0,round((UPN / `Bogotá`) * 100,2))) %>% 
      pivot_longer(cols = c("Bogotá", "UPN"),
                   names_to = "CATEGORIA",
                   values_to = "n") %>% 
      filter(CATEGORIA == "Bogotá") %>% 
      ggplot(aes(x = METODOLOGIA, y = n)) +
      geom_bar(stat = "identity", alpha = 0.5, width = 0.5) +
      geom_line(aes(y = `Participación - UPN`, group = CATEGORIA), color = "black", size = 0.7) +
      geom_point(aes(y = `Participación - UPN`, group = CATEGORIA))+
      geom_text_repel(
        aes(
          y =`Participación - UPN`, 
          label = `Participación - UPN`, 
          group = CATEGORIA
        ), 
        size = 3, 
        box.padding = 0.15, 
        nudge_x = 0.15,
        min.segment.length = 0
      ) +
      
      # Escala para las barras (porcentaje o valores absolutos)
      scale_y_continuous(
        name = "Número de programas",
      ) +
      labs(title = "Participación en Bogotá") + 
      theme(plot.title.position = "plot")
    
  })
  
  output$html_texto_modalidades2 <- renderUI({
    texto_oferta_modalidades_html()
  })
  

  output$ft_modalidades_pregrado <- renderUI({
    
    minimo <- Data_Primer_Curso %>% 
      summarise(min_ANO = min(ANO, na.rm = TRUE)) %>% 
      pull(min_ANO)

    Tabla <- MODALIDADES_TABLA %>% 
      filter(NIVEL_ACADEMICO == input$select_nivel_academico) %>%
      select(-NIVEL_ACADEMICO) %>% 
      select(-MODALIDAD) %>% 
      mutate(METODOLOGIA = ifelse(METODOLOGIA == "A DISTANCIA", 
                                  "A DISTANCIA (TRADICIONAL)", METODOLOGIA)) %>% 
      mutate(METODOLOGIA = ifelse(METODOLOGIA == "A DISTANCIA (VIRTUAL)", 
                                  "VIRTUAL", METODOLOGIA)) %>%
      mutate(METODOLOGIA = ifelse(METODOLOGIA == "PRESENCIAL-VIRTUAL", 
                                  "HIBRIDO", METODOLOGIA)) %>%
      mutate(n = ifelse(is.na(n), 0, n)) %>% 
      filter(ANO == ifelse(as.numeric(input$select_anio_oferta) -4 < minimo, minimo, as.numeric(input$select_anio_oferta) -4)
                    | ANO ==  input$select_anio_oferta) %>% 
      pivot_wider(names_from = c(ANO,CATEGORIA), values_from = n,names_sep = "-", values_fill = 0) 
    
    Tabla <- Tabla %>% 
      flextable() %>% 
      align(part = "header", align = "center") %>%
      align(j = 1:2, align = "center") %>%
      bg(part = "header", bg = "#2c7fb8") %>%
      color(part = "header", color = "white") %>%
      bg(j = 1, bg = "#D9D9D9") %>%
      bold(part = "header") %>%
      border(part = "all", border = fp_border_default(color = "black", width = 1)) %>%
      autofit() %>%
      fit_to_width(max_width = 10) %>% 
      separate_header(opts = "center-hspan",
                      split = "-",
                      fixed = FALSE) 

    flextable::htmltools_value(Tabla)

  })
  # 
  # 
  # output$plot_gestion_calidad <- renderPlot({
  #   
  #   gestion_filtred () %>%
  #     rename(var = funcionario_que_lo_atendio_fue_claro_en_la_informacion) %>% 
  #     mutate(var = case_when( var == 5 ~ "Muy claro", 
  #                             var == 4 ~ "Claro", 
  #                             var == 3 ~ "Neutral", var == 2 ~ "Confuso", 
  #                             var == 1 ~ "Muy confuso", 
  #                             TRUE ~ as.character(var))) %>%
  #     plot_barras(var, "", "","")
  #   
  # })
  # 

  
  }
    
  