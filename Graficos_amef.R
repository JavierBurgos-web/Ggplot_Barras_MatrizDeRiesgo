#GRAFICO #1 DE BARRAS
    
    datos <- read_excel("AMEF-Tablas.xlsx", sheet = "AMEF", range = "b5:N21")
    colnames(datos)[c(1,2,10)] <- c("categoria", "actividad","valor")               # Asignación de nuevos nombres a determinadas columnas
    
    datos <- datos %>% 
      arrange(desc(valor)) %>%                                                      # Ordena los datos de mayor a menor valor
      distinct(categoria, actividad, .keep_all = TRUE) %>%                          # Selecciona la fila con el valor máximo de cada actividad
      group_by(categoria) %>%                                                       # Agrupa los datos por Categoria (Macroprocesos)
      mutate(actividad_ordenada =                                                   # Crea una nueva columna (actividad_ordenada)
               reorder(actividad, -valor))                                          # se le asigna valores de la columna Actividad ordenados de mayor a menor en funcion de su Valor
    
    ggplot(datos, aes(x = actividad_ordenada, y = valor, fill = actividad)) +
      geom_bar(stat = "identity", show.legend=FALSE) +
      geom_text(aes(label = valor), vjust = -0.5, size = 3) +    # Agrega el valor encima de cada barra
      labs(title = "Valor NPR vs actividad o subproceso asociados los procesos de Atención y Cocina", 
           subtitle = paste(" ", "Analisis impartido a marzo de 2023"), 
           x = "Subprocesos o actividades", y = "Valor NPR") +
      facet_grid(. ~ categoria, scales = "free_x") +
      scale_y_continuous(limits = c(0, 350)) +   # Establece el límite inferior y superior del eje y
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            plot.title = element_text(size = 10, margin = margin(b = 3), hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(size = 10, margin = margin(t = 5, b = 30), hjust = 0.5, face = "italic"),
            axis.title.x = element_text(size = 10, margin = margin(t = 20)),
            axis.title.y = element_text(size = 10, margin = margin(r = 25)))
    

    
# GRAFICO #2 MATRIZ DE RIESGO_______________________________________________________________________________________________________________________________________________________
    amef <- read_excel("AMEF-Tablas.xlsx", sheet = "AMEF", range = "b5:N21")              # Selección del rango
    
    library(dplyr)
    amef$`Nivel de NPR` <- cut(amef$NPR, 
                               breaks = c(0, 1, 124, 499, 1000),
                               labels = c("NULO", "BAJO", "MEDIO", "ALTO"))
    
    # Crear la matriz de riesgo en función del Nivel de NPR
    ggplot(amef, aes(x = `Modo de falla`, y = `Subproceso o actividad`, fill = `Nivel de NPR`)) +
      geom_tile() +
      
      scale_fill_manual(values = c("NULO" = "gray", "BAJO" = "green", "MEDIO" = "orange", "ALTO" = "red")) +
      
      labs(title = "Matriz de riesgo", x = "Modo de falla", y = "Subproceso o actividad", fill = "Nivel de NPR") +
      
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            plot.title = element_text(size = 10, margin = margin(b = 3), hjust = 0.5, face = "bold"),
            axis.title.x = element_text(size = 10, margin = margin(t = 20)),
            axis.title.y = element_text(size = 10, margin = margin(r = 25)))




# GRAFICO #3 MATRIZ DE RIESGO (VALORES REASIGNADOS)_________________________________________________________________________________________________________________________________
    
    amef <- read_excel("AMEF-Tablas.xlsx", sheet = "AMEF", range = "b5:N21")              # Selección del rango
    
    library(dplyr)
    
    amef <- amef %>%
      mutate(RPN = Severidad * Ocurrencia * Detección)
    
    
    resumen <- amef %>%
      group_by(`Subproceso o actividad`, `Modo de falla`) %>%
      summarise(mediana_RPN = median(RPN), suma_RPN = sum(RPN))
    
    
    library(ggplot2)
    
    ggplot(resumen, aes(x = `Modo de falla`, y = `Subproceso o actividad`, fill = mediana_RPN)) +
      geom_tile() +
      
      scale_fill_gradient(low = "green", high = "red") +
      
      labs(title = "Matriz de riesgo", 
           subtitle = paste(" ", "Reasignando criterio NPR"), 
           x = "Modo de falla", y = "Subproceso o actividad", fill = "Valor NPR") +
      
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            plot.title = element_text(size = 10, margin = margin(b = 3), hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(size = 10, margin = margin(t = 5, b = 30), hjust = 0.5, face = "italic"),
            axis.title.x = element_text(size = 10, margin = margin(t = 20)),
            axis.title.y = element_text(size = 10, margin = margin(r = 25)))