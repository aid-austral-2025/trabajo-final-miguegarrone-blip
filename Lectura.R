paquetes <-
  c("tidyverse", "cowplot", "ggridges", "ggbeeswarm", "GGally", "plotly",
    "treemapify", "car", "vcd", "colorspace", "ggcleveland", "corrplot", "readxl",
    "lubridate", "gganimate", "gapminder", "forcats", "janitor", "ggforce",
    "ggalluvial","ggplot2","dplyr","tidyr","scales","plotly")
# Instalar los que no están
instalados <- paquetes %in% rownames(installed.packages())
if (any(instalados == FALSE)) {
  install.packages(paquetes[!instalados])
}

# Cargarlos a todos juntos
invisible(lapply(paquetes, library, character.only = TRUE))


# Calcular los nombres dinámicos
mes_actual <- floor_date(Sys.Date(), "month")  # Primer día del mes actual
nombres_mes <- c(
  format(mes_actual %m-% months(3), "%B"),  # n-3
  format(mes_actual %m-% months(2), "%B"),  # n-2
  format(mes_actual %m-% months(1), "%B")   # n-1
)

# Leer la hoja y renombrar columnas 2,3 y 4
estandar <- read_excel("Ingresos trimestrales Power BI.xlsx", sheet = "Estandar") %>% 
  rename_with(~ nombres_mes, .cols = 2:4)                                                      # El tercer elemento es el mes anterior
c10 <- read_excel("Ingresos trimestrales Power BI.xlsx", sheet = "Convenio 10%") %>% 
  rename_with(~ nombres_mes, .cols = 2:4)
bilateral <- read_excel("Ingresos trimestrales Power BI.xlsx", sheet = "Bilateral") %>% 
  rename_with(~ nombres_mes, .cols = 2:4)
fci <- read_excel("Ingresos trimestrales Power BI.xlsx", sheet = "FCI") %>% 
  rename_with(~ nombres_mes, .cols = 2:4)
descubierto <- read_excel("Ingresos trimestrales Power BI.xlsx", sheet = "Ints x desc") %>% 
  rename_with(~ nombres_mes, .cols = 2:4)



# Convertir a formato largo para ggplot
long_estandar <- pivot_longer(estandar, cols = all_of(nombres_mes),                 #all_of() le dice a tidyselect que use los nombres exactos del vector.
                        names_to = "Mes", values_to = "Monto") %>%
  mutate(Mes = factor(Mes, levels = nombres_mes),                                  #Ordena los niveles del factor Mes según el vector nombres_mes
          Unidad_de_negocio = "Estándar") %>% 
  filter(Monto > 0)
long_c10 <- pivot_longer(c10, cols = all_of(nombres_mes),                 
                    names_to = "Mes", values_to = "Monto") %>%
  mutate(Mes = factor(Mes, levels = nombres_mes),
         Unidad_de_negocio = "Convenio del 10") %>%
  filter(Monto > 0)
long_bilateral <- pivot_longer(bilateral, cols = all_of(nombres_mes),                
                    names_to = "Mes", values_to = "Monto") %>%
  mutate(Mes = factor(Mes, levels = nombres_mes),
         Unidad_de_negocio = "Bilateral") %>%
  filter(Monto > 0)
long_fci <- pivot_longer(fci, cols = all_of(nombres_mes),                 
                    names_to = "Mes", values_to = "Monto") %>%
  mutate(Mes = factor(Mes, levels = nombres_mes),
         Unidad_de_negocio = "FCI") %>%
  filter(Monto > 0)
long_descubierto <- pivot_longer(descubierto, cols = all_of(nombres_mes),
                                 names_to = "Mes", values_to = "Monto") %>%
  mutate(Mes = factor(Mes, levels = nombres_mes),
         Unidad_de_negocio = "Intereses por descubierto") %>%
  filter(Monto > 0)

#################################################################################################################################################################
# Unirlos en un solo data frame
unido <- bind_rows(long_estandar, long_c10, long_bilateral, long_fci, long_descubierto) %>%
  mutate(Monto = round(Monto, 0))  # Sin decimales

# Evitar notación científica en todo el script
options(scipen = 999)
##################################################################################################################################################################




                                






