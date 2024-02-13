# Repositorio ECV proyecto empiricA

# Preliminares --------------------------------------------------------------------------------------------

# Cargar librerias

if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(forcats)) install.packages("forcats", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(patchwork)) install.packages("patchwork", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(car)) install.packages("car", repos = "http://cran.us.r-project.org")
if(!require(survey)) install.packages("survey", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("survey", repos = "http://cran.us.r-project.org")
if(!require(openxlsx)) install.packages("openxlsx", repos = "http://cran.us.r-project.org")

# Cargando datos ------------------------------------------------------------------------------------------

ecv_persona <- read_csv("DATA/02_ecv6r_personas.csv")

ecv_capital_social <- read_csv("DATA/12_ecv6r_capital_social.csv")

ecv_gastos_alimentos <- read_csv("DATA/04_ecv6r_gastos_alimentos.csv")

ecv_otros_gastos <- read_csv("DATA/05_ecv6r_otros_gastos.csv")

# Definir las variables deseadas de la base ecv_persona

ecv_persona_def <- ecv_persona %>%
  select("horas_trabajadas_semanal" = "PA73",
         "exp_laboral" = "PA17A",
         "sexo" = "SEXO",
         "edad" = "EDAD",
         "ciudad" = "CIUDAD_AUTO",
         "estado_civil" = "PD19",
         "Nivel_educacion" = "PE47",
         "identificacion_etnia" = "PD18",
         "grupo_ocupacion" = "PA22",
         "rama_actividad" = "PA16",
         "parentesco_jefe" = "PD06",
         "provincia" = "PROVINCIA",
         "parentesco" = "PD04",
         "salario" = "PA37",
         "FEXP", "IDENTIF_HOG") %>%
  mutate(exp_laboral = as.numeric(exp_laboral)) %>%
  filter(edad >= 20, edad <= 70, 
         horas_trabajadas_semanal > 0,
         exp_laboral > 0, parentesco == "Jefe",
         !is.na(salario)
  )

# Salarios de todos los miembro de la familia 

ecv_ing_fam <- ecv_persona %>%
  select("IDENTIF_HOG",
         "salario" = "PA37") %>%
  filter(!is.na(salario)) %>%
  group_by(IDENTIF_HOG) %>%
  summarise(ing_fam = sum(salario, na.rm = TRUE))

# Unir las bases de ecv_ing_fam y ecv_persona

ecv_persona_def <- inner_join(ecv_persona_def, ecv_ing_fam, by = "IDENTIF_HOG")

  
# Definir las variables deseadas de la base ecv_capital_social

ecv_capital_social_def <- ecv_capital_social %>%
  select("satisfaccion" = "CS1914",
         "IDENTIF_HOG")

# Unir las bases de ecv_capital_social y ecv_persona

df_ecv_ing <- inner_join(ecv_persona_def, ecv_capital_social_def, by = "IDENTIF_HOG")

# Definir las variables de la base de gasto en alimentos

ecv_gastos_alimentos <- ecv_gastos_alimentos %>%
  select("gasto_total_a" = "GA07",
         "IDENTIF_HOG") %>%
  filter(!is.na(gasto_total_a)) %>%
  group_by(IDENTIF_HOG) %>%
  summarise(gasto_total_a = sum(gasto_total_a, na.rm = TRUE))

# Definir las variables de la base de otros gastos

ecv_otros_gastos_f <- ecv_otros_gastos %>%
  select("GA0102", "GA0202","GA0302","GA0402","GA0502","GA0602","GA0702","GA0802",
         "GB0102","GB0202","GB0302","GB0402","GB0502","GB0602","GB0702","GB0802",
         "GB0902","GB1002","GB1102","GB1202","GB1302","GB1402","GB1502","GB1602",
         "GB1702","GB1802","GB1902","GB2002","GB2102","GB2202","GB2302","GB2402",
         "GB2502","GB2602","GB2702","GB2802","GB2902","GB3002","GB3102","GB3202",
         "GB3302","GB3402","GB3502","GB3602","GB3702","GB3802","GC0102",
         "GC0202","GC0302","GC0402","GC0502","GC0602","GC0702","GC0802","GC0902",
         "GC1002","GC1102","GC1202","GC1302","GC1402","GC1502","GC1602","GC1702",
         "GC1802","GC1902","GC2002","GC2102","GC2202","GC2302","GC2402","GC2502",
         "GC2602","GC2702","GC2802","GC2902","GC3002","GD0102","GD0202","GD0302",
         "GD0402","GD0502","GD0602","GD0702","GD0802","GD0902","GD1002","GD1102",
         "GD1202","GD1302","GD1402","GD1502","GD1602","GD1702","GD1802","GD1902",
         "GD2002","GD2102","GD2202","GD2302","GD2402","GD2602",
         "GD2802","GD2902","GD3002","GD3102","IDENTIF_HOG") %>%
  mutate(GC1002 = as.numeric(GC1002),
         GC1702 = as.numeric(GC1702),
         GC1902 = as.numeric(GC1902),
         GC2102 = as.numeric(GC2102),
         GC2702 = as.numeric(GC2702))

ecv_otros_gastos_g <- ecv_otros_gastos_f %>%
  mutate_if(is.numeric, as.numeric) %>%
  mutate(gasto_total_o = rowSums(select(., -IDENTIF_HOG), na.rm = TRUE)) %>%
  group_by(IDENTIF_HOG) %>%
  select("gasto_total_o","IDENTIF_HOG")

# Definir las variables de la base de gasto en alimentos y otros gastos 

df_ecv_gasto <- inner_join(ecv_gastos_alimentos,ecv_otros_gastos_g, by = "IDENTIF_HOG" ) %>%
  mutate(consumo = rowSums(select(., -IDENTIF_HOG), na.rm = TRUE))

# Definir las variables de la base total

df_ecv_total <- inner_join(df_ecv_ing,df_ecv_gasto, by = "IDENTIF_HOG") %>%
  mutate(ingreso_brack = case_when(between(ing_fam, 10, 200) ~ 'ingreso_entre_10_200',
                                   between(ing_fam, 201, 400) ~ 'ingreso_entre_201_400',
                                   between(ing_fam, 401, 600) ~ 'ingreso_entre_401_600',
                                   between(ing_fam, 601, 800) ~ 'ingreso_entre_601_800',
                                   between(ing_fam, 801, 1000) ~ 'ingreso_entre_801_1000',
                                   between(ing_fam, 1001, 1200) ~ 'ingreso_entre_1001_1200',
                                   between(ing_fam, 1201, 1400) ~ 'ingreso_entre_1201_1400',
                                   between(ing_fam, 1401, 1600) ~ 'ingreso_entre_1401_1600',
                                   between(ing_fam, 1601, 1800) ~ 'ingreso_entre_1601_1800',
                                   between(ing_fam, 1801, 2000) ~ 'ingreso_entre_1801_2000',
                                   ing_fam >= 2001 ~ 'mas_de_2000')) %>%
  mutate(ingreso_brack = as.factor(ingreso_brack))

# ing_relativo -----

ing_relativo <- df_ecv_total %>%
  group_by(ingreso_brack) %>% 
  summarise(avg_ing = mean(salario, na.rm = TRUE))

# theme -----

theme <-
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.caption = element_text(hjust = 0, face = 'italic'),
        legend.background = element_rect(fill="white", 
                                         size=0.5, 
                                         linetype="solid", 
                                         colour ="black"),
        text =  element_text(color = 'black', 
                             size = 14),
        axis.text = element_text(size = 12,
                                 color = 'black'))

# Reordenar los niveles de la variable categórica ingreso_brack de mayor a menor frecuencia y graficar
df_ecv_total$ingreso_brack <- factor(df_ecv_total$ingreso_brack, levels = names(sort(table(df_ecv_total$ingreso_brack), decreasing = TRUE)))

graf_hist_ingreso <- ggplot(df_ecv_total, aes(x = ingreso_brack)) +
  geom_bar(width = 0.8,
           fill = "#647A8F",
           position = "dodge",
           color = "black") +
  labs(x = "salarios_brackets",
       y = "frecuencia",
       title = "") +
  theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, color = 'black'),
        axis.text.y = element_text(size = 12, colour = 'black'),
        plot.caption.position = 'plot')

# Base y grafico consumo promedio vs salario

df_ecv_ig_cs <- df_ecv_total %>%
  group_by(ingreso_brack) %>%
  summarise(avg_consumo = mean(consumo, na.rm = TRUE))

df_ecv_ig_cs$ingreso_brack <- factor(df_ecv_ig_cs$ingreso_brack, levels = names(sort(table(df_ecv_ig_cs$ingreso_brack), decreasing = TRUE)))


graf_ecv_ig_cs <- ggplot(df_ecv_ig_cs, aes(x = fct_reorder(ingreso_brack,avg_consumo), y = avg_consumo)) +
  geom_point() +
  labs(x = "salario",
       y = "consumo promedio",
       title = "salario vs consumo promedio") +
  theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, color = 'black'),
        axis.text.y = element_text(size = 12, colour = 'black'),
        plot.caption.position = 'plot')

# Exportar df_ecv_total a Excel

write.xlsx(df_ecv_total, "df_ecv_total.xlsx")
  
  
