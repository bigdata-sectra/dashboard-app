heatmap_info <- gsub("\n", "","Las pestañas anteriores muestran mapas de calor del tiempo de viaje promedio 
para el rango de fechas seleccionadas y agrupados según el intervalo de tiempo seleccionado en la parte superior
de este dashboard. En la primera pestaña se muestra el mapa de calor considerando todos los datos disponibles, 
es decir, sin filtro de outliers. A su vez, en la segunda pestaña se muestra el mapa de calor quitando 
de los datos aquellos considerados como outliers según la metodología del rango intercuartil (comentada en la pestaña de información 
del panel inferior izquierdo).")
Encoding(heatmap_info) <- "UTF-8"

boxplot_info <- gsub("\n", "","Las pestañas de este panel muestran diagramas de caja y bigotes (boxplots) para la categoría 
correspondiente a la fecha seleccionada, la que viene dada según la forma en la que se decidió agrupar los valores, es decir, si se agrupa 
según tipo de día (laboral/sábado/domingo) o según día de la semana (L/M/X/J/V/S/D), y para cada intervalo de tiempo seleccionado en la 
parte superior de este dashboard. Cada intervalo de tiempo muestra un gráfico construido con los datos crudos, el que contiene una caja 
cuyos límites superior e inferior corresponden a los percentiles 75 (Q3) y 25 (Q1), respectivamente, siendo la barra del medio el percentil 
50 o mediana (Q2), y un bigote superior e inferior que representan los valores máximo y mínimo, respectivamente. Definiendo el rango 
intercuartil como IQ = Q3 - Q1, dichos valores máximo y mínimo corresponden al mayor dato que cumple con ser menor o igual a Q3 + 1.5IQ y 
al menor dato que cumple con ser mayor o igual a Q1 - 1.5IQ, respectivamente. Todos aquellos datos por sobre el valor máximo y por debajo del 
valor mínimo son considerados como valores atípicos.
En la primera ventana de este panel se muestran, en color gris, los boxplots para la categoría correspondiente al día seleccionado y para cada 
intervalo de tiempo seleccionado en la parte superior. A su vez, en color azul se muestra la demora total promedio del día seleccionado, 
en segundos/kilómetro, para cada intervalo de tiempo. Este gráfico es el mismo que el mostrado en la pestaña 'Agrupados (c/o)' del panel 
'Datos por Día'. En la segunda pestaña se muestran, en color azul, los boxplots y todos los datos que caen dentro de cada intervalo de tiempo 
para la categoría correspondiente al día seleccionado.
")
Encoding(boxplot_info) <- "UTF-8"