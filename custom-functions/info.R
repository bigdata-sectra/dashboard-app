heatmap_info <- gsub("\n", "","Las pesta�as anteriores muestran mapas de calor del tiempo de viaje promedio 
para el rango de fechas seleccionadas y agrupados seg�n el intervalo de tiempo seleccionado en la parte superior
de este dashboard. En la primera pesta�a se muestra el mapa de calor considerando todos los datos disponibles, 
es decir, sin filtro de outliers. A su vez, en la segunda pesta�a se muestra el mapa de calor quitando 
de los datos aquellos considerados como outliers seg�n la metodolog�a del rango intercuartil (comentada en la pesta�a de informaci�n 
del panel inferior izquierdo).")

boxplot_info <- gsub("\n", "","Las pesta�as de este panel muestran diagramas de caja y bigotes (boxplots) para la categor�a 
correspondiente a la fecha seleccionada, la que viene dada seg�n la forma en la que se decidi� agrupar los valores, es decir, si se agrupa 
seg�n tipo de d�a (laboral/s�bado/domingo) o seg�n d�a de la semana (L/M/X/J/V/S/D), y para cada intervalo de tiempo seleccionado en la 
parte superior de este dashboard. Cada intervalo de tiempo muestra un gr�fico construido con los datos crudos, el que contiene una caja 
cuyos l�mites superior e inferior corresponden a los percentiles 75 (Q3) y 25 (Q1), respectivamente, siendo la barra del medio el percentil 
50 o mediana (Q2), y un bigote superior e inferior que representan los valores m�ximo y m�nimo, respectivamente. Definiendo el rango 
intercuartil como IQ = Q3 - Q1, dichos valores m�ximo y m�nimo corresponden al mayor dato que cumple con ser menor o igual a Q3 + 1.5IQ y 
al menor dato que cumple con ser mayor o igual a Q1 - 1.5IQ, respectivamente. Todos aquellos datos por sobre el valor m�ximo y por debajo del 
valor m�nimo son considerados como valores at�picos.
En la primera ventana de este panel se muestran, en color gris, los boxplots para la categor�a correspondiente al d�a seleccionado y para cada 
intervalo de tiempo seleccionado en la parte superior. A su vez, en color azul se muestra la demora total promedio del d�a seleccionado, 
en segundos/kil�metro, para cada intervalo de tiempo. Este gr�fico es el mismo que el mostrado en la pesta�a 'Agrupados (c/o)' del panel 
'Datos por D�a'. En la segunda pesta�a se muestran, en color azul, los boxplots y todos los datos que caen dentro de cada intervalo de tiempo 
para la categor�a correspondiente al d�a seleccionado.
")