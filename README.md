Código en R para interpolar datos a través de la función approx() del paquete base stats y la función splinefun() de la librería splines. Adicionalmente, el código utiliza las librerías dplyr para la manipulación y ordenación de datos y ggplot2 para su visualización.
Codigo en R para realizar interpolación ponderada
        👉 No usamos approx ni spline porque esas técnicas suavizan demasiado o inventan valores intermedios, se busca 
          conservar la forma y variabilidad real de las curvas para compararlas con los eventos climáticos.
        👉 La interpolación ponderada fue un punto medio entre precisión y robustez, más adecuada para datos heterogéneos de 
           distintos programas.
        👉 La interpolación ponderada / asignación por cercanía.  Cada punto en la escala de 
           tiempo se calculaba como una mediana ponderada de los valores más próximos, usando su proximidad temporal como 
           peso.
           Permite integrar curvas con resoluciones distintas sin sobre–ajustar.


