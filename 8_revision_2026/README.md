# Revisión numérica: plan y primeras correcciones (12 octubre 2026)

Este PR en borrador inicia la revisión; no contiene todavía el modelo final ni un nuevo total anual. Los resultados exploratorios del 9 de octubre sirven para elegir qué comprobar, no para sustituir las cifras del artículo.

## Croquis

Datos por lance → fechas y correspondencia de registros → modelo y validación geográfica → mapas diarios en unidades originales → esfuerzo de 365 días conservando horas → capturas e incertidumbre → áreas prioritarias → manuscrito y respuesta a revisores.

| Paso | Cambio en R | Criterio de cierre | Estado |
|---|---|---|---|
| 1. Entradas | Leer columnas por nombre; unir por código único; restaurar únicamente fechas de extracción verificadas con el código. Mantener esfuerzo corregido por el proveedor. | 194 lances por taxón; ningún cambio en otras columnas; registro de cada fecha modificada. | Implementado en `01_restore_dates.R`; 119 fechas Raja, 0 Scyliorhinus. |
| 2. Procedencia | Revisar coordenadas de un lance y diferencias en recuentos de Raja; contrastar fuentes originales. Normalizar estación solo tras acordar definición. | Cada diferencia resuelta o documentada; no revertir cambios deliberados. | Pendiente. |
| 3. Validación | Crear grupos geográficos comunes por lance, sin el error `f`/`folds`; elegir hiperparámetros y calibración usando solo entrenamiento de cada partición externa. | Evaluación externa por zona, sin compartir lances; tamaños y positivos documentados. | Pendiente de validación anidada. |
| 4. Media | Comparar modelo positivo en unidades originales con log1p y corrección de retransfomación estimada dentro de entrenamiento; valorar error y calibración por zona. | Media positiva y media total con interpretación correcta; comportamiento defendible en Raja. | Función de combinación y comprobación matemática implementadas; selección pendiente. |
| 5. Mapas | Profundidad positiva como en entrenamiento; máscara marina y dominio explícitos; invertir la transformación antes de combinar con probabilidad. | Valores de prueba concordantes con R; extrapolación cuantificada. | Diagnóstico previo disponible; integración pendiente. |
| 6. Incertidumbre | Muestrear lances conjuntamente para ambas partes; combinar e integrar dentro de cada réplica. Evaluar dependencia espacial y repetir la calibración cuando proceda. | Percentiles de la cantidad final; no multiplicar amplitudes de intervalos. | Ensayo exploratorio de 100 réplicas disponible; diseño final pendiente. |
| 7. GFW | Intervalos mensuales [primer día, primer día siguiente); versión explícita. Reasignar horas conservando masa, no interpolar bilinealmente cantidades extensivas. | 365 fechas cubiertas por consultas válidas; ausencia de filas no confundida con descarga fallida; horas conservadas antes de máscara. | Ventanas corregidas implementadas; doce días descargados como candidato v4 separado. |
| 8. Integración | Sumar BPUE × área barrida por celda y día, con unidades comprobadas, dentro de cada réplica. | Total anual solo tras cerrar modelo, dominio, esfuerzo completo y versión. | Pendiente. |
| 9. López / gestión | Comparar prioridades de ocurrencia y abundancia a igual área protegida; añadir Raja y escenarios conjuntos. | Fracción de captura potencial cubierta a 5/10/20 % del área; incertidumbre y sensibilidad. | No hay polígonos/raster de López: no presentar comparación interna como solape externo. |
| 10. Documentos | Actualizar resultados, comentarios de pendientes y respuesta a revisores a partir de salidas finales. | Cada comentario distingue resuelto, parcial y pendiente; control de cambios limitado a caracteres modificados. | Pendiente de resultados finales. |

## Qué significan los problemas de fechas

Las fechas utilizadas al extraer las variables ambientales concuerdan con las codificadas en los lances. En la tabla final de Raja, 119 fechas tienen un año distinto; se restauran desde la extracción por clave, sin copiar filas completas. Cambiar únicamente esas fechas no modifica predicciones ni RMSE de los modelos ya guardados: fecha y estación no forman parte de sus predictores seleccionados. Sí puede afectar análisis agrupados por año/estación, validación temporal y cualquier nueva extracción. El código original además convierte fechas día/mes/año sin declarar formato; se sustituye por lectura explícita.

La auditoría no permite afirmar que todo lo demás sea idéntico: hay un lance con coordenadas distintas, 11 recuentos de Raja distintos (y sus abundancias; siete cambian presencia), 58 áreas barridas antes ausentes ahora completas, pérdida de las 194 horas/fechas completas en Raja y diferencias de codificación/definición de estación. Las nueve diferencias de esfuerzo logarítmico se mantienen por indicación del autor. La posición/profundidad identifica el mismo código en 193/194 registros por taxón; el restante es el lance con coordenadas distintas. No hay indicios de una permutación general, pero falta rastrear estos cambios puntuales. No se restaura `date_time` sin comprobar su procedencia.

## Modelo de media, explicado con un ejemplo

La primera parte pregunta «¿aparecerán cápsulas?». La segunda pregunta «si aparecen, ¿cuántas por km²?». Si aparecen en la mitad de los casos y, cuando aparecen, la media es 50, la media de todos los casos es 25. Esta es la cantidad que necesitamos para sumar capturas potenciales.

Tomar logaritmos para ajustar el modelo es razonable. Pero deshacer el logaritmo de una media no devuelve la media original. Con capturas positivas de 1 y 99, la media es 50; la inversa del promedio de log1p es aproximadamente 13,14. Multiplicada por probabilidad 0,5 da 6,57, y aplicar la inversa después de multiplicar da aproximadamente 2,76. Las dos operaciones requieren atención: el orden de combinación y el objetivo de media aritmética.

Una corrección de retransfomación puede ayudar: `p * (exp(mu) * S - 1)`. Pero `S` debe estimarse sin usar los datos reservados para evaluar y debe ser adecuada para las condiciones donde se predice. Un único factor global no garantiza medias correctas en cada zona.

«Generalizar entre regiones» significa reservar una zona completa, entrenar con las demás y comprobar las predicciones de esa zona sin haber utilizado sus capturas. No exige cambiar las regiones del estudio ni añadir observaciones. Pregunta si un patrón aprendido en una parte de la costa funciona en otra.

En Raja, la corrección exploratoria fue alrededor de 2,04 al repartir lances aleatoriamente y 7,46 en el diagnóstico geográfico. Además, la versión corregida empeoró el RMSE geográfico frente a la inversión simple (aproximadamente 131 frente a 119 cápsulas/km²). Algunas capturas muy altas condicionan la corrección. Esto no identifica todavía el mejor modelo ni justifica multiplicar todo por 7,46. Se evaluarán alternativas con la misma partición externa y selección interna; el ensayo previo conserva hiperparámetros elegidos anteriormente y no constituye validación anidada completa.

## ECEME: contexto biológico descriptivo

La hoja Elasmobranquios contiene 987 cápsulas de 49 códigos de lance. Se considera evidencia de contenido embrionario todo estado numerado (incluida la errata «Etstado 2») o «Indeterminable (podrido)», según el criterio del autor. «Estado ?» e «Indeterminable» quedan como desconocidos. «Indeterminable (vacío)» se contabiliza como registrado vacío. Podrido no significa embrión vivo.

| Grupo | Cápsulas | Vacías | Estado numerado | Podrido | Desconocido | Vacías % | Con embrión/contenido % |
|---|---:|---:|---:|---:|---:|---:|---:|
| Scyliorhinus canicula | 881 | 538 | 147 | 188 | 8 | 61,07 | 38,02 |
| Raja spp. | 101 | 98 | 3 | 0 | 0 | 97,03 | 2,97 |
| Todas las especies | 987 | 639 | 150 | 190 | 8 | 64,74 | 34,45 |

Denominador: todas las cápsulas del grupo, incluidos desconocidos. Son proporciones observadas en este subconjunto, no estimaciones representativas de todo el estudio ni una corrección que deba multiplicar automáticamente los mapas. Fuente: workbook del TFM facilitado por el autor, hoja Elasmobranquios; original sin modificar. Las otras cinco cápsulas corresponden a Dipturus (3) y Galeus (2).

## Ejecutar lo ya implementado

Desde la raíz del repositorio:

```r
source('8_revision_2026/check_contracts.R')
```

Para las entradas reales, ejecutar `Rscript 8_revision_2026/01_restore_dates.R <raíz_local_de_datos>`.
Las copias corregidas y el registro por lance se guardan en `local_outputs/`, excluido de Git. Los originales se conservan. El cambio de fecha no altera columnas, orden, recuentos ni esfuerzo. El resto del plan se incorporará por commits separados a este PR en borrador, sin fusionarlo mientras queden decisiones científicas abiertas.

Documentación de GFW: [intervalo final excluido](https://github.com/GlobalFishingWatch/gfwr/blob/main/R/gfw_ais_fishing_hours.R), [versiones de datos](https://globalfishingwatch.org/our-apis/documentation/docs/v3/general-api-doc/key-concepts), [informes](https://globalfishingwatch.org/our-apis/documentation/docs/v3/4wings/report). Los doce días recuperados corresponden a `public-global-fishing-effort:v4.0` y al ámbito de integración; no se han mezclado con los 353 días históricos de versión no documentada. Para cerrar el año habrá que recuperar una serie consistente de 365 días o demostrar la equivalencia de versiones.
