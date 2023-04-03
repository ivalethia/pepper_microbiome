import pandas as pd
def porc_presen_set(file_path, grup_org, taxa, column_max_taxa, variable, value):
# Leer el archivo .csv
    df = pd.read_csv(file_path)
# Seleccionar un rango de columnas, desde la columna 48 hasta la 1247 para el archivo de BACTERIAS y de la 48 hasta la 211 para HONGOS para nivel de Especie; 48 - 293 para Bacterias con Familias y 48 - 117 para Hongos con Familias
    cols = df.columns[48:column_max_taxa]
# Crear una lista vacía para almacenar los resultados
    results = []
# Iterar sobre las columnas seleccionadas
    for col in cols:
# Calcular el porcentaje de celdas que cumplen la condición. Total de celdas que cumplen con el índice: Indicar el valor a buscar en la columna para el muestreo adecuado
        total_rows = df[df[variable] == value].shape[0]
# Total de celdas que tienen presencia de los microorganismos y cumplen con el valor condicional de la columna indicada
        condition = (df[col] > 0) & (df[variable] == value)
# Porcentaje de muestras que tienen presencia de microorganismos del grupo que cumplió con el valor condicional
        result = 100 * condition.sum() / total_rows
# Almacenar el resultado en la lista
        results.append(result)
# Convertir la lista de resultados en un DataFrame de Pandas
    results_df = pd.DataFrame({"column": cols, "result": results})
# Guardar el DataFrame en un archivo .csv
    results_df.to_csv(f'C:\fichero_con_datos\PORCENTAJE_{grup_org}_{taxa}_{variable}_{value}.csv', index=False)
# Corremos la función con todos sus parámetros
porc_presen_set("C:\fichero_con_datos\FAM_B_SR.csv", "BACTERIA", "ESPECIE", 1247, "Treatment", "Org")
