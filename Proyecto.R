library(readxl)

# Lectura de datos
excelM <- "C:/Users/mocor/Documents/MAQUINAS.xlsx"
MatVia <- as.matrix(read_excel(excelM, sheet = 'Hoja1', range = "A8:E13"))
MatDis <- as.matrix(read_excel(excelM, sheet = 'Hoja1', range = "A15:E20"))

# Validación de datos
cat("Dimensiones MatVia:", dim(MatVia), "\n")
cat("Dimensiones MatDis:", dim(MatDis), "\n")
print(MatVia)
print(MatDis)

# Inicialización de variables
NumMaq <- nrow(MatVia)
NumLug <- NumMaq

Conj_Maq <- seq(1, NumMaq)
Conj_Lug <- seq(1, NumLug)
MatAsig <- matrix(NA, nrow = NumMaq, ncol = 1)

# Selección inicial de máquina
rs <- sample(1:NumMaq, size = 1)
F1 <- c(rs)
cat("Primera máquina seleccionada en F1 es:", F1, "\n")

# Maquinas y lugares restantes
Maq_Res <- setdiff(Conj_Maq, F1)
Lug_Res <- setdiff(Conj_Lug, 1)
MatAsig[F1, 1] <- 1

# Bucle principal de asignación
while (length(Maq_Res) > 0) {
  cat("Maquinas por asignar Maq_Res:", Maq_Res, "\n")
  cat("Lugares por asignar Lug_Res:", Lug_Res, "\n")
  
  # Selección de nueva máquina
  NuevaMaq <- if (length(Maq_Res) == 1) Maq_Res else sample(Maq_Res, size = 1)
  cat("Nueva Máquina seleccionada:", NuevaMaq, "\n")
  
  # Inicialización de matrices de costos
  costo <- matrix(0, nrow = NumLug, ncol = NumMaq)
  costoT <- matrix(10e10, nrow = NumLug, ncol = 1)
  
  # Cálculo de costos para cada lugar
  for (r in Lug_Res) {
    for (k in F1) {
      if (!is.na(MatAsig[k])) {
        costo[r, k] <- MatVia[NuevaMaq, k] * MatDis[r, MatAsig[k]]
        cat("Para r =", r, "y k =", k, 
            "El costo es:", costo[r, k], "\n")
      }
    }
    costoT[r, 1] <- sum(costo[r, ], na.rm = TRUE)
  }
  
  cat("Matriz de costos:", costo, "\n")
  cat("Matriz costoT:", costoT, "\n")
  
  # Selección del lugar con menor costo
  CostoMin <- min(costoT)
  rs <- which(costoT == CostoMin)[1]  # Seleccionar el primero en caso de empate
  
  cat("Lugar seleccionado:", rs, "\n")
  
  # Actualización de listas y matrices
  F1 <- c(F1, NuevaMaq)
  MatAsig[NuevaMaq, 1] <- rs
  Maq_Res <- setdiff(Conj_Maq, F1)
  Lug_Res <- setdiff(Lug_Res, rs)
  
  cat("Máquinas seleccionadas F1:", F1, "\n")
  cat("Lugares seleccionados MatAsig:", MatAsig, "\n")
}

# Resultado final
cat("Asignación Final de Máquinas a Lugares:\n")
print(MatAsig)
