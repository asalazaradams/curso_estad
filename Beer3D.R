# Load libraries
library(plotly)
library(dplyr)
data=read.csv("https://raw.githubusercontent.com/asalazaradams/curso_estad/main/beer.csv")



# Fit the multiple regression model
model <- lm(Q ~ PB + I, data = data)
summary(model)  # Optional: Check coefficients

# Create a grid of PB and I values for the regression surface
PB_range <- seq(min(data$PB), max(data$PB), length.out = 20)
I_range <- seq(min(data$I), max(data$I), length.out = 20)
grid <- expand.grid(PB = PB_range, I = I_range)

# Predict Q values based on the model
grid$Q_pred <- predict(model, newdata = grid)

# Convert predictions to a matrix for the surface plot
Q_pred_matrix <- matrix(grid$Q_pred, nrow = length(PB_range), ncol = length(I_range))

# Create the interactive 3D plot with plotly
p <- plot_ly() %>%
  
  # Add the original data points as a scatter plot
  add_markers(data = data, x = ~PB, y = ~I, z = ~Q, 
              marker = list(size = 5, color = "blue"), name = "Data") %>%
  
  # Add the regression surface using the grid ranges and matrix
  add_surface(x = PB_range, y = I_range, z = Q_pred_matrix,
              colorscale = "Viridis", opacity = 0.7, name = "Regression Plane") %>%
  
  # Customize layout
  layout(
    title = "3D Interactive Multiple Regression",
    scene = list(
      xaxis = list(title = "PB"),
      yaxis = list(title = "I"),
      zaxis = list(title = "Q"),
      camera = list(eye = list(x = 1.5, y = 1.5, z = 0.5))  # Adjust camera angle
    )
  )

# Display the plot
p
