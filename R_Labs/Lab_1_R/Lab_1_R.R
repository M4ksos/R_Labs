#1
l <- TRUE
print (l)
#2
i <- 42
print(i)
#3
f <- 3.1234
print(f)
#4
s <- "hello"
print(s)
#5
v <- c(1, 2, 3, 4, 5);
print(v)
#6
sq <- 1:20
print(sq)
#7
set.seed(123)
rand_vec <- runif(10, min=-5, max=15)
print(rand_vec)
#8
subset_vec <- rand_vec[rand_vec > 1 & rand_vec < 10]
print(subset_vec)
#9
max_val <- max(rand_vec)
min_val <- min(rand_vec)
cat("Maximum:", max_val, "Minimum", min_val, "\n")
#10
seq_step <- seq(from = -15, to = 15, by = 0.1)
print(seq_step)
#11
f <- function(x) {
  5 * sin(x^2)
}

f(0)
f(pi)
#12
x_vals <- seq(-15,15,by=0.01)
y_vals <- f(x_vals)
plot(x_vals,y_vals, 
     type="l", 
     col = "red", 
     lty = 2, 
     pch = 16, 
     xlab = "x", 
     ylab = "5 * sin(x^2)", 
     main = "График функции")
#13
xs <- seq(-5,5,by=0.2)
ys <- seq(-5,5,by=0.2)

surface_func <- function(x,y) {
  (x/2) * exp(-(x^2 + y^2) / 2)
}

z_vals <- outer(xs,ys,surface_func)

persp(xs, ys, z_vals, 
      main = " f(x,y) = x/2 * exp(-(x^2+y^2)/2)", 
      zlab = "f(x,y)",
      theta = 30,
      phi = 15,
      col = "springgreen",
      shade = 0.5,
      ticktype = "detailed",
      nticks = 5)

#14
mat <- matrix(1:100, nrow = 10, ncol = 10)
print(mat)
#15
cat("rows:", nrow(mat), "cols:", ncol(mat), "\n")
#16
arr <- array(1:30, dim = c(5,2,3))
print(arr)
#17
my_list <- list(
  logical_val = TRUE,
  Integer_val = 100,
  double_val = 3.1234,
  char_val = "c"
)
print(my_list)
#18
chisla_vec <- rep(c("one","two","three","four","five"), each = 2)
chisla_factor <- factor(chisla_vec)
print(chisla_factor)
#19
levels(chisla_factor)
#20
table(chisla_factor)
unclass(chisla_factor)
#21
transport_df <- data.frame(
  type = c("bus", "motocycle", "bike", "car", "tramvai", "train", "plane"),
           wheels = c(6L, 2L, 2L, 4L, NA,NA, 6L),
           speed = c(60, 140, 40, 140, 30, 60, 1000),
           electric = c(F, F, F, F, T,T,F),
           stringAsFactors = FALSE
           )
print(transport_df)
#22
last_val <- transport_df[nrow(transport_df),
                         ncol(transport_df)]
print(last_val)
#23
first_row <- transport_df[1, ]
last_row <- transport_df[nrow(transport_df), ]
print(first_row)
print(last_row)
#24
transport_df[["type"]]
#25
transport_df$type
#26
even_rows <- transport_df[seq(2,nrow(transport_df),2),]
print(even_rows)
#27
even_cols <- transport_df[, seq(2,ncol(transport_df), 2),]
print(even_cols)
#28
first4 <- transport_df[1:4, ]
print(first4)
#29
electric_only <- transport_df[transport_df$electric == TRUE, ]
print(electric_only)
#30
selected_types <- transport_df[transport_df$type %in% c("car", "bus"), ]
print(selected_types)


