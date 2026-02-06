(5^2+5)/3 

log10(90+5*2) 

sum(50, 3, 5) 

sum(log(32,2), 8^4, 4+5, 12/3)

variable2 <- c(12,1,15,3,20,6,11)
variable2
mean(variable2)

variable2 +2

height <- c(150, 150, 142, 154, 168, 153, 151, 153, 142, 151)
height

height[6]

color <- c("red","blue", "yellow", "green", "red","blue", "yellow", "green", "blue", "yellow")
color

Survey = c("True", "False", "True", "False","True", "False", "True", "False", "True", "False")
Survey

table <- data.frame(height, color, Survey)
table

df <- data.frame(sample_id = c('i', 'ii', 'iii', 'iv'), x = c(1,2,3,4), y = c(5, 6, 7, 8))
df

sample_id <- c('i', 'ii', 'iii', 'iv')
y = c(5, 6, 7, 8)
x = c(1,2,3,4)
sample_id
x
y
df <- data.frame(sample_id, x,y)
df

#checking data type variables
str(df)
str(table)

name <- "Cathrine"
name
age <- 26
age

#Combining different data
text1 <- "R is"
text2 <- "awesome"

paste(text1, text2)


#creating matrix
thisMatrix = matrix(c(1:6), nrow = 3, ncol = 2)
thisMatrix


# Create a data frame
Data_Frame <- data.frame (Training = c("Strength", "Stamina", "Other"), Pulse = c(100, 150, 120),Duration = c(60, 30, 45)
)

# Print the data frame
Data_Frame
Data_Frame$Col4 <- c(20,45,58)
Data_Frame


#creating arrays
thisarray <- c(1:24)
thisarray

# An array with more than one dimension
multiarray <- array(thisarray, dim = c(4, 3, 2))
multiarray









