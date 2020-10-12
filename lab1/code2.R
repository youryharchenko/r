### vectors, data, matrices, subsetting
#Vectors
x = c(2,7,5)
x
y=seq(from=4,length=3,by=3) # генерує послідовність з 3-х чисел починаючи з 4-х з кроком 3
?seq # help по функції seq
y

#Operations on vectors
x+y # покомпонентне додавання
x/y # покомпонентне ділення
x^y # покомпонентна степінь

#Subsetting
x[2]   # друга
x[2:3] # з другої по третю
x[-2]  # без другої
x[-c(1,2)] # без першої та другої

#Matrices
z=matrix(seq(1,12),4,3) # створюємо матрицю - 4 рядка, 3 стовпчика, заповнюємо по стовбцях чичлами від 1 до 12
z

#Subsetting
z[3:4,2:3] # вибираємо елементи на перетині 3:4 рядки та 2:3 стовпчики
z[,2:3]    # вибираємо елементи на перетині всі рядки та 2:3 стовпчики
z[,1]      # вибираємо перший стовпчик, як вектор
z[,1,drop=FALSE] # вибираємо перший стовпчик, як вектор-стовпчик

#Dimension of matrix
dim(z) # розмірності матриці

#Variables in environment
ls() # список всіх змінних

#Remove variable from environment
rm(y) # видалити змінну y
ls()

### Generating random data, graphics

#Generating uniformly distributed variable
x=runif(50) # 50 випадкових рівномірнорозподілених чисел (0, 1)

#Generating normaly distributed variable
y=rnorm(50) # 50 випадкових нормальнорозподілених чисел

#Plot variables
plot(x,y) # графік для x та y
plot(x,y,xlab="Random Uniform",ylab="Random Normal",pch="*",col="blue") # графік для x та y з підписами синім кольором
points(x[1:2], y[1:2], col = "red") # дві перші точки в червоний колір

#Pair-plots
par(mfrow=c(2,1))
plot(x,y)
hist(y)
par(mfrow=c(1,1))

### Reading in data
Auto <- read.csv("Auto.csv", sep = "\t")

#Names of the object
names(Auto)

#Dimension of data.frame 
dim(Auto)

#Class of the data.frame
class(Auto)

#Summary of object
summary(Auto)

#Plot relation
plot(Auto$cylinders,Auto$mpg)

attach(Auto)

#Active library
search()

#Plot bar plot
plot(cylinders,mpg)
cylinders=as.factor(cylinders)
plot(cylinders,mpg,xlab="Cylinders",ylab="Mpg",col="red")

#Plot to .png file
png(file="mpg.png")
plot(cylinders,mpg,xlab="Cylinders",ylab="Mpg",col="red")
dev.off()

#Pair plot
pairs(Auto,col="brown")
pairs(mpg~cylinders+acceleration+weight,Auto)