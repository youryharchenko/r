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
par(mfrow=c(2,1)) # встановлюємо розмішення графіків (2 по вертикалі, 2 по горизонталі) )
plot(x,y) # виводимо графік для x та y
hist(y)   # виводимо гістограму для y
par(mfrow=c(1,1)) # встановлюємо розмішення графіків (1 по вертикалі, 1 по горизонталі) )

### Reading in data
Auto <- read.csv("Auto.csv") # завантажує csv-файл в data.frame Auto

#Names of the object
names(Auto) # друкує список імен стовпчиків

#Dimension of data.frame 
dim(Auto) # друкує розмірність data.frame Auto

#Class of the data.frame
class(Auto) # друкує class data.frame Auto

#Summary of object
summary(Auto) # друкує статистики для числових стовпчиків data.frame Auto

#Plot relation
plot(Auto$cylinders,Auto$mpg) # виводить графік, де x - стовпчик cylinders, а y - стовпчик mpg

attach(Auto) # приєднує data.frame Auto до списку пошуку імен зміних

#Active library
search() # друкує список пошуку імен

#Plot bar plot
plot(cylinders,mpg) # імена стовпчиків можна використовувати без кфаліфікації об'єктом
cylinders=as.factor(cylinders) # перетворює вектор cylinders на фактор
plot(cylinders,mpg,xlab="cylinders",ylab="mpg",col="red") # виводить графік з cylinders як факторм

#Plot to .png file
png(file="mpg.png") # відкриває файл для графіку в форматі png
plot(cylinders,mpg,xlab="cylinders",ylab="mpg",col="red") # виводить графік 
dev.off() # записує та закриває файл

#Pair plot
# pairs(Auto,col="brown") видає помилку Error in pairs.default(Auto, col = "brown") : non-numeric argument to 'pairs'
pairs(mpg~cylinders+acceleration+weight,Auto) # виводить попарні матриці розсіяння