***Lab 3***

В лабораторній роботі необхідно написати наступні функції на мові R та вивести результат роботи цих функцій на довільних даних:

**1. Функція add2(x, y), яка повертає суму двох чисел.**
```
add2 <- function(x, y) {
    x+y
  }
print (add2(8, 4)) 
```
Результат:

___
[1] 12
___

**2. Функція above(x, n), яка приймає вектор та число n, та повертає всі елементи вектору, які більше n. По замовчуванню n = 10.**
```
above <- function(x, n=10) {
    x[x>n]
  }
print (above(c(1, 2, 5, 11, 8, 45, 12, 10)))
```
Результат:
___
[1] 11 45 12
___

**3. Функція my_ifelse(x, exp, n), яка приймає вектор x, порівнює всі його елементи за допомогою exp з n, та повертає елементи вектору, які відповідають умові expression. Наприклад, my_ifelse(x, “>”, 0) повертає всі елементи x, які більші 0. Exp може дорівнювати “<”, “>”, “<=”, “>=”, “==”. Якщо exp не співпадає ні з одним з цих виразів, повертається вектор x.**
```
my_ifelse <- function(x, exp, n) {
      res <- x
      if (exp == "<") {
        res <- x[x < n]
      }
      else if (exp == ">") {
        res <- x[x > n]
      }
      else if (exp == "<=") {
        res <- x[x <= n]
      }
      else if (exp == ">=") {
        res <- x[x >= n]
      }
      else if (exp == "==") {
        res <- x[x == n]
      }
      res
    }
print (my_ifelse(c(1, -2, 5, -11, -8, -45, 11, 10), ">", 0))
print (my_ifelse(c(3, 4, 5, 2.15, -1, 0.2), "<", 3.14))
```
Результат:
___
```
>     print (my_ifelse(c(1, -2, 5, -11, -8, -45, 11, 10), ">", 0))
[1]  1  5 11 10
>     print (my_ifelse(c(3, 4, 5, 2.15, -1, 0.2), "<", 3.14))
[1]  3.00  2.15 -1.00  0.20
```
___

**4. Функція columnmean(x, removeNA), яка розраховує середнє значення (mean) по кожному стовпцю матриці, або data frame. Логічний параметр removeNA вказує, чи видаляти NA значення. По замовчуванню він дорівнює TRUE.**
```
columnmean <- function(x, removeNA=TRUE) {
      res <- matrix(nrow = 1, ncol = length(x[1,]))
      colnames(res) <- colnames(x)
      for(i in 1:length(x[1,])){
        if (removeNA == TRUE) {
          res[1, i] <- mean(x[,i], na.rm=TRUE)
        }
        else{
          res[1, i] <- mean(x[,i])
        }
      }
      res
    }
    
m <- matrix(data=cbind(rnorm(10, 0), c(1,1,NA,1,1,1,1,1,1,1), rnorm(10, 5)), nrow=10, ncol=5)
df <- data.frame(col1 = rnorm(10, 0),col2 = c(1,1,NA,1,1,1,1,1,1,1), col3=rnorm(10, 5))
print(columnmean(m, FALSE))
print(columnmean(df))
```
Результат:
___
```
>     print(columnmean(m, FALSE))
            [,1] [,2]     [,3]        [,4] [,5]
[1,] -0.03012522   NA 5.077997 -0.03012522   NA
>     print(columnmean(df))
           col1 col2     col3
[1,] -0.3411177    1 4.948385
```
___
