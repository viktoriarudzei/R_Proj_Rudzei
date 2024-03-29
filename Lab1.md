***Lab 1***

**1.Створити змінні базових (atomic) типів. Базові типи: character, numeric, integer, complex, logical.**

```
character <- 'Viktoria' 
numeric <- 1.07
integer <- 2000
complex <- 10i
logical <- TRUE
```

**2. Створити вектори, які: містить послідовність з 5 до 75; містить числа 3.14, 2.71, 0, 13; 100 значень TRUE.**

```
vect1 <- 5:75
vect2 <- c(3.14, 2.71, 0, 13)
vect3 <- rep(TRUE, 100)
```
**3. Створити наступну матрицю за допомогою matrix, та за допомогою cbind або rbind**
| 0.5 | 1.3 | 3.5 |
|---|---|---|
| 3.9 | 131 | 2.8 |
| 0   | 2.2 | 4.6 |
| 2   | 7   | 5.1 |

Матрицю за допомогою matrix:
```
matrix(c(0.5, 3.9, 0, 2, 1.3, 131, 2.2, 7, 3.5, 2.8, 4.6, 5.1),nrow=4, ncol=3)
```

Використання cbind:

```
col1 <- c(0.5, 3.9, 0, 2)
col2 <- c(1.3, 131, 2.2, 7)
col3 <- c(3.5, 2.8, 4.6, 5.1)
cbind_matrix <- cbind(col1, col2, col3)
cbind_matrix
```

Використання rbind:
```
row1 <- c(0.5, 1.3, 3.5)
row2 <- c(3.9, 131, 2.8)
row3 <- c(0, 2.2, 4.6)
row4 <- c(2, 7, 5.1)
rbind_matrix <- rbind(row1, row2, row3, row4)
rbind_matrix
```
Результат:
___
| 0.5 | 1.3 | 3.5 |
|---|---|---|
| 3.9 | 131 | 2.8 |
| 0   | 2.2 | 4.6 |
| 2   | 7   | 5.1 |
___

**4. Створити довільний список (list), в який включити всі базові типи.**
```
list <- list('Viktoria', 1.07, 2000, 10i, TRUE)
```
**5. Створити фактор з трьома рівнями «baby», «child», «adult».**
```
> x <- c("adult", "child", "child", "baby","child")
> x <- factor(x, levels = c("baby", "child", "adult"))
> x
```
**6. Знайти індекс першого значення NA в векторі 1, 2, 3, 4, NA, 6, 7, NA, 9, NA, 11. Знайти кількість значень NA.**
Індекс першого значення NA
```
vector <- c(1, 2, 3, 4, NA, 6, 7, NA, 9, NA, 11)
findNA <- min(which(is.na(vector)))
findNA
```
Результат:
___
5
___
Кількість значень NA:
```
vector <- c(1, 2, 3, 4, NA, 6, 7, NA, 9, NA, 11)
countNA <- length(which(is.na(vector)))
countNA
```
Результат:
___
3
___

**7. Створити довільний data frame та вивести в консоль.**
```
x1<-c("a","b","c")
x2<-c(1,2,3)
y<-data.frame(first,second)
y
```
Результат:
___
|    |x1 |x2|
|----|------|------|
|1   |a     |1     |
|2   |b     |2     |
|3   |c     |3     |
___
**8. Змінити імена стовпців цього data frame.**
```
colnames(y) <- c("x1new", "x2new")
y
```
Результат:
___
|    |x1new |x2new|
|----|-------|---|
|1   |a      |3  |
|2   |b      |1  |
|3   |c      |2  |
___
