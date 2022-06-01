***Лабораторна робота № 5***

Для лабораторної роботи необхідно завантажити zip файл з даними за посиланням: «https://www.dropbox.com/s/i9wi47oyhfb7qlh/rprog_data_specdata.zip?dl=0».

```
generate_filename <- function (int_values) {
  return (sprintf("%03d.csv", int_values))
}

get_pollutions <- function(directory, regions) {
  setwd(directory)
  files <- generate_filename(regions)
  pollutions_by_region <- lapply(files, read.csv)
  return (pollutions_by_region)
}
```
**1. Написати функцію pmean, яка обчислює середнє значення (mean) забруднення сульфатами або нітратами серед заданого переліка моніторів. Ця функція приймає три аргументи: «directory», «pollutant», «id». Directory – папка, в якій розміщені дані, pollutant – вид забруднення, id – перелік моніторів. Аргумент id має значення за замовчуванням 1:332. Функція повинна ігнорувати NA значення.**

```
pmean <- function(directory, pollutant, id = 1:332) {
  pollutions_by_id <- get_pollutions(directory, id)
  pollutions <- do.call("rbind", pollutions_by_id)
  mean_pollution <- mean(pollutions[[pollutant]], na.rm=TRUE)
  return(mean_pollution)
}

print("--- pmean")
print(pmean('specdata', 'sulfate'))

complete_cases_as_vector <- function(dataframe) {
  return (sapply(dataframe, function(x) sum(complete.cases(x)) ))
}

pmean("specdata", "sulfate", 1:10)
pmean("specdata", "sulfate", 250:255)
pmean("specdata", "sulfate", 55)
pmean("specdata", "sulfate", 200)
pmean("specdata", "nitrate")
```
Результат:
___
```
> pmean("specdata", "sulfate", 1:10)
[1] 4.064128
> pmean("specdata", "sulfate", 250:255)
[1] 4.358325
> pmean("specdata", "sulfate", 55)
[1] 3.587319
> pmean("specdata", "sulfate", 200)
[1] 4.390778
> pmean("specdata", "nitrate")
[1] 1.702932
```
___


**2. Написати функцію complete, яка виводить кількість повних спостережень (the number of completely observed cases) для кожного файлу. Функція приймає два аргументи: «Directory» та «id» та повертає data frame, в якому перший стовпчик – ім’я файлу, а другий – кількість повних спостережень.**
```
complete <- function(directory, id){
  pollutions <- get_pollutions(directory, id)

  complete_cases_per_id = data.frame(
    id = id,
    nobs = complete_cases_as_vector(pollutions)
  )

  return (complete_cases_per_id)
}

print("--- complete")
print(complete('.', 1:10))

remove_if_many_na <- function(dataframe, threshold){
  return (dataframe[lapply(dataframe, function (x) { sum(complete.cases(x)) > threshold}) == TRUE])
}

remove_rows_with_na <- function(dataframe){
  return (lapply(dataframe, function (x) x[complete.cases(x),]))
}

get_correlations <- function(dataframe, col1, col2){
  return (sapply(dataframe, function(x) cor(x[[col1]], x[[col2]]) ))
}

```
___

**3. Написати функцію corr, яка приймає два аргументи: directory (папка, де знаходяться файли спостережень) та threshold (порогове значення, за замовчуванням дорівнює 0) та обчислює кореляцію між сульфатами та нітратами для моніторів, кількість повних спостережень для яких більше порогового значення. Функція повинна повернути вектор значень кореляцій. Якщо ні один монітор не перевищує порогового значення, функція повинна повернути numeric вектор довжиною 0. Для обчислення кореляції між сульфатами та нітратами використовуйте вбудовану функцію
«cor» з параметрами за замовчуванням.**
```
corr <- function(directory, threshold = 0) {
  pollutions <- get_pollutions(directory, 1:332)
  valid_regions <- remove_if_many_na(pollutions, threshold)
  valid_regions_without_na <- remove_rows_with_na(valid_regions)

  if (length(valid_regions_without_na) == 0) { return (c()) }

  correlations <- get_correlations(valid_regions_without_na, 'sulfate', 'nitrate')
  return (correlations)
}

cr <- corr("specdata", 150)
head(cr); summary(cr)

```
Результат:
___
```
> cr <- corr("specdata", 150)
> head(cr); summary(cr)
[1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-0.21057 -0.04999  0.09463  0.12525  0.26844  0.76313 

```
___
