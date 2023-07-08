#!/bin/bash

# Указываем путь к файлу
file="/app/NBodyProblem.cabal"
file1="/app/app/Main.hs"
file2="/app/src/NBodyProblem/Console.hs"

# Указываем номер строки, которую нужно удалить (нумерация строк начинается с 1)
line_number=29
line1=37
line2=53
line3=71
line4=4
line5=69
line6=31

# Удаляем строку из файла с помощью команды sed
sed -i "${line_number}d" "$file"
sed -i "${line1}d" "$file"
sed -i "${line2}d" "$file"
sed -i "${line3}d" "$file"
sed -i "${line4}d" "$file1"
sed -i "${line5}d" "$file1"
sed -i "${line6}d" "$file2"