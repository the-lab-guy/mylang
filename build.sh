#!/bin/bash

if [ $# -eq 0 ]
then
  echo 'Usage : build.bat <assembly file without .extension>'

else
  echo Assembling $1.asm ...
  nasm -g -f elf64 $1.asm

  echo Linking $1.exe ...
  gcc -g -no-pie -fno-pie -o $1.exe $1.o

  echo $1.exe ready to run ...
  read -p Continue?

  ./$1.exe

fi
