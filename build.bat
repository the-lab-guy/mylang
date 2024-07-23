@echo OFF

if "%1"=="" goto HELP

echo Assembling %1.asm ...
nasm -g -f elf64 %1.asm

echo Linking %1.exe ...
gcc -g -o %1.exe %1.o

echo %1.exe ready to run ...
pause
.\%1.exe

goto END

:HELP
echo Usage : build.bat <assembly file without .extension>

:END
