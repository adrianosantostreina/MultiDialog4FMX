@echo off
setlocal
echo.
echo ============================================
echo  Compilando Testes - MultiDialog4FMX
echo  Usando: Delphi 12.3 Athens (Studio 23.0)
echo ============================================
echo.

call "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"

echo.
echo Compilando com DCC32...
echo.

dcc32 -B -Q -E".\Win32\Debug" -N".\Win32\Debug\dcu" MultiDialog4FMX.Tests.dpr

if %ERRORLEVEL% EQU 0 (
    echo.
    echo ============================================
    echo  COMPILACAO BEM-SUCEDIDA!
    echo ============================================
    echo.
    echo Executavel gerado em: .\Win32\Debug\MultiDialog4FMX.Tests.exe
    echo.
) else (
    echo.
    echo ============================================
    echo  ERRO NA COMPILACAO!
    echo  Codigo de erro: %ERRORLEVEL%
    echo ============================================
    echo.
)
