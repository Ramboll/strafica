:: -*- coding: us-ascii-dos -*-
@echo off
cd /d "%~dp0"
set QUERY=HKLM\Software\R-core\R /v InstallPath
for /f "tokens=2*" %%a in ('reg query %QUERY% ^| findstr \R\') do set R=%%b
if not "%R%" == "" goto :found
set QUERY=HKLM\Software\Wow6432Node\R-core\R /v InstallPath
for /f "tokens=2*" %%a in ('reg query %QUERY% ^| findstr \R\') do set R=%%b
if not "%R%" == "" goto :found
echo Failed to find path to R in registry!
pause
goto :eof

:found
@echo off
echo Found R at %R%.
echo .
set PATH=%PATH%;%R%\bin
cd /d "%~dp0"
cmd.exe
