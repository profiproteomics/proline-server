@echo off
echo --- Starting HornetQ
start "hornetQ" start_hornetQ.bat
echo --- Waiting (20s) for HornetQ to start 
timeout /t 20 /nobreak
echo --- Starting ProlineCortex
start "cortex" start_cortex.bat