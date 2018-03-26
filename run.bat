@echo off
echo "Running main in %1"
scala -classpath bin %1
::java -cp scala-library.jar;bin %1