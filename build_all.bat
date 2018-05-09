@echo off
if not exist bin md bin

echo "Building all files in src/"
scalac.bat -d bin src/*
