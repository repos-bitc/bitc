#!/bin/sh

# Script to run latex multiple times until it converges, and then
# produce an output file.

latex $1
latex $1
latex $1
