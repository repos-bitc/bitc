#!/bin/sh

# Script to run pdf2ps via an intermediate file
ps2pdf $1 /tmp/$$.pdf
mv /tmp/$$.pdf $2
