#!/bin/bash

pandoc -s README.md -o README.html
scp README.html $1@180.235.228.215:/var/www/html

