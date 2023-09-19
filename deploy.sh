#! /usr/bin/bash

IN=docs/
OUT=/Data/WWW/sites/therimalaya.github.io/

rsync -zaP $IN $OUT
cd $OUT
git add .
git commit -m "site updated ---"
git push origin main
