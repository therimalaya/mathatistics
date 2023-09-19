#! /usr/bin/bash

set -o allexport
source .env set
set +o allexport

rsync -zaP ${IN} ${OUT}
cd ${OUT}
git add .
git commit -m "site updated ---"
git push origin main
