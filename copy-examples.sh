#!/bin/bash

for i in $(grep executable concur-dom.cabal | cut -d " " -f 2)
do
  cp $(cabal list-bin $i).jsexe/{all.js,index.html} docs/examples/$i.jsexe
done
