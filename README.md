# Analysis Tools [![Build Status](https://travis-ci.com/Submitty/AnalysisTools.svg?branch=master)](https://travis-ci.com/Submitty/AnalysisTools)
This repository contains a variety of tools used for source code analysis.

## Building
    git clone https://github.com/Submitty/AnalysisTools
    cd AnalysisTools
    stack build

## Guidelines for Contribution
 - Make sure all Haskell code builds with no warnings (`lichen.cabal` by default sets `-Wall -Werror`).
 - Run `hlint` over the code to ensure correct style (you may want to use `hlint '--ignore=Parse error'` to avoid some bugs).
 - Generally, follow the [Submitty developer guidelines](http://submitty.org/developer/development_instructions).
