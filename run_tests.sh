#!/bin/bash

emacs -batch -f package-initialize -L . -f buttercup-run-discover
