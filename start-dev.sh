#!/bin/sh
erl -pa $PWD/ebin -pa $PWD/deps/*/ebin \
    -sname dev \
    -s erldbf
