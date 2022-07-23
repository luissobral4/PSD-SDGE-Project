#!/bin/bash

erl -pa dependencies/default/lib/chumak/ebin -pa Colector -eval "tcpHandler:run($@)"