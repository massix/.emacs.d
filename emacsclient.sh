#!/bin/sh

# Depending on the link used, open either the gui or the terminal
if [[ $(basename $0) = "e" ]]; then
    arguments="-nw"
elif [[ $(basename $0) = "eframe" ]]; then
    arguments="-c --no-wait"
elif [[ $(basename $0) = "eeval" ]]; then
    arguments="--eval"
elif [[ $(basename $0) = "ecd" || $(basename $0) = "efile" ]]; then
    arguments=""
else
    arguments="--no-wait"
fi

if [[ $(basename $0) = "ecd" ]]; then
    dir=$(${HOME}/bin/eeval '(file-name-directory (buffer-file-name (window-buffer)))' | tr -d \")
    echo $dir
fi

if [[ $(basename $0) = "efile" ]]; then
    file=$(${HOME}/bin/eeval '(buffer-file-name (window-buffer))' | tr -d \")
    echo $file
fi

# Avoid running the emacsclient for ecd and efile `functions'
[[ ! -z ${arguments} ]] && /usr/local/bin/emacsclient -a "" ${arguments} "$@"
