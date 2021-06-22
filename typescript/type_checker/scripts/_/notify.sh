#!/usr/bin/env bash
set -eu

if [ -z ${HUSKY+x} ]; then
    echo 'Notifying...'
else
    echo "notify.sh: Invoked via husky";
    exit
fi


if command -v osascript &> /dev/null
then
    osascript -e "display alert \"$1\""
fi