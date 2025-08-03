#!/usr/bin/env bash

# This is to be used for developing

cd "$(dirname "$0")" || exit

lein with-profile dev run
