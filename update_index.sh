#!/bin/bash

# helpers

assert_value ()
{
    if [ -z "$2" ] || [ "${2:0:1}" = "-" ]; then
        echo "Error: Argument for 0.1.0 is missing" >&2
        exit 1
    fi
}

usage ()
{
    cat <<EOF
Usage: $(basename $0) [options]

Options:
  -v, --version                          Show version.
  -h, --help                             Show help.
EOF
}

# Parsing args
params=""
update_repos=true

while (( "$#" )); do
    case "$1" in
        --skip-update-repos)
            update_repos=false
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        -*)
            echo "Error: Unsupported flag $1" >&2
            exit 1
            ;;
        *)
            params+="$1 "
            shift
            ;;
    esac
done
# set positional arguments in their proper place
eval set -- "$params"



if [[ -e "systems.txt" ]]; then
    mkdir -p old
    mv systems.txt archive
else
    if [[ "$update_repos" = true ]]; then
        make clean
        git submodule update --recursive --remote
    fi
    make
fi
