#! /bin/bash -e
set -o pipefail

check_command_exists () {
    type "$1" &> /dev/null
}

if ! check_command_exists docker; then
    echo "You have to install docker command"
    exit 1
fi

if ! check_command_exists docker-compose; then
    echo "You have to install docker-compose command"
    exit 1
fi

mkdir -p test-cache/emacs-d-cache

docker-compose build
docker-compose run --rm tests
