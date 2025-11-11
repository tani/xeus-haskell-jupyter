#!/bin/bash
docker run --rm -i -v "$(pwd):/work" xeus-haskell "cd /tmp && ctest ."
