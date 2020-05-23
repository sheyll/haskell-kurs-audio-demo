#! /usr/bin/env nix-shell 
#! nix-shell -i bash -p sox

play -x -r 44100 -b 16 -c1  -e signed-integer -t raw -
