#!/bin/bash

# Run the parser on every error ipdl file in the passed-in directory.
find $1 -name "*.ipdl" | grep -v '/test/ipdl/error/' | xargs -n 1 cargo run --release --example try_parse --
echo status $? >&2
