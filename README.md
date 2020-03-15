# `hcat`

A simple terminal program, written in Haskell. For illustration purposes.
This is just a simple `cat` program.


## Usage

See the help:

    hcat --help

Cat a file:

    hcat /path/to/file

Cat multiple files:

    hcat /path/to/file1 /path/to/file2 ...


## Build

Download the project, e.g.:

    git clone https://..../hcat.git
    cd hcat

Configure the project:

    make setup

Build it:

    make build

The executable is buried deep inside the `dist-newstyle` directory.
Install it somewhere if you like.

To clean:

    make clean


## Development

Download the project, e.g.:

    git clone https://..../hcat.git
    cd hcat

Configure the project:

    make setup

To have `cabal` build and run it, this is the format:

    cabal new-run hcat -- /path/to/file1 /path/to/file2 ...

To see the help:

    cabal new-run hcat -- --help

To clean:

    make clean


