# quantum-random-numbers

[![Build Status](https://travis-ci.org/BlackBrane/quantum-random-numbers.svg?branch=master)](https://travis-ci.org/BlackBrane/quantum-random-numbers)

Retrieve, store and manage real quantum random numbers. They are obtained by measuring vacuum fluctuations of the electromagnetic field, and served by [Australian National University](http://qrng.anu.edu.au/).

The package is to ensure QRNs are promptly available for your application by keeping a sufficient number locally. When they are depleted to a specified level, new QRN data are downloaded concurrently over SSL. It can be configured by specifying the minimum store size (below which more data are retrieved) the target store size (the size of the store after retrieval) and the default display style.

This functionality is provided by:

* An executable program `qrn`
* A Haskell module `Quantum.Random`.

### Command line usage

Call `qrn` without any command line arguments to launch the interactive program, or alternatively
supply the desired command as arguments to only perform the specified operation.

#### Setup

Assuming GHC and Cabal are installed:

```
cabal update
cabal install qrn
qrn fill
```

One might also opt to set appropriate store size defaults before filling:

```
qrn set minsize 150
qrn set tarsize 300
qrn fill
```

#### Available commands

```
add [# bytes]     –  Request specified number of QRN bytes from ANU and add them to the store
live [# bytes]    –  Request specified number of QRN bytes from ANU and display them directly
observe [# bytes] –  Take and display QRN data from store, retrieving more if needed. Those taken from the store are removed
peek [# bytes]    –  Display up to the specified number of bytes from the store without removing them
peekAll           –  Display all data from the store without removing them
fill              –  Fill the store to the target size with live ANU quantum random numbers
restoreDefaults   –  Restore default settings
reinitialize      –  Restore default settings, and refill store to target size
status            –  Display status of store and settings
save [filepath]   –  save binary qrn file to specified file path
load [filepath]   –  load binary file and append data to store
set minSize       –  Set the number of bytes below which the store is refilled
set targetSize    –  Set the number of bytes to have after refilling
help/?            –  Display this text
quit              –  Quit
```

Commands are case-insensitive.

#### Display options

By default, data is displayed by showing a color for every 4 bits (every half-byte). One can also
specify the data to be displayed as binary, or equivalently with up/down arrows representing quantum
mechanical spin states. Either of these latter two can be combined with the colors as well (thus
explicating the encoding).

So the available display modifiers are:
* `colors` (the default)
* `bits`/`binary`
* `spins`
* `colorBits`/`colorBinary`
* `colorSpins`

Simply type these modifiers after any display command. For example:

`observe 25 colorspins`

`live 50 binary`

### Usage in Haskell code

All user-facing functionality may be accessed from the `Quantum.Random` module, though a user can
also just import particular constituent modules when only a subset of the functionality is needed.

The most basic service is to retrieve data directly from ANU, which is provided by functions
from the `Quantum.Random.ANU` module. There are two variants yielding either a list of bytes or a
list of booleans. In both cases the argument specifies the number of bytes.

```haskell
fetchQRN :: Int -> IO [Word8]
fetchQRNBits :: Int -> IO [Bool]
```

Operations involving the data store are exported by `Quantum.Random.Store`. An important one is

```haskell
extract :: Int -> IO [Word8]
```
This also invokes the machinery to retrieve more data and refill the store as needed.

Most of the IO actions in the package use a custom exception type to handle the unlikely
error conditions that may be encountered. See the `Quantum.Random.Exceptions` module for details.

### Physical Origin

Detailed information on the physical setup used to produces these random number streams can be
found in these papers:

* [Real time demonstration of high bitrate quantum random number generation with coherent laser light](http://arxiv.org/abs/1107.4438)
* [Maximization of Extractable Randomness in a Quantum Random-Number Generator](http://arxiv.org/abs/1411.4512)
