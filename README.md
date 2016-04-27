# quantum-random-numbers
Retrieve, store and manage real quantum random numbers. They are obtained by measuring the vacuum and served by [Australian National University](http://qrng.anu.edu.au/).

This package provides:
* A Haskell library to performs these tasks, in conjunction with a local data store set up upon installation
* An executable program `qrn` providing an interface to these functionalities.

The data store is managed according to a simple protocol: There is a minimum size and a target maximum size adjustable by the user. When data is requested that would reduce the store below the minimum level, it requests from ANU enough QRN data to both fulfill the request as well as fill the data store back to the "target" size.


### Command line usage

Call `qrn` without any command line arguments to launch the interactive program, or alternatively
supply the desired command as arguments to only perform the specified operation.

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
set minSize       –  Set the number of bytes below which the store is refilled
set targetSize    –  Set the number of bytes to have after refilling
help/?            –  Display this text
quit              –  Quit
```

Commands are case-insensitive.

#### Display options

By default, data is displayed by showing a color for every 4 bits (every half-byte). One can also specify the data to be displayed as binary,
or equivalently with up/down arrows representing quantum mechanical spin states. Either of these latter two can be combined with the colors as well (thus explicating the encoding).

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

The most basic functionality, to retrieve data directly from ANU, is provided by these functions from the `QRN.ANU` module
yielding either a list of bytes or a list of booleans. In both cases the argument specifies the number of bytes.

```haskell
fetchQRN :: Int -> IO [Word8]
fetchQRNBits :: Int -> IO [Bool]
```

There are also versions of these functions that explicitly handle errors instead of halting execution when a problem occurs:

```haskell
fetchQRN' :: Int -> IO (Either String [Word8])
fetchQRNBits' :: Int -> IO (Either String [Bool])
```

The functions for working with the data store are exported by the `QRN.Store`. An important one is

```haskell
extract :: Int -> IO [Word8]
```
which invokes the machinery to retrive more data and update the store as needed.

### Future work

Some future refinements that may be added are:

* Secure data retrieval
* Asynchronous data retrieval
* Hexidecimal display
