# yente

A matchmaker for textual data.

# Overview

*yente* matches names across two data files. Matching is based on rareness of words, which means that one does not need to preprocess the names to remove common words (i.e. the, and) that are non-informative parts of names. It is highly customizable. The user may  allow for misspellings, implement phonetic algorithms, trim words at a prespecified number of characters, output any number of potential matches (with and without ties), and combine any of the preceding customizations. *yente* also allows for simple multi-core processing of names to maximize computational power. 
    
See below for installation, usage, best practices, and some examples of *yente*.

# Installation

**Precompiled binaries**

Precompiled executables (currently for Windows and Mac OS X) are available in the `binaries` subdirectory. Download the executable and copy it to your path. Type `echo $PATH` from a terminal on UNIX style platforms (including Mac) to see your path. On Windows, type `path` from a DOS prompt.

Note: Windows installations require [Cygwin]<http://www.cygwin.com/>. While Cygwin needs to be installed, you may still run *yente* throush DOS. 


**From source**

*yente* is written in Haskell. To compile from source,

* Install Haskell. The Haskell Platform is recommended: <https://www.haskell.org/platform/>.
* Cygwin is required on Windows systems: <http://www.cygwin.com/>.
* Clone this repository.
* Open a terminal shell and cd into the repository root directory. Then, execute the following commands:
  * `cabal sandbox init`
  * `cabal update`
  * `cabal install yente.cabal` 
* Copy the binary from the *dist* subdirectory to your executable path.

# Usage


To do*: For now, run `yente --help` to see options.

## Preprocessing

## Matching

## Output

## Multicore support

## Examples

# Best practices


# To Do

* Usage
  * **DONE** - Change CLI
  * Metaphone
  * Easy access to multi-core

* Performance
  * Create test set for performance analytics
  * Circular implementation of token counting and name norming
  * Switch to ByteString or Text
  * Allow all unicode characters
  * Memoize name matching

* Bugs
  * Cosine without misspellings should not use intersaction (can lead to values > 1 if a word appears multiple times)

