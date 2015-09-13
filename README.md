# yente

A matchmaker for textual data.

# Overview

*yente* matches names across two data files. Matching is based on rareness of words, which means that one does not need to preprocess the names to remove common, non-informative words in names (i.e. and, the, company) It is highly customizable. The user may optionally allow for misspellings, implement phonetic algorithms, trim the constituent words of a name at a prespecified number of characters, output any number of potential matches (with and without ties), and combine any of the preceding customizations. By construction, *yente* is both word-order and case insensitive (Shawn Spencer matches SPENCER, SHAWN). *yente* is a multi-core program, allowing users to maximize computational power. 
    
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

# Basic use

These instructions assume that *yente* is installed to your path as described above. 

Assume you have a two data sets with each consisting of a list of entities (people, companies, etc...) with identifiers (an unique code such as an integer). These datasets should be saved as either a comma separated or tab separated file. These files should have header rows. The entity names should be a column called `name` and the identifiers in a column called `id`. 

Let's say that for each entity in the `FROM-FILE` you want to find all the possible matches in the `TO-FILE`. To do so, open a terminal or DOS prompt and type: `yente FROM-FILE TO-FILE -o OUTPUT-FILE`. This will print the matches to your terminal and save the results in `OUTPUT-FILE`. Per UNIX standards, the `-o OUTPUT-FILE` may be omitted and terminal redirection can be used.

In the default, *yente* will output the best match for each entity in the `FROM-FILE`. That matches will be scored between 0 and 1, with 1 be a perfect match.


Further options are available to customize the matching process as described below.

Should you require any help, type `yente --help` to see options.

# Advanced use

*yente* consists of three parts:
    1. A preprocessor transforms text via phonetic algorithms and/or word (phonetic code) length truncation.
    2. A matcher that finds matches based on word rarity (cosine similarity with an inverse density function) and allows for misspellings.
    3. An output control that provides a certain number of results and/or restricts results based on scores.

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

