# yente
A matchmaker for textual data.

# Installation

**Precompiled binaries**

To do

**From source**

*yente* is written in Haskell. To compile from source,


* Install Haskell. The Haskell Platform is recommended: <https://www.haskell.org/platform/>.
* Cygwin is required on Windows systems: <http://www.cygwin.com/>.
* Clone this repository.
* Open a terminal shell and cd into the repository root directory. Then, execute the following commands:
  * `cabal update`
  * `cabal install yente` 


# Usage

*To do*: For now, run `yente --help` to see options.

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

