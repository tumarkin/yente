# yente

A matchmaker for textual data.

# Overview

*yente* matches names across two data files. It has the following features:

* **Intelligent**: Matching is based on rareness of words, which means that one does not need to preprocess the names to remove common, non-informative words in names (i.e. and, the, company)
* **Robust**: *yente* incorporates feautes that are commonly needed in name matching. It is both word-order and case insensitive (Shawn Spencer matches SPENCER, SHAWN). And, *yente* removes punctuation by default.
* **Customizable**: Users may optionally allow for misspellings, implement phonetic algorithms, trim the constituent words of a name at a prespecified number of characters, output any number of potential matches (with and without ties), and combine any of the preceding customizations.
* **High-ish performance**: *yente* is a multi-core program, allowing users to maximize computational power. Performance improvements are ongoing.
* Unicode aware: By default, *yente* automatically converts unicode accented characters to their ASCII equivalents.

# Information

See the [wiki](https://github.com/tumarkin/yente/wiki) for information on installation, usage, and best practices. It also includes some examples for matching problems that commonly arise in research.

# Contributing

Submit a pull request and I will respond.

If *yente* has in any way made your life easier, please send me an email or star this repository. If you would like to see a feature added, let me know through the Github forum.

