# yente

A matchmaker for textual data.

# Overview

*yente* matches names across two data files. It has the following features:
    
* **Intelligent**: Matching is based on rareness of words, which means that one does not need to preprocess the names to remove common, non-informative words in names (i.e. and, the, company) 
* **Robust**: *yente* incorporates feautes that are commonly needed in name matching. It is both word-order and case insensitive (Shawn Spencer matches SPENCER, SHAWN). And, *yente* removes punctuation by default.
* **Customizable**: Users may optionally allow for misspellings, implement phonetic algorithms, trim the constituent words of a name at a prespecified number of characters, output any number of potential matches (with and without ties), and combine any of the preceding customizations. 
* **High-ish performance**: yente* is a multi-core program, allowing users to maximize computational power. Performance improvements are ongoing.

# Information 

See the [wiki]<https://github.com/tumarkin/yente/wiki> for information on installation, usage, and best practices. It also includes some examples for matching problems that commonly arise in research. 

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

# Examples

* Basic usage: `yente FROM-FILE TO-FILE -o OUTPUT-FILE`

# FAQ

* Why use Haskell?

* What's with the name?

* Where do those strange names in the examples come from?

* Are there any planned improvements? 

    * Performance
    * Subgroup matching (allow for searching of names that belong to a shared group)
    * Full unicode support


# Contributing

Submit a pull request and I will respond. *yente* 

If *yente* has in any way made your life easier, please send me an email or star this repository. If you would like to see a feature added, send me an email.

