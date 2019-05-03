# NATE: Nonhuman Abstract Tech Expert

Team Members: [Jane Zhang](https://github.com/jz393), [Andrei Kozyrev](https://github.com/akozyrev), [Shreeya Gad](https://github.com/shreeyagad), [Junan Qu](https://github.com/JunanQu)

## Introduction

For our [CS 3110](http://www.cs.cornell.edu/courses/cs3110/2019sp/) midterm project, the four of us created an [OCaml](http://ocaml.org/) chatbot to answer questions related to the field of computer science. 

The bot gathers its knowledge from an extensive text corpus of Wikipedia articles related to the field of CS, ranging from people (e.g. Alan Turing), places (e.g. Silicon Valley), subfields (e.g. Artificial Intelligence), and companies (e.g. Google). First, it takes in the user's input (the question). Then, it uses the term frequency–inverse document frequency (TFIDF) algorithm to find the most relevant document in the data corpus to search for the answer. Once the document is found, it has two separate mechanisms to find the answer to extract from the doc: Jaccard similarity (which compares the question to each sentence in the doc) and Cosine similarity (which embeds the questions/doc sentences first before computing similarity scores). Once it has found the sentence with the highest similarity score, it will respond with that as the answer. 

Example: `Who is Mark Zuckerberg?` -> `Mark Elliot Zuckerberg, born May 14, 1984, is an American technology entrepreneur and philanthropist`

The bot can also find misspelled words in the user's question and provide possible corrections with its autocorrection feature. If an inputted word cannot be found in the corpus, it will search for candidates in the corpus with the lowest edit (Levenshtein) distance. Additionally, given the input question, the bot can recommend related topics for the user to learn about from a set of topic clusters. 

All algorithms were designed from scratch (no external libraries/packages other than built in OCaml modules), and implementations are our own work unless noted otherwise.

We named our bot after our professor, Dr. Nate Foster, whom we thank for structuring the Spring 2019 course, developing our assignments, and teaching us about functional programming paradigms.  

## Usage

1. `git clone https://github.com/jz393/NATE-QA`
2. `make build`
3. `make bot`
