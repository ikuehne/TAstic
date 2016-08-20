TAstic: Plagiarism detection
============================

TAstic is a very simple command-line plagiarism-detection system for plain text
files.  For example:

~~~
$ tastic shakespeare/comedies/* -t 0.05
shakespeare/comedies/asyoulikeit has similarity 5.07812% with shakespeare/comedies/measureforemeasure.
~~~

It can additionally be run as a client-server mode, with many clients
submitting documents to be compared at a central server.  The server is written
entirely in Haskell using the Warp HTTP server.

Intended use
------------

Despite its very concise implementation (currently ~400 lines, not counting the
server), TAstic has a number of features that make it good at detecting
plagiarism:

  - It is insensitive to coarse manipulations of text.  For example, reordering
    functions in source code will not have much affect on its similarity score
    with the original.
  - Small changes to the documents result in small changes to the similarity
    scores.
  - Highly similar documents have high similarity scores, and highly dissimilar
    ones have low similarity scores.
  - It is insensitive to formatting.  Adding spaces and punctuation will not
    affect similarity scores.

These properties mean that it is a good tool for suggesting pairs of documents
to look at more closely.  It should *never* be the final arbiter; it does not
know anything about assignments other than their literal contents, and it is
prone to give misleadingly high scores in edge cases such as very short
documents.

Documentation can be found at
[Read the Docs](http://tastic.readthedocs.io/en/latest/).
