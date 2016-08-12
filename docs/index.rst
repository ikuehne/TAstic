.. TAstic documentation master file, created by
   sphinx-quickstart on Thu Jul  7 19:44:35 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

TAstic: Simple similarity detection
===================================

:Release: |version|
:Date:    |today|

Motivation
----------

To the author's knowledge, there are no free/libre plagiarism detection systems
currently in use.  ``TAstic`` provides a very simple and reasonably effective
version of such a system (currently less than 300 lines of source code), under
GPL and designed to be easy to extend and modify. It exists both as a Python
library and a command-line tool, has no external dependencies other than a
Python interpreter, and may easily be incorporated into other software.

Command-line usage
------------------

The simplest way to invoke TAstic is by simply providing it a list of files to
compare for similarity::

    tastic file1 file2 ...

The provided files should be plain-text; for details of the comparison
performed, see below.  If any pair of the provided files is found to have a
similarity above a certain threshold, a report is printed to stdout.  The
threshold is adjustable via the ``-t`` flag; for example, to provide a report
for each pair, run::

    tastic -t 0 file1 file2 ...

By default the threshold is 25%.  Again, for interpretation of the threshold
see below.

``tastic`` can also search a list of provided directories for a file with a
given name using the ``-a`` option::

    tastic -a filename dir1 dir2 ...

The above command will perform the same operation as ::

    tastic dir1/filename dir2/filename ...

The argument to ``-a`` can be any Python-style regex; for example ::

    tastic -a '(file)+name' dir1 dir2

will match ``dir1/filefilename`` and ``dir2/filename``.  Only the first match
found in each directory will be used.  If there are no matches in a particular
directory, a warning will be issued over stderr, and that directory will be
ignored.

Comparison algorithm
--------------------

Similarity metric
^^^^^^^^^^^^^^^^^

The similarity between two documents is reported as a number between 0 and 1.
That number is derived as if by the following algorithm:

* Split each document into sequences of three words.  For example, "The quick
  brown fox" would become "The quick brown", "quick brown fox".
* Count the number of unique 3-sequences the documents have in common.
* Divide that number by the total number of unique 3-sequences in both
  documents.

Determining the metric
^^^^^^^^^^^^^^^^^^^^^^

A naive translation of the above algorithm would require :math:`O(k^²n)` time
for :math:`k` documents of length :math:`n`; it would additionally require
:math:`O(kn)` space.  This is unacceptably high for large :math:`k` and
:math:`n`; instead, we use the MinHash_ algorithm to get a good approximation of
the metric in :math:`O(k^2+n)` time and :math:`O(k)` space.

.. _MinHash: en.wikipedia.org/wiki/MinHash
