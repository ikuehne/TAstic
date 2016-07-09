.. _overview:

*******************
  Getting Started
*******************

*Note*: The following is currently entirely aspirational; it is supposed to put
in coherent form what functionality TAstic should eventually have.

Setting up a new assignment
===========================

TAstic provides tools for dealing with submissions for particular assignments.
Before they can be useful, TAstic needs to know something about the current
assignment and submissions, so it must be initialized with some related
information.  It also places some restrictions on the structure of assignments:
each assignment must be a directory, with each submission for that assignment a
subdirectory.  Related files (test scripts, the provided ``Makefile``, etc.) go
in a special subdirectory, by default called "resources".  Configuration is
contained in ``tastic.yaml``.  A typical assignment, call it ``lab1``, might
look like::

    lab1/
        tastic.yaml
        resources/
            test.py
        student1/
            code.py
        student2/
            code.py

To get it started with default settings, from inside the assignment directory
run::

    tastic init

Or alternatively::

    tastic init path/to/assignment

This will create a basic ``tastic.yaml`` in the assignment directory.  After
changing the configuration appropriately (see below), run ``tastic test`` to
build each assignment, run all unit tests, and log and report any errors.
