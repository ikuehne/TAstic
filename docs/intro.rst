.. _intro:

.. toctree::
   :hidden:

   configuration

*********
  Usage
*********

The TAstic command tool, like ``git``, ``apt``, and many other standard UNIX
tools with several different uses, basic TAstic usage consists of the primary
invocation (``tastic``) followed by a sub-command (such as ``init`` or
``compare``).  Each sub-command is written as much as possible to follow
standard UNIX conventions: flags are passed in long (``--verbose``) or short
(``-v``) form, and followed immediately by any arguments; unnamed arguments can
appear anywhere in the argument list where such usage is unambiguous, and so on.
Specific documentation for each sub-command follows.

``init``: Setting up a new assignment
=====================================

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

Configuring TAstic
------------------

TAstic is configured in YAML files called ``tastic.yaml``.  Parameters for each
assignment default to those given by the ``tastic.yaml`` in that assignment's
directory, but any omitted parameters will be given sensible defaults.  To see
those defaults, just run ``tastic init`` and look at the generated
configuration file.

Individual options will be documented below as they become relevant...

``compare``: Comparing submissions for similarity
=================================================

TAstic comes with simple document comparison facilities.  A description of the
algorithm can be found in :ref:`the API documentation<libtastic>`; here it
suffices to say that while any documents it marks as similar are likely to be
suspiciously similar, *any documents it points out should be inspected for
similarity*; it is not a human and the comparison it performs is fairly
simple-minded.

A "bare" invocation ``tastic compare`` will compare all submissions in the
current assignment, and report any with a similarity above a certain threshold.
The threshold can be set with the ``--threshold`` option, which takes a number
between 0 and 1 representing the minimum Jaccard coefficient required for a
pair to be reported.
