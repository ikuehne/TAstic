.. _libtastic:

****************
  API Overview
****************

The ``tastic`` command-line tool is a thin wrapper over functionality provided
by the ``libtastic`` Python library.  The functionality immediately required by
the tool is in ``libtastic/tastic.py``, and subcommands are in, for example,
``libtastic/init.py``.  Most of these subcommands, in turn, consist of
interfaces to generic functionality contained in ``libtastic/core``.  For
example, ``compare`` is internally based on the MinHash algorithm, an
implementation of which lives in ``libtastic/minhash.py``.

Subcommand API
==============

TAstic subcommands are represented by ``libtastic/core/subcommand.SubCommand``
instances.  This abstract class is a wrapper over functionality provided by the
``argparse`` module.  Each class must implement a ``name`` method, which
returns the name of the subcommand; an ``invoke`` method, which takes a
``Namespace`` parsed from the command-line arguments and performs the
subcommand action.
