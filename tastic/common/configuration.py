"""Parsing config files and packagin configurations."""

import os
import re

import yaml

from .error import Error

CONFIG_NAME = "tastic.yaml"

class InvalidConfigError(Error):
    """Error raised when a malformed tastic.yaml is found."""

    pass

class Configuration(object):
    r"""Generating and storing all configuration information for TAstic.

    Fields may be set and retrieved by simply accessing object attributes::

    >>> conf = Configuration(submission_format: r'(?P<name>)')
    >>> conf.submission_format
    r'(?P<name>)'

    This means that it can be used as a ``Namespace`` by ``argparse``.
    """

    ############################################################################
    # Class attributes.
    #

    # A mapping of recognized fields to expected types.
    fields = {"submission_format": str,
              "rename": bool,
              "submission_dir": bool}

    @staticmethod
    def _check_type(field, value):
        """Make sure that the given value is of the expected type."""
        expected = Configuration.fields[field]

        if not isinstance(value, expected):
            raise InvalidConfigError("Expected type %s for %s; got %s." %
                    (expected, field, type(value)))


    ############################################################################
    # Instance methods.
    #

    def __init__(self, yamls=None, **kwargs):
        """From a list of configuration files and optionally keyword settings,
        get a configuration.

        Settings loaded from files are preferred to settings given as arguments.

        :param yamls: An iterable of configuration file paths.  Settings are
        pulled preferentially from the _end_ of the list; that is, they are
        loaded from the start to the end, so settings that appear earlier can be
        overriden.  If no list is provided, only pulls from the given keyword
        arguments.
        """

        self.__dict__.update(kwargs)

        if yamls is not None:
            # Load every config file in the list.
            for path in yamls:
                self.load(path)

        self._validate()

    def load(self, config_path):
        """Load the given configuration file."""
        try:
            with open(config_path, 'r') as f:
                self.__dict__.update(yaml.load(f))
        # Either the YAML file wasn't a dictionary, or we got a parse error of
        # some kind.
        except (TypeError, yaml.YAMLError):
            raise InvalidConfigError("Could not read config file at %s"
                    % config_path)

        self._validate()

    def is_submission(self, candidate):
        """Return True iff the candidate path is a submission.

        Checks that the candidate matches the filename specification for
        submissions.  If submissions are supposed to be directories, also checks
        for that.
        """
        if not re.compile(self.submission_format).match(candidate):
            return False

        if self.submission_dir and not os.path.isdir(candidate):
            return False

        return True

    def _validate(self):
        """Check that all fields are known and have the expected types.

        Raise an appropriate exception otherwise.
        """
        unknown_fields = list(self.__dict__.keys() -
                              Configuration.fields.keys())

        if unknown_fields:
            unknowns = ', '.join(unknown_fields)
            raise InvalidConfigError("Unrecognized fields: %s" % unknowns)

        unspecified_fields = list(Configuration.fields.keys() -
                                  self.__dict__.keys())

        if unspecified_fields:
            unspecifieds = ', '.join(unspecified_fields)
            raise InvalidConfigError("No value found: %s" % unspecifieds)

        for item in self.__dict__.items():
            Configuration._check_type(*item)

    def dump(self, out):
        close = False

        # Open the file if it is not already a filestream.
        if isinstance(out, str):
            out = open(out, 'w')
            close = True

        dumpable = {}
        for field in Configuration.fields:
            dumpable[field] = self.__getattribute__(field)

        yaml.safe_dump(dumpable, out, default_flow_style=False)

        if close:
            out.close()
