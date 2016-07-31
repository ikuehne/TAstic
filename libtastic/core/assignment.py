"""Representing assignments."""

import os
import os.path

from libtastic.core import configuration

def path_base(path):
    """Return the last part of ``path``.

    For example::

    >>> path_base("/usr/bin")
    "bin"
    >>> path_base("/usr/bin/")
    "bin"

    The second case distinguishes this from ``os.path.basename``.
    """
    (absolute, base) = os.path.split(path)
    # i.e. if the path did not end in a slash.
    if base:
        return base
    return os.path.basename(absolute)

class Assignment(object):
    """Representing assignments.

    Each assignment contains the related Configuration, a list of submissions
    for the assignment, and some miscellaneous other data such as the name of
    the assignment.
    """

    config_name = "tastic.yaml"

    def __init__(self, path, default_config, use_found=True, name=None):
        """Create a new ``Assignment``.

        :param path: The path to the assignment directory, either absolute or
        relative.

        :param default_config: A ``Configuration`` from which to get default
        settings.

        :param use_found: Set to ``False`` to not use any ``tastic.yaml`` found
        in the given directory.

        :param name: The name of the assignment.  By default, just use the name
        of the assignment directory.
        """
        self.path = path
        if not os.path.isdir(path):
            raise IOError("Not a directory: %s" % path)

        # Get the assigned name,
        self.name = name
        # or read it if none provided.
        if name == None:
            self.name = path_base(path)

        # Get the default configuration,
        self.config = default_config
        if use_found:
            try:
                self.config.load(os.path.join(self.path,
                                              configuration.CONFIG_NAME))

            # and if we don't find a configuration file, just use defaults.
            except IOError:
                pass

        self.add_submissions()

    def add_submissions(self):
        """Updates the current list of submissions.

        Searches the assignment directory for new submissions, and adds any
        found. Also removes any submissions that have been removed from the
        assignment directory.
        """
        # TODO: package submissions in some kind of Submission object.
        self.submissions = []
        for submission in os.listdir(self.path):
            if self.config.is_submission(submission):
                self.submissions.append(submission)
