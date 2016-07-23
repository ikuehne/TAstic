import argparse
import os.path
import pkg_resources

from common import configuration, assignment

DEFAULT_CONFIG = pkg_resources.resource_filename(__name__,
                                                 "resources/default.yaml")

def create_assignment(path):
    default = configuration.Configuration(yamls=[DEFAULT_CONFIG])
    return assignment.Assignment(path, default_config=default)

def arg_parser(parser=None):
    """Create an ``ArgumentParser`` for the init subcommand.

    :param parser: The parser to modify.
    """
    parser.add_argument("assignment",
                        nargs="?",
                        default=os.path.realpath("."))
    for (field, t) in configuration.Configuration.fields.items():
        parser.add_argument("--" + field, nargs=1, type=t, required=False)

def main():
    parser = arg_parser()
    parser.parse_args()

if __name__ == "__main__":
    main()
