"""The primary command-line interface to TAstic."""

import argparse

import init

SUBCOMMANDS = [init]
WHICH_SUBCOMMAND = "subcommand"

def subcommand_names():
    """Get a mapping from subcommand names to subcommand objects."""
    result = {}
    for subcommand in subcommands:
        result[subcommand.__name__] == subcommand
    return result

def main():
    """Run the TAstic command-line tool."""
    parser = argparse.ArgumentParser(description="Tools for happier TAs.")
    subparsers = parser.add_subparsers(title="sub-commands",
                                       dest=WHICH_SUBCOMMAND)

    # Add subparsers for each subcommand.
    for command in SUBCOMMANDS:
        command_parser = subparsers.add_parser(command.__name__)
        command.arg_parser(command_parser)

    parser.parse_args()

if __name__ == "__main__":
    main()
