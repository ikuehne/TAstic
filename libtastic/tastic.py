"""The primary command-line interface to TAstic."""

import argparse

import libtastic.init

INVOKE = "invoke"
SUBCOMMANDS = [libtastic.init.InitSubCommand]
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
        command(subparsers, INVOKE)

    args = parser.parse_args()
    # If a subcommand is indicated, invoke the subcommand,
    try:
        args.__getattribute__(INVOKE)(args)
    # and otherwise tell the user they're doing it wrong.
    except AttributeError:
        parser.print_help()

if __name__ == "__main__":
    main()
