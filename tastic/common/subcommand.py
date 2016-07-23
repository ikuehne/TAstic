"""Command-line parsing and invocation of subcommands."""

class SubCommand(object):

    _IMPLEMENTATION_ERROR = "SubCommands must implement %s."

    def invoke(self, args):
        raise NotImplementedError(_IMPLEMENTATION_ERROR % "invoke")

    def name(self):
        raise NotImplementedError(_IMPLEMENTATION_ERROR % "name")

    def __init__(self, subparsers, invoke):
        """Create a new SubCommand.

        For example, if we have a SubCommand subclass MySubCommand and an
        argparse.ArgumentParser parser, which takes no command-line arguments:

        >>> subparsers = parser.add_subparsers(title="sub-commands")
        >>> subcommand = MySubCommand(subparsers, "invoke")
        >>> args.subcommand.parse(["%s" % subcommand.name()])
        >>> args.invoke(args)

        :param subparsers: The ``subparsers`` action from which to create the
        parser for this subcommand.

        :param invoke: The NameSpace attribute to be used to invoke this
        subcommand.
        """
        self.parser = subparsers.add_parser(self.name())
        self.parser.set_defaults(**{invoke: self.invoke})
