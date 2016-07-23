import argparse
import os.path
import pkg_resources

from common import configuration, assignment, subcommand

DEFAULT_CONFIG = pkg_resources.resource_filename(__name__,
                                                 "resources/default.yaml")

class InitSubCommand(subcommand.SubCommand):

    def invoke(self, args):
        pass

    def name(self):
        return "init"

    def __init__(self, subparsers, invoke):
        super(InitSubCommand, self).__init__(subparsers, invoke)
        self.parser.add_argument("assignment",
                                 nargs="?",
                                 default=os.path.realpath("."))
        for (field, t) in configuration.Configuration.fields.items():
            self.parser.add_argument("--" + field,
                                     nargs=1,
                                     type=t,
                                     required=False)

    def create_assignment(self, path):
        default = configuration.Configuration(yamls=[DEFAULT_CONFIG])
        return assignment.Assignment(path, default_config=default)
