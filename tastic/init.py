import argparse
import os.path
import pkg_resources

from common import configuration, assignment, subcommand

DEFAULT_CONFIG = pkg_resources.resource_filename(__name__,
                                                 "resources/default.yaml")

class InitSubCommand(subcommand.SubCommand):

    def invoke(self, args):
        conf = configuration.Configuration([DEFAULT_CONFIG])
        self.assignment = assignment.Assignment(args.assignment,
                                                default_config=conf)
        local_conf = os.path.join(self.assignment.path,
                                  assignment.Assignment.config_name)
        if not os.path.exists(local_conf):
            conf.dump(local_conf)

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

        default = configuration.Configuration(yamls=[DEFAULT_CONFIG])
