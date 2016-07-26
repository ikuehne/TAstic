"""Utility for comparing submissions."""

import argparse

from libtastic.common.minhash import MinHash
from libtastic.common.subcommand import SubCommand

class CompareSubCommand(SubCommand):
    """A sub-command for comparing submissions."""

    THRESHOLD_HELP = ("A number from 0 to 1 representing the minimum Jaccard "
                      "coefficient to report a pair of submissions.")

    def invoke(self, args):
        if args.threshold < 0 or args.threshold > 1:
            self.parser.print_help()
            exit(1)

        minhashes = {}
        for infile in args.submissions:
            minhashes[infile.name] = MinHash(infile)

        for i, (filename1, minhash1) in enumerate(minhashes.items()):
            for filename2, minhash2 in list(minhashes.items())[i + 1:]:
                jaccard = minhash1.jaccard(minhash2)
                if jaccard >= args.threshold:
                    print("%s has similarity %g%% with %s." %
                            (filename1, jaccard, filename2))

    def name(self):
        return "compare"

    def __init__(self, subparsers, invoke):
        super(CompareSubCommand, self).__init__(subparsers, invoke)
        self.parser.add_argument("submissions",
                                 nargs="+",
                                 help="A list of submissions to compare.",
                                 type=argparse.FileType("r"))
        self.parser.add_argument("-t", "--threshold",
                                 default=0.25,
                                 help=CompareSubCommand.THRESHOLD_HELP,
                                 type=float,
                                 dest="threshold")
