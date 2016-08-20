"""Minhash set-similarity estimation."""

import collections
import random
import string
import zlib

from tastic import error

def _words(stream, strip_punctuation=True):
    """Take the given file-like input stream and return an iterator of words."""
    for line in stream:
        for word in line.split():
            if strip_punctuation:
                yield word.strip(string.punctuation)
            else:
                yield word

def _shingles(stream, n=3, strip_punctuation=True):
    """Generate all shingles of size ``n`` (default 3) from ``stream``.
    
    Shingles will be returned as lists.
    """
    buff = collections.deque()
    for word in _words(stream, strip_punctuation):
        buff.append(word)
        if len(buff) == n:
            yield list(buff)
            buff.popleft()

class MinHashError(error.Error):
    pass

class MinHash(object):

    @staticmethod
    def _generate_hash(rand):
        """Generate a random hash function.

        The resulting hash function takes a string and returns an
        integer (of up to 32 bits).

        :param rand: A random.Random instance.
        """
        rand_char = chr(rand.getrandbits(7))
        salt1 = rand.getrandbits(32)
        salt2 = rand.getrandbits(32)

        def result(string):
            salty = rand_char.join(list(string)).encode('utf8')
            return salt1 ^ zlib.crc32(salty)

        return result

    def __init__(self, f=None,
                 count=256, seed=0xabbeefcd,
                 strip_punctuation=True,
                 shingle_size=3):
        """Create a new MinHash.

        :param f: A file-like stream or filename, which if provided will be
        shingled and added to the MinHash.  If not provided, the MinHash will
        start empty.

        :param count: The number of hash functions to use. Defaults to 256.

        :param seed: The random seed to use to generate the hash functions.
        MinHashes can only be compared if they were created with the same
        ``count`` and ``seed``.

        :param strip_punctuation: Whether to strip punctuation from the shingled
        words. Default ``True``.

        :param shingle_size: The size of the shingles to use. Default 3.
        """
        rand = random.Random()
        rand.seed(seed)
        self._hashes = [MinHash._generate_hash(rand) for i in range(count)]
        self._mins = None

        if f is not None:
            self.add_file(f, strip_punctuation, shingle_size)

    def add(self, string):
        """Process the given ``string``, which must have an encoding."""

        if self._mins is None:
            self._mins = []
            for (i, h) in enumerate(self._hashes):
                self._mins.append(h(string))
        else:
            for (i, m) in enumerate(self._mins):
                hash_value = self._hashes[i](string)
                if hash_value < m:
                    self._mins[i] = hash_value

    def add_file(self, f, strip_punctuation=True, n=3):
        """Shingle and incorporate the given file.

        :param f: A file-like stream or filename.
        """
        close = False
        if isinstance(f, str):
            f = open(f, 'r')
            close = True

        for shingle in _shingles(f, strip_punctuation=strip_punctuation, n=n):
            self.add(shingle)

        if close:
            f.close()

    def jaccard(self, other):
        """Approximate the Jaccard coefficient with the other set."""
        if self._mins is None or other._mins is None:
            return 0.0

        if len(self._mins) != len(other._mins):
            raise MinHashError("Only MinHashes of the same size can be "
                               "compared.")

        count_same = 0
        for (m1, m2) in zip(self._mins, other._mins):
            if m1 == m2:
                count_same += 1
        return count_same / len(self._mins)

    def hashes(self):
        """Return a list of minimum hashes seen so far, as a ``bytes`` object.
        """
        if self._mins is None:
            mins = [0 for _ in self._hashes]
        else:
            mins = self._mins

        bytes_iter = map(lambda x: x.to_bytes(4, byteorder="big"), mins)
        return b"".join(bytes_iter)
