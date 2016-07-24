"""``setuptools`` packaging."""

from setuptools import setup, find_packages

if __name__ == "__main__":
    setup(
        name="tastic",
        description="Tools for happier TAs.",
        version="0.1.0",
        packages=find_packages(),

        author="Ian Kuehne",
        author_email="ikuehne617@gmail.com",
        license="GPLv3",

        # Non-code resources.
        package_data={
            # For now, just include all YAML files, as the only data we need are
            # default configurations.
            "": "*.yaml"
        },

        url="http://tastic.readthedocs.io/en/latest/"
    )
