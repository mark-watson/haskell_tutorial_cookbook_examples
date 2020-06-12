# Installation:

    pip install neuralcoref
    pip install -U spacy
    python -m spacy download en
    pip install falcon

    python setup.py install

This installs the executable **corefserver** on your path so you can run the server from any
directory using:

    corefserver

## If you get an error

ValueError: spacy.strings.StringStore size changed, may indicate binary incompatibility. Expected 112 from C header, got 88 from PyObject

Then you can either downgrade spaCy to 2.1.0, or build neuralcoref from source if you want to use a more recent version of spaCy.
