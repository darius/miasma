from distutils.core import setup, Extension

module1 = Extension('shoothead',
                    sources = ['shoothead.c'])

setup (name = 'shoothead',
       version = '1.0',
       description = 'This lets us call dynamically-generated machine code.',
       ext_modules = [module1])
