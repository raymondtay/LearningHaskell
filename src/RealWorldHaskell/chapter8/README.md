Let's build a couple of functions to determine if a file is an ELF object file
this is the format used for executables on almost all modern UNIX like systems.

The functions in these modules (i.e., `Data.ByteString.Char8`) only work with byte-sized Char
values, so they are only suitable for use with ASCII and some European character sets. Values
above 255 are truncated.
