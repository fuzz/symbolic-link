# SymbolicLink

SymbolicLink provides tools for working with symbolic links on POSIX
systems.

The executable `symlink` changes to the user's home directory,
reads in a sequence of source/target mappings from a YAML file
in `.symlinks` and attempts to create them. If the target exists
and is a symbolic link it will be removed and replaced, otherwise
`symlink` will refuse to clobber it.

The function `filePathExist` works like `System.Posix.Files.fileExist`
but does not follow the symlink, thus making it suitable for working with
unreferenced symlinks. Unreferenced symlinks are not necessarily
"broken"; one should not have to handle exceptions to work with them.
