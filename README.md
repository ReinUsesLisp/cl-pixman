# cl-pixman
`cl-pixman` is a Common Lisp wrapper for [pixman](http://pixman.org) written on
CFFI. It is licensed under the LLGPL.

I wrote this wrapper to have an alternative to SDL2 blitting for low-level
pixel operation. It directly uses CFFI pointers to avoid.
Supported API is minimal (only `pixman_image_*` API is partially implemented)
so feel free to contribute.

## Installation
If you are using [Quicklisp](http://www.quicklisp.org/beta/):  
```bash
cd <your quicklisp directory>/local-projects/  
git clone <me>  
```
Then you can just
```
(ql:quickload "pixman")
```
in your REPL.  
It was tested on SBCL using GNU/Linux on a x86 machine.

You'll need a `pixman` shared object accessible to your Lisp at runtime,
if you are using GNU, you can use your distro package.
Darwin and Windows were not tested yet.

## TODO list
Everything but some `pixman_image_*` API.  
Examples.  
Test Darwin and Windows compatibility.

