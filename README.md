# cl-pixman
`cl-pixman` is a Common Lisp wrapper for [pixman](http://pixman.org) written
on CFFI. Pixman is a portable library for low-level pixel manipulation.  

I wrote this wrapper to have an alternative to SDL2 blitting for low-level
pixel operation. It directly uses CFFI pointers to avoid.
Supported API is minimal (only `pixman_image_*` API is partially implemented)
so feel free to contribute.  

It is licensed under the LLGPL.

## Installation
If you are using [quicklisp](http://www.quicklisp.org/beta/):  
```bash
cd <your quicklisp directory>/local-projects/  
git clone <me>  
```
Then you can just
```
(ql:quickload "pixman")
```
in your REPL.  

You'll need a `pixman` shared object accessible to your Lisp at runtime,
if you are using GNU, you can use your distro package.  

### Tested platforms
* SBCL 1.3.19 on GNU/Linux on amd64.
* CCL 1.11-r16635 on macOS 10.12.5 on amd64 through virtual machine.
* CCL 1.11-r16635 on Windows 7 on amd64 (pixman.dll built from VS 2017).

### TODO list
* Everything but some `pixman_image_*` API.  
* Examples.  

