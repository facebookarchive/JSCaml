# JSCaml

JSCaml is a compiler for typed JavaScript, using OCaml as the target runtime.

The compiler is an extension of the Flow static typechecker for JavaScript.
To find out more about Flow, check out [flowtype.org](http://flowtype.org/).

## Why would you want to use JSCaml?

JSCaml assumes that your JavaScript files are fully type checked with Flow
and that the Flow types are correct and can be relied on. That implies that
you have to forgo using "JavaScript the incredibly bad parts" such as eval.

Moreover, if you are interested in running benchmarks, you'll probably find
that existing JavaScript JIT compilers are more than a match for the OCaml code
produced by JSCaml.

So why would you use JCaml at all? Two reasons come to mind:

1. You want to run your JavaScript code on small devices where JIT compilers are
not available or do a bad job.
2. If you are actually writing your code in
[REASON](http://facebook.github.io/reason/) but you want to make use of
JavaScript libraries and you also want to run on small devices.

In either case you can translate all of your JavaScript code to REASON and then
you won't have a use for JSCaml. You might, however, find it a bit easier to
just clean up the JavaScript code and make it fully type checked with Flow.
Once you've done that you can use JSCaml to produce precompiled machine code
that starts up quickly and uses far less memory than a high end JavaScript VM.

That said, JSCaml is still incomplete and more work is needed before realistic
programs can routinely be compiled and expected to just run without problems.
So, unless you are interested to work on the compiler and its runtime, hang on
for a bit and check back later.

## Getting the sources

After cloning the JSCaml repository you also need to get hold of the submodules:

```
git submodule init
git submodule update --recursive --remote
```

Next, copy the relevant Webkit files to the runtime/regexp subdirectory
and patch them to make them self contained.

```
cd regexp
./copy-patch-and-build.sh
cd ..
```

## Building the JSCaml compiler

First set up your environment as instructed in
[Flow repository](https://github.com/facebook/flow),
making sure that you can build and test Flow.

To build JSCaml use the buildcompiler.sh script in the compiler directory. It
copies some files over into the flow sub directory, runs its make, copies the
resulting extended flow binary back into the JSCaml bin directory and removes
the copied files.

## Building the JSCaml runtime

The JSCaml runtime is a library, written in OCaml, that provides the builtin
object model for translated JavaScript programs. The library makes use of some
C/C++ helper files that come from [Webkit](https://github.com/WebKit/webkit)
as mentioned in the section on getting the sources.

To build the runtime run the build.sh script in the runtime directory.
This creates a bunch of files in the runtime/_build and runtime/_cobj
directories, most of which are later required to be referenced by the ocamlopt
call that compiles the translated (into OCaml) JS files together with the
builtin object model binaries into a single executable.

It would be nice the JSCaml runtime could be packaged into a single file and if
there were a simple way for ocamlopt get everything from the unified file, but
this seems to be impossible. If you know how to do this, or can make it possible
somehow, please help.

## Running the JSCaml tests

As yet, there is no support for running a large test suite such as
https://github.com/tc39/test262 and quite a bit of work is needed to deal
with the lack of Flow type annotations and the use of features
from "JavaScript the very bad parts".

In the early development of JSCaml modified versions of these tests have been
actually been run and about 5000 of them pass, so there is hope.

In this repository, right now, there is a single "smoke alarm" test.
To compile this test run the build.sh script. (This will actually compile
all of the JS files in the directory.)

To run the test file, just run the .out/test binary.

## License
JSCaml is BSD-licensed. We also provide an additional patent grant.
