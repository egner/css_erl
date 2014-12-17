css_erl
=======

This is an erlang library for reading and writing CSS files.

The syntax is CSS 2.1, with some extensions for CSS 3.
Support for CSS 3 is incomplete and also problematic since
the language is not very well specified at this point in time.
The parser also accepts a number of extensions and some invalid
input in order to keep on reading.

The main use of the library has been to find obsolete code
and typos when developing CSS in relation to Erlang/Yaws.

Definitions in CSS are merged at run-time, making it difficult
to spot obsolete code. This means the authors of CSS code tend
to avoid removing obsolete code in order to avoid breaking
something. This quickly results in the usual messy CSS files.

As part of our build tools, we extract all potential identifiers
from the Erlang, Yaws and HTML source files by extracting all
literals that match the 'ident' syntax. Then the CSS sources are
parsed. Identifiers in the CSS that do not appear anywhere in
the Erlang, Yaws or HTML sources are reported. Very likely they
refer to obsolete CSS or indicate a typo, either in CSS or in
the Erlang, Yaws or HTML sources.

The approach is an instance of "it might work in practice but
it will never work in theory", because identifiers in Erlang
can be constructed by arbitrary complicated computations.
However, the method has been proven to work well in practice
in case you can rewrite the Erlang code such that it contains
all CSS identifiers as literals.

Starting points:

* `make`: build and run unit tests. You might want to set the
  environment variable `PA_YAWS` to let the lib know where to
  find `yaws/ebin`, if you have one.
* `make lib && ./css_rewrite.es test/css_file_SUITE_data/somedefs.css a.css && more a.css`:
  Read `somedefs.css` and print it out again.
* `make lib && ./css_idents.es test/\*_data/somedefs2.css test/css_idents_SUITE.erl`:
  Find out that `hello-world` is in the CSS but not in the .erl file.
* `make repl`: build the lib and the test modules and start a properly set up interactive Erlang interpreter.

#### License.

This code is reproduced here with permission and has originally
been developed for Entelios AG, an EnerNOC company, 2013-2014.

Refer to the individual files for the actual license agreement.
MIT License, Sebastian Egner, 2014.
