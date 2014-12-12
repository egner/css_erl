css_erl
=======

This is an erlang library for reading and writing CSS files.

The syntax is CSS 2.1, with some extensions for CSS 3.
Support for CSS 3 is incomplete an also problematic since
the language is not very well specified at this point in time.

The main use of the library has been to find obsolete code
and typos when developping CSS in relation to Erlang/Yaws.

Definitions in CSS are merged at run-time, making it difficult
to spot obsolete code. This means the authors of CSS code tend
to avoid removing obsolete code in order to avoid breaking
something. This quickly results in messy CSS files.

As part of our build tools, we extrac all potential identifiers
from the Erlang, Yaws and HTML source files by extracting all
literals that match the 'ident' syntax. The the CSS sources are
parsed. Identifiers in the CSS that do not appear anywhere in
the Erlang, Yaws or HTML sources are reported. They very likely
refer to obsolete CSS or signify a typo (in CSS or in Erlang).

The approach is an instance of "it might work in practice but
it will never work in theory", because identifiers in Erlang
can be constructed by arbitrary complicated computations.
However, the method has been proven to work well in practice
in case you can rewrite the Erlang code such that it contains
all CSS identifiers as literals.

Starting points:
- `make`: build and run unit tests. You might want to edit `PA_YAWS`
  in `Makefile` to let the lib know here `yaws/ebin` is.
- `./css_rewrite.es test/css_file_SUITE_data/somedefs.css a.css`:
  Read `somedefs.css` and print it out again.
- `./css_idents.es test/\*_data/somedefs2.css test/css_idents_SUITE.erl`:
  Find out that `hello-world` is in the CSS but not in the .erl file.

#### License.

This code is reproduced here with permission and has originally
been developed for Entelios AG, an EnerNOC company, 2013-2014.

Refer to the individual files for the actual license agreement.
MIT License, Sebastian Egner, 2014.
