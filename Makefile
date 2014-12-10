# -*- mode: makefile; coding: us-ascii -*-
# vim: set filetype=makefile fileencoding=utf-8 sw=4 sts=4:
# Refer to license at the end of this file.
#
# Make or run tests for css_erl library.
# SE, created 10-Dec-2014, GNU Make 3.81 on MacOSX 10.9.5.

all: .css_erl

clean:
	rm ebin/*.beam ebin/*.erl

# --

.css_erl: ebin/css_yecc.beam ebin/css_leex.beam ebin/css_file.beam ebin/css_idents.beam

ebin/css_idents.beam: src/css_idents.erl
	erlc -o ebin $^

ebin/css_file.beam: src/css_file.erl
	erlc -o ebin $^

ebin/css_yecc.beam: ebin/css_yecc.erl
	erlc -o ebin $^

ebin/css_leex.beam: ebin/css_leex.erl
	erlc -o ebin $^

ebin/css_yecc.erl: src/css_yecc.yrl
	erlc -o ebin $^

ebin/css_leex.erl: src/css_leex.xrl
	erlc -o ebin $^


