# -*- mode: makefile; coding: us-ascii -*-
# vim: set filetype=makefile fileencoding=utf-8 sw=4 sts=4:
# Refer to license at the end of this file.
#
# Make or run tests for css_erl library.
# SE, created 10-Dec-2014, GNU Make 3.81 on MacOSX 10.9.5.

.SUFFIXES:

.PHONY: all

all: css_erl

# -- library 'css_erl' --

CSS_ERL_MODULES = css_leex css_yecc css_util css_file css_idents

.PHONY: css_erl

css_erl: $(addprefix ebin/, $(addsuffix .beam, $(CSS_ERL_MODULES)))

ebin/%.beam : src/%.erl
	erlc -o ebin $^

ebin/css_yecc.beam: src/css_yecc.yrl
	erlc -o ebin $^
	erlc -o ebin ebin/css_yecc.erl

ebin/css_leex.beam: src/css_leex.xrl
	erlc -o ebin $^
	erlc -o ebin ebin/css_leex.erl

# -- cleaning up --

.PHONY: clean

clean:
	rm -f ebin/*.beam ebin/*.erl

# -----------------------------------------------------------------------------
#
# The MIT License (MIT)
#
# Copyright (c) 2014, Sebastian Egner.
# Copyright (c) 2013-2014, Entelios AG, an EnerNOC company.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#
# (Source: http://opensource.org/licenses/MIT)
