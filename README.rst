lintnode - a JSLint server for more expedient linting
=====================================================

I was setting up `flymake-mode`_ with JSLint_, and thinking it was
pretty great, but that rhino start-up cost is pretty big for a flymake
application.  If we just kept JSLint running, wouldn't that be a lot
faster?

Then I caught a talk on the `node.js`_ server, and saw a way.

In my environment, this cuts jslint invocation time in half.

This project also depends on the Express_ framework, with the connect-form
and haml packages.  I recommend installation with npm_ to manage these
dependencies.

The ``jslint.curl`` script depends on curl, but you can easily
reproduce it with any other http client.

.. _flymake-mode: http://www.emacswiki.org/emacs/FlymakeJavaScript
.. _JSLint: http://www.jslint.com/
.. _node.js: http://nodejs.org/
.. _Express: http://expressjs.com/
.. _npm: http://npmjs.org/


Usage
-----

::

  $ node lintnode/app.js --port 3003 &
  Express started at http://localhost:3003/ in development mode

  $ lintnode/jslint.curl myfilthycode.js

The exit code of ``jslint.curl`` is currently not nearly as relevant
as the output on standard out.  The output should be fully compatible
with `JSLint's Rhino version`__.

.. __: http://www.jslint.com/rhino/


Emacs Usage
-----------

See the included `flymake-jslint.el`__.

.. __: flymake-jslint.el


Configuration
-------------

`jslint_port` may be passed on the node command line with the
``--port`` parameter.  It defaults to 3003.

`jstlint_options` is currently only configurable by editing
``app.js``.  For documentation on JSLint's options, see `JSLint
options`_.

.. _JSLint options: http://www.jslint.com/lint.html#options


Support
-------

This project is hosted at github, which has a wiki and an issue tracker:

  http://github.com/keturn/lintnode


License
-------

This software is distributed under the same license__ as JSLint, which
looks like the MIT License with one additional clause:

  The Software shall be used for Good, not Evil.

.. __: LICENSE
