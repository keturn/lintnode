/* HTTP interface to JSLint.

   Takes roughly half the time to jslint something with this than to
   start up a new rhino instance on every invocation.
   
   Invoke from bash script like:
   
     curl --form source="<${1}" ${JSLINT_URL}

   If you use source="@${1}" instead, curl does it like a file upload.
   That includes a filename, which is nice, but has express make a
   temp file for the upload.
*/

/*global require */
/*global $, configure, use, set, enable, dirname, get, post, run */
/*global inspect, debug */
require.paths.unshift('lib');
require('express');
require('express/plugins'); /*global ContentLength, CommonLogger */
var posix = require('posix');
var JSLINT = require('./fulljslint');

configure(function () {
    use(ContentLength);
    use(CommonLogger);
    set('root', dirname(__filename));
    enable('cache view contents');
    enable('show exceptions');
});

var jslint_port = 3003;

/* copied from jslint's rhino.js */
var jslint_options = {
    bitwise: true,
    eqeqeq: true,
    immed: true,
    newcap: true,
    nomen: true,
    onevar: true,
    plusplus: true,
    regexp: true,
    rhino: true,
    undef: true,
    white: true
};

var outputErrors = function (errors) {
    var e, i, output = [];
    // debug("Handling " + errors.length + "errors" + '\n');
    function write(s) {
        output.push(s + '\n');
    }
    /* This formatting is copied from JSLint's rhino.js, to be compatible with
       the command-line invocation. */
    for (i = 0; i < errors.length; i += 1) {
        e = errors[i];
        if (e) {
            write('Lint at line ' + e.line + ' character ' +
                        e.character + ': ' + e.reason);
            write((e.evidence || '').
                        replace(/^\s*(\S*(\s+\S+)*)\s*$/, "$1"));
            write('');
        }
    }
    return output.join('');
};

get('/', function () {
    this.render('upload.haml.html');
});

post('/jslint', function handleLintPost() {
    var request = this, source, filename;
    source = request.param('source');
    // debug("/jslint handler triggered, source is");
    // debug(inspect(source));
    request.contentType('text');

    filename = request.param('filename') || source.filename;
    
    function doLint(sourcedata) {
        var passed, results;
        // debug("sourcedata is " + sourcedata);
        passed = JSLINT.JSLINT(sourcedata, jslint_options);
        if (passed) {
            // debug("no errors\n");
            results = "jslint: No problems found in " + filename + "\n";
        } else {
            results = outputErrors(JSLINT.JSLINT.errors);
            // debug("results are" + results);
        }
        return results;
    }

    if (source.tempfile) {
        // FIXME: It's pretty silly that we have express write the upload to
        // a tempfile only to read the entire thing back into memory
        // again.
        return posix.cat(source.tempfile).addCallback(
            function (sourcedata) {
                var results;
                results = doLint(sourcedata);
                request.halt(200, results);
                posix.unlink(source.tempfile);
            });
    } else {
        // debug("handling source directly from postdata");
        return doLint(source);
    }
});


/* This action always return some JSLint problems. */
var exampleFunc = function () {
    this.contentType('text');
    JSLINT.JSLINT("a = function(){ return 7 + x }()",
           jslint_options);
    return outputErrors(JSLINT.errors);
};

get('/example/errors', exampleFunc);
post('/example/errors', exampleFunc);


/* This action always returns JSLint's a-okay message. */
post('/example/ok', function () {
    this.contentType('text');
    return "jslint: No problems found in example.js\n";
});


run(jslint_port);
