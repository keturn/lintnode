/* HTTP interface to JSLint.

   Takes roughly half the time to jslint something with this than to
   start up a new rhino instance on every invocation.

   Invoke from bash script like:

     curl --form source="<${1}" ${JSLINT_URL}

   If you use source="@${1}" instead, curl does it like a file upload.
   That includes a filename, which is nice, but has express make a
   temp file for the upload.
*/

/*global process, require */
var express = require("express");
var JSLINT = require('./fulljslint');
var fs = require('fs');

var app = express.createServer();

app.configure(function () {
    app.use(require('connect-form')({keepExtensions: true}));
    app.use(express.errorHandler(
        { dumpExceptions: true, showStack: true }));
    app.use(express.bodyDecoder());
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

app.get('/', function (req, res) {
    res.render('upload.haml');
});

app.post('/jslint', function (request, res) {
    var filename;
    if (! request.form) {
        throw new TypeError("form data required");
    }
    return request.form.complete(function (err, fields, files) {
        var headers = {'Content-Type': 'text/plain'};

        function doLint(sourcedata) {
            var passed, results;
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

        if (files.source) {
            // FIXME: It's pretty silly that we have express write the upload to
            // a tempfile only to read the entire thing back into memory
            // again.
            filename = files.source.filename;
            fs.readFile(files.source.path, 'utf8',
                function (err, sourcedata) {
                    var results;
                    results = doLint(sourcedata);
                    res.send(results, headers, 200);
                    fs.unlink(files.source.path);
                });
        } else {
            filename = fields.filename;
            res.send(doLint(fields.source), headers, 200);
        }
    });
});


/* This action always return some JSLint problems. */
var exampleFunc = function (req, res) {
    JSLINT.JSLINT("a = function(){ return 7 + x }()",
        jslint_options);
    res.send(outputErrors(JSLINT.JSLINT.errors),
        {'Content-Type': 'text/plain'});
};

app.get('/example/errors', exampleFunc);
app.post('/example/errors', exampleFunc);


/* This action always returns JSLint's a-okay message. */
app.post('/example/ok', function (req, res) {
    res.send("jslint: No problems found in example.js\n",
        {'Content-Type': 'text/plain'});
});


function parseCommandLine() {
    var port_index = process.ARGV.indexOf('--port');
    if (port_index > -1) {
        jslint_port = process.ARGV[port_index + 1];
    }
}

parseCommandLine();

app.listen(jslint_port);
