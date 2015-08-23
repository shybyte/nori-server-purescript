"use strict";

// module FileSystem

var fs = require('fs');

exports.readTextFile = function(filename) {
  return function () {
    return fs.readFileSync(filename, 'utf8');
  }
};