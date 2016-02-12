
(function () {
  var exports = module.exports;
  exports.reader = module.require("./reader").reader;

  exports.parse = function (src, options) {
    options = options || {};
    var addSrc = options.hasOwnProperty('addSourceForLeafs') ?
      options.addSourceForLeafs : true;
    var errors = [];

    var nodes = exports.reader.readSeq(src, function xform(type, read, start, end) {
      var result = { type: type };
      if (type === "error") {
        result.error = read.error;
        errors.push(result);
      } else if (addSrc && type !== 'list')
        result.source = src.slice(start.idx, end.idx);
      if (type === "list") result.children = read;
      return result;
    });

    return {
      type: "toplevel",
      errors: errors,
      children: nodes
    };
  };

})();
