(function () {
  var exports = module.exports;

  exports.deepReduce = deepReduce;

  /**
   * Perform a deeply recursive reduce on a set of JSON, or a JSON-encodable Object hierarchy.
   *
   * @param  {Array|Object} collection
   * @param  {Function}     fn
   * @param  {*}            memo
   * @return {*}
   */
  function deepReduce(collection, fn, memo) {

    /**
     * @inner
     * @param  {*} value
     * @param  {String[]} path
     * @return {*}
     */
    function iterator(value, path) {
      var type = Object.prototype.toString.call(value);
      memo = fn(memo, value, path);
      if (type === '[object Array]') {
        for (var i = 0, len = value.length; i < len; i++) {
          iterator(value[i], path.concat(i));
        }
      } else if (type === '[object Object]') {
        for (var key in value) {
          iterator(value[key], path.concat(key));
        }
      }
      return memo;
    }

    return iterator(collection, []);
  }
})();

