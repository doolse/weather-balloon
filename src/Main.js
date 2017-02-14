const byline = require('byline');

exports.bylineStream = function (readStream) {
  return function() {
    return byline.createStream(readStream);
  }
}
