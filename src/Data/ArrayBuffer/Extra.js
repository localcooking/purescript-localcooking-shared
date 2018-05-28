"use strict";


exports.newUint8Array = function newUint8Array (xs) {
    return new Uint8Array(xs);
};

exports.uint8ArrayToArray = function uint8ArrayToArray (xs) {
    var ys = Array.from !== undefined ? Array.from(xs) : [].slice.call(xs);
    return ys;
};
