const t = require("../../test.js");
const assert = require("assert");

const {
    splitPath,
    matchPath
} = require("../utils.js");

t.describe("A path splitter", () => {
    t.it("splits paths", () => {
        assert.deepEqual(splitPath("/hello"), ["hello"]);
        assert.deepEqual(splitPath("/hello/world"), ["hello", "world"]);
    });

    t.it("ignores double /s", () => {
        assert.deepEqual(splitPath("/hello//world"), ["hello", "world"]);
    });

    t.it("ignores trailing /s", () => {
        assert.deepEqual(splitPath("/hello/"), ["hello"]);
    });

    t.it("doesn't break on pattern segments", () => {
        assert.deepEqual(splitPath("/hello/<name>"), ["hello", "<name>"]);
    });
});

t.describe("A route matcher", () => {
    t.it("matches literal paths", () => {
        assert(matchPath("/hello/world", "/hello/world").valid,
               "Match should be valid");
    });

    t.it("doesn't match different length paths", () => {
        assert(!matchPath("/hello", "/hello/world").valid,
               "Match shouldn't be valid");
        assert(!matchPath("/hello/world", "/hello").valid,
               "Match shouldn't be valid");
    });

    t.it("matches pattern segments", () => {
        assert(matchPath("/hello/<name>", "/hello/world").valid,
               "Match should be valid");
    });

    t.it("correctly pulls out segment data", () => {
        const match = matchPath("/hello/<name>/<test>", "/hello/world/blah");

        assert(match.valid, "Match should be valid");
        assert.deepEqual(match.data, { name: "world", test: "blah" });
    });
});
