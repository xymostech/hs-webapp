import assert from "assert";
import { describe, it } from "../../test.js";

import { splitPath, matchPath } from "../utils";

describe("A path splitter", () => {
    it("splits paths", () => {
        assert.deepEqual(splitPath("/hello"), ["hello"]);
        assert.deepEqual(splitPath("/hello/world"), ["hello", "world"]);
    });

    it("ignores double /s", () => {
        assert.deepEqual(splitPath("/hello//world"), ["hello", "world"]);
    });

    it("ignores trailing /s", () => {
        assert.deepEqual(splitPath("/hello/"), ["hello"]);
    });

    it("doesn't break on pattern segments", () => {
        assert.deepEqual(splitPath("/hello/<name>"), ["hello", "<name>"]);
    });
});

describe("A route matcher", () => {
    it("matches literal paths", () => {
        assert(matchPath("/hello/world", "/hello/world").valid,
               "Match should be valid");
    });

    it("doesn't match different length paths", () => {
        assert(!matchPath("/hello", "/hello/world").valid,
               "Match shouldn't be valid");
        assert(!matchPath("/hello/world", "/hello").valid,
               "Match shouldn't be valid");
    });

    it("matches pattern segments", () => {
        assert(matchPath("/hello/<name>", "/hello/world").valid,
               "Match should be valid");
    });

    it("correctly pulls out segment data", () => {
        const match = matchPath("/hello/<name>/<test>", "/hello/world/blah");

        assert(match.valid, "Match should be valid");
        assert.deepEqual(match.data, { name: "world", test: "blah" });
    });
});
