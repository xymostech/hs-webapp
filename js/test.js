import _ from "lodash";

const describeStack = [];
export const describe = function(name, func) {
    describeStack.push(name);
    func();
    describeStack.pop();
};

const runTests = [];
export const it = function(name, func) {
    try {
        func();

        runTests.push({
            succeed: true,
            name: describeStack.join(" ") + " " + name
        });
    } catch (e) {
        runTests.push({
            succeed: false,
            name: describeStack.join(" ") + " " + name,
            message: e.message
        });
    }
};

const printResults = function() {
    _.each(runTests, (test) => {
        if (test.succeed) {
            console.log("\u2713", test.name);
        } else {
            console.log("\u2717", test.name + ": " + test.message);
        }
    });
};

if (require.main === module) {
    _.each(process.argv.slice(2), function(file) {
        require(file);
    });

    printResults();
}
