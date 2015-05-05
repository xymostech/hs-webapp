import _ from "lodash";

const describeStack: Array<string> = [];
export const describe = function(name: string, func: Function) {
    describeStack.push(name);
    func();
    describeStack.pop();
};

const runTests: Array<{succeed: boolean; name: string; message: string}> = [];
export const it = function(name: string, func: Function) {
    try {
        func();

        runTests.push({
            succeed: true,
            name: describeStack.join(" ") + " " + name,
            message: ""
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
