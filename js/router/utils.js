import _ from "lodash";

export const splitPath = function(path: string): Array<string> {
    // Split the path into its segments, ignoring empty segments
    // TODO(emily): Maybe redirect a//b => a/b
    return _.filter(path.slice(1).split("/"), (segment) => segment !== "");
}

export const matchPath = function(pattern: string,
                                  path: string): {valid: boolean; data: Object} {
    const patternPieces = splitPath(pattern);
    const pathPieces = splitPath(path);

    if (patternPieces.length !== pathPieces.length) {
        return { valid: false };
    }

    const data = {};
    const argPathRegex = /^<(.*)>$/;

    const matches = _.all(
        _.zip(patternPieces, pathPieces), ([patternPiece, pathPiece]) => {
            const match = patternPiece.match(argPathRegex);
            if (match) {
                const argName = match[1];
                data[argName] = pathPiece;
                return true;
            } else {
                return patternPiece === pathPiece;
            }
        });

    return {
        valid: matches,
        data: data
    };
}
