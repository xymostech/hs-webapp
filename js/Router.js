var React = require("react");
var _ = require("lodash");

var Router = React.createClass({
    propTypes: {
        routes: React.PropTypes.arrayOf(React.PropTypes.shape({
            pattern: React.PropTypes.string,
            klass: React.PropTypes.any
        })),
        fallbackKlass: React.PropTypes.any,
        path: React.PropTypes.string
    },

    getDefaultProps: function() {
        return {
            routes: []
        };
    },

    render: function() {
        var element = _.find(_.map(this.props.routes, (route) => {
            var match = this._matchPath(route.pattern, this.props.path);
            if (match.valid) {
                return <route.klass {...match.data} />;
            }
        }));

        if (element) {
            return element;
        } else {
            return <this.props.fallbackKlass />;
        }
    },

    _matchPath: function(pattern, path) {
        var splitPattern = this._splitPath(pattern);
        var splitPath = this._splitPath(path);

        if (splitPattern.length !== splitPath.length) {
            return { valid: false };
        }

        var data = {};
        var argPathRegex = /^<(.*)>$/;

        var matches = _.all(
            _.zip(splitPattern, splitPath), ([patternPiece, pathPiece]) => {
                var match = patternPiece.match(argPathRegex);
                if (match) {
                    var argName = match[1];
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
    },

    _splitPath: function(path) {
        // Split the path into its segments, ignoring empty segments
        // TODO(emily): Maybe redirect a//b => a/b
        return _.filter(path.slice(1).split("/"), (segment) => segment !== "");
    }
});

module.exports = Router;
