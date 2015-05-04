const React = require("react");
const _ = require("lodash");

const {
    splitPath,
    matchPath
} = require("./utils.js");

const Router = React.createClass({
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
        const element = _.find(_.map(this.props.routes, (route) => {
            const match = matchPath(route.pattern, this.props.path);
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

});

module.exports = Router;
