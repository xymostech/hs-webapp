var React = require("react");

var Router = require("./Router.js");

var Test = React.createClass({
    render: function() {
        return <span>Hello, world!</span>;
    }
});

var Helloer = React.createClass({
    render: function() {
        return <span>Hello, {this.props.name}!</span>;
    }
});

var NotFound = React.createClass({
    render: function() {
        return <span>Not found!</span>;
    }
});

var routes = [
    {
        pattern: "/test",
        klass: Test
    }, {
        pattern: "/hello/<name>",
        klass: Helloer
    }
];

var MainApp = React.createClass({
    render: function() {
        return <Router
                   routes={routes}
                   fallbackKlass={NotFound}
                   path={this.props.location.pathname} />;
    }
});

React.render(<MainApp location={window.location} />, document.body);

window._ = require("lodash");
window.React = require("react/addons");
