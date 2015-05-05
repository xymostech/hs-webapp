import React from "react";
import _ from "lodash";

import Router from "./router/Router.js";

const Test = React.createClass({
    render: function() {
        return <span>Hello, world!</span>;
    }
});

const Helloer = React.createClass({
    render: function() {
        return <span>Hello, {this.props.name}!</span>;
    }
});

const NotFound = React.createClass({
    render: function() {
        return <span>Not found!</span>;
    }
});

const routes = [
    {
        pattern: "/test",
        klass: Test
    }, {
        pattern: "/hello/<name>",
        klass: Helloer
    }
];

const MainApp = React.createClass({
    render: function() {
        return <Router
                   routes={routes}
                   fallbackKlass={NotFound}
                   path={this.props.location.pathname} />;
    }
});

React.render(<MainApp location={window.location} />, document.body);

window._ = _;
window.React = React;
