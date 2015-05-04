var React = require("react");

var MainApp = React.createClass({
    render: () => {
        return <span>Hello, world!</span>;
    }
});

React.render(<MainApp />, document.body);
