var path = require("path");
var webpack = require("webpack");

var PORT = 7001;

module.exports = {
    entry: [
        "webpack-dev-server/client?http://localhost:" + PORT,
        "webpack/hot/only-dev-server",
        "./js/app"
    ],
    output: {
        path: path.join(__dirname, "build"),
        filename: "bundle.js",
        publicPath: "http://localhost:7001/"
    },
    plugins: [
        new webpack.HotModuleReplacementPlugin()
    ],
    resolve: {
        extensions: ["", ".js"]
    },
    module: {
        loaders: [
            {
                test: /\.js$/,
                include: path.join(__dirname, "js"),
                loaders: ["babel"]
            }
        ]
    },

    PORT: PORT
};
