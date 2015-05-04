module.exports = {
    entry: "./js/app.js",
    output: {
        path: "./build",
        filename: "app.js"
    },
    module: {
        loaders: [
            {
                test: /\.js$/,
                exclude: /node_modules/,
                loader: "babel-loader"
            }
        ]
    },
    devServer: {
        contentBase: "./",
        port: 7001,
        hot: true
    }
};
