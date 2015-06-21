var config = require("./webpack.config");
var webpack = require("webpack");
var WebpackDevServer = require("webpack-dev-server");

new WebpackDevServer(webpack(config), {
    hot: true,
    publicPath: config.output.publicPath
}).listen(config.PORT, "::", function(err, result) {
    if (err) {
        console.log(err);
    }
    console.log("Listening at [::]:" + config.PORT);
});
