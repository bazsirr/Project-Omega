const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");

module.exports = (env, argv) => {
  const isDev = argv.mode === "development";
  return {
    entry: "./index.js",
    output: {
      path: path.join(__dirname, "dist"),
      filename: "bundle.js",
      publicPath: "./"
    },
    module: {
      rules: [
        {
          test: /\.js$/,
          exclude: /node_modules/,
          use: "babel-loader"
        },
        {
          test: /\.css$/,
          use: [
            isDev ? "style-loader" : MiniCssExtractPlugin.loader,
            "css-loader"
          ]
        }
      ]
    },
    plugins: [
      new HtmlWebpackPlugin({
        template: "./index.html",
        filename: "index.html"
      }),
      ...(isDev ? [] : [new MiniCssExtractPlugin({ filename: "styles.css" })])
    ]
  };
};
