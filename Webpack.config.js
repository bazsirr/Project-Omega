const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");

module.exports = (env, argv) => {
  const isDev = argv.mode === "development";
  return {
    entry: "./fable_output/App.js",
    output: {
      path: path.join(__dirname, "dist"),
      filename: isDev ? "bundle.js" : "bundle.[contenthash].js",
      publicPath: isDev ? "/" : "./"
    },
    devServer: {
      port: 8080,
      hot: true,
      static: { directory: path.join(__dirname, "public") }
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
        template: "./public/index.html",
        filename: "index.html"
      }),
      ...(isDev ? [] : [new MiniCssExtractPlugin({ filename: "styles.[contenthash].css" })])
    ]
  };
};
