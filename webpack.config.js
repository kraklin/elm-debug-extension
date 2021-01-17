const path = require('path');
const webpack = require('webpack');
const ZipPlugin = require('zip-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const {CleanWebpackPlugin} = require('clean-webpack-plugin');
const ExtensionReloader = require('webpack-extension-reloader');
const {ESBuildPlugin, ESBuildMinifyPlugin} = require('esbuild-loader');
const WextManifestWebpackPlugin = require('wext-manifest-webpack-plugin');
const FixStyleOnlyEntriesPlugin = require('webpack-fix-style-only-entries');
const OptimizeCSSAssetsPlugin = require('optimize-css-assets-webpack-plugin');

const PACKAGE = require('./package.json');
const nodeEnv = process.env.NODE_ENV || 'development';
const targetBrowser = process.env.TARGET_BROWSER;

const extensionReloaderPlugin =
  nodeEnv === 'development'
    ? new ExtensionReloader({
        port: 9090,
        reloadPage: true,
        entries: {
          // TODO: reload manifest on update
          contentScript: 'contentScript',
          background: 'background',
          extensionPage: ['popup', 'options','devtools'],
        },
      })
    : () => {
        this.apply = () => {};
      };

const getExtensionFileType = (browser) => {
  if (browser === 'opera') {
    return 'crx';
  }
  if (browser === 'firefox') {
    return 'xpi';
  }

  return 'zip';
};

module.exports = {
  devtool: false, // https://github.com/webpack/webpack/issues/1194#issuecomment-560382342

  mode: nodeEnv,

  stats: {
    all: false,
    builtAt: true,
    errors: true,
    hash: true,
  },

  entry: {
    manifest: './source/manifest.json',
    background: './source/scripts/background.js',
    contentScript: './source/scripts/contentScript.js',
    popup: ['./source/popup/index.js', './source/popup/styles.scss'],
    devtools: ['./source/devtools/devtools.js' ],
    devtoolsPanel: ['./source/devtools/index.js', './source/devtools/styles.scss' ],
    options: ['./source/options/index.js', './source/options/styles.scss'],
    styles: ['./source/popup/styles.scss', './source/styles/options.scss'],
  },

  output: {
    path: path.resolve(__dirname, 'extension', targetBrowser),
    filename: 'js/[name].bundle.js',
  },

  module: {
    rules: [
      {
        type: 'javascript/auto', // prevent webpack handling json with its own loaders,
        test: /manifest\.json$/,
        use: {
          loader: 'wext-manifest-loader',
          options: {
            usePackageJSONVersion: true, // set to false to not use package.json version for manifest
          },
        },
      },
      {
        test: /\.js$/,
        loader: 'esbuild-loader',
        options: {
          target: 'es2015'
        }
      },
      {
        test: /\.scss$/,
        use: [
          {
            loader: 'file-loader',
            options: {
              name: '[path]/[name].css',
              context: './source/',
              outputPath: 'css/',
            },
          },
          'extract-loader',
          {
            loader: 'css-loader',
            options: {
              sourceMap: nodeEnv === 'development',
            },
          },
          {
            loader: 'postcss-loader',
            options: {
              ident: 'postcss',
              // eslint-disable-next-line global-require
              plugins: [require('autoprefixer')()],
            },
          },
          'resolve-url-loader',
          'sass-loader',
        ],
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: [
          nodeEnv === 'development' ? 'elm-hot-webpack-loader' : null,
          {
            loader: 'elm-webpack-loader',
            options: {
              optimize: nodeEnv === 'production',
              debug: nodeEnv === 'development',

            }
          }
        ].filter(x => x),
      },
      {
        test   : /\.(ttf|eot|svg|woff(2)?)(\?[a-z0-9=&.]+)?$/,
        loader : 'file-loader'
      }
    ],
  },

  plugins: [
    new webpack.ProgressPlugin(),
    // Generate manifest.json
    new WextManifestWebpackPlugin(),
    // Generate sourcemaps
    new webpack.SourceMapDevToolPlugin({filename: false}),
    // Remove style entries js bundle
    new FixStyleOnlyEntriesPlugin({silent: true}),
    new webpack.EnvironmentPlugin(['NODE_ENV', 'TARGET_BROWSER']),
    new CleanWebpackPlugin({
      cleanOnceBeforeBuildPatterns: [
        path.join(process.cwd(), `extension/${targetBrowser}`),
        path.join(
          process.cwd(),
          `extension/${PACKAGE.name}-${PACKAGE.version}-${targetBrowser}.${getExtensionFileType(targetBrowser)}`
        ),
      ],
      cleanStaleWebpackAssets: false,
      verbose: true,
    }),
    new HtmlWebpackPlugin({
      template: 'source/devtools/devtools-page.html',
      // inject: false,
      chunks: ['devtools'],
      filename: 'devtools-page.html',
    }),
    new HtmlWebpackPlugin({
      template: 'source/devtools/panel.html',
      // inject: false,
      chunks: ['devtoolsPanel'],
      filename: 'devtools.html',
    }),
    new HtmlWebpackPlugin({
      template: 'source/options/index.html',
      // inject: false,
      chunks: ['options'],
      filename: 'options.html',
    }),
    new HtmlWebpackPlugin({
      template: 'source/popup/index.html',
      // inject: false,
      chunks: ['popup'],
      filename: 'popup.html',
    }),
    new ESBuildPlugin(),
    new CopyWebpackPlugin([{from: 'source/assets', to: 'assets'}]),
    extensionReloaderPlugin,
  ],

  optimization: {
    minimizer: [
      new ESBuildMinifyPlugin({
        target: 'es2015'
      }),
      new OptimizeCSSAssetsPlugin({
        cssProcessorPluginOptions: {
          preset: ['default', {discardComments: {removeAll: true}}],
        },
      }),
      new ZipPlugin({
        path: path.resolve(__dirname, 'extension'),
        extension: `${getExtensionFileType(targetBrowser)}`,
        filename: `${PACKAGE.name}-${PACKAGE.version}-${targetBrowser}`
      }),
    ],
  },
};
