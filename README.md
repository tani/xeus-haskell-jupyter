# ![xeus-haskell](xeus-logo.svg)

[![Build Status](https://github.com/tani/xeus-haskell/actions/workflows/main.yml/badge.svg)](https://github.com/tani/xeus-haskell/actions/workflows/main.yml)

`xeus-haskell` is a Jupyter kernel for haskell based on the native implementation of the
Jupyter protocol [xeus](https://github.com/jupyter-xeus/xeus).

## Quickstart

### For native JupyterLab

```
# Install Pixi
# curl -fsSL https://pixi.sh/install.sh | sh

git clone https://github.com/tani/xeus-haskell
pushd xeus-haskell
pixi run -e dev prebuild
pixi run -e dev build
pixi run -e dev install
pixi run -e dev serve # JupyterLab is ready!
```

### For webassembly JupyterLite

```
# Install Pixi
# curl -fsSL https://pixi.sh/install.sh | sh

git clone https://github.com/tani/xeus-haskell
pushd xeus-haskell
pixi run -e wasm-host prebuild
pixi run -e wasm-build prebuild
pixi run -e wasm-build build
pixi run -e wasm-build serve # JupyterLite is ready!
```

## Trying it online

To try out xeus-haskell interactively in your web browser, just click on the link:

[Jupyterlite for Haskell](https://tani.github.io/xeus-haskell)

## Dependencies

`xeus-haskell` depends on

- [MicroHs](https://github.com/augustss/MicroHs)
- [xeus-zmq](https://github.com/jupyter-xeus/xeus-zmq)
- [xtl](https://github.com/xtensor-stack/xtl)
- [nlohmann_json](https://github.com/nlohmann/json)
- [cppzmq](https://github.com/zeromq/cppzmq)

## Contributing

See [CONTRIBUTING.md](./CONTRIBUTING.md) to know how to contribute and set up a
development environment.

## License

This software is licensed under the `Apache Software License 2.0`. See the [LICENSE](LICENSE)
file for details.
