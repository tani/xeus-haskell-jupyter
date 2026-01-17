# TODO: Unimplemented Features & Roadmap

This document tracks planned improvements and currently missing features for `xeus-haskell`.

## Core Kernel Features

- [x] **`is_complete` implementation**: Map parsability of mixed cells to Jupyter's completeness status.
- [ ] **Incomplete Status**: Refine `is_complete` to distinguish between `invalid` and `incomplete` code (e.g., unclosed delimiters).
- [x] **`inspect_request`**: Support for "Introspection" (Shift+Tab in Jupyter) to show documentation or type signatures for identifiers.
- [x] **`history_request`**: Implementation of the Jupyter history protocol to allow searching and retrieving previous cell inputs.
- [ ] **`Advanced Completion`**: Improve `completion_request` to support qualified names (e.g., `Prelude.putStrLn`) and type-aware suggestions.
- [ ] **Kernel Interrupt**: Add support for interrupting long-running Haskell executions (SIGINT handling).

## REPL Improvements

- [ ] **Selective Re-compilation**: Instead of concatenating all previous definitions for every new definition, implement a dependency-tracking system to only re-compile what is necessary.
- [ ] **Better Error Reporting**: Map MicroHs compile errors back to the specific line numbers in the Jupyter cell.
- [ ] **WASM Optimization**: Reduce the size of the WebAssembly bundle and optimize the initial "warmup" time in the browser.
- [x] **Language Extensions**: Work with the MicroHs upstream to support more modern Haskell extensions (e.g., `GADTs`, `TypeFamilies`).

## Rich Output (`Display` System)

- [ ] **Standard Library Instances**: Add `Display` instances for more types in the MicroHs standard library.
- [ ] **Data Visualization**: Create bridges for common Haskell charting libraries to output Jupyter-compatible JSON/Vega-Lite data.
- [ ] **Widgets**: Initial investigation into supporting Jupyter Widgets (ipywidgets protocol).

## Infrastructure & Testing

- [ ] **Enhanced CI**: Add automated UI tests using `playwright` or `selenium` to verify the kernel works in a real JupyterLab/JupyterLite instance.
- [ ] **Benchmarking**: Create a suite of performance benchmarks to track compilation and execution speed regressions.
- [ ] **Conda-forge/Emscripten-forge**: Package the kernel for easier installation without needing to build from source.
