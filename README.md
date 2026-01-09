<!--
  SPDX-FileCopyrightText: 2025 Alex Ionescu
  SPDX-License-Identifier: MPL-2.0
-->

# CloudChor

[![Hackage](https://img.shields.io/hackage/v/cloudchor?style=for-the-badge)](https://hackage.haskell.org/package/cloudchor)

CloudChor is a Haskell library for lighweight and efficient choreographic programming aimed at developing cloud services.
It is a backwards-compatible extension of the [HasChor](https://github.com/gshen42/HasChor) library.

For more details, see the PEPM '26 paper [Towards Lightweight and Efficient Choreographic Cloud Services](https://doi.org/10.1145/3779209.3779537).

## Repository structure

* [`src/`](src/) contains the source code of the library.
  * [`Choreography.Location.Multi`](src/Choreography/Location/Multi.hs) is the module that implements multiply-located values.
* [`examples/`](examples/) includes example programs using the library.
  * [`clean-room`](examples/clean-room/): An implementation of a data clean room protocol as a choreography.
  * [`unsoundness`](examples/unsoundness): Two counter-examples showing how unrestricted IO can lead to unsoundness.
  * [`HasChor`](examples/HasChor): The original examples from the HasChor repository.
* [`benchmark/`](benchmark/) contains the setup for running benchmarks to compare `cond` vs. `cond_`.
* [`paper/`](paper/) contains the full version of the paper, including appendices.

## License

This repository is provided under the terms of the [Mozilla Public License 2.0](LICENSES/MPL-2.0.txt).
It is based on HasChor, which is provided under the [BSD-3-Clause License](LICENSES/BSD-3-Clause.txt).
