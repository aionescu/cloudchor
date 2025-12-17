# CloudChor

CloudChor is a Haskell library for lighweight and efficient choreographic programming with multiply-located values.
It is an extension of the [HasChor](https://github.com/gshen42/HasChor) library.

For more details, see the PEPM '26 paper: [Towards Lightweight and Efficient Choreographic Cloud Services](https://doi.org/10.1145/3779209.3779537).

## Repository structure

* [`src/`](src/) contains the source code of the library.
  * [`Choreography.Location.Multi`](src/Choreography/Location/Multi.hs) is the module that implements multiply-located values.
* [`examples/`](examples/) includes example programs using the library.
  * [`clean-room`](examples/clean-room/): An implementation of a data clean room protocol as a choreography.
  * [`unsoundness`](examples/unsoundness): Two counter-examples showing how unrestricted IO can lead to unsoundness.
  * [`HasChor`](examples/HasChor): The original examples from the HasChor repository.
* [`bench/`](bench/) contains the setup for running benchmarks to compare `cond` vs. `cond_`.

## License

This repository is provided under the terms of the [Mozilla Public License 2.0](LICENSE.txt).
It is based on HasChor, which is provided under the [BSD-3-Clause License](LICENSE-HasChor.txt).
