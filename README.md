# abnf

[![Build](https://github.com/zakolenko/abnf/workflows/build/badge.svg?branch=main)](https://github.com/purebits/abnf/actions?query=branch%3Amain+workflow%3Abuild) [![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.github.purebits/abnf-core_2.13/badge.svg)](https://maven-badges.herokuapp.com/maven-central/io.github.purebits/abnf-core_2.13)

An ABNF parser based on [cats-parse](https://github.com/typelevel/cats-parse) and [droste](https://github.com/higherkindness/droste).
 
## TODO

- documentation
- tests
- ABNF grammar based parser generator 

## Usage

The packages are published on Maven Central.

```scala
libraryDependencies += "io.github.purebits" %% "abnf-core" % "<version>"
libraryDependencies += "io.github.purebits" %% "abnf-algebra" % "<version>"
```

## Documentation

Links:

- [Website](https://purebits.github.io/abnf/)
- [API documentation](https://purebits.github.io/abnf/api/)

## Contributing

The abnf project welcomes contributions from anybody wishing to participate.  All code or documentation that is provided must be licensed with the same license that abnf is licensed with (Apache 2.0, see [LICENCE](./LICENSE.md)).

People are expected to follow the [Scala Code of Conduct](./CODE_OF_CONDUCT.md) when discussing abnf on GitHub, Gitter channel, or other venues.

Feel free to open an issue if you notice a bug, have an idea for a feature, or have a question about the code. Pull requests are also gladly accepted. For more information, check out the [contributor guide](./CONTRIBUTING.md).

## License

All code in this repository is licensed under the Apache License, Version 2.0.  See [LICENCE](./LICENSE.md).
