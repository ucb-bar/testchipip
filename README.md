# testchipip

Useful IP components for chips.
BAR projects generally use these components with [rocket-chip](https://github.com/freechipsproject/rocket-chip).

## Blocks
* Block device model
* Clock utilities for Chisel, e.g. clock mux, clock divider, etc.
* SERDES <-> TileLink
* Custom serial interface for debug with simulator interface
* TileLink splitter, switcher

## Usage
Testchipip can be used in your project in one of two ways:
1) As an sbt subproject that depends on rocket-chip, as in [chipyard](https://github.com/ucb-bar/chipyard/)
2) As a maven dependency (e.g. write

```
libraryDependencies += "edu.berkeley.cs" %% "testchipip" % "1.0-020719-SNAPSHOT"
```
in your build.sbt). Check [sonatype](https://oss.sonatype.org/content/repositories/snapshots/edu/berkeley/cs/testchipip_2.12/) to see the latest published version.

