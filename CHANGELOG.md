# Changelog for `raymarcher`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.2.0.0 - 2023-04-12

### Changed

- Converted raymarching to Accelerate (using the accelerate-llvm-native backend) for a major performance improvement.