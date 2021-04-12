# Patch
[![Build Status](https://github.com/evolution-gaming/patch/workflows/CI/badge.svg)](https://github.com/evolution-gaming/patch/actions?query=workflow%3ACI)
[![Coverage Status](https://coveralls.io/repos/evolution-gaming/patch/badge.svg)](https://coveralls.io/r/evolution-gaming/patch)
[![Codacy Badge](https://app.codacy.com/project/badge/Grade/f9d2e05d108c4c259680b4b5f7753001)](https://www.codacy.com/gh/evolution-gaming/patch/dashboard?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=evolution-gaming/patch&amp;utm_campaign=Badge_Grade)
[![version](https://img.shields.io/badge/Version-click-blue)](https://evolution.jfrog.io/artifactory/api/search/latestVersion?g=com.evolution&a=patch_2.13&repos=maven-local-releases)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellowgreen.svg)](https://opensource.org/licenses/MIT)

## Setup

```scala
resolvers += sbt.librarymanagement.MavenRepository(
  "artifactory-evolution-public",
  "https://evolution.jfrog.io/artifactory/public")

libraryDependencies += "com.evolution" %% "patch" % "0.0.3"
```