---
layout: default
title:  "Home"
section: "home"
---

Monocle is an `Optics` library where Optics gather the concepts of `Lens`, `Traversal`,
`Optional`, `Prism` and `Iso`. Monocle is strongly inspired by Haskell [Lens](https://github.com/ekmett/lens).

## Getting Started

```scala
import sbt._
resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.sonatypeRepo("snapshots")

scalaVersion := "2.12.0"         // or "2.11.8", "2.10.6"

val libraryVersion = "1.3.2"     // or "1.4.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "com.github.julien-truffaut"  %%  "monocle-core"    % libraryVersion,
  "com.github.julien-truffaut"  %%  "monocle-generic" % libraryVersion,
  "com.github.julien-truffaut"  %%  "monocle-macro"   % libraryVersion,
  "com.github.julien-truffaut"  %%  "monocle-state"   % libraryVersion,
  "com.github.julien-truffaut"  %%  "monocle-refined" % libraryVersion,
  "com.github.julien-truffaut"  %%  "monocle-unsafe"  % libraryVersion,
  "com.github.julien-truffaut"  %%  "monocle-law"     % libraryVersion % "test" 
)

// for @Lenses macro support
addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)
