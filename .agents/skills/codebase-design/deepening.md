# Deepening a module

Load this when a shallow module, or a cluster of them that keep changing together, is to be merged into one deeper module. This guide uses the terms defined in [`SKILL.md`](SKILL.md).

What shapes a deepening most is what the new module depends on. Sort each dependency into one of the four kinds below and name the kind out loud: it decides whether a seam is needed, what fills it, and what the tests run against. When a module has dependencies of several kinds, handle each by its own kind.

## Dependency kinds

### 1. In-process

Logic that never leaves the process: calculations and data held in memory, with nothing read from or written to the outside. This kind is the easy one. Fold the shallow pieces together behind an interface shaped for its callers, and let the tests run the real code through it; there is nothing to swap, so no seam is needed.

### 2. Local, with a stand-in

Something outside the process that has a faithful local stand-in a test can run for real: a temporary directory for the file system, an embedded or throwaway database for the real one, a local copy of a message broker. Deepen the module so it owns the dependency outright, and have the tests run the stand-in. Keep the dependency entirely inside the module: callers see only what they actually have to choose, such as a location.

### 3. Across the network, and yours

Code you own that sits across a network or a process gap: another of your own programs, a worker of yours that you message through a broker, an internal endpoint. Gather the logic into a single deep module on the near side, and put the transport behind a seam at its edge. Two adapters fill the seam - the real transport for live use, and one that keeps everything in memory for tests - which is exactly what makes it a real seam rather than a hypothetical one.

### 4. Third-party

A system you do not control: a payment provider, a mail sender, a vendor's hosted platform. Reach it only through a seam, so the live client is one adapter and a fake written for tests is the other. Unlike the in-memory adapter for your own code in kind 3, this fake imitates a system you cannot see inside, so keep it to what the provider documents and nothing more.

## Seams and tests

The seam rules and "replace, don't layer" in [`SKILL.md`](SKILL.md) apply as they stand. The one thing deepening adds is order: the old tests go only after the new ones cover the behaviour, since removing them first leaves a stretch where nothing guards it.
