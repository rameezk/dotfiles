---
name: codebase-design
description: The shared vocabulary and principles for shaping code into deep modules - module, interface, implementation, depth, seam, adapter, leverage, locality. Use whenever a module is being designed, split, merged or otherwise reshaped, when placing a seam, when judging whether a module earns its place, or when a skill elsewhere has to talk about module shape.
---

# Codebase Design

The shape this skill aims for is the **deep module**: little to learn before calling it, much done once it is called, testable where it is called, and the single home of its concern. This skill is the language for talking about that shape - in a design discussion, a spec, a test plan or a review - and the principles for choosing between shapes.

Two other sources of language sit beside this one. `docs/CONTEXT.md`, when it exists, names *what* the code is about; this vocabulary names *how it is shaped*. Use the domain terms for the concepts and these terms for the structure, never one in place of the other. The ADRs under `docs/adr/` record what is already settled: design inside them, and treat a shape that would contradict an accepted ADR as a new decision rather than something to slip in (see [[decision-context]]).

## Vocabulary

Whenever one of these ideas comes up, say its term and never a looser stand-in for it. Each term draws a line that a vaguer word would blur. That holds when an answer is written plainly too: keep the term, and explain it in passing if the reader may not know it, rather than swapping it for an everyday word.

- **Module** - the unit this vocabulary describes, at any scale. A single function counts, and so does a whole package or a feature that runs through several layers, as long as callers reach it through an interface.
- **Interface** - the full contract a caller depends on. Names and types are part of it, and so are the preconditions, the side effects, the ways it can fail and any set-up it needs.
- **Implementation** - the code behind the interface: what a caller never needs to know.
- **Depth** - how much a module does for its callers, set against how much they must understand to use it. A **deep** module hides a lot behind a little. With a **shallow** one, learning to call it costs nearly as much as writing its body yourself. Depth is judged from the caller's side: an implementation split into many small pieces is still deep if callers see none of them.
- **Seam** - a point in the code where a different piece of behaviour can be substituted while callers carry on unchanged. A module's interface defines one. Deciding where seams belong is one design question; deciding what fills them is another.
- **Adapter** - code that fills a seam by meeting its interface, such as the real database client or an in-memory fake used by tests. An adapter can be tiny or large; the term is about the job it does at the seam.
- **Leverage** - why depth matters to callers: each thing learned about the interface unlocks more behaviour, and the work done once inside is reused by every caller, tests included.
- **Locality** - why depth matters to maintainers: when something has to be fixed, changed or understood, there is one module to open rather than a hunt through every caller.

One stretch of code can be both an **implementation** and an **adapter**. Talking about its contents, call it the implementation; talking about the seam it serves, call it an adapter.

Some common words are left out on purpose:

- **"service"** and **"component"** - say module. Both carry framework and deployment baggage that says nothing about shape.
- **"API"** - say interface. "API" usually stops at the signature and leaves out the rest of what a caller relies on.
- **"boundary"** - say seam when you mean where something can be swapped, and interface when you mean what callers see. "Boundary" is used for too many things to pin either down.

## Principles

Name each principle when you use it, in the words of its bold heading, so a reader can tell which rule a judgement rests on.

- **The deletion test.** To judge whether a module earns its place, imagine deleting it and putting what it does back into its callers. If the complexity comes back - repeated in each caller, or pushed onto each of them to learn - the module was paying its way. If the complexity just disappears, because the module only handed calls along, it was a pass-through: shallow, and a candidate to fold into its caller or its neighbour. Report the result as "by the deletion test, this module is deep" or "... is shallow", followed by what the test showed.
- **The interface is the test surface.** Tests drive a module exactly as its callers do, through its interface at its seam. When a test cannot get at what it needs that way and wants a private helper or internal state, the fault lies with the module's shape: change the shape instead of opening a back door. Seams that exist only inside a deep module, for its own implementation to use, stay out of its interface.
- **One adapter is a hypothetical seam; two adapters make a real one.** Count what actually plugs into a seam. Until there are two - the one used for real, and one more, often for tests - the seam is a detour every reader must follow for no benefit, so leave it out until the second one exists.
- **Replace, don't layer.** When shallow modules are merged into a deeper one, write the tests at the new interface and delete the tests that pinned the old shallow pieces. Keeping both leaves the suite twice the size and tied to a structure that no longer exists.

## Going further

- **Deepening a module**, or merging a cluster of shallow ones: see [`deepening.md`](deepening.md) for how each kind of dependency shapes the result, where seams belong, and how the tests move.
- **Choosing between interfaces**, when a module's shape is still open: see [`design-it-twice.md`](design-it-twice.md) for drawing up several very different interfaces in parallel and comparing them before picking one.

## Hand-offs

- **A choice the user has to make** - which of several interfaces to build, seam placement, whether to merge two modules - is decided in [[refine]]. Lay out the options in this vocabulary with the one you would pick, and send the user there rather than settling it on your own.
- **A domain term that comes up or gets sharper** while designing goes into `docs/CONTEXT.md`, and **a structural decision that is hard to reverse** and clears the ADR bar is recorded as an ADR. Both go through [[decision-context]].
