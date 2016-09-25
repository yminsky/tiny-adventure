# Tiny Adventure #

This is a work in progress, a very small adventure game, mostly made
for the purposes of playing with my kids. Really, there's almost
nothing here...

If you want to understand the basic structure, a good place to start
is the [State](state.mli) module, which is the basic data structure that tracks
everything about the game.

This is build against a bleeding edge versions of
[base](https://github.com/janestreet/base), so you'll need to do
something like this to get the right versions in OPAM.

```
opam remote add js-bleeding git@github.com:janestreet/opam-repository.git
```