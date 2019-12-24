# NixOS Status

This is unofficial status page for [NixOS](https://nixos.org).

[![twitter](/assets/twitter.png)](https://twitter.com/grhmc/status/1208167878584885249)

## Build

We're using Nix of course. To buid the HTML file:

```
$ nix-build -A html
```

And open the file:

Linux:

```
$ xdg-open result/Main.html
```

MacOS:

```
$ open result/Main.html
```

## Development

Use `nix-shell` to enter the development environment:

```
$ nix-shell
```

Within the shell you will have [elm](https://elm-lang.org) available.

Within nix-shell you can start the reactor:

```
$ elm reactor
```

Then open http://localhost:8000/src/Main.elm in browser.

[Direnv](https://github.com/direnv/direnv) environment is compatible with [lorri](https://github.com/target/lorri).
