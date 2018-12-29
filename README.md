# gtk-time

Playing with gi-gtk-declarative.

Instructions:

```bash
git clone https://github.com/danieljharvey/gtk-time

brew install gobject-introspection gtk+ gtk+3
```

Add this to .bashrc or equivalent so packages are found:

```
export PKG_CONFIG_PATH=/usr/local/opt/libffi/lib/pkgconfig
```

If this path doesn't work, `brew info libffi` will tell you the correct install path to use.

All good to go? Great.

```bash
stack build
```

And then, assuming all went well...

```bash
stack exec gtk-test-exe
```

Hooray!
