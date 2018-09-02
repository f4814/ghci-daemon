# ghci-daemon
This package allows you to wrap a "stack ghci" command and control it by sending
commands to a network socket (or using the `ghci-client` executable).

## How to use?
Start the daemon:
```
ghci-daemon
```

Send a command:
```
ghci-client -c ":reload"
```

(If you installed this package using stack you might want to use `stack exec`
to call the executables.)

## Easy Vim integration
ghci-daemon can be integrated into vim without any plugins.

1. Make sure the daemon is running.
2. Load vim's ghc compiler plugin.
   ```
   :compiler ghc
   ```
3. Set makeprg to ghci-client
   ```
   :set makepgr=ghci-client \-c:reload
   ```

Running `:make` will now reload your program and load the errors into your
quickfix list.

## Limitations
* The deamon only supports single threaded operation
* The client does not handle interrupts (e.g `Ctrl-C`) correctly (it just
  closes). If you have to stop the currently executed command, you have to use
  `ghci-client --free` after killing the client.
