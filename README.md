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

## Limitations
* The deamon only supports single threaded operation
* The client does not handle interrupts (e.g `Ctrl-C`) correctly (it just
  closes). If you have to stop the currently executed command, you have to use
  `ghci-client --free` after killing the client.
