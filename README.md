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

