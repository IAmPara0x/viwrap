# viwrap

### Todos

- [X] Unify `PtyAct` API to use either `Handle` or `Fd`.
- [X] Add terminal effect
- [X] Implement FFI for getting and setting terminal size
- [X] implement a way to detect when a prompt is up (This task will need continuous work as we encounter new edge cases)
- [X] Start implementing basic vim like editing.
- [ ] Implement Advance vim editting features
- [ ] Figure out a better way to handle colored prompt
- [ ] Install singal handlers
	  - [ ] SIGWICH as we need to resize the master fd when the stdin size changes

### Bugs

- [X] (B1) When we exit slave process there's hardware error due to master FD reading the input, figure out what's happening and fix it.

## working on

- [ ] Implement Advance vim editting features

## Questions

- [ ] can we get controlling terminal for the slave proceess?
	  - But why do we even need a controling terminal?
	
- [ ] How to identify when the prompt is up?

- [X] How to determine when to supperese output from hmaster and when not to?
