# viwrap

### Todos

- [ ] Unify `PtyAct` API to use either `Handle` or `Fd`.
- [ ] Start implementing basic vim like editing.
- [X] Add terminal effect
- [ ] Figure out a better way to handle colored prompt
- [ ] Implement FFI for getting and setting terminal size

- [ ] Install singal handlers
	  - [ ] SIGWICH as we need to resize the master fd when the stdin size changes
		

### Bugs
- [ ] (B1) When we exit slave process there's hardware error due to master FD reading the input, figure out what's happening and fix it.
- [X] (B2) long lines doesn't wrap.

## working on

- [X] long lines doesn't wrap.

## Questions

- [ ] can we get controlling terminal for the slave proceess?

	  1. of course the answer is yes, coz posix-pty already has it, find out how to implement it, coz have a controlling terminal will fix B2.

	  2. Actually, the answer for the is not clear whether we can get controlling terminal or not. But whatever may be the case we actually don't need it to wrap lines.
