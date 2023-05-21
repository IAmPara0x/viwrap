# viwrap

### Todos

- [X] Unify `PtyAct` API to use either `Handle` or `Fd`.
- [X] Add terminal effect
- [X] Implement FFI for getting and setting terminal size
- [X] implement a way to detect when a prompt is up (This task will need continuous work as we encounter new edge cases)
- [X] Start implementing basic vim like editing.
- [X] Improve Logging
- [X] long lines doesn't wrap properly.
- [X] SIGWICH as we need to resize the master fd when the stdin size changes

- [ ] Implement Tab when the cursor is at the middle of the line
- [ ] Differentiate betweeen \t and space
- [ ] Find a way to find Jittery cursor.
- [ ] Fix Jittery cursor
- [ ] Implement Advance vim editting features
- [ ] Figure out a better way to handle colored prompt

- [ ] Install signal handlers
  - [X] install and integrate SIGINT

- [ ] improve the algorithm to find where to insert the text such that it requires less calls to
      termsize and cursor position

- [ ] Document the code

- [ ] Make Viwrap more robust

- [ ] Implement test suite

### Bugs

- [X] (B1) When we exit slave process there's hardware error due to master FD reading the input, figure out what's happening and fix it.
- [X] ghci doesn't work with viwrap
- [X] Can't get terminal size reliably
- [X] sometimes doesn't show cursor when it's at beginning and there's no text
- [X] sometimes the current line doesn't get added to the line history
- [ ] sometimes viwrap becomes un responsive (most likely due to mvar)
- [?] Movement through line history is very buggy
- [ ] Spago repl doesn't work
- [ ] Can't get cursor position reliably
- [ ] ghci doesn't wrap lines when using viwrap

## working on

- [ ] Document the code
- [ ] Implement Advance vim editting features
- [ ] Improve ANSI escape sequence parser
- [ ] Find a way to find Jittery cursor.
- [ ] Install signal handlers

## Questions

- [X] can we get controlling terminal for the slave proceess?
	  - [?] But why do we even need a controling terminal?

- [ ] How to identify when the prompt is up?

- [X] How to determine when to supperese output from hmaster and when not to?

- [ ] Check If we can implement batching such that we take multiple input at a time and process it.

- [ ] How should we test viwrap?

### Remarks

- [X] There's no need for VIEdit
