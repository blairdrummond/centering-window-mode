# centering-window-mode

## Installation

This does the same thing as [centered-window-mode](https://github.com/anler/centered-window-mode) except that it doesn't use fringes, which makes it work in terminal. It also works in horizontally split windows, it doesn't have to be the only window in the frame. It does this while also being pretty good (though not perfect) at updating under changes. You can individually make certain windows use a fixed behaviour without disabling the mode.

## Pros and Cons

### Pros
- Works with **nlinum** mode (unfortunately doesn't support relative-linum)
- Works in terminal!
- Allows for multiple centered windows in one frame
- Individual settings for different windows

### Cons

- nlinum support isn't quite right yet... It isn't persistent...
- Can't track *all* changes in window configuration. transpose-frame doesn't seem to send the notification that it needs to update for instance. So you need to make some other window change for it to update to good settings.
- No Fringe. So if you like the other colours in the fringe... you won't find that here...
- Due to a an issue (see issues) cannot split horizontally while in centering window mode

## How to use it

Run these elisp commands

```lisp
(load "<path-to-file>/centering-window-mode.el")
(centering-window-mode)
```

**M-x** centering-window-mode

**M-x** centering-override-center

**M-x** centering-override-no-center

**M-x** centering-override-default

## Screenshots

![Normal Emacs](screenshots/1482968269.png)
![Emacs Under urxvt](screenshots/1482968282.png)
![Before](screenshots/1482968309.png)
![After](screenshots/1482968319.png)
