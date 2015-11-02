hcalc
-----
An extended version of the toy calculator in [Learn You A Haskell](http://learnyouahaskell.com/functionally-solving-problems). This is a work in progress.

Features
--------
This is, first and foremost, a standard RPN calculator. So you can do

    Enter expression: 5 3 + 2 -
    -6.0

and the like. All numbers are considered to be `Double`s. (So you don't have `mod` and everything. I could just typecast things and whatnot, but, y'know.)

Constants like `e` and `pi` are supported. There are also standard unary functions like the trig ones:
    
    Enter expression: 4 pi / sin
    0.7071067811865475

Then, you have all sorts of what I call "fold operators" that collapse the entire stack to a single value. For instance, you can say

    Enter expression: 5 4 3 max 6 min
    5.0

These are essentially just functions with arbitrary arity. You'll find the arithmetic mean, geometric mean, RMS etc. also implemented inside.

Here's a probably new thing I've thought up. Prefixing an `_` to the name of a unary function maps it over the stack. So you can do stuff like

    Enter expression: 3 4 5 _log gm
    1.3483150398977648

where `gm` is the geometric mean.

Plans
-----
I'm not sure, but I think this is going to turn into one of those stack-based programming languages I hear of.

License
-------
Do whatever you like. (Someday, I think I'm going to grow to regret this.)

It'd be nice if you could attribute me, whatever you do, though. 
