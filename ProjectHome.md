This library is partly inspired by Perl's Time::Period library, but now owes more to the time classes of [cfengine](http://www.cfengine.org).  There was a Ruby Quiz ([#114](http://rubyquiz.com/quiz144.html)) to do something similar, but they called it a time "window."  It's intended for use in long-running — or frequently running — programs whose action may be limited to particular time frames.  For example, if a program is empowered to send me a page I might want to tell it only to do so during work hours.

The library supports two notations for time periods, one lispy, one modeled on cfengine's classes.

First release (0.01 - you gotta start somewhere) April 18th, 2008.

See the [documentation](API.md).