This library can handle two quite different notations for time
periods, one lispy and one modeled on the syntax used by
[cfengine](http://www.cfengine.org/)'s class notation.  It was written
to be used in long-running lisp processes doing various kinds of
system (computer) and environmental monitoring, so I could say "don't
page me at 3am about this" or "don't worry about this on weekends."  A
Python version of this library was useful, so I've made this CL
version public, too.

The library depends on the behavior of your lisp's implementation of
**`DECODE-UNIVERSAL-TIME`**.  It cannot currently be told to compare a
period against different time zones.

# Time Periods

The keyword-heavy list notation for time periods is:

 - **`(:SECOND s)`**
 - **`(:MINUTE m)`**
 - **`(:HOUR h)`** hours in 24-hour notation (i.e., midnight is 0, 11pm is 23:00)
 - **`(:DATE d)`** checks the day of the month
 - **`(:YEAR y)`**
 - **`(:DAY-OF-WEEK d)`** where *d* is one of **`:MONDAY :TUESDAY :WEDNESDAY :THURSDAY :FRIDAY :SATURDAY :SUNDAY`**
 - **`(:MONTH m)`** where *m* is a keyword representing a month name, as for the **`:DAY-OF-WEEK`** above, **`:MARCH`**, etc.
 - **`(:CLASS :user-defined-class)`** commonly used periods may be given names
 - **`(NOT expr)`**
 - **`(AND expr1 ... exprn)`** will short-circuit once false is found
 - **`(OR expr1 ... exprn)`** will short-circuit once true is found

All of these period notations except **`:class`** have a range version
**`(:year-range y1 y2)`**.  All of the ranges understand that time
cycles, so that **`(:hour-range 22 8)`** does the correct thing.  You
*may* indicate a year range with the first value larger than the
second, but a **`GIBBERISH-CYCLING-YEAR-RANGE`** warning will be
signaled about it.  **`(:year-range 2008 2005)`** is effectively
**`(not (:year-range 2006 2007))`**.  All ranges are inclusive, which
may do surprising things for hours (see string notation for ranges).

The case-insensitive string period notation is:

 - Sec00 - Sec59
 - Min00 - Min59
 - Hr00 - Hr23
 - Day1 - Day32  (note here it is called "Day" not "Date," favoring cfengine over CL naming)
 - Monday, Tuesday, etc.  - days of the week are themselves
 - January, February, etc.
 - Yr2000, etc.
 - any user defined period classes are simply the name of that class

The boolean operators, in order of precedence, are

 - `not` is either `!` or `~`:   **`!Friday`**
 - `and` is `.` :   **`January.Yr2008`**
 - `or` is `|` :  **`Saturday|Monday`**

Precedence may be forced by using parentheses, **`May.(Monday|Wednesday|Friday)`**.

There are two range notations for the strings.  Most of the time, at
least for English speakers, time ranges are inclusive, "Monday -
Friday," for example.  In hours, however, most of the time and for
most people 2-4 means 2:00-3:59.  The default range notation,
**`Hr2-4`**, however, means 2:00-4:59.  There are therefore two ways
to indicate a range with period strings:

 - inclusive range with a single dash: **`Hr5-16`**, **`Sunday-Tuesday`**, **`Min30-59`**
 - exclusive range with an arrow: **`Hr5->16`**, **`Sunday->Tuesday`**, **`Min30->0`**

Most of the time the exclusive range will be used only with hours.

# Examples

Some basic use:

```lisp
PERIOD> (in-period-p '(:minute-range 0 30))
T
PERIOD> (in-period-p '(or (:day-of-week :tuesday) (:minute-range 0 30)))
T
PERIOD> (in-period-p "Saturday-Wednesday.April.Yr2017")
T
```


# Package

The package name is **`:cl-period`** with a single nickname, **`:period`**.

It depends on [CL-YACC](http://www.pps.jussieu.fr/~jch/software/cl-yacc/).

You will need [LIFT](http://common-lisp.net/project/lift/) to run the unit tests.

## Conditions and Restarts

*condition*  
**`contract-violation`**  
  This base error condition isn't directly signalled, but is the
  parent of the three error conditions below. 

*condition*  
**`period-arity-violation`**  
  A time period or range was called with too many, or too few, arguments.

*condition*  
**`period-range-violation`**  
  A time period or range was called with values that made no sense for
  the given period (say, Hour 74). 

*condition*  
**`period-syntax-violation`**  
  The string parser wasn't able to decipher a period string.

*warning condition*  
**`gibberish-cycling-year-range`**  
  Raised when the first part of a year range is greater than the second
  part.  Feel free to muffle this on your own if you think this sort of
  range makes sense. 

If the string syntax is really broken CL-YACC will pass up parsing
conditions.

There are no restarts.

## Functions

*function*  
**`compile-period-string`** *`period-string` => period-list*  
 Converts a string period into the lispy period format.  May signal
 any of the conditions and the warning listed above. 

*generic function*  
**`in-period-p`** *`period &optional (time (get-universal-time))` => boolean*  
  Takes either a string or list period and returns **`t`** or
  **`nil`** if the given period matches the time.  A string will have
  to be compiled before being tested, so frequently run period checks
  should probably precompile all periods.  Some future version may
  memoize these string periods. 

*generic function*  
**`define-period-class`** *`name period` => period-list*  
 Adds new classes which are stored in the global variable
 *`**PERIOD-CLASSES**`*.  The class `name` may be either a keyword or
 a string (which will be coerced to a keyword).  The `period` may be
 either a string or list period representation:

```lisp
PERIOD> (define-period-class :weekend "Saturday-Sunday")
(:DAY-OF-WEEK-RANGE :SATURDAY :SUNDAY)
PERIOD> (in-period-p "Monday|Weekend")
```
