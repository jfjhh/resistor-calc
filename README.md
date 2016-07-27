# Resistor Combinations

I needed to create a weird resistor value, so I wrote these programs that will
find the closest possible combination from (a subset of) the E12 resistor
values.

The Scheme implementation is hilariously complex and inefficient, but I was
mostly just messing around with Scheme itself.

The C implementation is fast enough, but could be made incredibly faster by
sorting the results and searching them for a value, not by recalculating all the
combinations every time.

For easy use, the C code was modified to output a table of all the values.
Simply go to the line number of the resistor value you desire (or grep for it)
to get the combination.

- `c/E12_limited-kit_1-1M_combinations.log` contains values from a kit that I
  have that only has base values of `10`, `22`, and `47` in 5 decades, plus a
  `1M` value, too.

- `c/E12_1-10M_combinations.log` contains values from the full E12 set of
  values, up to 6 decades, plus a `10M` value, too. This is the most precise
  set: use it if you have the resistors for it.

