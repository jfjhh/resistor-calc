/**
 * Combines E12 resistor values into a massive lookup table.
 * Copyright (C) 2016  Alex Striff
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

static double DOUBLE_EPSILON = 1e-3; /* An order of magnitude better than 1%. */
static double END_VALUE      = 1e+7; /* A 10M resistor. */

typedef double resistor_t;
typedef resistor_t (*combination_f)(resistor_t, resistor_t);
typedef struct {
	combination_f func;
	char sym[4];
} combination_t;

typedef struct {
	resistor_t a;
	resistor_t b;
	resistor_t value;
	combination_t c;
} calculation_t;

static calculation_t best_combination = { 0 };

static uint32_t decades = 6;
static resistor_t base_values[] = { /* E12 values. */
	10.0, 12.0, 15.0, 18.0, 22.0, 27.0,
	33.0, 39.0, 47.0, 56.0, 68.0, 82.0,
};

static resistor_t target;

resistor_t series(resistor_t a, resistor_t b)
{
	return a + b;
}

resistor_t parallel(resistor_t a, resistor_t b)
{
	return 1.0 / ((1.0 / a) + (1.0 / b));
}

static combination_t combinations[] = {
	{series,   "&&"},
	{parallel, "||"},
};

void combine(combination_t combination,
		const resistor_t values[], size_t size)
{
	if (!values || !size || !(combination.func) || !(combination.sym))
		return;

	resistor_t head = values[0];
	for (size_t i = 0; i < size; i++) {
		resistor_t value = (combination.func)(head, values[i]);
		resistor_t diff  = fabs(target - value);
		if (fabs(target - value) < fabs(target - (best_combination.value))) {
			best_combination
				= (calculation_t) { head, values[i], value, combination };
			if (diff < DOUBLE_EPSILON)
				return;
		}
	}

	combine(combination, values + 1, size - 1);
}

int main(void)
{
	size_t base_length = sizeof(base_values) / sizeof(resistor_t);
	size_t comb_length = sizeof(combinations) / sizeof(combination_t);
	size_t length      = base_length * decades + 1;
	resistor_t values[length];
	FILE *outfile;

	if (!(outfile = fopen("E12_1-10M_combinations.log", "w")))
		return EXIT_FAILURE;

	for (uint32_t i = 0; i < decades; i++)
		for (size_t j = 0; j < base_length; j++)
			values[(i * base_length) + j] = base_values[j] * pow(10, i);
	values[length - 1] = END_VALUE;

	fputs("<Target>\t<R1>\t\t<func>\t<R2>\t\t"
			"<Value>\t\t<Diff>\t\t<% Diff>\n\n",
			outfile);

	/* I am aware it is stupidly inefficient to recalculate every time. This is
	 * for one-time table generation, though, so letting it run for a while
	 * doesn't really bother me. */
	for (resistor_t r = 1.0; r <= END_VALUE; r++) {
		target = r;

		for (size_t i = 0; i < comb_length; i++)
			combine(combinations[i], values, length);

		resistor_t value = best_combination.value;
		resistor_t diff  = fabs(target - value);
		resistor_t perc  = (diff / target) * 100.0;

		fprintf(outfile, "%.4e\t%.4e\t<%s>\t%.4e\t%.4e\t%.4e\t%2.2f%%\n",
				target, best_combination.a, best_combination.c.sym,
				best_combination.b, value, diff, perc);
	}

	return EXIT_SUCCESS;
}

