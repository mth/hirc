/*
 *  History - collects history from dict database.
 *  Copyright (C) 2008 Madis Janson
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <stdio.h>
#include <ctype.h>

int main(int argc, char** argv) {
	FILE *f;
	char buf[32768], *key, *k, *p;
	int exists = 0;
	if (argc != 3 || !*argv[2]) {
		puts("illegal arguments");
		return 1;
	}
	if (!(f = fopen(argv[1], "rt"))) {
		perror(argv[1]);
		return 2;
	}
	key = argv[2];
	for (k = key; *k; ++k) {
		if (*k < ' ' && *k >= 0) {
			puts("illegal definition key");
			return 1;
		}
		*k = tolower(*k);
	}
	while (fgets(buf, sizeof buf, f)) {
		for (k = key, p = buf; *p != '\t'; ++p)
			if (!*p || (char) tolower(*p) != *(k++))
				goto next_line;
		if (!*k) {
			while (*++p && *p != '\t');
			if (*p) {
				fputs(p + 1, stdout);
				exists = 1;
			}
		}
	next_line:;
	}
	fclose(f);
	if (!exists)
		printf("Mis %s?\n", key);
	return 0;
}
