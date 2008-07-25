/*
 *  AcroGrep - finds acronyms from text-file database.
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
	char buf[512], *s;
	int c, l, found = 0;
	if (argc != 3) {
		puts("illegal argv");
		return 1;
	}
	if (!(f = fopen(argv[1], "rt"))) {
		perror(argv[1]);
		return 2;
	}
	s = argv[2];
	for (l = 0; s[l]; ++l)
		s[l] = toupper(s[l]);
	c = *s;
	while (fgets(buf, sizeof buf, f) && *buf <= c) {
		if (*buf == c && !strncmp(buf, s, l) && buf[l] == '\t') {
			printf("%s - %s", s, buf + l + 1);
			found = 1;
		}
	}
	if (!found)
		printf("%s? WTF is that?\n", s);
	fclose(f);
	return 0;
}
