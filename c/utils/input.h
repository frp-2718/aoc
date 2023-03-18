#ifndef UTILS_H
#define UTILS_H

char *file_to_string(char *filename);

// Returns a NULL-terminated array of strings.
char **file_to_lines(char *filename);
void free_lines(char** lines);

#endif // UTILS_H
