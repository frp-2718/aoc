#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CHUNK 100


void free_lines(char** lines) {
    if (lines) {
        for (size_t i = 0; lines[i]; i++) {
            free(lines[i]);
        }
    }
    free(lines);
    lines = NULL;
}

char *file_to_string(char *filename) {
    errno = 0;
    FILE *fp = fopen(filename, "r");
    if (fp == NULL) {
        perror("Failed to open file");
        return NULL;
    }
    if (fseek(fp, 0, SEEK_END) != 0) {
        perror("Failed to read file");
        return NULL;
    }

    size_t file_size = ftell(fp);
    if (file_size < 0) {
        perror("Failed to read file");
        return NULL;
    }
    rewind(fp);

    char *file_contents = malloc(file_size + 1);
    if (file_contents == NULL) {
        perror("Failed to allocate memory");
        fclose(fp);
        return NULL;
    }
    if (fread(file_contents, 1, file_size, fp) != (unsigned long)file_size) {
        if (ferror(fp)) {
            perror("Error while reading file");
            fclose(fp);
            return NULL;
        } else if (feof(fp)) {
            perror("EOF found");
            fclose(fp);
            return NULL;
        }
    }
    file_contents[file_size] = '\0';

    if (fclose(fp)) {
        perror("Error while closing file");
        return NULL;
    }

    return file_contents;
}

char **file_to_lines(char *filename) {
    errno = 0;
    FILE *fp = fopen(filename, "r");
    if (fp == NULL) {
        perror("Failed to open file");
        return NULL;
    }

    size_t nlines = 0;
    char **lines = malloc(CHUNK * sizeof(char *));
    if (lines == NULL) goto error;

    char buffer[BUFSIZ];
    while (fgets(buffer, BUFSIZ, fp)) {
        size_t len = strlen(buffer);
        char *new = malloc((len + 1) * sizeof(char));
        if (new == NULL) goto error;
        strcpy(new, buffer);
        if (nlines >= CHUNK) {
            lines = realloc(lines, sizeof(char *) * (nlines + CHUNK));
            if (lines == NULL) goto error;
        }
        lines[nlines++] = new;
    }

    lines = realloc(lines, (nlines + 1) * sizeof(char *));
    if (lines == NULL) goto error;
    lines[nlines] = NULL;

    if (fclose(fp)) {
        perror("Error while closing file");
        return NULL;
    }

    return lines;

    error:
        perror("Failed to allocate memory");
        free_lines(lines);
        fclose(fp);
        return NULL;
}
