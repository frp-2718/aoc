#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#include "utils/input.h"

#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define MIN(x, y) (((x) < (y)) ? (x) : (y))

#define ROW 10
/* #define ROW 2000000 */
#define XY_MAX 20
/* #define XY_MAX 4000000 */

typedef struct {
    int x, y;
} Point;

typedef struct {
    Point sensor;
    Point beacon;
} Sensor;

typedef struct {
    int lower, upper;
} Interval;

int abs(int x);
int dist(Point a, Point b);
void merge(size_t len, Interval intervals[len]);
int compare(const void *ptr1, const void *ptr2);
size_t sum_intervals(size_t len, Interval intervals[len]);
size_t coverage(size_t nsensors, Sensor sensors[nsensors], size_t row);
bool out_of_reach(Point p, size_t nsensors, Sensor sensors[]);
size_t distress(size_t nsensors, Sensor sensors[]);
size_t distress_signal(Point p);

int main(int argc, char* argv[]) {
    if (argc != 2) {
        printf("Usage: %s FILENAME\n", argv[0]);
        return EXIT_FAILURE;
    }

    char **input = file_to_lines(argv[1]);
    if (!input) {
        perror("Error reading file");
        return EXIT_FAILURE;
    }

    size_t nsensors = 0;
    while (input[nsensors++]);
    nsensors--;

    Sensor sensors[nsensors];
    for (size_t i = 0; input[i]; i++) {
        sscanf(input[i], "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d",
                &sensors[i].sensor.x, &sensors[i].sensor.y, &sensors[i].beacon.x,
                &sensors[i].beacon.y);
    }

    free_lines(input);

    // part 1
    printf("%ld\n", coverage(nsensors, sensors, ROW));

    // part 2
    printf("%zu\n", distress(nsensors, sensors));

    return EXIT_SUCCESS;
}

size_t distress(size_t nsensors, Sensor sensors[]) {
    for (size_t n = 0; n < nsensors; n++) {
        size_t d = dist(sensors[n].sensor, sensors[n].beacon);
        Point sensor = sensors[n].sensor;
        for (size_t i = 0; i <= d; i++) {
            Point p1 = (Point){sensor.x + i, sensor.y + (d - i + 1)};
            if (p1.x <= XY_MAX && p1.y <= XY_MAX && p1.x >= 0 && p1.y >= 0
                    && out_of_reach(p1, nsensors, sensors)) {
                return distress_signal(p1);
            }
            Point p2 = (Point){sensor.x + i, sensor.y - (d - i + 1)};
            if (p2.x <= XY_MAX && p2.y <= XY_MAX && p2.x >= 0 && p2.y >= 0
                    && out_of_reach(p2, nsensors, sensors)) {
                return distress_signal(p2);
            }
            Point p3 = (Point){sensor.x - i, sensor.y - (d - i + 1)};
            if (p3.x <= XY_MAX && p3.y <= XY_MAX && p3.x >= 0 && p3.y >= 0
                    && out_of_reach(p3, nsensors, sensors)) {
                return distress_signal(p3);
            }
            Point p4 = (Point){sensor.x - i, sensor.y + (d - i + 1)};
            if (p4.x <= XY_MAX && p4.y <= XY_MAX && p4.x >= 0 && p4.y >= 0
                    && out_of_reach(p4, nsensors, sensors)) {
                return distress_signal(p4);
            }
        }
    }
    return 0;
}

size_t distress_signal(Point p) {
    return (size_t)p.x * 4000000 + (size_t)p.y;
}

bool out_of_reach(Point p, size_t nsensors, Sensor sensors[]) {
    for (size_t n = 0; n < nsensors; n++) {
        size_t dist_sensor = dist(sensors[n].sensor, sensors[n].beacon);
        size_t dist_point = dist(sensors[n].sensor, p);
        if (dist_point <= dist_sensor) {
            return false;
        }
    }
    return true;
}

int abs(int x) {
    return x < 0 ? -x : x;
}

int dist(Point a, Point b) {
    return abs(b.x - a.x) + abs(b.y - a.y);
}

void merge(size_t len, Interval intervals[len]) {
    for (size_t i = 0; i < len-1; i++) {
        for (size_t j = i+1; j < len; j++) {
            if ((intervals[j].lower == 0 && intervals[j].upper == 0) ||
                 intervals[j].lower > intervals[i].upper ||
                 intervals[j].upper < intervals[i].lower) {
                continue;
            } else {
                intervals[i].lower = MIN(intervals[i].lower, intervals[j].lower);
                intervals[i].upper = MAX(intervals[i].upper, intervals[j].upper);
                intervals[j].lower = 0;
                intervals[j].upper = 0;
            }
        }
    }
}

int compare(const void *ptr1, const void *ptr2) {
    Interval i1 = * (const Interval *) ptr1;
    Interval i2 = * (const Interval *) ptr2;

    if (i1.lower == i2.lower && i1.upper == i2.upper) {
        return 0;
    } else if (i1.lower < i2.lower || (i1.lower == i2.lower && i1.upper < i2.upper)) {
        return -1;
    } else {
        return 1;
    }
}

size_t sum_intervals(size_t len, Interval intervals[len]) {
    size_t total = 0;
    for (size_t i = 0; i < len; i++) {
        total += intervals[i].upper - intervals[i].lower;
    }
    return total;
}

size_t coverage(size_t nsensors, Sensor sensors[nsensors], size_t row) {
    Interval intervals[nsensors];

    for (size_t i = 0; i < nsensors; i++) {
        Point line = {sensors[i].sensor.x, row};
        int db = dist(sensors[i].sensor, sensors[i].beacon);
        int dl = dist(sensors[i].sensor, line);

        if (db >= dl) {
            intervals[i].lower = sensors[i].sensor.x - (db - dl);
            intervals[i].upper = sensors[i].sensor.x + (db - dl);
        } else {
            intervals[i].lower = 0;
            intervals[i].upper = 0;
        }
    }

    qsort(intervals, nsensors, sizeof(Interval), compare);
    merge(nsensors, intervals);
    return sum_intervals(nsensors, intervals);
}
