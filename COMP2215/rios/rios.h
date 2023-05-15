#ifndef RIOS_H
# define RIOS_H

#include "pico/stdlib.h"

bool ISR(struct repeating_timer *t);
void start_rios(int32_t tickRate);
void add_task(int (*TickFct)(int), uint32_t period);

#endif