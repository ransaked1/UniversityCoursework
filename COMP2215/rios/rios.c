#include "rios.h"
#include <stdio.h>
#include "pico/stdlib.h"
#include "hardware/sync.h"
/* Limited by int8_t: */
#define MAX_TASKS 20

/*
   Copyright (c) 2013 Frank Vahid, Tony Givargis, and
   Bailey Miller. Univ. of California, Riverside and Irvine.
   RIOS version 1.2
*/
typedef struct task {
    uint8_t running;      /* 1 indicates task is running */
    int state;            /* Current state of state machine */
    uint32_t period;      /* Rate at which the task should tick */
    uint32_t elapsedTime; /* Time since task's previous tick */
    int (*TickFct)(int);  /* Function to call for task's tick */
} task;


task tasks[MAX_TASKS];
int8_t tasksNum = -1;

int32_t tasksPeriodGCD = 0;  /* Timer tick rate */

uint8_t runningTasks[MAX_TASKS + 1] = { 255 }; /* Track running tasks, [0] always idleTask */
const uint32_t idleTask = 255;             /* 0 highest priority, 255 lowest */
uint8_t currentTask = 0;                   /* Index of highest priority task in runningTasks */

unsigned schedule_time = 0;
bool ISR(struct repeating_timer* t) {
    uint8_t i;

    for (i = 0; i <= tasksNum; ++i) { /* Heart of scheduler code */
        if ((tasks[i].elapsedTime >= tasks[i].period) /* Task ready */
            && (runningTasks[currentTask] > i) /* Task priority > current task priority */
            && (!tasks[i].running)             /* Task not already running (no self-preemption) */
            ) {

            uint32_t flags = save_and_disable_interrupts();
            tasks[i].elapsedTime = 0;      /* Reset time since last tick */
            tasks[i].running = 1;          /* Mark as running */
            currentTask += 1;
            runningTasks[currentTask] = i; /* Add to runningTasks */
            restore_interrupts(flags);

            tasks[i].state = tasks[i].TickFct(tasks[i].state); /* Execute tick */

            flags = save_and_disable_interrupts();
            tasks[i].running = 0;                 /* Mark as not running */
            runningTasks[currentTask] = idleTask; /* Remove from runningTasks */
            currentTask -= 1;
            restore_interrupts(flags);
        }
        tasks[i].elapsedTime += tasksPeriodGCD;
    }

    return true;
}

void add_task(int (*TickFct)(int), uint32_t period) {
    tasks[++tasksNum].state = -1;
    tasks[tasksNum].period = period;
    tasks[tasksNum].elapsedTime = tasks[tasksNum].period;
    tasks[tasksNum].running = 0;
    tasks[tasksNum].TickFct = TickFct;
}

void start_rios(int32_t tickRate) {
    tasksPeriodGCD = tickRate;

    struct repeating_timer timer;
    add_repeating_timer_ms(tickRate, ISR, NULL, &timer);

    while (1) {};
}