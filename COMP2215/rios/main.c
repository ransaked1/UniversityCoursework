#include "pico/stdlib.h"
#include <stdlib.h>
#include <stdio.h>
#include "rios.h"

#define PIN1 27
#define PIN2 8

int64_t disable_pin(alarm_id_t id, void* user_data) {
    uint* pin = (uint*)user_data;
    gpio_put(*pin, 0);
    return 0;
}

void flash_pin(int gpio) {
    gpio_put(gpio, 1);
    add_alarm_in_ms(250, disable_pin, &gpio, true);
}

int TickFct_1(int state) {

    flash_pin(PIN1);
    return ++state;
}

int TickFct_2(int state) {
    flash_pin(PIN2);
    return ++state;
}


int main() {
    stdio_init_all();

    gpio_init(PIN1);
    gpio_set_dir(PIN1, GPIO_OUT);

    gpio_init(PIN2);
    gpio_set_dir(PIN2, GPIO_OUT);

    add_task(&TickFct_1, 500);
    add_task(&TickFct_2, 1000);
    add_task(&TickFct_1, 2000);
    add_task(&TickFct_2, 4000);
    add_task(&TickFct_1, 8000);
    add_task(&TickFct_2, 16000);

    start_rios(500);
}
