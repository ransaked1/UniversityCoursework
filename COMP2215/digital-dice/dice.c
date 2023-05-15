#include "pico/stdlib.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>

//libaries for red flashing light
#include "pico/stdlib.h"
#include "hardware/pio.h"
#include "hardware/clocks.h"
#include "ws2812.pio.h"
#include <hardware/pwm.h>

#define START_BUTTON 22
#define RGB 28

int random_dice() {
    // Seed the random number generator with the current time
    srand(time(NULL));

    return (rand() % (5)) + 1;
}

void roll(int number) {
    int i=1;
    while (i <= number) {
        pio_sm_put_blocking(pio0, 0, (256 * 256 * 255));
        sleep_ms(1);
        pio_sm_put_blocking(pio0, 0, 0);
        sleep_ms(500);
        i++;
    }
}

int main() {

    gpio_init(START_BUTTON);
    gpio_set_dir(START_BUTTON, GPIO_IN);

    //setting up RGB
    stdio_init_all();
    uint offset = pio_add_program(pio0, &ws2812_program);
    ws2812_program_init(pio0, 0, offset, RGB, 800000, true);

    while (true)
    {
        if (!gpio_get(START_BUTTON))
        {
            sleep_ms(1000);
            roll(random_dice());
            sleep_ms(1000);
        }
    }
}
