#include "pico/stdlib.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

//libaries for red flashing light
#include "pico/stdlib.h"
#include "hardware/pio.h"
#include "hardware/clocks.h"
#include "ws2812.pio.h"
#include <hardware/pwm.h>

#define HIGH 20000
#define LOW 0

#define BUZZER 18
#define SEC_BUTTON 20
#define MIN_BUTTON 21
#define START_BUTTON 22
#define RGB 28

void buzz() {
    for (int i = 0; i < 50; i++)
    {
        pwm_set_gpio_level(BUZZER, HIGH);
        sleep_ms(2);
        pwm_set_gpio_level(BUZZER, LOW);
        sleep_ms(1);
    }
}

void alarm() {
    while (true) {
        pio_sm_put_blocking(pio0, 0, (256 * 256 * 255));
        buzz();
        pio_sm_put_blocking(pio0, 0, 0);
        sleep_ms(300);
        if (!gpio_get(START_BUTTON) == true) {
            return;
        }
    }
}

int main() {
    //set up button for seconds
    gpio_init(SEC_BUTTON);
    gpio_set_dir(SEC_BUTTON, GPIO_IN);

    //set button for minutes
    gpio_init(MIN_BUTTON);
    gpio_set_dir(MIN_BUTTON, GPIO_IN);


    //set button stating countdown
    gpio_init(START_BUTTON);
    gpio_set_dir(START_BUTTON, GPIO_IN);

    //set up buzzer
    gpio_init(BUZZER);
    gpio_set_dir(BUZZER, GPIO_OUT);

    //setting up RGB
    stdio_init_all();
    uint offset = pio_add_program(pio0, &ws2812_program);
    ws2812_program_init(pio0, 0, offset, RGB, 800000, true);

    //setting up Buzzer
    gpio_set_function(BUZZER, GPIO_FUNC_PWM);
    uint sliceNum = pwm_gpio_to_slice_num(BUZZER);
    pwm_config config = pwm_get_default_config();
    pwm_init(sliceNum, &config, true);

    int delay = 0;
    bool holdSec = false;
    bool holdMin = false;

    while (true)
    {
        if (!gpio_get(START_BUTTON))
        {
            sleep_ms(delay * 1000);
            alarm();
            delay = 0;
            sleep_ms(1000);
        }

        if (!gpio_get(SEC_BUTTON))
        {
            if (holdSec == false) {
                delay += 1;
                holdSec = true;
            }
        }
        else { holdSec = false; }

        if (!gpio_get(MIN_BUTTON))
        {
            if (holdMin == false) {
                delay += 60;
                holdMin = true;
            }
        }
        else { holdMin = false; }
    }
}
