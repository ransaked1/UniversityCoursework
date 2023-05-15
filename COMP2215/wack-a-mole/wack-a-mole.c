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

#define HIGH 20000
#define LOW 0

#define BUZZER 18
#define BUTTON1 20
#define BUTTON2 21
#define BUTTON3 22
#define PIN1 6
#define PIN2 7
#define PIN3 8
#define RGB 28

int random_pin() {

    int random_number = rand() % 3;

    // Map the random number to 6, 7, or 8
    switch (random_number) {
    case 0:
        return 6;
    case 1:
        return 7;
    case 2:
        return 8;
    }
    return 0;
}

int random_time() {
    return rand() % 1000 + 200;
}

void buzz() 
{
    for (int i = 0; i < 50; i++)
    {
        pwm_set_gpio_level(BUZZER, HIGH);
        sleep_ms(2);
        pwm_set_gpio_level(BUZZER, LOW);
        sleep_ms(1);
    }
}

void fail() 
{
   pio_sm_put_blocking(pio0, 0, (256 * 256 * 255));
   buzz();
   pio_sm_put_blocking(pio0, 0, 0);
   sleep_ms(300);
}

void success()
{
    pio_sm_put_blocking(pio0, 0, (256 * 256 * 255) * 256);
    sleep_ms(100);
    pio_sm_put_blocking(pio0, 0, 0);
    sleep_ms(300);
}

void blink()
{
    pio_sm_put_blocking(pio0, 0, (32 * 256 * 256 * 256)  + 256 * 255 + 255);
    sleep_ms(100);
    pio_sm_put_blocking(pio0, 0, 0);
    sleep_ms(500);
}

int get_button(int pin)
{
    if (pin == PIN1)
        return BUTTON1;
    if (pin == PIN2)
        return BUTTON2;
    if (pin == PIN3)
        return BUTTON3;
    return 0;
}

void light_on(int pin)
{
    gpio_put(pin, 1);
}

void light_off(int pin)
{
    gpio_put(pin, 0);
}

int start_game() {
    int score = 0;

    while (true)
    {
        int pin = random_pin();
        int button = get_button(pin);
        light_on(pin);

        int time = random_time();
        for (int i = 1; i <= time; i++)
        {   
            bool b1_state = gpio_get(BUTTON1);
            bool b2_state = gpio_get(BUTTON2);
            bool b3_state = gpio_get(BUTTON3);
            if (b1_state == false)
                if (pin == PIN1) {
                    light_off(pin);
                    success();
                    score++;
                    break;
                }
                else {
                    fail();
                    return score;
                }
            if (b2_state == false)
                if (pin == PIN2) {
                    light_off(pin);
                    success();
                    score++;
                    break;
                }
                else {
                    fail();
                    return score;
                }
            if (b3_state == false)
                if (pin == PIN3) {
                    light_off(pin);
                    success();
                    score++;
                    break;
                }
                else {
                    fail();
                    return score;
                }
            if (i == time) {
                fail();
                return score;
            }
            sleep_ms(1);
        }
    }
    return score;
}

void show_score(int score)
{
    int i = 1;
    while (i <= score)
    {
        blink();
        i++;
    }
}

int main() 
{
    stdio_init_all();

    //set up button for seconds
    gpio_init(BUTTON1);
    gpio_set_dir(BUTTON1, GPIO_IN);

    //set button for minutes
    gpio_init(BUTTON2);
    gpio_set_dir(BUTTON2, GPIO_IN);

    //set button stating countdown
    gpio_init(BUTTON3);
    gpio_set_dir(BUTTON3, GPIO_IN);

    gpio_init(PIN1);
    gpio_set_dir(PIN1, GPIO_OUT);

    gpio_init(PIN2);
    gpio_set_dir(PIN2, GPIO_OUT);

    gpio_init(PIN3);
    gpio_set_dir(PIN3, GPIO_OUT);

    //set up buzzer
    gpio_init(BUZZER);
    gpio_set_dir(BUZZER, GPIO_OUT);

    //setting up RGB
    uint offset = pio_add_program(pio0, &ws2812_program);
    ws2812_program_init(pio0, 0, offset, RGB, 800000, true);

    //setting up Buzzer
    gpio_set_function(BUZZER, GPIO_FUNC_PWM);
    uint sliceNum = pwm_gpio_to_slice_num(BUZZER);
    pwm_config config = pwm_get_default_config();
    pwm_init(sliceNum, &config, true);

    sleep_ms(2000);

    int final_score = start_game();

    sleep_ms(1000);

    show_score(final_score);
}
