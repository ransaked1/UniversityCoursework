#include <stdio.h>
#include "pico/stdlib.h"
#include <hardware/pwm.h>

#define BUZZER 18

#define PAUSE 100 //defines delay between dots/dashes
#define PAUSE_CHAR 300 //defines delay between each letter of a word (3x PAUSE)
#define PAUSE_WORD 700 //defines delay between each word (7x PAUSE)

#define HIGH 10000
#define LOW 0

char str[] = "Text to translate";
char* ptr = str;

const char* morse_chars[] = {
        ".-",    // A
        "-...",  // B
        "-.-.",  // C
        "-..",   // D
        ".",     // E
        "..-.",  // F
        "--.",   // G
        "....",  // H
        "..",    // I
        ".---",  // J
        "-.-",   // K
        ".-..",  // L
        "--",    // M
        "-.",    // N
        "---",   // O
        ".--.",  // P
        "--.-",  // Q
        ".-.",   // R
        "...",   // S
        "-",     // T
        "..-",   // U
        "...-",  // V
        ".--",   // W
        "-..-",  // X
        "-.--",  // Y
        "--.."   // Z
};

void dot() {
    for (int i = 0; i < 50; i++)
    {
        pwm_set_gpio_level(BUZZER, HIGH);
        sleep_ms(2);
        pwm_set_gpio_level(BUZZER, LOW);
        sleep_ms(1);
    }
    sleep_ms(PAUSE);
}

void dash() {
    for (int i = 0; i < 100; i++)
    {
        pwm_set_gpio_level(BUZZER, HIGH);
        sleep_ms(2);
        pwm_set_gpio_level(BUZZER, LOW);
        sleep_ms(1);
    }
    sleep_ms(PAUSE);
}

void play_morse_char(const char* letter) {

    for (; *letter; ++letter)
    {
        if (*letter == '.')
            dot();
        if (*letter == '-')
            dash();
    }
    sleep_ms(PAUSE_CHAR);
}


int main() {
    gpio_set_function(BUZZER, GPIO_FUNC_PWM);

    uint sliceNum = pwm_gpio_to_slice_num(BUZZER);
    pwm_config config = pwm_get_default_config();
    pwm_init(sliceNum, &config, true);

    for (; *ptr; ++ptr)
    {
        if (*ptr >= 'A' && *ptr <= 'Z')
            play_morse_char(morse_chars[*ptr - 'A']);
        if (*ptr >= 'a' && *ptr <= 'z')
            play_morse_char(morse_chars[*ptr - 'a']);
        if (*ptr == ' ')
            sleep_ms(PAUSE_WORD);
    }
}