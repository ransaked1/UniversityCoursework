#include <stdio.h>
#include "pico/stdlib.h"
#include "hardware/pwm.h"

// Define the frequency of each note
#define C4 262
#define C4_SHARP 277
#define D4 294
#define D4_SHARP 311
#define E4 330
#define F4 349
#define F4_SHARP 370
#define G4 392
#define G4_SHARP 415
#define A4 440
#define A4_SHARP 466
#define B4 494

#define C5 523
#define C5_SHARP 554
#define D5 587
#define D5_SHARP 622
#define E5 659
#define F5 698
#define F5_SHARP 740
#define G5 784
#define G5_SHARP 831
#define A5 880
#define A5_SHARP 932
#define B5 988

#define C6 1047
#define C6_SHARP 1109
#define D6 1175
#define D6_SHARP 1245
#define E6 1319
#define F6 1397
#define F6_SHARP 1480
#define G6 1568
#define G6_SHARP 1661
#define A6 1760
#define A6_SHARP 1865
#define B6 1976


// Define the duration of each note in milliseconds
#define W 2000
#define H 1000
#define Q 500
#define E 250

// Define the buzzer pin
#define BUZZER_PIN 18

// Define the song as an array of notes and durations
int song[] = {
  E5, Q, E5, Q, E5, Q, C5, Q, E5, Q, G5, H,
  G4, H, C5, Q, G4, H, E4, H, A4, Q, B4, Q,
  A4_SHARP, Q, A4, Q, G4_SHARP, E, G4, Q, A4, H, F5, Q,
  G5, Q, E5, Q, C5, Q, D5, E, B4, E, C5, Q, G4, H,
  E4, H, A4, Q, B4, Q, A4_SHARP, Q, A4, Q, G4_SHARP, E, G4, Q,
  A4, H, F5, Q, G5, Q, E5, Q, C5, Q, D5, E, B4, E, C5, Q, G4, H,
  0, W, 0, W, 0,
  B4, Q, C5, Q, D5, E, D5, E, C5, Q, B4, H,
  B4, Q, C5, Q, D5, E, D5, E, C5, Q, B4, H,
  0, W
};

int main() {
    // Initialize the standard I/O library
    stdio_init_all();

    // Setup the BUZZER
    gpio_set_function(BUZZER_PIN, GPIO_FUNC_PWM);

    // Initialize the PWM channel for the buzzer
    uint slice_num = pwm_gpio_to_slice_num(BUZZER_PIN);
    pwm_config config = pwm_get_default_config();
    pwm_init(slice_num, &config, true);
    pwm_set_wrap(slice_num, 10535);
    pwm_set_clkdiv(slice_num, 120.0f);

    // Play the song
    for (int i = 0; i < sizeof(song) / sizeof(song[0]); i += 2) {
        int note = song[i];
        int duration = song[i + 1];

        // Play the note
        int period = 1000000 / note;
        pwm_set_chan_level(slice_num, PWM_CHAN_A, period / 2);
        sleep_ms(duration);

        // Stop the note
        pwm_set_chan_level(slice_num, PWM_CHAN_A, 0);
        sleep_ms(10);
    }

    return 0;
}