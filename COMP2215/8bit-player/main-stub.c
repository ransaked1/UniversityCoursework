#include <stdio.h>
#include "pico/stdlib.h"
#include "hardware/pwm.h"

// Frequency definitions for the notes
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


// Duration definitions
#define W 2000
#define H 1000
#define Q 500
#define E 250

// Buzzer pin on the maker pi pico
#define BUZZER_PIN 18

// Define the song as an array of notes and durations, I left Super Mario and a Rick Roll :)
// I put the note followed by the duration but you can separate them in 2 different arrays.
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

    // Initialize the PWM channel for the buzzer
    // Try different setups here to get a better sound

    // A for loop that will go through the notes
    for (/* something here*/) {
        // Play the note
        // Hint: Set the level for the pwm and sleep for the given duration

        // Stop the note
        //Hint: Set the level to 0 and sleep for a few milliseconds
    }

    return 0;
}