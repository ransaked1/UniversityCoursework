#include "pico/stdlib.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
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
    //Select a random pin
}

int random_time() {
    //Select a random duration (something between 200-1000 ms works well for human reaction)
}

void buzz() 
{
    //Make a buzz for like 50ms
}

void fail() 
{
   //Flash the LED red on the maker board
}

void success()
{
    //Flash the LED green on the maker board
}

void blink()
{
    //Flash the LED blue on the maker board
}

int get_button(int pin)
{
    //Map each pin number to a button number
}

void light_on(int pin)
{
    //Light the pin given
}

void light_off(int pin)
{
    //Turn off the pin given
}

int start_game() {
    int score = 0;

    while (true)
    {
        //Get a random pin, map the button and turn the gpio for that pin on
        int pin = random_pin();
        int button = get_button(pin);
        light_on(pin);

        //Get a random time for the light to stay on
        int time = random_time();

        // Here starts the game loop
        for (int i = 1; i <= time; i++)
        {   
            // Check the state of the buttons and if the pin is correct turn the pin off, run the success function and increment the score
            // Remember to break or return in each case. The loop has to restart or the game has to end in every case
            // It is a fail if the time runs out
            sleep_ms(1);
        }
    }
    return score;
}

void show_score(int score)
{
    //Show the score by blinking the blue light
}

int main() 
{
    stdio_init_all();

    //set up button 

    //set up pins

    //set up buzzer

    //set up RGB LED

    //set up Buzzer using PWM or gpio

    //wait a couple seconds before starting the game
    sleep_ms(2000);

    //start the game and record the score when it ends
    int final_score = start_game();

    //wait a sec...
    sleep_ms(1000);

    //show the score with the blinks
    show_score(final_score);
}
