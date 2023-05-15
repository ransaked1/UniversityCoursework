#include <stdio.h>
#include <stdlib.h>

#include "pico/stdlib.h"
#include "hardware/pio.h"
#include "hardware/clocks.h"
#include "ws2812.pio.h"

#define GPIO 28

static inline void setPixel(uint32_t pixel_grb) {
    pio_sm_put_blocking(pio0, 0, pixel_grb * 256);
}

void fade() {
    int base = 32 * 256 * 256 * 256;

    for (int i = 0; i < 256; i++) {
        setPixel(base + (256 * 256 * i) + (255 - i));
        sleep_ms(10);
    }
    for (int i = 0; i < 256; i++) {
        setPixel(base + (256 * 256 * (255 - i)) + (256 * i));
        sleep_ms(10);
    }
    for (int i = 0; i < 256; i++) {
        setPixel(base + (256 * 255 ) + i);
        sleep_ms(10);
    }
}

int main() {
    stdio_init_all();
    uint offset = pio_add_program(pio0, &ws2812_program);
    ws2812_program_init(pio0, 0, offset, GPIO, 800000, true);
    while (1) {
        fade();
    }
}
