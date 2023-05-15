cmake_minimum_required(VERSION 3.26)

include(pico_sdk_import.cmake)

project(test_project C CXX ASM)
set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 17)
pico_sdk_init()

add_executable(wack-a-mole
        wack-a-mole.c
        )

# create map/bin/hex file etc.
pico_add_extra_outputs(wack-a-mole)

# pull in common dependencies
target_link_libraries(wack-a-mole pico_stdlib hardware_pio hardware_pwm)

