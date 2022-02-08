package uk.ac.soton.comp1206.demo;

import java.time.*;
import java.time.format.*;

import org.springframework.web.bind.annotation.*;

@RestController
public class TestController {

    @GetMapping("/")
    public String index() {
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("HH:mm:ss");
        LocalTime localTime = LocalTime.now();
        return "The time at the tone will be " + formatter.format(localTime);
    }

    @RequestMapping(value="/hello/{name}", method = RequestMethod.GET)
    public @ResponseBody String getName(@PathVariable("name") String username) {
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("HH:mm:ss");
        LocalTime localTime = LocalTime.now();
        return "The time at the tone will be " + formatter.format(localTime ) + " " + username;
    }

}
