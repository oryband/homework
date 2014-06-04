package com.dsp.ass2;

import java.util.logging.Logger;
import java.util.logging.ConsoleHandler;


public class Utils {
    public static final String delim = "\t";


    // Use custom string format for logger.
    public static Logger setLogger(Logger logger) {
        ShortFormatter formatter = new ShortFormatter();
        ConsoleHandler handler = new ConsoleHandler();

        logger.setUseParentHandlers(false);
        handler.setFormatter(formatter);
        logger.addHandler(handler);

        return logger;
    }
}
