package com.dsp.ass1;

import java.util.logging.Formatter;
import java.util.logging.LogRecord;


public class ShortFormatter extends Formatter {
    public ShortFormatter() {
        super();
    }

    @Override
    public String format(final LogRecord record) {
        StringBuilder builder = new StringBuilder(1000);
        // builder.append(df.format(new Date(record.getMillis()))).append(" - ");
        builder.append("[").append(record.getSourceClassName()).append(".");
        builder.append(record.getSourceMethodName()).append("] - ");
        builder.append("[").append(record.getLevel()).append("] - ");
        builder.append(formatMessage(record));
        builder.append("\n");
        return builder.toString();
    }
}
