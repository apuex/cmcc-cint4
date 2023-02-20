package com.github.apuex.cmcc.cint4;

import java.io.Serializable;
import java.nio.ByteBuffer;

/**
 * 时间的结构
 */
public class TTime implements Serializable {

    public static void encode(ByteBuffer buf, TTime v) {
        buf.putShort(v.Years);
        buf.put(v.Month);
        buf.put(v.Day);
        buf.put(v.Hour);
        buf.put(v.Minute);
        buf.put(v.Second);
    }

    public static TTime decode(ByteBuffer buf) {
        TTime v = new TTime();
        v.Years = buf.getShort();
        v.Month = buf.get();
        v.Day = buf.get();
        v.Hour = buf.get();
        v.Minute = buf.get();
        v.Second = buf.get();
        return v;
    }

    @Override
    public boolean equals(Object o) {
        TTime r = null;
        if(o instanceof TTime) {
            r = (TTime) o;
        } else {
            return false;
        }

        boolean result =
            ( this.Years == r.Years
            && this.Month == r.Month
            && this.Day == r.Day
            && this.Hour == r.Hour
            && this.Minute == r.Minute
            && this.Second == r.Second
            );

        return result;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder
            .append("TTime { ")
            .append("Years=").append(this.Years)
            .append(", ").append("Month=").append(this.Month)
            .append(", ").append("Day=").append(this.Day)
            .append(", ").append("Hour=").append(this.Hour)
            .append(", ").append("Minute=").append(this.Minute)
            .append(", ").append("Second=").append(this.Second)
            .append(" }");

        return builder.toString();
    }

    public short Years; // 年
    public byte Month; // 月
    public byte Day; // 日
    public byte Hour; // 时
    public byte Minute; // 分
    public byte Second; // 秒
}

