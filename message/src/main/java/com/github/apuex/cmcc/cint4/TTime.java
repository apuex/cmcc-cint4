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

    public short Years; // 年
    public byte Month; // 月
    public byte Day; // 日
    public byte Hour; // 时
    public byte Minute; // 分
    public byte Second; // 秒
}

