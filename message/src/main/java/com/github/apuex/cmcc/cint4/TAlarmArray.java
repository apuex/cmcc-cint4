package com.github.apuex.cmcc.cint4;

import java.io.Serializable;
import java.nio.ByteBuffer;

/**
 * 告警的数组
 */
public class TAlarmArray implements Serializable {
    public static void encode(ByteBuffer buf, TAlarmArray v) {
    }

    public static TAlarmArray decode(ByteBuffer buf) {
        TAlarmArray v = new TAlarmArray();
	return v;
    }

}

