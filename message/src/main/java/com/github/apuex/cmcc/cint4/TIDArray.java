package com.github.apuex.cmcc.cint4;

import java.io.Serializable;
import java.nio.ByteBuffer;

/**
 * TID数组
 */
public class TIDArray implements Serializable {
    public static void encode(ByteBuffer buf, TIDArray v) {
    }

    public static TIDArray decode(ByteBuffer buf) {
        TIDArray v = new TIDArray();
	return v;
    }

}

