package com.github.apuex.cmcc.cint4;

import java.io.Serializable;
import java.nio.ByteBuffer;

/**
 * 值的数组
 */
public class TATDArray implements Serializable {
    public static void encode(ByteBuffer buf, TATDArray v) {
    }

    public static TATDArray decode(ByteBuffer buf) {
        TATDArray v = new TATDArray();
	return v;
    }

}

