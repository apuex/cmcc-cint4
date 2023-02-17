package com.github.apuex.cmcc.cint4;

import java.io.Serializable;
import java.nio.ByteBuffer;

/**
 * AI、DI值的结构的父类
 */
public class TATD implements Serializable {
    public static void encode(ByteBuffer buf, TATD v) {
    }

    public static TATD decode(ByteBuffer buf) {
        TATD v = new TATD();
	return v;
    }

}

