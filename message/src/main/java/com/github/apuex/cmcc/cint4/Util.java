package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;

/**
 * AI、DI值的结构的父类
 */
public class Util {
    static public final Charset charset = Charset.forName("GB18030");
    static public final byte padding = 0x20;

    static public void encodeString(ByteBuffer buf, String s, int maxLength) {
        byte[] ba = s.getBytes(charset);
	if(ba.length <= maxLength) {
            buf.put(ba);
	    for(int i = 0; i != (maxLength - ba.length); ++i) {
	        buf.put(padding);
	    }
	} else {
            throw new IllegalArgumentException(String.format("String '%s' is too long.", s));
	}
    }

    static public String decodeString(ByteBuffer buf, int maxLength) {
        byte[] ba = new byte[maxLength];
	buf.get(ba);
	String s = new String(ba, charset);
	return s.trim();
    }
}

