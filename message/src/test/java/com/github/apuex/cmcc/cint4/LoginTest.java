package com.github.apuex.cmcc.cint4;

import org.junit.Assert;
import org.junit.Test;

import java.io.FileInputStream;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

public class LoginTest {
    @Test
    public void testEncode() {
        byte[] expected = new byte[] 
            { (byte)0x5A, (byte)0x6B, (byte)0x7C, (byte)0x7E, (byte)0x3A, (byte)0x00, (byte)0x00, (byte)0x00
            , (byte)0x01, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x65, (byte)0x00, (byte)0x00, (byte)0x00
            , (byte)0x75, (byte)0x73, (byte)0x65, (byte)0x72, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
	    , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
	    , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x70, (byte)0x61, (byte)0x73, (byte)0x73
	    , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
	    , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
	    , (byte)0xB3, (byte)0x46
            };
        Login v = new Login(1, "user", "pass");
	v.CRC16 = (short)0xCAFE;
	byte[] actual = new byte[58];
	ByteBuffer buf = ByteBuffer.wrap(actual);
	ByteBuffer buffer = ByteBuffer.wrap(actual);
	buf.order(ByteOrder.LITTLE_ENDIAN);

        System.out.printf("BEFORE:\n");
        System.out.printf("arrayOffset: %d\n", buf.arrayOffset());
        System.out.printf("capacity: %d\n", buf.capacity());
        System.out.printf("remaining: %d\n", buf.remaining());
        System.out.printf("limit: %d\n", buf.limit());
        System.out.printf("position: %d\n", buf.position());
	Login.encode(buf, v);
	v.Length = buf.position();
        System.out.printf("AFTER:\n");
        System.out.printf("arrayOffset: %d\n", buf.arrayOffset());
        System.out.printf("capacity: %d\n", buf.capacity());
        System.out.printf("remaining: %d\n", buf.remaining());
        System.out.printf("limit: %d\n", buf.limit());
        System.out.printf("position: %d\n", buf.position());

        System.out.printf("actual = [ ");
	for(int i = 0; i != v.Length; ++i) {
            System.out.printf("(byte)0x%02X, ", 0xff & actual[i]);
	}
        System.out.printf("]\n");

	Assert.assertArrayEquals(expected, actual);
    }
}

