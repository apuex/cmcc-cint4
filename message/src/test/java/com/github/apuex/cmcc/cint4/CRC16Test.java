package com.github.apuex.cmcc.cint4;

import org.junit.Assert;
import org.junit.Test;

public class CRC16Test {
    @Test
    public void testCalcCRC16() {
        final byte[] input = new byte[] { 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39 };
	final short crc16 = Util.CRC16(input, 0, input.length);
	System.out.printf("CRC16 = 0x%04X\n", crc16);
	Assert.assertEquals(0x29B1, crc16);
    }
}

