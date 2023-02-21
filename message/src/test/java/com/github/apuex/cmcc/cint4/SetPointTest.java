/*
 * Copyright (c) 2021-2023 WINCOM.
 * Copyright (c) 2021-2023 Wangxy <xtwxy@hotmail.com>
 *
 * All rights reserved. 
 *
 * This program and the accompanying materials
 * are made available under the terms of the Mozilla Public License 2.0.
 *
 * Contributors:
 *   Wangxy - initial implementation and documentation.
*/

package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Map;
import java.util.TreeMap;

import org.junit.Assert;
import org.junit.Test;

public class SetPointTest {
    @Test
    public void testEncode() {
        byte[] expected = new byte[] 
            { (byte)0x5A, (byte)0x6B, (byte)0x7C, (byte)0x7E, (byte)0x5F, (byte)0x00, (byte)0x00, (byte)0x00
            , (byte)0x02, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0xE9, (byte)0x03, (byte)0x00, (byte)0x00
            , (byte)0x31, (byte)0x32, (byte)0x33, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x34, (byte)0x35, (byte)0x36, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x37, (byte)0x38
            , (byte)0x39, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x31, (byte)0x20, (byte)0x20, (byte)0x40, (byte)0xE2, (byte)0x01
            , (byte)0x00, (byte)0x01, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0xC7, (byte)0x86
            };
        TD td = new TD();
        td.SiteID = "123";
        td.DeviceID = "456";
        td.SignalID = "789";
        td.SignalNumber = "1";
        td.Value = 123456;
        td.Status = EnumState.CRITICAL;
        SetPoint v = new SetPoint(2, td);
      	byte[] actual = new byte[95];
      	ByteBuffer buf = ByteBuffer.wrap(actual);
      	buf.order(ByteOrder.LITTLE_ENDIAN);

      	Map<TID, EnumType> typeMap = new TreeMap<TID, EnumType>(new TIDComparator());
      	typeMap.put(Util.tidFrom(td), EnumType.DI);
      	SetPointCodec codec = new SetPointCodec(new TATDCodec(typeMap));
      	codec.encode(buf, v);
      	v.Length = buf.position();

        System.out.printf("actual[%d] = [ ", v.Length);
      	for(int i = 0; i != v.Length; ++i) {
          System.out.printf("(byte)0x%02X, ", 0xff & actual[i]);
      	}
        System.out.printf("]\n");

      	Assert.assertArrayEquals(expected, actual);
    }

    @Test
    public void testDecode() {
        byte[] input = new byte[] 
            { (byte)0x5A, (byte)0x6B, (byte)0x7C, (byte)0x7E, (byte)0x5F, (byte)0x00, (byte)0x00, (byte)0x00
            , (byte)0x02, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0xE9, (byte)0x03, (byte)0x00, (byte)0x00
            , (byte)0x31, (byte)0x32, (byte)0x33, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x34, (byte)0x35, (byte)0x36, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x37, (byte)0x38
            , (byte)0x39, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x31, (byte)0x20, (byte)0x20, (byte)0x40, (byte)0xE2, (byte)0x01
            , (byte)0x00, (byte)0x01, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0xC7, (byte)0x86
            };
        TD td = new TD();
        td.SiteID = "123";
        td.DeviceID = "456";
        td.SignalID = "789";
        td.SignalNumber = "1";
        td.Value = 123456;
        td.Status = EnumState.CRITICAL;
        SetPoint expected = new SetPoint(2, td);
      	expected.Length = input.length;
      	expected.CRC16 = (short)0x86C7;
      	ByteBuffer buf = ByteBuffer.wrap(input);
      	buf.order(ByteOrder.LITTLE_ENDIAN);

      	Map<TID, EnumType> typeMap = new TreeMap<TID, EnumType>(new TIDComparator());
      	typeMap.put(Util.tidFrom(td), EnumType.DI);
      	SetPointCodec codec = new SetPointCodec(new TATDCodec(typeMap));
        SetPoint actual = codec.decode(buf);

      	Assert.assertEquals(expected, actual);
    }
}

