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
import java.util.LinkedList;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;

public class SetAlarmModeCodecTest {
    @Test
    public void testEncode() {
        byte[] expected = new byte[] 
            { (byte)0x5A, (byte)0x6B, (byte)0x7C, (byte)0x7E, (byte)0x67, (byte)0x00, (byte)0x00, (byte)0x00
            , (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0xF5, (byte)0x01, (byte)0x00, (byte)0x00
            , (byte)0x02, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x04, (byte)0x00, (byte)0x00, (byte)0x00
            , (byte)0x01, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x05, (byte)0x00, (byte)0x00, (byte)0x00
            , (byte)0x33, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x34, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x35, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x36, (byte)0x20, (byte)0x20, (byte)0xA5, (byte)0x49
            };
        List<TID> l = new LinkedList<TID>();
        TID tid = new TID();
        tid.Type = EnumType.DEVICE;
        tid.SiteID = "3";
        tid.DeviceID = "4";
        tid.SignalID = "5";
        tid.SignalNumber = "6";
        l.add(tid);
        SetAlarmMode v = new SetAlarmMode(2, EnumAlarmMode.HINT, new TIDArray(l));
      	byte[] actual = new byte[103];
      	ByteBuffer buf = ByteBuffer.wrap(actual);
      	buf.order(ByteOrder.LITTLE_ENDIAN);

      	SetAlarmModeCodec codec = new SetAlarmModeCodec();
      	codec.encode(buf, v);

        Util.printBytes("actual", actual);

      	Assert.assertArrayEquals(expected, actual);
    }

    @Test
    public void testDecode() {
        byte[] input = new byte[] 
            { (byte)0x5A, (byte)0x6B, (byte)0x7C, (byte)0x7E, (byte)0x67, (byte)0x00, (byte)0x00, (byte)0x00
            , (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0xF5, (byte)0x01, (byte)0x00, (byte)0x00
            , (byte)0x02, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x04, (byte)0x00, (byte)0x00, (byte)0x00
            , (byte)0x01, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x05, (byte)0x00, (byte)0x00, (byte)0x00
            , (byte)0x33, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x34, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x35, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x36, (byte)0x20, (byte)0x20, (byte)0xA5, (byte)0x49
            };
        List<TID> l = new LinkedList<TID>();
        TID tid = new TID();
        tid.Type = EnumType.DEVICE;
        tid.SiteID = "3";
        tid.DeviceID = "4";
        tid.SignalID = "5";
        tid.SignalNumber = "6";
        l.add(tid);
        SetAlarmMode expected = new SetAlarmMode(2, EnumAlarmMode.HINT, new TIDArray(l));
      	expected.Length = input.length;
      	expected.CRC16 = (short)0x49A5;
      	ByteBuffer buf = ByteBuffer.wrap(input);
      	buf.order(ByteOrder.LITTLE_ENDIAN);
      	SetAlarmModeCodec codec = new SetAlarmModeCodec();
        SetAlarmMode actual = codec.decode(buf);

      	Assert.assertEquals(expected, actual);
    }
}

