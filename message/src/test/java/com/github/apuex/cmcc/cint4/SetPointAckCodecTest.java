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

import org.junit.Assert;
import org.junit.Test;

public class SetPointAckCodecTest {
    @Test
    public void testEncode() {
        byte[] expected = new byte[]
            { (byte)0x5A, (byte)0x6B, (byte)0x7C, (byte)0x7E, (byte)0x5F, (byte)0x00, (byte)0x00, (byte)0x00
            , (byte)0x02, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0xEA, (byte)0x03, (byte)0x00, (byte)0x00
            , (byte)0x03, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x31, (byte)0x32, (byte)0x33, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x34, (byte)0x35, (byte)0x36, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x37, (byte)0x38, (byte)0x39, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x31, (byte)0x20
            , (byte)0x20, (byte)0x01, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0xD7, (byte)0xEB
            };
        TID tid = new TID();
        tid.Type = EnumType.AI;
        tid.SiteID = "123";
        tid.DeviceID = "456";
        tid.SignalID = "789";
        tid.SignalNumber = "1";
        SetPointAck v = new SetPointAck(2, tid, EnumResult.SUCCESS);
      	byte[] actual = new byte[95];
      	ByteBuffer buf = ByteBuffer.wrap(actual);
      	buf.order(ByteOrder.LITTLE_ENDIAN);

      	SetPointAckCodec codec = new SetPointAckCodec();
      	codec.encode(buf, v);

        Util.printBytes("actual", actual);

      	Assert.assertArrayEquals(expected, actual);
    }

    @Test
    public void testDecode() {
        byte[] input = new byte[] 
            { (byte)0x5A, (byte)0x6B, (byte)0x7C, (byte)0x7E, (byte)0x5F, (byte)0x00, (byte)0x00, (byte)0x00
            , (byte)0x02, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0xEA, (byte)0x03, (byte)0x00, (byte)0x00
            , (byte)0x03, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x31, (byte)0x32, (byte)0x33, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x34, (byte)0x35, (byte)0x36, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x37, (byte)0x38, (byte)0x39, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20
            , (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x20, (byte)0x31, (byte)0x20
            , (byte)0x20, (byte)0x01, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0xD7, (byte)0xEB
            };
        TID tid = new TID();
        tid.Type = EnumType.AI;
        tid.SiteID = "123";
        tid.DeviceID = "456";
        tid.SignalID = "789";
        tid.SignalNumber = "1";
        SetPointAck expected = new SetPointAck(2, tid, EnumResult.SUCCESS);
      	expected.Length = input.length;
      	expected.CRC16 = (short)0xEBD7;
      	ByteBuffer buf = ByteBuffer.wrap(input);
      	buf.order(ByteOrder.LITTLE_ENDIAN);
      	SetPointAckCodec codec = new SetPointAckCodec();
        SetPointAck actual = codec.decode(buf);

      	Assert.assertEquals(expected, actual);
    }
}

